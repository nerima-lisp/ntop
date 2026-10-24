# Architecture

## Source layout

The `src/` tree groups code by feature and by technical role. Each directory under `src/feature/` holds one feature, including `cpu`, `memory`, `gpu`, `network`, `disk`, `temperature`, `battery`, and `process`. Each feature's package file declares only the layers that feature uses. The architecture checker enforces the package placement and layer rules described below.

Package declarations are distributed with their source areas. `src/package.lisp` defines `define-ntop-package`, which adds `COMMON-LISP` to each package's use list. A use list makes exported symbols available without a package qualifier. The declarations live in `src/shared/package.lisp`, `src/platform/package.lisp`, `src/ui/package.lisp`, `src/feature/<name>/package.lisp`, `src/app/package.lisp`, and `src/cli/package.lisp`. `ntop.asd` loads the macro and these declaration files before the production source files.

| Source area | Package names | Role |
| --- | --- | --- |
| `src/shared/` | `ntop.shared.domain`, `ntop.shared.application` | Shared measurement, history, configuration, and feature collection interfaces |
| `src/feature/<name>/` | `ntop.<name>.<layer>` | Feature domain behavior, collection adapters, use cases, and presentation |
| `src/platform/<os>/` | `ntop.platform.<os>` | Operating-system APIs for Darwin and Linux |
| `src/ui/` | `ntop.ui` | Shared drawing, layout, formatting, theme, and input contracts |
| `src/app/` | `ntop.app.domain`, `ntop.app.application`, `ntop.app.infrastructure`, `ntop.app.presentation` | Application state, coordination, terminal lifecycle, and whole-screen presentation |
| `src/cli/` | `ntop.cli` | Command-line options and the process entry point |

In `src/shared/`, `src/feature/<name>/`, and `src/app/`, the checker maps a filename stem such as `domain-`, `application-`, `infrastructure-`, or `presentation-` to the corresponding package. A descriptive suffix may follow the stem. For example, `src/feature/process/domain-query-parser.lisp` belongs to `ntop.process.domain`. Files in `src/platform/<os>/` map to `ntop.platform.<os>`; files in `src/ui/` and `src/cli/` map to their area's single package.

## Domain-driven design (DDD) layers and coordination

Feature packages can use four domain-driven design (DDD) layers, and each feature declares only the layers it needs. `domain` contains feature types and calculations. `application` contains use cases where a feature has them; the process feature uses it for search and termination. `infrastructure` collects operating-system data. `presentation` turns feature state into tables, graphs, and widgets. The shared packages provide domain types, history, and application configuration. The shared application package defines collection interfaces that feature infrastructure packages implement to provide samples or report unavailable data.

The `ntop.app.*` packages coordinate features. `ntop.app.domain` holds application state and actions. `ntop.app.application` handles input and sampling and composes state updates with rendering. `ntop.app.infrastructure` owns terminal and sampler lifecycle and creates platform collectors. `ntop.app.presentation` builds the application screen. The CLI parses options and enters through `ntop.cli:main`.

## Allowed project dependencies

The architecture policy applies to references between `ntop.*` packages. It does not constrain references to external libraries.

| Source package | Allowed `ntop.*` references |
| --- | --- |
| `ntop.shared.domain` | None |
| `ntop.shared.application` | `ntop.shared.domain` |
| `ntop.<feature>.domain` | `ntop.shared.domain` |
| `ntop.<feature>.application` | The same feature's domain, `ntop.shared.domain`, `ntop.shared.application` |
| `ntop.<feature>.infrastructure` | The same feature's domain and application, both shared packages, any `ntop.platform.*` package |
| `ntop.<feature>.presentation` | The same feature's domain and application, both shared packages, `ntop.ui` |
| `ntop.platform.*` | None |
| `ntop.ui` | `ntop.shared.domain` |
| `ntop.app.*` | Any `ntop.*` package |
| `ntop.cli` | `ntop.app.*`, `ntop.shared.application` |

The checker classifies every `ntop.app.*` package as the composition area, where cross-feature references are allowed. This lets application code combine feature samples and presentation components without adding feature-to-feature dependencies.

## Architecture checks

The ASDF `ntop/test` test system includes `t/architecture/checker.lisp` and `t/architecture/checker-test.lisp`. The checker derives an expected package from each production source path and filename, verifies each source file's `in-package` declaration, and checks package imports and package-qualified references against the dependency policy.

The tests cover representative allowed and forbidden dependencies, verify the package-definition file inventory, and scan all Lisp source files under `src/` for violations. A fixture with a CPU-domain reference to the memory domain must be rejected.

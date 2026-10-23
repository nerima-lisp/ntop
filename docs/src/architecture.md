# Architecture

Production packages live in `src/package.lisp`. `define-ntop-package` gives
each one a use list containing only `COMMON-LISP`.

Layer stems are `domain`, `application`, `infrastructure`, and `presentation`;
each may be followed by a hyphen and a descriptive suffix. In `src/shared/`,
`domain` and `domain-*` files belong to `ntop.shared.domain`, while
`application` and `application-*` files belong to `ntop.shared.application`.
Under `src/feature/<name>/`, each of the four stems maps to
`ntop.<name>.<layer>`. For example,
`src/feature/process/domain-query-parser.lisp` belongs to `ntop.process.domain`.
Files under `src/app/` use the same stem rules with the `ntop.app` prefix. All
files under `src/platform/<os>/` belong to
`ntop.platform.<os>`; `src/ui/` belongs to `ntop.ui`, and `src/cli/` belongs to
`ntop.cli`.

| Source layer | Package names | Allowed project dependencies |
| --- | --- | --- |
| Shared domain | `ntop.shared.domain` | None |
| Shared application | `ntop.shared.application` | `ntop.shared.domain` |
| Feature domain | `ntop.<feature>.domain` | `ntop.shared.domain` |
| Feature application | `ntop.<feature>.application` | Same feature domain, `ntop.shared.domain`, `ntop.shared.application` |
| Feature infrastructure | `ntop.<feature>.infrastructure` | Same feature domain and application, `ntop.shared.domain`, `ntop.shared.application`, `ntop.platform.*` |
| Feature presentation | `ntop.<feature>.presentation` | Same feature domain and application, `ntop.shared.domain`, `ntop.shared.application`, `ntop.ui` |
| Platform boundary | `ntop.platform.<os>` | None |
| Shared UI | `ntop.ui` | `ntop.shared.domain` |
| Application composition root | `ntop.app.{domain,application,infrastructure,presentation}` | Any `ntop.*` package |
| CLI | `ntop.cli` | `ntop.app.*`, `ntop.shared.application` |

The table applies to both package imports and package-qualified references.
`ntop.app.*` coordinates state and collection across features; for example,
`ntop.app.domain:collection-snapshot` combines sample types from each feature.
`ntop.platform.darwin` and `ntop.platform.linux` isolate operating-system APIs.

The architecture suite checks source paths, `in-package` declarations,
package imports, and package-qualified references. Its negative fixture adds a
cross-feature domain reference that the suite must reject. The suite is part of
`ntop/test`; on macOS, run it with `python3 scripts/darwin-workflow.py test`,
and on Linux run `nix flake check`.


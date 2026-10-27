from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path


def source_registry_environment(root: Path) -> dict[str, str]:
    result = subprocess.run(
        [sys.executable, str(root / "scripts/darwin-workflow.py"), "resolve",
         "--emit-environment-json"],
        cwd=root,
        capture_output=True,
        check=False,
        text=True,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"ASDF source registry resolution failed with exit code "
            f"{result.returncode}: {result.stderr.strip()}"
        )
    environment = json.loads(result.stdout)
    registry = environment.get("CL_SOURCE_REGISTRY")
    if not isinstance(registry, str) or not registry:
        raise RuntimeError("ASDF source registry output was incomplete.")
    return {"CL_SOURCE_REGISTRY": registry}

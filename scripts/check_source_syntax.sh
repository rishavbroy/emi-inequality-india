#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

echo "=== BASH SYNTAX ==="
while IFS= read -r -d '' path; do
  bash -n "$path"
done < <(find scripts -type f -name '*.sh' -print0)

echo "=== PYTHON SYNTAX ==="
python3 - <<'PY'
import ast
from pathlib import Path

paths = sorted(Path("scripts").rglob("*.py"))
for path in paths:
    ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
print(f"Parsed {len(paths)} active Python file(s).")
PY

echo "=== JSON AND LOCKFILE CONTRACT ==="
python3 - <<'PY'
import json
from pathlib import Path

lock_path = Path("renv.lock")
settings_path = Path("renv/settings.json")
lock = json.loads(lock_path.read_text(encoding="utf-8"))
json.loads(settings_path.read_text(encoding="utf-8"))

fields = {}
current = None
for raw in Path("DESCRIPTION").read_text(encoding="utf-8").splitlines():
    if raw.startswith((" ", "\t")) and current is not None:
        fields[current] += " " + raw.strip()
        continue
    if ":" not in raw:
        current = None
        continue
    current, value = raw.split(":", 1)
    fields[current] = value.strip()

required = set()
for field in ("Depends", "Imports", "LinkingTo"):
    value = fields.get(field, "")
    for item in value.split(","):
        package = item.strip().split()[0] if item.strip() else ""
        if package and package != "R":
            required.add(package)

recorded = set(lock.get("Packages", {}))
missing = sorted(required - recorded)
if missing:
    raise SystemExit(
        "renv.lock is missing DESCRIPTION runtime dependencies: " + ", ".join(missing)
    )
print(
    f"Validated renv.lock JSON and {len(required)} DESCRIPTION runtime dependency record(s)."
)
PY

echo "=== RENV SYNCHRONIZATION ==="
Rscript - <<'RS'
status <- renv::status(dev = TRUE)
if (!isTRUE(status$synchronized)) quit(status = 1L)
cat("renv library, lockfile, and development dependencies are synchronized.\n")
RS

echo "=== R SOURCE SYNTAX ==="
Rscript - <<'RS'
files <- unique(c(
  "_targets.R",
  list.files("R", "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
  list.files("scripts", "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
  list.files("tests", "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
))
files <- sort(files[file.exists(files)])
for (path in files) parse(path)
cat("Parsed", length(files), "active R/test file(s).\n")

qmds <- sort(unique(c(
  list.files("paper", "\\.qmd$", recursive = TRUE, full.names = TRUE),
  list.files("docs", "\\.qmd$", recursive = TRUE, full.names = TRUE),
  list.files("analysis", "\\.qmd$", recursive = TRUE, full.names = TRUE),
  list.files("application-samples/cover-notes", "\\.qmd$", recursive = TRUE, full.names = TRUE)
)))
qmds <- qmds[file.exists(qmds)]
for (path in qmds) {
  output <- tempfile(fileext = ".R")
  tryCatch({
    knitr::purl(path, output = output, quiet = TRUE)
    parse(output)
  }, finally = unlink(output))
}
cat("Parsed R chunks from", length(qmds), "active QMD source(s).\n")
RS

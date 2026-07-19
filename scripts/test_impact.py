#!/usr/bin/env python3
"""List or run tests likely affected by changes since a Git revision.

This is a conservative development aid. The complete suite remains the final
check before a patch is delivered or a public build is certified.
"""

import argparse
import csv
import os
from pathlib import Path
import re
import subprocess
import sys
from collections import defaultdict

ROOT = Path(__file__).resolve().parents[1]
TEST_DIR = ROOT / "tests" / "testthat"
FUNCTION_DEF = re.compile(
    r"(?m)^([A-Za-z.][A-Za-z0-9._]*)\s*(?:<-|=)\s*function\s*\("
)
TEST_DESC = re.compile(r'test_that\(\s*"([^"]+)"')


def git(*args, check=True):
    result = subprocess.run(
        ["git", *args], cwd=ROOT, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    if check and result.returncode:
        raise SystemExit(result.stderr.strip() or f"git {' '.join(args)} failed")
    return [line for line in result.stdout.splitlines() if line]


def changed_paths(base):
    git("rev-parse", "--verify", base)
    tracked = git("diff", "--name-only", "--diff-filter=ACMRD", base, "--")
    untracked = git("ls-files", "--others", "--exclude-standard")
    return sorted(set(tracked + untracked))


def current_text(path):
    file = ROOT / path
    return file.read_text(encoding="utf-8", errors="replace") if file.is_file() else ""


def revision_text(base, path):
    result = subprocess.run(
        ["git", "show", f"{base}:{path}"], cwd=ROOT,
        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
    )
    return result.stdout.decode("utf-8", errors="replace") if result.returncode == 0 else ""


def definitions(text):
    return set(FUNCTION_DEF.findall(text))


def mentions(text, name):
    return re.search(
        rf"(?<![A-Za-z0-9._]){re.escape(name)}(?![A-Za-z0-9._])",
        text,
    ) is not None


def source_records():
    paths = list((ROOT / "R").rglob("*.R")) + list((ROOT / "scripts").rglob("*.R"))
    if (ROOT / "_targets.R").is_file():
        paths.append(ROOT / "_targets.R")
    records = []
    for path in sorted(set(paths)):
        text = path.read_text(encoding="utf-8", errors="replace")
        found = definitions(text)
        if found:
            records.append((text, found))
    return records


def caller_closure(seed):
    affected = set(seed)
    records = source_records()
    changed = True
    while changed:
        changed = False
        for text, found in records:
            if found <= affected or not any(mentions(text, name) for name in affected):
                continue
            old_size = len(affected)
            affected.update(found)
            changed = changed or len(affected) != old_size
    return affected


def path_tests(path):
    tests = set()
    if path == "tests/testthat.R":
        tests.update(test.name for test in TEST_DIR.glob("test-*.R"))
    if path.startswith("tests/testthat/test-") and path.endswith(".R"):
        tests.add(Path(path).name)
    if re.search(r"^(scripts/|_targets\.R$|Makefile$|DESCRIPTION$|renv\.lock$|\.gitignore$)", path):
        tests.add("test-public-scripts.R")
    if path.startswith("config/"):
        tests.update({"test-config.R", "test-current-output-contract.R"})
    if re.search(r"^data/metadata/(file_manifest|data_sources|checksums)\.csv$", path):
        tests.add("test-raw-file-manifest.R")
    if re.search(r"^data/metadata/(district_harmonization_crosswalk|manual_district_corrections)\.csv$", path):
        tests.update({
            "test-district-join-map.R", "test-district-keys.R",
            "test-district-tracker.R", "test-manual-district-corrections.R",
        })
    if path.startswith("paper/"):
        tests.update({"test-public-scripts.R", "test-output-tables.R", "test-output-figures.R"})
    if path.startswith(("docs/", "analysis/")):
        tests.update({"test-public-scripts.R", "test-diagnostics.R"})
    if path.startswith("application-samples/"):
        tests.add("test-public-scripts.R")
    return tests


def impacted_tests(base):
    changed = changed_paths(base)
    reasons = defaultdict(set)
    seeds = set()

    for path in changed:
        if path.endswith((".R", ".r")) and path.startswith(("R/", "scripts/")):
            seeds.update(definitions(current_text(path)))
            seeds.update(definitions(revision_text(base, path)))
        for test in path_tests(path):
            reasons[test].add(f"path: {path}")
        conventional = f"test-{Path(path).stem}.R"
        if (TEST_DIR / conventional).is_file():
            reasons[conventional].add(f"matching filename: {path}")

    affected = caller_closure(seeds) if seeds else set()
    for test in sorted(TEST_DIR.glob("test-*.R")):
        text = test.read_text(encoding="utf-8", errors="replace")
        hits = sorted(name for name in affected if mentions(text, name))
        if hits:
            label = ", ".join(hits[:6])
            if len(hits) > 6:
                label += f", +{len(hits) - 6} more"
            reasons[test.name].add(f"functions: {label}")

    selected = sorted(name for name in reasons if (TEST_DIR / name).is_file())
    if changed and not selected:
        selected = sorted(path.name for path in TEST_DIR.glob("test-*.R"))
        for name in selected:
            reasons[name].add("fallback: no narrower mapping found")
    return changed, affected, selected, reasons


def filter_expression(test_files):
    names = [re.sub(r"\.[Rr]$", "", re.sub(r"^test-", "", name)) for name in test_files]
    return "^(?:" + "|".join(re.escape(name) for name in sorted(names)) + ")$"


def write_inventory(output):
    handle = open(output, "w", newline="", encoding="utf-8") if output else sys.stdout
    try:
        writer = csv.writer(handle)
        writer.writerow(("file", "test"))
        for path in sorted(TEST_DIR.glob("test-*.R")):
            text = path.read_text(encoding="utf-8", errors="replace")
            for description in TEST_DESC.findall(text):
                writer.writerow((path.relative_to(ROOT).as_posix(), description))
    finally:
        if output:
            handle.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", default="HEAD")
    parser.add_argument("--run", action="store_true")
    parser.add_argument("--inventory", action="store_true")
    parser.add_argument("--output")
    args = parser.parse_args()
    os.chdir(ROOT)

    if args.inventory:
        write_inventory(args.output)
        return 0

    changed, affected, tests, reasons = impacted_tests(args.base)
    print(f"Compared current tree with {args.base}: {len(changed)} changed path(s).")
    for path in changed:
        print(f"  - {path}")
    print(f"Affected function closure: {len(affected)}")
    print(f"Candidate test files: {len(tests)}")
    for test in tests:
        print(f"  - tests/testthat/{test}")
        for reason in sorted(reasons[test]):
            print(f"      {reason}")

    if args.run and tests:
        env = os.environ.copy()
        env["EMI_TEST_FILTER"] = filter_expression(tests)
        return subprocess.call(["Rscript", "tests/testthat.R"], cwd=ROOT, env=env)
    if tests:
        print("Run the complete suite before delivering the patch.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

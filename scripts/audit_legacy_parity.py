#!/usr/bin/env python3
"""Audit refactored public outputs against the legacy ECON 580 draft artifacts.

This script is intentionally read-only. It checks for known parity blockers that
should be resolved on the match-legacy-outputs branch before treating the
refactored paper as result-equivalent to archive/legacy-paper-drafts.
"""
from __future__ import annotations

import csv
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LEGACY_RMD = ROOT / "archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd"
LEGACY_PDF = ROOT / "archive/legacy-paper-drafts/580-Draft-ECON-580.pdf"
CURRENT_QMD = ROOT / "paper/report.qmd"
CURRENT_PDF = ROOT / "paper/report.pdf"

FAILURES: list[str] = []
WARNINGS: list[str] = []


def rel(path: Path) -> str:
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)


def fail(msg: str) -> None:
    FAILURES.append(msg)


def warn(msg: str) -> None:
    WARNINGS.append(msg)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="ignore") if path.exists() else ""


def csv_rows(path: str) -> list[dict[str, str]]:
    p = ROOT / path
    if not p.exists():
        fail(f"Missing CSV: {path}")
        return []
    with p.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(value: str | None) -> float | None:
    if value is None:
        return None
    try:
        return float(str(value).replace(",", ""))
    except ValueError:
        return None


def pdf_pages(path: Path) -> int | None:
    if not path.exists() or shutil.which("pdfinfo") is None:
        return None
    out = subprocess.run(["pdfinfo", str(path)], text=True, capture_output=True, check=False)
    m = re.search(r"^Pages:\s+(\d+)", out.stdout, flags=re.MULTILINE)
    return int(m.group(1)) if m else None


def audit_pdf_and_qmd_shape() -> None:
    for path in (LEGACY_RMD, LEGACY_PDF, CURRENT_QMD, CURRENT_PDF):
        if not path.exists():
            fail(f"Missing expected artifact: {rel(path)}")

    legacy_pages = pdf_pages(LEGACY_PDF)
    current_pages = pdf_pages(CURRENT_PDF)
    if legacy_pages and current_pages and legacy_pages != current_pages:
        warn(f"PDF page count differs: legacy={legacy_pages}, current={current_pages}.")

    qmd = read_text(CURRENT_QMD)
    legacy = read_text(LEGACY_RMD)
    if 'author: "Rishav Roy"' in qmd and '# author: "Rishav Roy"' in legacy:
        warn("Current report prints an author, while the legacy Rmd commented the author out.")
    if "\\usepackage{setspace}\\doublespacing" in legacy and "doublespacing" not in qmd:
        warn("Current Quarto YAML does not reproduce the legacy double-spacing header include.")
    if "Final map figures are withheld" in qmd:
        warn("Current report withholds map figures that the legacy paper rendered as Figures 2 and 3.")


def audit_selection_tables() -> None:
    quant = csv_rows("outputs/tables/main/sum_tbl_probit_quant.csv")
    by_var = {r.get("Variable") or r.get("variable"): r for r in quant}
    if by_var.get("AGE", {}).get("n") not in {"127246", "127246.0"}:
        warn(f"Selection AGE N differs from legacy 127246: current={by_var.get('AGE', {}).get('n')}.")
    if "ENROLLMENT_COST" not in by_var and "Enrollment cost (Rs.)" not in by_var:
        fail("Selection numeric summary lacks the legacy Enrollment cost row.")
    if "TOTAL" in by_var:
        fail("Selection numeric summary contains raw TOTAL instead of legacy ENROLLMENT_COST.")
    required_dmeans = {
        "dmean_num_TUTION_FEE_WAIVED",
        "dmean_num_RECD_SCHOLARSHIP_STIPEND",
        "dmean_num_RECD_STATIONERY",
        "dmean_num_MID_DAY_MEAL_ETC_RECD",
        "dmean_num_ENROLLMENT_COST",
    }
    missing_dmeans = sorted(required_dmeans - set(by_var))
    if missing_dmeans:
        fail("Selection numeric summary is missing legacy district aggregates: " + ", ".join(missing_dmeans))

    cat = csv_rows("outputs/tables/main/sum_tbl_probit_cat.csv")
    cat_vars = {r.get("Variable") or r.get("variable") for r in cat}
    if "father_educ" not in cat_vars and "Father's education" not in cat_vars:
        fail("Selection categorical summary lacks the legacy father's education row.")

    mfx = csv_rows("outputs/tables/main/probit_mfx.csv")
    terms = [r.get("Term") or r.get("term") or "" for r in mfx]
    if not any("Father" in t or "father_educ" in t for t in terms):
        fail("AME table lacks legacy father's education marginal effects.")
    if not any("Enrollment cost" in t or "dmean_num_ENROLLMENT_COST" in t for t in terms):
        fail("AME table lacks the legacy enrollment-cost marginal effect.")
    if any(t in {"RELIGION", "SOCIAL_GROUP", "DIST_FROM_NEAREST_PRIMARY_CLASS"} for t in terms):
        fail("AME table exposes raw variable names instead of legacy contrast labels.")


def audit_iv_tables() -> None:
    summary = csv_rows("outputs/tables/main/sum_tbl_iv.csv")
    by_var = {r.get("Variable") or r.get("variable"): r for r in summary}
    emie = by_var.get("emie_2007") or by_var.get("EMIE")
    if emie:
        n = fnum(emie.get("n") or emie.get("N"))
        mean = fnum(emie.get("Mean") or emie.get("mean"))
        maxv = fnum(emie.get("Max") or emie.get("max"))
        if n is not None and int(round(n)) != 454:
            fail(f"IV EMIE summary N is {n:g}; legacy Table 4 reports 454.")
        if mean is not None and abs(mean - 16.28) > 0.05:
            fail(f"IV EMIE mean is {mean:.2f}; legacy Table 4 reports 16.28.")
        if maxv is not None and abs(maxv - 100.0) > 0.05:
            fail(f"IV EMIE max is {maxv:.2f}; legacy Table 4 reports 100.00.")
    else:
        fail("IV summary table lacks EMIE/emie_2007 row.")

    cons = csv_rows("outputs/tables/main/cons_iv.csv")
    emie_rows = [r for r in cons if (r.get("Term") or r.get("term")) in {"EMIE (fitted)", "emie_2007", "EMIE"}]
    if emie_rows:
        est = fnum(emie_rows[0].get("Estimate") or emie_rows[0].get("estimate"))
        if est is not None and abs(est - 0.201) > 0.01:
            fail(f"Second-stage EMIE estimate is {est:.3f}; legacy Table 6 reports 0.201.")
    else:
        fail("Second-stage IV table has no EMIE row.")

    fs = csv_rows("outputs/tables/main/fs_cons.csv")
    if len(fs) > 5:
        fail(f"First-stage public table has {len(fs)} rows; legacy Table 5 has one regression column, not one row per coefficient repeated.")
    if fs and not any((r.get("Term") or r.get("term") or "") in {"Linguistic distance", "wavg_ling_degrees"} for r in fs):
        fail("First-stage public table lacks the legacy linguistic-distance coefficient row.")


def audit_source_flags() -> None:
    source_checks = {
        "R/districts/fuzzy_join_districts.R": ["match_status = \"key_only\"", "merge_dfs_into_tracker <- function(...)"],
        "R/measures/build_district_panel.R": ["merge(out, measures_2017, by = c(\"state_std\", \"district_std\")"],
        "R/selection/estimate_selection_probit.R": ["dmean_num_RECD_TXT_BOOKS", "father_educ"],
        "R/selection/build_selection_data.R": ["collapse_to_unique_key", "father_proxy"],
    }
    for path, needles in source_checks.items():
        txt = read_text(ROOT / path)
        for needle in needles:
            if needle not in txt:
                continue
            if needle == "match_status = \"key_only\"":
                fail("fuzzy_join_districts() still emits key_only rows instead of legacy fuzzy/manual tracker matches.")
            elif needle == "merge_dfs_into_tracker <- function(...)":
                fail("merge_dfs_into_tracker() is still a stub, not the legacy cascading fuzzy join.")
            elif needle.startswith("merge(out, measures_2017"):
                fail("build_district_panel() still directly merges 2007/2017/2001 by standardized state/district keys instead of tracker rows.")
            elif needle == "dmean_num_RECD_TXT_BOOKS" and "dmean_num_ENROLLMENT_COST" not in txt:
                fail("estimate_selection_probit() still omits legacy district-average schooling inputs.")
            elif needle == "father_educ" and "father_educ" not in txt:
                fail("estimate_selection_probit() lacks father's education covariates.")
            elif needle == "collapse_to_unique_key":
                warn("build_selection_data() collapses B5/B6 by PID only; legacy joined by PID plus district/household/sampling identifiers.")
            elif needle == "father_proxy" and "father_proxy" not in txt:
                fail("build_selection_data() lacks the legacy father-education proxy construction.")


def main() -> int:
    audit_pdf_and_qmd_shape()
    audit_selection_tables()
    audit_iv_tables()
    audit_source_flags()

    print("Legacy parity audit")
    print(f"  failures: {len(FAILURES)}")
    print(f"  warnings: {len(WARNINGS)}")
    if FAILURES:
        print("\nFailures:")
        for item in FAILURES:
            print(f"- {item}")
    if WARNINGS:
        print("\nWarnings:")
        for item in WARNINGS:
            print(f"- {item}")
    return 1 if FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())

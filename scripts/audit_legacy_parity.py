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
    cleaned = re.sub(r"[^0-9eE+\-.]", "", str(value).replace(",", ""))
    if cleaned in {"", ".", "-", "+"}:
        return None
    try:
        return float(cleaned)
    except ValueError:
        return None


def first_present(row: dict[str, str], names: tuple[str, ...]) -> str | None:
    for name in names:
        if name in row and row[name] not in {None, ""}:
            return row[name]
    lower = {k.lower(): v for k, v in row.items()}
    for name in names:
        if name.lower() in lower and lower[name.lower()] not in {None, ""}:
            return lower[name.lower()]
    return None


def row_key(row: dict[str, str]) -> str:
    return first_present(row, ("var", "Variable", "variable", "Term", "term", "label")) or ""


def by_any_key(rows: list[dict[str, str]]) -> dict[str, dict[str, str]]:
    out: dict[str, dict[str, str]] = {}
    for row in rows:
        for key in {row_key(row), first_present(row, ("label", "Label")) or ""}:
            if key:
                out[key] = row
    return out


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
    selection_n = csv_rows("outputs/tables/main/selection_n.csv")
    if selection_n:
        n = fnum(first_present(selection_n[0], ("n", "N")))
        if n is not None and int(round(n)) != 127246:
            fail(f"Selection-data row count is {n:g}; legacy selection_df has 127246 rows.")

    quant = csv_rows("outputs/tables/main/sum_tbl_probit_quant.csv")
    by_var = by_any_key(quant)
    age_row = by_var.get("AGE") or by_var.get("Age")
    age_n = fnum(first_present(age_row or {}, ("N", "n")))
    if age_n is not None and int(round(age_n)) != 127246:
        fail(f"Selection AGE N differs from legacy 127246: current={age_n:g}.")
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
    cat_vars = {row_key(r) for r in cat} | {first_present(r, ("label", "Label")) or "" for r in cat}
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
    by_var = by_any_key(summary)
    emie = by_var.get("emie_2007") or by_var.get("EMIE")
    if emie:
        n = fnum(first_present(emie, ("N", "n")))
        mean = fnum(first_present(emie, ("Mean", "mean")))
        maxv = fnum(first_present(emie, ("Max", "max")))
        if n is not None and int(round(n)) != 454:
            fail(f"IV EMIE summary N is {n:g}; legacy Table 4 reports 454.")
        if mean is not None and abs(mean - 16.28) > 0.05:
            fail(f"IV EMIE mean is {mean:.2f}; legacy Table 4 reports 16.28.")
        if maxv is not None and abs(maxv - 100.0) > 0.05:
            fail(f"IV EMIE max is {maxv:.2f}; legacy Table 4 reports 100.00.")
    else:
        fail("IV summary table lacks EMIE/emie_2007 row.")

    expected_summary = {
        "npeople_0708": (1979019.0, 5000.0, "mean"),
        "consumption_0708": (861.53, 0.5, "mean"),
        "dependency_ratio": (58.24, 0.5, "mean"),
        "pct_fem_head": (19.49, 0.5, "mean"),
    }
    for var, (expected, tol, stat) in expected_summary.items():
        row = by_var.get(var)
        if not row:
            fail(f"IV summary table lacks {var} row.")
            continue
        got = fnum(first_present(row, ("Mean", "mean")))
        if got is not None and abs(got - expected) > tol:
            fail(f"IV {var} mean is {got:.2f}; legacy reports {expected:.2f}.")

    cons = csv_rows("outputs/tables/main/cons_iv.csv")
    cons_by_term = {row_key(r): r for r in cons}
    expected_second_stage = {
        "EMIE": 0.201,
        "Consumption, 2007-08": -0.104,
        "Constant": 457.536,
    }
    for term, expected in expected_second_stage.items():
        row = cons_by_term.get(term)
        if not row:
            fail(f"Second-stage IV table has no {term} row.")
            continue
        est = fnum(first_present(row, ("Estimate", "estimate")))
        tol = 0.02 if term != "Constant" else 1.0
        if est is not None and abs(est - expected) > tol:
            fail(f"Second-stage {term} estimate is {est:.3f}; legacy Table 6 reports {expected:.3f}.")

    fs_raw = csv_rows("outputs/tables/main/first_stage.csv")
    if fs_raw and any(re.fullmatch(r"\d+", row_key(r) or "") for r in fs_raw):
        fail("First-stage raw table still has numeric coefficient terms; coefficient names were lost.")
    fs = csv_rows("outputs/tables/main/fs_cons.csv")
    if len(fs) > 5:
        fail(f"First-stage public table has {len(fs)} rows; legacy Table 5 has one regression column, not one row per coefficient repeated.")
    fs_by_term = {row_key(r): r for r in fs}
    ling = fs_by_term.get("Linguistic distance") or fs_by_term.get("wavg_ling_degrees")
    if not ling:
        fail("First-stage public table lacks the legacy linguistic-distance coefficient row.")
    else:
        est = fnum(first_present(ling, ("Estimate", "estimate")))
        if est is not None and abs(est - 2.945) > 0.05:
            fail(f"First-stage linguistic-distance estimate is {est:.3f}; legacy Table 5 reports 2.945.")
    f_row = fs_by_term.get("Instrument's F-Statistic") or fs_by_term.get("First-stage F")
    if not f_row:
        fail("First-stage public table lacks the legacy instrument F-statistic row.")
    else:
        fstat = fnum(first_present(f_row, ("Estimate", "estimate")))
        if fstat is not None and abs(fstat - 39.20) > 0.25:
            fail(f"First-stage instrument F-statistic is {fstat:.2f}; legacy Table 5 reports 39.20.")


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
            elif needle == "dmean_num_RECD_TXT_BOOKS" and "dmean_num_ENROLLMENT_COST" not in txt:
                fail("estimate_selection_probit() still omits legacy district-average schooling inputs.")
            elif needle == "father_educ" and "father_educ" not in txt:
                fail("estimate_selection_probit() lacks father's education covariates.")
            elif needle == "legacy_household_key" and "legacy_household_key" not in txt:
                fail("build_selection_data() lacks full household keys to prevent father-proxy many-to-many joins.")
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

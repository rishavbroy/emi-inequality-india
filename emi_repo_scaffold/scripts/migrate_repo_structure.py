#!/usr/bin/env python3
"""Migrate the local EMI project folder into the agreed repo structure.

Default mode is a dry run. Re-run with --execute to perform moves/deletions.
The script is intentionally conservative: it moves/archives source files, creates
folders, and removes only generated caches/render byproducts and operating-system
metadata.
"""
from __future__ import annotations
import argparse, shutil, os
from pathlib import Path

DIRS = [
    "config", "R/io", "R/clean", "R/districts", "R/measures", "R/selection", "R/iv", "R/diagnostics", "R/output", "R/samples",
    "scripts", "data/raw", "data/raw_future", "data/interim", "data/processed", "data/metadata", "assets/ilo_figures",
    "paper/sections", "paper/output", "docs/roadmap", "analysis/diagnostics", "analysis/exploratory",
    "outputs/figures/main", "outputs/figures/appendix", "outputs/figures/diagnostics", "outputs/tables/main", "outputs/tables/appendix", "outputs/tables/diagnostics", "outputs/diagnostics/objects", "outputs/diagnostics/reports", "outputs/diagnostics/logs",
    "application-samples/cover-notes/writing", "application-samples/cover-notes/coding", "application-samples/specs", "application-samples/output", "application-samples/templates",
    "presentations/2025_uw_undergrad_symposium/assets", "tests/testthat", "archive/623-version", "archive/application-sample-sources", "archive/application-sample-legacy-pdfs", "archive/legacy-paper-drafts", "archive/legacy-rendered-artifacts", "papers"
]

MOVES = {
    "README.txt": "archive/legacy-rendered-artifacts/README.txt",
    "English Education Economic Returns.bib": "paper/references.bib",
    "580-Draft-ECON-580.Rmd": "archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd",
    "580-Draft-ECON-580.pdf": "archive/legacy-paper-drafts/580-Draft-ECON-580.pdf",
    "OLD 623 Final Research Paper.Rmd": "archive/623-version/OLD 623 Final Research Paper.Rmd",
    "623-Final-Research-Paper.pdf": "archive/623-version/623-Final-Research-Paper.pdf",
    "COMPACTED NOTES Research Paper ECON 623.docx": "archive/COMPACTED NOTES Research Paper ECON 623.docx",
    "THOROUGH NOTES Research Paper ECON 623.docx": "archive/THOROUGH NOTES Research Paper ECON 623.docx",
    "TO-DO Research Paper ECON 623.docx": "docs/roadmap/TO-DO Research Paper ECON 623.docx",
    "RishavRoy_CodingSample.Rmd": "archive/application-sample-sources/RishavRoy_CodingSample.Rmd",
    "RishavRoy_WritingSample_CodeExcerpts.Rmd": "archive/application-sample-sources/RishavRoy_WritingSample_CodeExcerpts.Rmd",
    "RishavRoy_WritingSample_Paper.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Paper.pdf",
    "RishavRoy_WritingSample_Code.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Code.pdf",
    "RishavRoy_WritingSample_Code47pg.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Code47pg.pdf",
    "RishavRoy_WritingSample_Code25pg.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Code25pg.pdf",
    "RishavRoy_WritingSample_Paper5pg.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Paper5pg.pdf",
    "RishavRoy_WritingSample_Paper10pg.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Paper10pg.pdf",
    "Undergraduate Symposium Presentation 2025.pdf": "presentations/2025_uw_undergrad_symposium/slides.pdf",
    "Undergraduate Symposium Presentation 2025.pptx": "presentations/2025_uw_undergrad_symposium/slides.pptx",
    "Average Monthly Real Earnings Over Time - Total.png": "assets/ilo_figures/average_monthly_real_earnings_total.png",
    "LFPR WPR and Unemployment for All Over Time.png": "assets/ilo_figures/lfpr_wpr_unemployment_all.png",
    "Unemployment Rate By General Education.png": "assets/ilo_figures/unemployment_rate_by_general_education.png",
    "Average Monthly Real Earnings Over Time - Youth Vs. Adult.png": "assets/ilo_figures/average_monthly_real_earnings_youth_vs_adult.png",
    "Labor Force Participation Rate by Age.png": "assets/ilo_figures/labor_force_participation_rate_by_age.png",
    "map_EMIE.png": "outputs/figures/main/map_emi_exposure.png",
    "map_consumption.png": "outputs/figures/main/map_consumption_growth.png",
    "map_edu.png": "outputs/figures/main/map_education.png",
    "map_ling_dist.png": "outputs/figures/main/map_linguistic_distance.png",
    "map_pucca.png": "outputs/figures/main/map_pucca.png",
    "map_region.png": "outputs/figures/main/map_region.png",
    "collage1_map.png": "outputs/figures/main/collage_main_maps.png",
    "collage2_map.png": "outputs/figures/main/collage_iv_region_maps.png",
    "ILO-fig.png": "outputs/figures/main/fig_ilo_trends.png",
}

RAW_DIRS = [
    "NSS 2007-08 Participation and Expenditure in Education 64th Round",
    "NSS 2007-08 Household Consumer Expenditure Survey 64th Round",
    "NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018",
    "Indian Census 2001",
    "District Boundaries 2020",
    "District Changes Data",
]

FUTURE_DIRS = [
    "DISE 2005-2018", "EC 2005 Fifth Economic Census", "EC 2013-2014 Sixth Economic Census",
    "UDISE+ 2018-22 Unified District Information System for Education Plus",
    "ASER 2007-18 HH + School Data_2007-2018-20240412T214004Z-001",
    "ASI 2007-08 Annual Survey of Industries", "ASI 2017-18 Annual Survey of Industries",
    "NAS 2017-2021 National Achievement Survey Learning Outcomes Data",
    "NSS 2007-08 Employment, Unemployment and Migration Survey 64th Round",
    "NSS 2009-10 Employment and Unemployment 66th Round July 2009 - June 2010",
    "NSS 2014 Education 71st Round Data", "TUS 2019 Time Use Survey January 2019-December 2019",
]

DELETE_PATTERNS = ["*_cache", "*_files", "*.nb.html", "*.log", "*.aux", "*.out", "*.toc", "*.synctex.gz", "*.fls", "*.fdb_latexmk", ".RData", ".RDataTmp", ".Rhistory", ".DS_Store", "__MACOSX", "autorun.inf"]

def action(msg): print(msg)

def move(root: Path, src: str, dst: str, execute: bool):
    s = root / src
    if not s.exists(): return
    d = root / dst
    action(f"MOVE {s} -> {d}")
    if execute:
        d.parent.mkdir(parents=True, exist_ok=True)
        if d.exists():
            action(f"  SKIP: destination exists: {d}")
        else:
            shutil.move(str(s), str(d))

def delete_path(p: Path, execute: bool):
    action(f"REMOVE {p}")
    if execute:
        if p.is_dir(): shutil.rmtree(p)
        elif p.exists(): p.unlink()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=".")
    ap.add_argument("--execute", action="store_true")
    args = ap.parse_args()
    root = Path(args.root).resolve()
    for d in DIRS:
        path = root / d
        action(f"MKDIR {path}")
        if args.execute: path.mkdir(parents=True, exist_ok=True)
    for src, dst in MOVES.items(): move(root, src, dst, args.execute)
    # Assets currently inside 580 Paper Images.
    for src, dst in list(MOVES.items()):
        p = root / "580 Paper Images" / src
        if p.exists():
            move(root, str(Path("580 Paper Images") / src), dst, args.execute)
    for d in RAW_DIRS:
        move(root, d, str(Path("data/raw") / d), args.execute)
    for d in FUTURE_DIRS:
        move(root, d, str(Path("data/raw_future") / d), args.execute)
    # Delete generated caches/render byproducts and metadata files.
    for pattern in DELETE_PATTERNS:
        for p in root.glob(pattern):
            delete_path(p, args.execute)
    print("\nDone. This was a dry run." if not args.execute else "\nDone. Changes executed.")

if __name__ == "__main__": main()

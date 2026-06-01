#!/usr/bin/env python3
"""Migrate the local EMI project folder into the agreed repo structure.

Default mode is a dry run. Re-run with --execute to perform moves/deletions.
The script is conservative: it creates folders, moves/archives known source files,
moves data into ignored raw/raw_future locations, removes generated caches/render
byproducts, and prints any remaining top-level items outside the intended repo
structure.
"""
from __future__ import annotations

import argparse
import fnmatch
import shutil
import hashlib
from pathlib import Path
from typing import Iterable

DIRS = [
    "config",
    "R/io", "R/clean", "R/districts", "R/measures", "R/selection", "R/iv", "R/diagnostics", "R/output", "R/samples",
    "scripts",
    "data/raw", "data/raw_future", "data/interim", "data/processed", "data/metadata",
    "assets/ilo_figures",
    "paper/sections", "paper/output",
    "docs/roadmap", "docs/feedback", "docs/admin",
    "analysis/diagnostics", "analysis/exploratory",
    "outputs/figures/main", "outputs/figures/appendix", "outputs/figures/diagnostics",
    "outputs/tables/main", "outputs/tables/appendix", "outputs/tables/diagnostics",
    "outputs/diagnostics/objects", "outputs/diagnostics/reports", "outputs/diagnostics/logs",
    "application-samples/cover-notes/writing", "application-samples/cover-notes/coding",
    "application-samples/specs", "application-samples/output", "application-samples/templates",
    "presentations/2025_uw_undergrad_symposium/assets",
    "tests/testthat",
    "archive/623-version", "archive/application-sample-sources", "archive/application-sample-legacy-pdfs",
    "archive/legacy-paper-drafts", "archive/legacy-rendered-artifacts", "archive/legacy-rmd-chunks",
    "archive/implementation-bundles",
    "relevant-literature",
]

ALLOWLIST_TOP_LEVEL = {
    ".git", ".gitignore", ".Rprofile", "README.md", "LICENSE", "Makefile", "_quarto.yml", "_targets.R", "renv.lock",
    "renv", "config", "R", "scripts", "data", "assets", "paper", "docs", "analysis", "outputs",
    "application-samples", "presentations", "tests", "archive", "relevant-literature",
}

# Exact top-level moves.
MOVES = {
    "README.txt": "archive/legacy-rendered-artifacts/README.txt",
    "IMPLEMENTATION_NOTES.md": "docs/admin/IMPLEMENTATION_NOTES.md",
    "English Education Economic Returns.bib": "paper/references.bib",
    "Citations Import for Zotero.bib": "archive/Citations Import for Zotero.bib",
    "580-Draft-ECON-580.Rmd": "archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd",
    "580-Draft-ECON-580.pdf": "archive/legacy-paper-drafts/580-Draft-ECON-580.pdf",
    "580-Draft-ECON-580.tex": "archive/legacy-rendered-artifacts/580-Draft-ECON-580.tex",
    "OLD 623 Final Research Paper.Rmd": "archive/623-version/OLD 623 Final Research Paper.Rmd",
    "623-Final-Research-Paper.pdf": "archive/623-version/623-Final-Research-Paper.pdf",
    "Econ623-Instructions research paper.pdf": "archive/Econ623-Instructions research paper.pdf",
    "COMPACTED NOTES Research Paper ECON 623.docx": "archive/COMPACTED NOTES Research Paper ECON 623.docx",
    "THOROUGH NOTES Research Paper ECON 623.docx": "archive/THOROUGH NOTES Research Paper ECON 623.docx",
    "TO-DO Research Paper ECON 623.docx": "docs/roadmap/TO-DO Research Paper ECON 623.docx",
    "file_list.txt": "docs/admin/file_list_initial.txt",
    "schechter-feedback.pdf": "docs/feedback/schechter-feedback.pdf",
    "RishavRoy_CodingSample.Rmd": "archive/application-sample-sources/RishavRoy_CodingSample.Rmd",
    "RishavRoy_WritingSample_CodeExcerpts.Rmd": "archive/application-sample-sources/RishavRoy_WritingSample_CodeExcerpts.Rmd",
    "RishavRoy_CodingSample.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_CodingSample.pdf",
    "RishavRoy_CodingSample.tex": "archive/application-sample-sources/RishavRoy_CodingSample.tex",
    "RishavRoy_WritingSample_Paper.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Paper.pdf",
    "RishavRoy_WritingSample_Code.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Code.pdf",
    "RishavRoy_WritingSample_Code47pg.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_Code47pg.pdf",
    "RishavRoy_WritingSample_CodeExcerpts.pdf": "archive/application-sample-legacy-pdfs/RishavRoy_WritingSample_CodeExcerpts.pdf",
    "RishavRoy_WritingSample_CodeExcerpts.tex": "archive/application-sample-sources/RishavRoy_WritingSample_CodeExcerpts.tex",
    "Rishav_Roy_Policy Impacts Writing Sample.pdf.pdf": "archive/application-sample-legacy-pdfs/Rishav_Roy_Policy Impacts Writing Sample.pdf.pdf",
    "Undergraduate Symposium Presentation 2025.pdf": "presentations/2025_uw_undergrad_symposium/slides.pdf",
    "Undergraduate Symposium Presentation 2025.pptx": "presentations/2025_uw_undergrad_symposium/slides.pptx",
    "district_tracker.csv": "data/processed/district_tracker_legacy.csv",
    "State Codes from NSS 71st Round.pdf": "data/raw_future/reference_docs/State Codes from NSS 71st Round.pdf",
    "State and District Codes from NSS 68th Round.pdf": "data/raw_future/reference_docs/State and District Codes from NSS 68th Round.pdf",
}

# Glob moves handle variant filenames such as files copied from previous applications.
GLOB_MOVES = [
    # Preserve 623 source drafts/final PDFs, but do not archive generated caches or HTML byproducts.
    ("623*.Rmd", "archive/623-version"),
    ("623*.pdf", "archive/623-version"),
    ("RishavRoy_WritingSample_Paper5pg*.pdf", "archive/application-sample-legacy-pdfs"),
    ("RishavRoy_WritingSample_Paper10pg*.pdf", "archive/application-sample-legacy-pdfs"),
    ("RishavRoy_WritingSample_Code25pg*.pdf", "archive/application-sample-legacy-pdfs"),
    ("emi_repo_implementation_bundle*", "archive/implementation-bundles"),
]

RAW_DIRS = [
    "NSS 2007-08 Participation and Expenditure in Education 64th Round",
    "NSS 2007-08 Household Consumer Expenditure Survey 64th Round",
    "NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018",
    "Indian Census 2001",
    "District Boundaries 2020",
    "District Changes Data",
]

FUTURE_DIRS = [
    "DISE 2005-2018",
    "District Report Cards additional-DISE-state-wise-raw-data",
    "EC 2005 Fifth Economic Census", "EC 2013-2014 Sixth Economic Census",
    "UDISE+ 2018-22 Unified District Information System for Education Plus",
    "Indian Census 2011",
    "Archived District Reports",
    "ASER 2007-18 HH + School Data_2007-2018-20240412T214004Z-001",
    "ASI 2007-08 Annual Survey of Industries", "ASI 2017-18 Annual Survey of Industries",
    "NAS 2017-2021 National Achievement Survey Learning Outcomes Data",
    "NSS 2007-08 Employment, Unemployment and Migration Survey 64th Round",
    "NSS 2009-10 Employment and Unemployment 66th Round July 2009 - June 2010",
    "NSS 2014 Education 71st Round Data",
    "PLFS 2017-18 Periodic Labour Force Survey July 2017 - June 2018",
    "PLFS 2018-19 Periodic Labour Force Survey July 2018 - June 2019",
    "PLFS 2020-21 Migration in India Unit Level Data of Periodic Labour Force Survey July 2020-June 2021",
    "PLFS 2023 Periodic Labour Force Survey",
    "TUS 2019 Time Use Survey January 2019-December 2019",
]

FUTURE_FILES = [
    "DISE 2005-2018.zip",
    "District Report Cards 2003 Raw Data.ZIP",
    "District Report Cards 2003 Raw Data.xls",
    "District Report Cards 2006-07 Raw Data.xls",
    "District Report Cards additional-DISE-state-wise-raw-data.zip",
    "UDISE 2007-08 DRC Vol-I.pdf",
    "UDISE 2007-08 DRC Vol-II.pdf",
    "UDISE 2007-08 District Report Card Microdata.xls",
    "udise_schools.csv",
    "udise_schools.zip",
    "GDL-SubnatHDI.csv",
    "India Human Development Survey-II IHDS-II 2011-12 Data.zip",
    "ASER 2007-18 HH + School Data_2007-2018-20240412T214004Z-001.zip",
    "ASER 2022-24 Household+School Data 2022-20240412T214136Z-001.zip",
    "ASER Data Enquiry Form_Researchers.doc",
    "ASER Data Enquiry Research Brief.docx",
]

LITERATURE_PDFS = [
    "Creating Long Panels Using Census Data 1961-2001.pdf",
    "India Economic Survey Report 2022-2023.pdf",
    "Kumar et al 2019 Right to Education Act Universalisation or Entrenched Exclusion.pdf",
    "Madheswaran and Singhari 2016 Social exclusion and caste discrimination.pdf",
    "Mediating inequalities Exploring English-medium instruction in a suburban Indian village school.pdf",
    "Preview of Bedi 2019 English Language in India A Dichotomy between Economic Growth and Inclusive Growth.pdf",
    "Preview of The Routledge International Handbook of Language Education Policy in Asia.pdf",
    "Refeque and Azad 2022 How do linguistic and technical skills affect earnings in India.pdf",
    "Shastry 2012 Human Capital Response to Globalization .pdf",
]

ILO_IMAGE_MOVES = {
    "Average Monthly Real Earnings Over Time - Total.png": "assets/ilo_figures/average_monthly_real_earnings_total.png",
    "LFPR WPR and Unemployment for All Over Time.png": "assets/ilo_figures/lfpr_wpr_unemployment_all.png",
    "Unemployment Rate By General Education.png": "assets/ilo_figures/unemployment_rate_by_general_education.png",
    "Average Monthly Real Earnings Over Time - Youth Vs. Adult.png": "assets/ilo_figures/average_monthly_real_earnings_youth_vs_adult.png",
    "Labor Force Participation Rate by Age.png": "assets/ilo_figures/labor_force_participation_rate_by_age.png",
}

OUTPUT_MOVES = {
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

DELETE_PATTERNS = [
    "*_cache", "*_files", "*.nb.html", "*.log", "*.aux", "*.out", "*.toc", "*.synctex.gz",
    "*.fls", "*.fdb_latexmk", ".RData", ".RDataTmp", ".Rhistory", ".DS_Store", "__MACOSX", "autorun.inf",
    ".dkm", ".dta", ".ind", ".missRecode", ".nsf",
]

PLACEHOLDER_MARKERS = [
    "placeholder",
    "This file is a placeholder",
    "TODO: replace",
    "Move English Education Economic Returns.bib here",
]


def action(msg: str) -> None:
    print(msg)


def is_placeholder_file(path: Path) -> bool:
    if not path.exists() or not path.is_file():
        return False
    if path.stat().st_size > 200_000:
        return False
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return False
    lowered = text.lower()
    return any(marker.lower() in lowered for marker in PLACEHOLDER_MARKERS)


def file_hash(path: Path, chunk_size: int = 1024 * 1024) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(chunk_size), b""):
            h.update(chunk)
    return h.hexdigest()


def files_identical(a: Path, b: Path) -> bool:
    if not (a.is_file() and b.is_file()):
        return False
    try:
        if a.stat().st_size != b.stat().st_size:
            return False
        return file_hash(a) == file_hash(b)
    except OSError:
        return False


def safe_destination(dst: Path) -> Path:
    if not dst.exists():
        return dst
    stem, suffix = dst.stem, dst.suffix
    for i in range(1, 10_000):
        candidate = dst.with_name(f"{stem}__conflict_{i}{suffix}")
        if not candidate.exists():
            return candidate
    raise RuntimeError(f"Could not find non-conflicting destination for {dst}")


def move(root: Path, src: str | Path, dst: str | Path, execute: bool, overwrite_placeholder: bool = True) -> None:
    s = root / src
    if not s.exists():
        return
    d = root / dst
    if d.exists() and overwrite_placeholder and is_placeholder_file(d):
        action(f"REPLACE PLACEHOLDER {d}")
        if execute:
            d.unlink()
    if d.exists() and files_identical(s, d):
        action(f"REMOVE {s}  [identical to existing {d}]")
        if execute:
            s.unlink()
        return
    if d.exists():
        d2 = safe_destination(d)
        action(f"MOVE {s} -> {d2}  [destination existed and differed; using conflict-safe name]")
        d = d2
    else:
        action(f"MOVE {s} -> {d}")
    if execute:
        d.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(s), str(d))


def move_if_exists(root: Path, name: str, dest_dir: str, execute: bool) -> None:
    move(root, name, Path(dest_dir) / name, execute)


def delete_path(p: Path, execute: bool) -> None:
    action(f"REMOVE {p}")
    if execute:
        if p.is_dir():
            shutil.rmtree(p)
        elif p.exists():
            p.unlink()


def delete_matching(root: Path, patterns: Iterable[str], execute: bool) -> None:
    for pattern in patterns:
        for p in root.glob(pattern):
            # Avoid deleting files already inside archive or app-sample cover-note source templates.
            try:
                rel = p.relative_to(root)
            except ValueError:
                rel = p
            if str(rel).startswith(("archive/", "application-samples/cover-notes/", "application-samples/templates/")):
                continue
            delete_path(p, execute)


def move_580_paper_images(root: Path, execute: bool) -> None:
    folder = root / "580 Paper Images"
    if not folder.exists():
        return
    for src, dst in ILO_IMAGE_MOVES.items():
        move(root, Path("580 Paper Images") / src, dst, execute)
    # Anything left in the legacy image folder is preserved with the old paper draft.
    if folder.exists() and any(folder.iterdir()):
        move(root, "580 Paper Images", "archive/legacy-rendered-artifacts/580 Paper Images", execute)
    elif folder.exists():
        delete_path(folder, execute)


def move_by_globs(root: Path, execute: bool) -> None:
    for pattern, dest_dir in GLOB_MOVES:
        for p in list(root.glob(pattern)):
            if p.name in MOVES:
                continue
            move(root, p.name, Path(dest_dir) / p.name, execute)


def merge_directory_contents(root: Path, src_dir: str | Path, dst_dir: str | Path, execute: bool) -> None:
    src_path = root / src_dir
    dst_path = root / dst_dir
    if not src_path.exists() or not src_path.is_dir():
        return
    for child in sorted(src_path.iterdir(), key=lambda p: p.name.lower()):
        move(root, child.relative_to(root), dst_path / child.name, execute)
    # Remove the now-empty source directory after moving its contents.
    # In dry-run mode, print the intended directory cleanup even though the children
    # have not actually moved yet.
    if not execute:
        action(f"REMOVE {src_path}  [after moving its contents]")
    elif src_path.exists():
        try:
            is_empty = not any(src_path.iterdir())
        except FileNotFoundError:
            is_empty = False
        if is_empty:
            delete_path(src_path, execute)


def move_top_level_pdfs_to_papers(root: Path, execute: bool) -> None:
    for name in LITERATURE_PDFS:
        move_if_exists(root, name, "relevant-literature", execute)
    merge_directory_contents(root, "Papers", "relevant-literature", execute)


def report_remaining_top_level(root: Path) -> None:
    remaining = []
    for p in sorted(root.iterdir(), key=lambda x: x.name.lower()):
        if p.name not in ALLOWLIST_TOP_LEVEL:
            remaining.append(p.name + ("/" if p.is_dir() else ""))
    print("\nTop-level cleanliness report")
    print("----------------------------")
    if not remaining:
        print("No unexpected top-level items detected.")
    else:
        print("Unexpected top-level items remain:")
        for item in remaining:
            print(f"  - {item}")
        print("Review these manually or extend migrate_repo_structure.py if they are expected in your local folder.")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=".", help="Project root to migrate")
    ap.add_argument("--execute", action="store_true", help="Perform moves/removals. Omit for dry run.")
    args = ap.parse_args()
    root = Path(args.root).resolve()

    print(f"Project root: {root}")
    print("Mode: EXECUTE" if args.execute else "Mode: DRY RUN")

    for d in DIRS:
        path = root / d
        action(f"MKDIR {path}")
        if args.execute:
            path.mkdir(parents=True, exist_ok=True)

    for src, dst in MOVES.items():
        move(root, src, dst, args.execute)

    move_by_globs(root, args.execute)

    for d in RAW_DIRS:
        move(root, d, Path("data/raw") / d, args.execute)

    for d in FUTURE_DIRS:
        move(root, d, Path("data/raw_future") / d, args.execute)

    for f in FUTURE_FILES:
        move(root, f, Path("data/raw_future") / f, args.execute)

    move_top_level_pdfs_to_papers(root, args.execute)

    for src, dst in OUTPUT_MOVES.items():
        move(root, src, dst, args.execute)

    move_580_paper_images(root, args.execute)

    delete_matching(root, DELETE_PATTERNS, args.execute)

    print("\nDone. This was a dry run." if not args.execute else "\nDone. Changes executed.")
    report_remaining_top_level(root)


if __name__ == "__main__":
    main()

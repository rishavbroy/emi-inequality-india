#!/usr/bin/env python3
"""Extract chunks from the legacy R Markdown file.

This script does not perform the semantic refactor by itself. It creates a stable
chunk archive and a chunk map so a coding agent can migrate code while preserving
comments and authorial voice.
"""
from __future__ import annotations
import argparse, csv, re
from pathlib import Path

CHUNK_RE = re.compile(r"^```\{r([^}]*)\}\s*$", re.MULTILINE)

def extract_chunks(rmd_path: Path, out_dir: Path) -> None:
    text = rmd_path.read_text(encoding="utf-8", errors="replace")
    out_dir.mkdir(parents=True, exist_ok=True)
    rows = []
    for idx, match in enumerate(CHUNK_RE.finditer(text), start=1):
        start = match.end()
        end = text.find("\n```", start)
        if end < 0:
            continue
        header = match.group(1).strip()
        code = text[start:end].lstrip("\n")
        line = text[:match.start()].count("\n") + 1
        name = re.sub(r"[^a-zA-Z0-9]+", "-", header.split(",")[0].strip().lower()).strip("-") or f"chunk-{idx:02d}"
        chunk_file = out_dir / f"{idx:02d}-{name}.R"
        chunk_file.write_text(code, encoding="utf-8")
        rows.append({"chunk": idx, "line": line, "header": header, "file": str(chunk_file), "n_lines": len(code.splitlines())})
    with (out_dir / "chunk_map.csv").open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=["chunk", "line", "header", "file", "n_lines"])
        writer.writeheader(); writer.writerows(rows)
    print(f"Extracted {len(rows)} chunks to {out_dir}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("rmd", nargs="?", default="580-Draft-ECON-580.Rmd")
    parser.add_argument("--out", default="archive/legacy-rmd-chunks")
    args = parser.parse_args()
    extract_chunks(Path(args.rmd), Path(args.out))

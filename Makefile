.PHONY: init-renv restore snapshot pipeline-draft pipeline-final pipeline-final-no-samples diagnostics public-diagnostics extended-diagnostics lineage-geometry benchmarking rerun-extended-diagnostics rerun-benchmarks rerun-analysis analysis-notes render-analysis clean clean-all clean-analysis clean-public-diagnostics clean-extended-diagnostics clean-benchmarking report samples check-report-values check-report-values-final audit-crossrefs audit-crossrefs-final audit-outputs-final public-build-audit public-build-audit-full public-build-audit-incremental public-build-audit-full-incremental public-build-audit-incremental-review public-build-audit-full-incremental-review public-build-audit-with-diagnostics public-build-audit-full-with-diagnostics public-build-audit-full-with-benchmarks check-public check-public-draft check-public-final check-public-final-no-samples check-public-text check-rendered-text check-sample-specs test tests test-affected test-inventory clean-targets clean-renders clean-renders-core clean-renders-no-samples

TEXCACHE_ROOT ?= /private/tmp/emi-inequality-india-texcache
QUARTO_CACHE_ROOT ?= /private/tmp/emi-inequality-india-quarto-cache
QUARTO_HOME := $(QUARTO_CACHE_ROOT)/home
export TEXMFVAR := $(TEXCACHE_ROOT)/texmf-var
export TEXMFCACHE := $(TEXCACHE_ROOT)/texmf-cache
export TEXMFCONFIG := $(TEXCACHE_ROOT)/texmf-config
export DENO_DIR := $(QUARTO_CACHE_ROOT)/deno
export QUARTO_CACHE := $(QUARTO_CACHE_ROOT)/quarto

TEXCACHE_DIRS := $(TEXMFVAR) $(TEXMFCACHE) $(TEXMFCONFIG)
QUARTO_CACHE_DIRS := $(DENO_DIR) $(QUARTO_CACHE) $(QUARTO_HOME)/Library/Caches/quarto

$(TEXCACHE_DIRS):
	mkdir -p $@

$(QUARTO_CACHE_DIRS):
	mkdir -p $@

init-renv:
	Rscript scripts/init_renv.R

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::settings$$snapshot.type("explicit"); renv::snapshot(prompt = FALSE)'


pipeline-draft: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .pipeline-draft-ok
	EMI_CONFIG=config/draft.yml Rscript scripts/run_targets_strict.R

pipeline-final: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .pipeline-final-ok .public-final-ok
	EMI_CONFIG=config/final.yml Rscript scripts/run_targets_strict.R

pipeline-final-no-samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .pipeline-final-ok .public-final-ok
	EMI_CONFIG=config/final.yml EMI_RENDER_APPLICATION_SAMPLES=false Rscript scripts/run_targets_strict.R

diagnostics: extended-diagnostics

public-diagnostics:
	EMI_CONFIG=config/final.yml Rscript scripts/run_targets_checked.R --starts-with diag_public_

extended-diagnostics:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript scripts/run_targets_checked.R --starts-with diag_ext_

lineage-geometry:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript scripts/run_targets_checked.R --targets district_lineage_v2_sources,census_2001_languages
	Rscript scripts/build_lineage_geometry_v2.R
	$(MAKE) extended-diagnostics

benchmarking:
	EMI_CONFIG=config/final.yml EMI_RUN_BENCHMARKS=true Rscript scripts/run_targets_checked.R --starts-with bench_


rerun-extended-diagnostics:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript -e 'targets::tar_invalidate(starts_with("diag_ext_"))'
	$(MAKE) extended-diagnostics

rerun-benchmarks:
	EMI_CONFIG=config/final.yml EMI_RUN_BENCHMARKS=true Rscript -e 'targets::tar_invalidate(starts_with("bench_"))'
	$(MAKE) benchmarking



analysis-notes:
	$(MAKE) public-diagnostics
	$(MAKE) extended-diagnostics
	$(MAKE) benchmarking
	$(MAKE) render-analysis

render-analysis: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	HOME=$(QUARTO_HOME) EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true EMI_RUN_BENCHMARKS=true EMI_RENDER_ANALYSIS_NOTES=true EMI_RENDER_APPLICATION_SAMPLES=false Rscript scripts/run_targets_checked.R --targets analysis_markdown_files

rerun-analysis:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true EMI_RUN_BENCHMARKS=true EMI_RENDER_ANALYSIS_NOTES=true EMI_RENDER_APPLICATION_SAMPLES=false Rscript -e 'targets::tar_invalidate(starts_with("analysis_md_")); targets::tar_invalidate("analysis_markdown_files")'
	$(MAKE) render-analysis

clean-analysis:
	find analysis -type f \( -name '*.html' -o -name '*.pdf' -o -name '*.tex' -o -name '*.log' \) -delete
	find analysis -type f -name '*.qmd' -exec sh -c 'for qmd do rm -f "$${qmd%.qmd}.md"; done' sh {} +

clean-public-diagnostics:
	rm -rf outputs/diagnostics/build outputs/diagnostics/public
	rm -f outputs/diagnostics/*.csv
	mkdir -p outputs/diagnostics/build outputs/diagnostics/public

clean-extended-diagnostics:
	rm -rf outputs/diagnostics/extended
	mkdir -p outputs/diagnostics/extended

clean-benchmarking:
	mkdir -p outputs/benchmarking
	find outputs/benchmarking -mindepth 1 ! -name README.md -exec rm -rf {} +

report: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	EMI_CONFIG=config/final.yml Rscript scripts/run_targets_checked.R --targets report

samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	EMI_CONFIG=config/final.yml EMI_RENDER_APPLICATION_SAMPLES=true Rscript scripts/run_targets_checked.R --targets writing_sample_pdfs,coding_sample_pdfs

check-report-values:
	Rscript scripts/check_report_values.R

check-report-values-final:
	EMI_CONFIG=config/final.yml Rscript scripts/check_report_values.R --strict

audit-crossrefs:
	Rscript scripts/audit_crossrefs.R

audit-crossrefs-final:
	Rscript scripts/audit_crossrefs.R --strict-report

audit-outputs-final:
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R


public-build-audit:
	bash scripts/run_public_build_audit.sh --without-samples

public-build-audit-full:
	bash scripts/run_public_build_audit.sh --with-samples

public-build-audit-incremental:
	bash scripts/run_public_build_audit.sh --without-samples --incremental

public-build-audit-full-incremental:
	bash scripts/run_public_build_audit.sh --with-samples --incremental

public-build-audit-incremental-review:
	bash scripts/run_public_build_audit.sh --without-samples --incremental --archive-always

public-build-audit-full-incremental-review:
	bash scripts/run_public_build_audit.sh --with-samples --incremental --archive-always

public-build-audit-with-diagnostics:
	bash scripts/run_public_build_audit.sh --without-samples --with-extended-diagnostics

public-build-audit-full-with-diagnostics:
	bash scripts/run_public_build_audit.sh --with-samples --with-extended-diagnostics

public-build-audit-full-with-benchmarks:
	bash scripts/run_public_build_audit.sh --with-samples --with-extended-diagnostics --with-benchmarks

check-public-text:
	Rscript scripts/check_public_text.R

check-rendered-text:
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final

check-sample-specs:
	Rscript scripts/check_sample_specs.R

check-public: check-public-draft

check-public-draft: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	Rscript scripts/check_public_text.R
	Rscript scripts/check_sample_specs.R
	$(MAKE) pipeline-draft
	Rscript scripts/check_rendered_text.R

check-public-final: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok
	$(MAKE) pipeline-final
	EMI_CONFIG=config/final.yml Rscript scripts/check_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	Rscript scripts/check_required_outputs.R --require-final-stamp
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final
	Rscript scripts/check_public_final.R
	touch .public-final-ok

check-public-final-no-samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok
	$(MAKE) pipeline-final-no-samples
	EMI_CONFIG=config/final.yml Rscript scripts/check_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	Rscript scripts/check_required_outputs.R --require-final-stamp
	EMI_CONFIG=config/final.yml EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_rendered_text.R --final
	EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_public_final.R
	touch .public-final-ok

test: tests

tests:
	Rscript tests/testthat.R

BASE ?= HEAD

test-affected:
	python3 scripts/test_impact.py --base "$(BASE)" --run

test-inventory:
	python3 scripts/test_impact.py --inventory

clean:
	$(MAKE) clean-renders
	$(MAKE) clean-analysis
	$(MAKE) clean-extended-diagnostics
	$(MAKE) clean-benchmarking

clean-all: clean clean-targets

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders-core:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/build outputs/diagnostics/public paper/output/*
	rm -f outputs/diagnostics/*.csv
	mkdir -p outputs/diagnostics/build outputs/diagnostics/public
	rm -f paper/report.pdf paper/report.html paper/report.tex paper/appendix.pdf paper/appendix.html paper/appendix.tex
	rm -f posters/2026_predoc_conference/poster.pdf posters/2026_predoc_conference/poster.typ
	rm -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.tex
	rm -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.tex
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok

clean-renders-no-samples: clean-renders-core

clean-renders: clean-renders-core
	rm -rf application-samples/.work/*
	rm -f application-samples/output/*.pdf application-samples/output/*.tex application-samples/output/*.html

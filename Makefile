.PHONY: all prepare-data init-renv restore snapshot download-census-tables download-natural-earth-boundaries pipeline pipeline-fast replicate-processed verify-processed-replication clean-processed-replication diagnostics public-diagnostics extended-diagnostics lineage-geometry-build lineage-geometry benchmarking rerun-extended-diagnostics rerun-benchmarks qmd-renders clean clean-all clean-public-diagnostics clean-extended-diagnostics clean-benchmarking paper poster samples check-report-values check-report-values-final audit-crossrefs audit-crossrefs-final audit-outputs-final output-manifest check-public check-public-fast check-public-final check-public-final-no-samples check-public-text check-rendered-text check-sample-specs test tests test-affected test-inventory clean-targets clean-renders clean-renders-core clean-renders-no-samples

TMP_ROOT ?= $(if $(strip $(TMPDIR)),$(patsubst %/,%,$(TMPDIR)),/tmp)
TEXCACHE_ROOT ?= $(TMP_ROOT)/emi-inequality-india-texcache
QUARTO_CACHE_ROOT ?= $(TMP_ROOT)/emi-inequality-india-quarto-cache
QUARTO_HOME := $(QUARTO_CACHE_ROOT)/home
CONFIG ?= config/final.yml
RENDER_SAMPLES ?= false
RENDER_POSTER ?= false

.DEFAULT_GOAL := all
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

all:
	bash scripts/run_full_build.sh

prepare-data: download-census-tables download-natural-earth-boundaries

init-renv: restore
	@echo "init-renv is an alias for restore; renv.lock is not modified."

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::settings$$snapshot.type("explicit"); renv::snapshot(prompt = FALSE)'

download-census-tables:
	bash scripts/download_census_tables.sh


download-natural-earth-boundaries:
	bash scripts/download_natural_earth_boundaries.sh


pipeline: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .pipeline-final-ok .pipeline-fast-ok .public-final-ok
	EMI_CONFIG=$(CONFIG) EMI_RENDER_APPLICATION_SAMPLES=$(RENDER_SAMPLES) EMI_RENDER_POSTER=$(RENDER_POSTER) Rscript scripts/run_targets_strict.R

pipeline-fast: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	$(MAKE) pipeline CONFIG=config/fast.yml RENDER_SAMPLES=$(RENDER_SAMPLES) RENDER_POSTER=$(RENDER_POSTER)

verify-processed-replication: replicate-processed
	Rscript scripts/check_processed_replication.R

replicate-processed:
	Rscript -e 'targets::tar_make(script = "_targets_processed.R", store = "_targets_processed"); bad <- targets::tar_errored(store = "_targets_processed"); if (length(bad)) stop("Processed targets failed: ", paste(bad, collapse = ", "), call. = FALSE)'

clean-processed-replication:
	Rscript -e 'if (dir.exists("_targets_processed")) targets::tar_destroy(destroy = "all", script = "_targets_processed.R", store = "_targets_processed", ask = FALSE)'
	rm -rf outputs/replication/processed

diagnostics: extended-diagnostics

public-diagnostics:
	EMI_CONFIG=$(CONFIG) Rscript scripts/run_targets_checked.R --starts-with diag_public_

extended-diagnostics:
	EMI_CONFIG=$(CONFIG) EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript scripts/run_targets_checked.R --starts-with diag_ext_

lineage-geometry-build:
	bash scripts/build_lineage_geometry.sh

lineage-geometry:
	$(MAKE) lineage-geometry-build
	$(MAKE) extended-diagnostics

benchmarking:
	EMI_CONFIG=$(CONFIG) EMI_RUN_BENCHMARKS=true Rscript scripts/run_targets_checked.R --starts-with bench_


rerun-extended-diagnostics:
	EMI_CONFIG=$(CONFIG) EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript -e 'targets::tar_invalidate(starts_with("diag_ext_"))'
	$(MAKE) extended-diagnostics

rerun-benchmarks:
	EMI_CONFIG=$(CONFIG) EMI_RUN_BENCHMARKS=true Rscript -e 'targets::tar_invalidate(starts_with("bench_"))'
	$(MAKE) benchmarking


clean-public-diagnostics:
	rm -rf outputs/build outputs/diagnostics/public
	rm -f outputs/diagnostics/*.csv
	mkdir -p outputs/build outputs/diagnostics/public

clean-extended-diagnostics:
	rm -rf outputs/diagnostics/extended
	mkdir -p outputs/diagnostics/extended

clean-benchmarking:
	mkdir -p outputs/benchmarking
	find outputs/benchmarking -mindepth 1 ! -name README.md -exec rm -rf {} +

poster: $(QUARTO_CACHE_DIRS)
	EMI_CONFIG=$(CONFIG) EMI_RENDER_POSTER=true Rscript scripts/run_targets_checked.R --targets poster

paper: check-public-final-no-samples

samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	EMI_CONFIG=$(CONFIG) EMI_RENDER_APPLICATION_SAMPLES=true Rscript scripts/run_targets_checked.R --targets writing_sample_pdfs,coding_sample_pdfs

qmd-renders:
	$(MAKE) paper
	$(MAKE) samples CONFIG=$(CONFIG)
	$(MAKE) poster CONFIG=$(CONFIG)

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



output-manifest:
	Rscript scripts/write_output_manifest.R

check-public-text:
	Rscript scripts/check_public_text.R

check-rendered-text:
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final

check-sample-specs:
	Rscript scripts/check_sample_specs.R

check-public: check-public-fast

check-public-fast: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	Rscript scripts/check_public_text.R
	Rscript scripts/check_sample_specs.R
	$(MAKE) pipeline-fast RENDER_SAMPLES=false RENDER_POSTER=false
	Rscript scripts/check_rendered_text.R

check-public-final: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-fast-ok
	$(MAKE) pipeline CONFIG=config/final.yml RENDER_SAMPLES=true RENDER_POSTER=false
	EMI_CONFIG=config/final.yml Rscript scripts/check_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	EMI_REQUIRE_APPLICATION_SAMPLES=true EMI_REQUIRE_POSTER=false Rscript scripts/check_required_outputs.R --require-final-stamp
	EMI_CONFIG=config/final.yml EMI_REQUIRE_APPLICATION_SAMPLES=true EMI_REQUIRE_POSTER=false Rscript scripts/check_rendered_text.R --final
	EMI_REQUIRE_APPLICATION_SAMPLES=true EMI_REQUIRE_POSTER=false Rscript scripts/check_public_final.R
	touch .public-final-ok

check-public-final-no-samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-fast-ok
	$(MAKE) pipeline CONFIG=config/final.yml RENDER_SAMPLES=false RENDER_POSTER=false
	EMI_CONFIG=config/final.yml Rscript scripts/check_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	EMI_REQUIRE_APPLICATION_SAMPLES=false EMI_REQUIRE_POSTER=false Rscript scripts/check_required_outputs.R --require-final-stamp
	EMI_CONFIG=config/final.yml EMI_REQUIRE_APPLICATION_SAMPLES=false EMI_REQUIRE_POSTER=false Rscript scripts/check_rendered_text.R --final
	EMI_REQUIRE_APPLICATION_SAMPLES=false EMI_REQUIRE_POSTER=false Rscript scripts/check_public_final.R
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
	$(MAKE) clean-extended-diagnostics
	$(MAKE) clean-benchmarking

clean-all: clean clean-targets

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all"); if (dir.exists("_targets_processed")) targets::tar_destroy(destroy = "all", script = "_targets_processed.R", store = "_targets_processed", ask = FALSE)'

clean-renders-core:
	rm -rf outputs/figures/* outputs/tables/* outputs/build outputs/diagnostics/public outputs/replication paper/output/*
	rm -f outputs/diagnostics/*.csv
	mkdir -p outputs/build outputs/diagnostics/public
	rm -f paper/paper.pdf paper/paper.html paper/paper.tex paper/appendix.pdf paper/appendix.html paper/appendix.tex
	rm -rf posters/2026_predoc_conference/generated
	rm -f posters/2026_predoc_conference/poster.pdf posters/2026_predoc_conference/poster.png posters/2026_predoc_conference/RishavRoy-Education.png posters/2026_predoc_conference/poster.typ
	rm -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.tex
	rm -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.tex
	rm -f .public-final-ok .pipeline-final-ok .pipeline-fast-ok

clean-renders-no-samples: clean-renders-core

clean-renders: clean-renders-core
	rm -rf application-samples/.work/*
	rm -f application-samples/output/*.pdf application-samples/output/*.tex application-samples/output/*.html

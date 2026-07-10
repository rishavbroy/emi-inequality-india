.PHONY: init-renv restore snapshot rebuild-qmds pipeline-draft pipeline-final pipeline-final-no-samples diagnostics extended-diagnostics benchmarking rerun-extended-diagnostics rerun-benchmarks analysis-notes render-analysis clean-analysis clean-public-diagnostics clean-extended-diagnostics clean-benchmarking report samples audit-report-values audit-report-values-final audit-crossrefs audit-crossrefs-final audit-outputs-final audit-legacy-content public-build-audit public-build-audit-full public-build-audit-incremental public-build-audit-full-incremental public-build-audit-incremental-review public-build-audit-full-incremental-review public-build-audit-with-diagnostics public-build-audit-full-with-diagnostics public-build-audit-full-with-benchmarks check-public check-public-draft check-public-final check-public-final-no-samples check-public-text check-rendered-text check-sample-specs test tests clean-targets clean-renders clean-renders-core clean-renders-no-samples

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

rebuild-qmds:
	Rscript scripts/rebuild_static_qmds_from_legacy.R
	Rscript scripts/postprocess_public_qmds.R

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

extended-diagnostics:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript scripts/run_targets_checked.R --starts-with diag_ext_

benchmarking:
	EMI_CONFIG=config/final.yml EMI_RUN_BENCHMARKS=true Rscript scripts/run_targets_checked.R --starts-with bench_

rerun-extended-diagnostics:
	EMI_CONFIG=config/final.yml EMI_RUN_EXTENDED_DIAGNOSTICS=true Rscript -e 'targets::tar_invalidate(starts_with("diag_ext_"))'
	$(MAKE) extended-diagnostics

rerun-benchmarks:
	EMI_CONFIG=config/final.yml EMI_RUN_BENCHMARKS=true Rscript -e 'targets::tar_invalidate(starts_with("bench_"))'
	$(MAKE) benchmarking

analysis-notes: extended-diagnostics benchmarking render-analysis

render-analysis: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	HOME=$(QUARTO_HOME) Rscript scripts/render_analysis_notes.R

clean-analysis:
	find analysis -type f \( -name '*.html' -o -name '*.pdf' -o -name '*.tex' \) -delete

clean-public-diagnostics:
	rm -rf outputs/diagnostics/build outputs/diagnostics/public
	mkdir -p outputs/diagnostics/build outputs/diagnostics/public

clean-extended-diagnostics:
	rm -rf outputs/diagnostics/extended
	mkdir -p outputs/diagnostics/extended

clean-benchmarking:
	mkdir -p outputs/benchmarking
	find outputs/benchmarking -mindepth 1 ! -name README.md -exec rm -rf {} +

report: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	Rscript scripts/check_required_outputs.R --require-final-stamp
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd

samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	Rscript scripts/check_required_outputs.R --require-final-stamp
	Rscript scripts/render_application_samples.R

audit-report-values:
	Rscript scripts/audit_report_values.R

audit-report-values-final:
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict

audit-crossrefs:
	Rscript scripts/audit_crossrefs.R

audit-crossrefs-final:
	Rscript scripts/audit_crossrefs.R --strict-report

audit-outputs-final:
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R

audit-legacy-content:
	python3 -m py_compile scripts/audit_legacy_parity.py
	python3 scripts/audit_legacy_parity.py

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
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	Rscript scripts/render_application_samples.R
	Rscript scripts/check_rendered_text.R

check-public-final: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok
	$(MAKE) pipeline-final
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict --allow-status-placeholders
	Rscript scripts/audit_crossrefs.R --strict-report
	Rscript scripts/check_required_outputs.R --require-final-stamp
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	Rscript scripts/render_application_samples.R
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final
	Rscript scripts/check_public_final.R
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R
	touch .public-final-ok

check-public-final-no-samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok
	$(MAKE) pipeline-final-no-samples
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict --allow-status-placeholders
	Rscript scripts/audit_crossrefs.R --strict-report
	Rscript scripts/check_required_outputs.R --require-final-stamp
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	EMI_CONFIG=config/final.yml EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_rendered_text.R --final
	EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_public_final.R
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R
	touch .public-final-ok

test: tests

tests:
	Rscript tests/testthat.R

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders-core:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/build outputs/diagnostics/public paper/output/*
	mkdir -p outputs/diagnostics/build outputs/diagnostics/public
	rm -f paper/report.pdf paper/report.html paper/report.tex paper/appendix.pdf paper/appendix.html paper/appendix.tex
	rm -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.tex
	rm -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.tex
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok

clean-renders-no-samples: clean-renders-core

clean-renders: clean-renders-core
	rm -rf application-samples/.work/*
	rm -f application-samples/output/*.pdf application-samples/output/*.tex application-samples/output/*.html

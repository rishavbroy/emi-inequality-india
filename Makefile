.PHONY: init-renv restore snapshot rebuild-qmds pipeline-draft pipeline-final pipeline-final-no-samples diagnostics report samples audit-report-values audit-report-values-final audit-crossrefs audit-crossrefs-final audit-outputs-final audit-legacy-content public-build-audit public-build-audit-full public-build-audit-incremental public-build-audit-full-incremental check-public check-public-draft check-public-final check-public-final-no-samples check-public-text check-rendered-text check-sample-specs test tests clean-targets clean-renders clean-renders-core clean-renders-no-samples

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

diagnostics:
	EMI_CONFIG=config/diagnostics.yml Rscript -e 'targets::tar_make(starts_with("diag_"))'

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
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R
	Rscript scripts/check_required_outputs.R --require-final-stamp
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	Rscript scripts/render_application_samples.R
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final
	Rscript scripts/check_public_final.R
	touch .public-final-ok

check-public-final-no-samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok
	$(MAKE) pipeline-final-no-samples
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R
	Rscript scripts/check_required_outputs.R --require-final-stamp
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	EMI_CONFIG=config/final.yml EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_rendered_text.R --final
	EMI_REQUIRE_APPLICATION_SAMPLES=false Rscript scripts/check_public_final.R
	touch .public-final-ok

test: tests

tests:
	Rscript tests/testthat.R

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders-core:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/* paper/output/*
	rm -f paper/report.pdf paper/report.html paper/report.tex paper/appendix.pdf paper/appendix.html paper/appendix.tex
	rm -f docs/district-matching.html docs/district-matching.pdf docs/district-matching.tex
	rm -f docs/long-paths-and-8-3-filenames.html docs/long-paths-and-8-3-filenames.pdf docs/long-paths-and-8-3-filenames.tex
	rm -f .public-final-ok .pipeline-final-ok .pipeline-draft-ok

clean-renders-no-samples: clean-renders-core

clean-renders: clean-renders-core
	rm -rf application-samples/.work/*
	rm -f application-samples/output/*.pdf application-samples/output/*.tex application-samples/output/*.html

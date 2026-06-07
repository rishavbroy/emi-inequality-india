.PHONY: init-renv restore snapshot rebuild-qmds pipeline-draft pipeline-final diagnostics report samples audit-report-values audit-report-values-final audit-crossrefs audit-crossrefs-final audit-outputs-final check-public check-public-draft check-public-final check-public-text check-rendered-text check-sample-specs test tests clean-targets clean-renders

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
	EMI_CONFIG=config/draft.yml Rscript -e 'targets::tar_make()'

pipeline-final: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	EMI_CONFIG=config/final.yml Rscript -e 'targets::tar_make()'

diagnostics:
	EMI_CONFIG=config/diagnostics.yml Rscript -e 'targets::tar_make(starts_with("diag_"))'

report: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd

samples: $(TEXCACHE_DIRS) $(QUARTO_CACHE_DIRS)
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
	EMI_CONFIG=config/final.yml Rscript -e 'targets::tar_make()'
	EMI_CONFIG=config/final.yml Rscript scripts/audit_report_values.R --strict
	Rscript scripts/audit_crossrefs.R --strict-report
	EMI_CONFIG=config/final.yml Rscript scripts/audit_outputs_final.R
	HOME=$(QUARTO_HOME) quarto render paper/report.qmd
	HOME=$(QUARTO_HOME) quarto render docs/district-matching.qmd
	HOME=$(QUARTO_HOME) quarto render docs/long-paths-and-8-3-filenames.qmd
	Rscript scripts/render_application_samples.R
	EMI_CONFIG=config/final.yml Rscript scripts/check_rendered_text.R --final
	Rscript scripts/check_public_final.R

test: tests

tests:
	Rscript tests/testthat.R

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/* paper/output/* application-samples/.work/*

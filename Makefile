.PHONY: init-renv restore snapshot pipeline-draft pipeline-final diagnostics report samples check-public check-public-text check-sample-specs test tests clean-targets clean-renders

init-renv:
	Rscript scripts/init_renv.R

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::settings$$snapshot.type("explicit"); renv::snapshot(prompt = FALSE)'

pipeline-draft:
	EMI_CONFIG=config/draft.yml Rscript -e 'targets::tar_make()'

pipeline-final:
	EMI_CONFIG=config/final.yml Rscript -e 'targets::tar_make()'

diagnostics:
	EMI_CONFIG=config/diagnostics.yml Rscript -e 'targets::tar_make(starts_with("diag_"))'

report:
	quarto render paper/report.qmd

samples:
	Rscript scripts/render_application_samples.R

check-public-text:
	Rscript scripts/check_public_text.R

check-sample-specs:
	Rscript scripts/check_sample_specs.R

check-public:
	Rscript scripts/check_public_text.R
	Rscript scripts/check_sample_specs.R
	quarto render paper/report.qmd
	quarto render docs/district-matching.qmd
	quarto render docs/long-paths-and-8-3-filenames.qmd
	Rscript scripts/render_application_samples.R

test: tests

tests:
	Rscript tests/testthat.R

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/* paper/output/* application-samples/.work/*

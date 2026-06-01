.PHONY: init-renv restore snapshot pipeline-draft pipeline-final diagnostics report samples tests clean-targets clean-renders

init-renv:
	Rscript scripts/init_renv.R

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::snapshot(prompt = FALSE)'

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

tests:
	Rscript -e 'testthat::test_dir("tests/testthat")'

clean-targets:
	Rscript -e 'targets::tar_destroy(destroy = "all")'

clean-renders:
	rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/* paper/output/* application-samples/.work/*

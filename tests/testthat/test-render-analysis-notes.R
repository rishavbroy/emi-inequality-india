test_that("analysis Quarto renderer retries only a SIGSEGV status once", {
  statuses <- c(139L, 0L)
  calls <- 0L
  runner <- function(command, args) {
    calls <<- calls + 1L
    statuses[[calls]]
  }
  expect_message(
    expect_identical(run_analysis_quarto_render("note.qmd", runner), 0L),
    "status 139"
  )
  expect_identical(calls, 2L)

  calls <- 0L
  runner_fail <- function(command, args) {
    calls <<- calls + 1L
    1L
  }
  expect_identical(run_analysis_quarto_render("note.qmd", runner_fail), 1L)
  expect_identical(calls, 1L)
})

test_that("public build helper scripts parse", {
  expect_silent(parse("scripts/postprocess_public_qmds.R"))
  expect_silent(parse("scripts/check_required_outputs.R"))
  expect_silent(parse("scripts/check_rendered_text.R"))
})

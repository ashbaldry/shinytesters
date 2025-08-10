test_that("Error happens when update function has no session argument", {
  local_mocked_bindings(!!!create_test_update_fns("selectInput"), .package = "shiny")

  expect_error(
    shiny::selectInput(inputId = "select", label = "Label"),
    "Unable to determine session argument for `selectInput()`",
    fixed = TRUE
  )
})

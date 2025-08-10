test_that("Able to test updates to shinyWidgets using testServer", {
  skip_on_cran()
  skip_if_not(do.call(require, list("shinyWidgets")))

  testthat::local_mocked_bindings(
    updateColorPickr = create_test_update_fn("updateColorPickr", .package = "shinyWidgets"),
    .package = "shinyWidgets"
  )

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateColorPickr(
        inputId = "result",
        label = "New Label",
        value = "#999999",
        action = "show"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "#999999")
      expect_identical(input$result.action, "show")
      expect_null(input$result.swatches)
    }
  )
})

test_that("Able to test updates to shinyWidgets using use_shiny_testers", {
  skip_on_cran()
  skip_if_not(do.call(require, list("shinyWidgets")))

  use_shiny_testers(.package = "shinyWidgets")

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateColorPickr(
        inputId = "result",
        label = "New Label",
        value = "#999999",
        action = "show"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "#999999")
      expect_identical(input$result.action, "show")
      expect_null(input$result.swatches)
    }
  )
})

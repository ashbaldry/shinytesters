test_that("use_shiny_testers correctly picks up update functions for supplied shiny package", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateDateInput(
        inputId = "result",
        label = "New Label",
        value = as.Date("2000-01-01"),
        min = as.Date("1999-12-31")
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = as.Date("2025-01-02"))
      session$setInputs(trigger = 1L)

      expect_identical(input$result, as.Date("2000-01-01"))
      expect_identical(input$result.min, as.Date("1999-12-31"))
      expect_null(input$result.max)
    }
  )
})

test_that("with_shiny_testers correctly picks up update functions for supplied shiny package", {
  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateDateInput(
        inputId = "result",
        label = "New Label",
        value = as.Date("2000-01-01"),
        min = as.Date("1999-12-31")
      )
    })
  }

  with_shiny_testers(
    shiny::testServer(
      app = example_server_fn,
      expr = {
        session$setInputs(result = as.Date("2025-01-02"))
        session$setInputs(trigger = 1L)

        expect_identical(input$result, as.Date("2000-01-01"))
        expect_identical(input$result.min, as.Date("1999-12-31"))
        expect_null(input$result.max)
      }
    )
  )
})

test_that("with_shiny_testers correctly picks up update functions for supplied shiny package", {
  skip_on_cran()
  skip_if_not(do.call(require, list("shinyWidgets")))

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateColorPickr(
        inputId = "result",
        label = "New Label",
        value = "#ae3245",
      )
    })
  }

  with_shiny_testers(
    .package = "shinyWidgets",
    shiny::testServer(
      app = example_server_fn,
      expr = {
        session$setInputs(trigger = 1L)

        expect_identical(input$result, "#ae3245")
      }
    )
  )
})

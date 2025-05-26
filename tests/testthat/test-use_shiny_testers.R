test_that("Able to test updates to shiny::checkboxInput using testServer", {
  skip_on_cran()
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateCheckboxInput(
        inputId = "result",
        label = "New Label",
        value = TRUE
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_identical(input$result, TRUE)
      expect_identical(input$result.label, "New Label")
    }
  )
})

test_that("Able to test updates to shiny::checkboxGroupInput using testServer", {
  skip_on_cran()
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateCheckboxGroupInput(
        inputId = "result",
        label = "New Label",
        choices = stats::setNames(letters, LETTERS),
        selected = NULL
      )
    })

    observeEvent(input$trigger2, {
      updateCheckboxGroupInput(
        inputId = "result",
        selected = c("F", "G", "P"),
        choiceNames = LETTERS[1:20],
        choiceValues = letters[1:20]
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_null(input$result)
      expect_identical(input$result.choices, stats::setNames(letters, LETTERS))

      session$setInputs(trigger2 = 1L)
      expect_identical(input$result, c("F", "G", "P"))
      expect_identical(input$result.choices, stats::setNames(letters[1:20], LETTERS[1:20]))
    }
  )
})

test_that("Able to test updates to shiny::textInput using testServer", {
  skip_on_cran()
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateTextInput(
        inputId = "result",
        label = "New Label",
        value = NULL,
        placeholder = "New placeholder"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = "Example text")
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "Example text")
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.placeholder, "New placeholder")
    }
  )
})

test_that("Able to test updates to shiny::numericInput using testServer", {
  skip_on_cran()
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateNumericInput(
        inputId = "result",
        label = "New Label",
        value = NULL,
        max = 12,
        step = 0.4
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = "Example text")
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "Example text")
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.step, 0.4)
      expect_null(input$result.min)
    }
  )
})

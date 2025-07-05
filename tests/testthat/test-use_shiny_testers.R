# textArea is identical to text
test_that("Able to test updates to shiny::textInput using testServer", {
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
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateNumericInput(
        inputId = "result",
        label = "New Label",
        value = 5,
        max = 12,
        step = 0.4
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = 2)
      session$setInputs(trigger = 1L)

      expect_identical(input$result, 5)
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.step, 0.4)
      expect_null(input$result.min)
    }
  )
})

test_that("Able to test updates to shiny::checkboxInput using testServer", {
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

# radioButtons is identical to checkboxGroup
test_that("Able to test updates to shiny::checkboxGroupInput using testServer", {
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

test_that("Able to test updates to shiny::selectInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateSelectInput(
        inputId = "result",
        label = "New Label",
        choices = stats::setNames(letters, LETTERS),
        selected = NULL
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_null(input$result)
      expect_identical(input$result.choices, stats::setNames(letters, LETTERS))
    }
  )
})

test_that("Able to test updates to shiny::selectizeInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateSelectizeInput(
        inputId = "result",
        label = "New Label",
        choices = stats::setNames(letters, LETTERS),
        selected = NULL
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_null(input$result)
      expect_identical(input$result.choices, stats::setNames(letters, LETTERS))
      expect_null(input$result.options)
    }
  )
})

test_that("Able to test updates to shiny::varSelectInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateVarSelectInput(
        inputId = "result",
        label = "New Label",
        data = iris,
        selected = "Sepal.Width"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "Sepal.Width")
      expect_identical(input$result.choices, names(iris))
    }
  )
})

test_that("Able to test updates to shiny::sliderInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateSliderInput(
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
      session$setInputs(result = 4)
      session$setInputs(trigger = 1L)

      expect_identical(input$result, 4)
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.step, 0.4)
      expect_null(input$result.min)
    }
  )
})

test_that("Able to test updates to a ranged shiny::sliderInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateSliderInput(
        inputId = "result",
        label = "New Label",
        value = c(4L, 6L),
        max = 12,
        step = 0.4
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = c(1L, 10L))
      session$setInputs(trigger = 1L)

      expect_identical(input$result, c(4L, 6L))
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.step, 0.4)
      expect_null(input$result.min)
    }
  )
})

test_that("Able to test updates to shiny::dateInput using testServer", {
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

test_that("Able to test updates to shiny::dateRangeInput using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateDateRangeInput(
        inputId = "result",
        label = "New Label",
        start = NULL,
        end = as.Date("2000-01-01"),
        min = as.Date("1999-12-31")
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = as.Date(c(NA, NA)))
      session$setInputs(trigger = 1L)

      expect_identical(input$result, as.Date(c(NA, "2000-01-01")))
      expect_identical(input$result.min, as.Date("1999-12-31"))
      expect_null(input$result.start)
    }
  )
})

# tabsetPanel and navlistPanel are identical
test_that("Able to test updates to shiny::navbarPage using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateNavbarPage(
        inputId = "result",
        selected = "new_tab"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result = "old_tab")
      session$setInputs(trigger = 1L)

      expect_identical(input$result, "new_tab")
    }
  )
})

test_that("Able to test updates to shiny::actionLink using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateActionLink(
        inputId = "result",
        label = "New Label"
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result.label = "Click Me")
      session$setInputs(trigger = 1L)

      expect_identical(input$result.label, "New Label")
    }
  )
})

test_that("Able to test updates to shiny::actionButton using testServer", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$trigger, {
      updateActionButton(
        inputId = "result",
        label = "New Label",
        disabled = TRUE
      )
    })
  }

  shiny::testServer(
    app = example_server_fn,
    expr = {
      session$setInputs(result.label = "Click Me")
      session$setInputs(trigger = 1L)

      expect_identical(input$result.label, "New Label")
    }
  )
})

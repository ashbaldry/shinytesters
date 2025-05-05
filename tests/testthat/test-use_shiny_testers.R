test_that("Able to test updates to shiny inputs using testServer", {
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
      session$elapse(10L)

      session$setInputs(trigger = 1L)

      expect_identical(input$result, "Example text")
      expect_identical(input$result.label, "New Label")
      expect_identical(input$result.placeholder, "New placeholder")
    }
  )
})

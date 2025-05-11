<!-- badges: start -->
[![R-CMD-check](https://github.com/ashbaldry/shinytesters/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbaldry/shinytesters/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ashbaldry/shinytesters/graph/badge.svg)](https://app.codecov.io/gh/ashbaldry/shinytesters)
<!-- badges: end -->

# shinytesters

The aim of `shinytesters` is to make it easier to test update functions in Shiny packages when using `testthat::testServer`.

## Installation

To install the latest development version of `shinytesters`, install from GitHub:

```r
remotes::install_github("ashbaldry/shinytesters")
```

## Usage

Add `use_shiny_testers()` at the start of any test that is using `shiny::testServer` to add inputs and other relevant 
arguments to the test session inputs.

```r
test_that("When clicking apply button, checkbox becomes checked", {
  use_shiny_testers()

  example_server_fn <- function(input, output, session) {
    observeEvent(input$apply_btn, {
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
      session$setInputs(apply_btn = 1L)

      expect_identical(input$result, TRUE)
      expect_identical(input$result.label, "New Label")
    }
  )
})
```

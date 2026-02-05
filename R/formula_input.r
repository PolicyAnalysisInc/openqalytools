#' Formula Input
#'
#' A single-line R code editor input with syntax highlighting. This component
#' provides an interactive code editor for entering R formulas or expressions,
#' with R syntax highlighting, bracket matching validation, and automatic
#' newline prevention. Supports custom term highlighting for domain-specific
#' vocabularies.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param value Initial value. Default is an empty string.
#' @param placeholder A character string giving the user a hint as to what
#'   can be entered into the input. Default is NULL (no placeholder).
#' @param width The width of the input, e.g., '400px' or '100%'. Default is
#'   NULL which uses the default width.
#' @param terms A named list where names are token types and values are
#'   character vectors of terms to highlight. Token types can include:
#'   \itemize{
#'     \item \code{keyword}: Domain-specific keywords (e.g., "cycle", "state")
#'     \item \code{variable}: Variable names from the model
#'     \item \code{table}: Table/data frame names
#'     \item \code{value}: Named values or constants
#'     \item \code{script_variable}: Script-assigned variables
#'   }
#'   Custom token types are also supported and will be styled with the CSS
#'   class \code{ace_oq-<tokentype>}.
#'
#' @return A Shiny input element that can be included in a UI definition.
#'   The input value is a list with two elements:
#'   \itemize{
#'     \item \code{value}: The text content of the formula
#'     \item \code{valid}: A logical indicating if brackets are balanced
#'   }
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(openqalytools)
#'
#' ui <- fluidPage(
#'   formulaInput(
#'     "formula1",
#'     value = "age + cost",
#'     placeholder = "Enter R formula",
#'     terms = list(
#'       keyword = c("cycle", "day", "week"),
#'       variable = c("age", "cost", "utility"),
#'       table = c("patients", "costs")
#'     )
#'   ),
#'   verbatimTextOutput("result")
#' )
#'
#' server <- function(input, output, session) {
#'   output$result <- renderText({
#'     req(input$formula1)
#'     paste("Formula:", input$formula1$value, "| Valid:", input$formula1$valid)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @tests
#' result <- formulaInput("test_id", value = "x + y")
#' expect_true(inherits(result, "shiny.tag.list"))
#'
#' @export
#' @importFrom htmltools htmlDependency tags tagList
#' @importFrom shiny restoreInput
#' @importFrom jsonlite toJSON
formulaInput <- function(inputId, value = "", placeholder = NULL, width = NULL,
                         terms = NULL) {
  # Restore value from bookmarked state if available
  value <- shiny::restoreInput(id = inputId, default = value)

  # Build style attribute
  style <- NULL
  if (!is.null(width)) {
    style <- paste0("width: ", htmltools::validateCssUnit(width), ";")
  }

  # Serialize terms to JSON if provided
  termsJson <- NULL
  if (!is.null(terms)) {
    if (!is.list(terms)) {
      stop("terms must be a named list")
    }
    if (is.null(names(terms)) || any(names(terms) == "")) {
      stop("terms must be a named list where names are token types")
    }
    termsJson <- jsonlite::toJSON(terms, auto_unbox = FALSE)
  }

  # Build the container div
  inputTag <- htmltools::tags$div(
    id = inputId,
    class = "formula-input",
    `data-value` = value,
    `data-placeholder` = if (!is.null(placeholder)) placeholder else "",
    `data-terms` = termsJson,
    style = style
  )

  # Return the input with dependencies attached
  htmltools::tagList(
    formula_input_dependency(),
    inputTag
  )
}

#' Update Formula Input
#'
#' Change the value and/or terms of a formula input on the client.
#'
#' @param session The session object passed to the Shiny server function.
#' @param inputId The id of the input object.
#' @param value The new value for the input.
#' @param terms A named list of terms to highlight. See \code{\link{formulaInput}}
#'   for details on the format. Set to an empty list \code{list()} to clear
#'   all term highlighting.
#'
#' @tests
#' # updateFormulaInput requires a session object, so we test it exists
#' expect_true(is.function(updateFormulaInput))
#'
#' @export
updateFormulaInput <- function(session, inputId, value = NULL, terms = NULL) {
  message <- list()
  if (!is.null(value)) message$value <- value
  if (!is.null(terms)) {
    if (!is.list(terms)) {
      stop("terms must be a named list")
    }
    message$terms <- terms
  }

  session$sendInputMessage(inputId, message)
}

#' @keywords internal
formula_input_dependency <- function() {
  # Ace Editor from CDN
  ace_dep <- htmltools::htmlDependency(
    name = "ace-editor",
    version = "1.32.6",
    src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6"),
    script = c("ace.min.js", "mode-r.min.js", "theme-chrome.min.js"),
    all_files = FALSE
  )

  # Local formula-input binding with custom mode for term highlighting
  # Note: script order matters - mode must load before main binding
  binding_dep <- htmltools::htmlDependency(
    name = "formula-input",
    version = "1.1.0",
    src = c(file = system.file("www", package = "openqalytools")),
    script = c(
      "formula-input-mode.js",
      "formula-input.js"
    ),
    stylesheet = "formula-input.css",
    all_files = FALSE
  )

  list(ace_dep, binding_dep)
}

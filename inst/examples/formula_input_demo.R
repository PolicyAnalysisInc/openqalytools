# Formula Input Demo - Dynamic Language Definition
#
# This example demonstrates the formula input with dynamic syntax highlighting.
# Variable names defined in the table are highlighted in blue in the formula editor.
#
# Run with: shiny::runApp(system.file("examples/formula_input_demo.R", package = "openqalytools"))

library(shiny)
library(openqalytools)

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      /* Custom style: variables in blue */
      .formula-input .ace_oq-variable {
        color: #0066cc !important;
        font-weight: 500;
      }

      .container { max-width: 800px; margin: 40px auto; }
      .var-table { margin-bottom: 20px; }
      .var-table input {
        width: 100%;
        padding: 6px 10px;
        border: 1px solid #ced4da;
        border-radius: 4px;
      }
      .var-table td { padding: 4px; }
      .add-btn { margin-top: 10px; }
      .formula-section { margin-top: 30px; }
      .result {
        margin-top: 20px;
        padding: 15px;
        background: #f8f9fa;
        border-radius: 4px;
        font-family: monospace;
      }
      h3 { color: #333; border-bottom: 2px solid #0066cc; padding-bottom: 10px; }
    "))
  ),

  div(class = "container",
    h2("Formula Input Demo"),
    p("This example demonstrates dynamic syntax highlighting. Variable names you define below will be highlighted in blue in the formula editor."),

    h3("1. Define Variables"),
    p("Add variable names to the table. These will be recognized in the formula."),

    div(class = "var-table",
      tags$table(
        tags$thead(
          tags$tr(
            tags$th("Variable Name", style = "width: 70%"),
            tags$th("Action", style = "width: 30%")
          )
        ),
        tags$tbody(id = "var-rows")
      )
    ),

    actionButton("add_var", "Add Variable", class = "btn-primary add-btn"),

    h3("2. Enter Formula", class = "formula-section"),
    p("Type a formula using your variables. They will be highlighted in blue."),
    p(tags$em("Try: age + cost * 2 or sum(age, cost, utility)")),

    formulaInput(
      "formula",
      value = "age + cost",
      placeholder = "Enter R formula using your variables...",
      width = "100%",
      terms = list(variable = c("age", "cost", "utility"))
    ),

    div(class = "result",
      h4("Result"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store variables
  variables <- reactiveVal(c("age", "cost", "utility"))

  # Track variable input IDs
  var_counter <- reactiveVal(0)
  var_inputs <- reactiveVal(character(0))

  # Initialize with default variables
  observe({
    vars <- variables()
    for (i in seq_along(vars)) {
      add_variable_row(vars[i])
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Add a variable row to the UI
  add_variable_row <- function(value = "") {
    n <- var_counter() + 1
    var_counter(n)

    input_id <- paste0("var_", n)
    remove_id <- paste0("remove_", n)

    # Track this input
    var_inputs(c(var_inputs(), input_id))

    insertUI(
      selector = "#var-rows",
      where = "beforeEnd",
      ui = tags$tr(
        id = paste0("row_", n),
        tags$td(
          textInput(input_id, label = NULL, value = value, width = "100%")
        ),
        tags$td(
          actionButton(remove_id, "Remove", class = "btn-sm btn-danger")
        )
      )
    )

    # Observer to remove this row
    observeEvent(input[[remove_id]], {
      removeUI(selector = paste0("#row_", n))
      var_inputs(setdiff(var_inputs(), input_id))
      update_terms()
    }, ignoreInit = TRUE)
  }

  # Add variable button
  observeEvent(input$add_var, {
    add_variable_row("")
  })

  # Update terms when any variable input changes
  update_terms <- function() {
    # Collect all current variable values
    vars <- sapply(var_inputs(), function(id) {
      val <- input[[id]]
      if (is.null(val)) "" else trimws(val)
    })

    # Filter out empty strings and get unique values
    vars <- unique(vars[vars != ""])

    # Update the formula input with new terms
    updateFormulaInput(
      session,
      "formula",
      terms = list(variable = if (length(vars) > 0) vars else character(0))
    )
  }

  # Watch for changes in any variable input
  observe({
    # Create dependency on all variable inputs
    lapply(var_inputs(), function(id) input[[id]])

    # Update terms (debounced by Shiny's reactivity)
    update_terms()
  })

  # Display formula result
  output$result <- renderText({
    req(input$formula)

    vars <- sapply(var_inputs(), function(id) {
      val <- input[[id]]
      if (is.null(val)) "" else trimws(val)
    })
    vars <- unique(vars[vars != ""])

    paste0(
      "Formula: ", input$formula$value, "\n",
      "Valid syntax: ", ifelse(input$formula$valid, "Yes", "No (unbalanced brackets)"), "\n",
      "Recognized variables: ", paste(vars, collapse = ", ")
    )
  })
}

shinyApp(ui, server)

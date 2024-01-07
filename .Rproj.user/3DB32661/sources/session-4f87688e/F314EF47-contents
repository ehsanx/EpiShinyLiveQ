library(shiny)
library(ggplot2)
library(tableone)

# Define UI for application
ui <- fluidPage(
  titlePanel("App Data Wrangling with mpg Data"),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Viewer",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("columns", "Select Columns:", choices = names(ggplot2::mpg), multiple = TRUE),
                   numericInput("rows", "Number of Rows to Show:", value = 10, min = 1),
                   downloadButton("downloadRCode", "Download R Code")
                 ),
                 mainPanel(
                   h2("Data Viewer"),
                   tableOutput("data"),
                   h2("R Code"),
                   verbatimTextOutput("rcode")
                 )
               )),
      tabPanel("TableOne",
               h2("TableOne Summary"),
               selectInput("tableoneVar", "Choose Variables:", choices = names(ggplot2::mpg), multiple = TRUE),
               selectInput("tableoneStrata", "Choose Strata:", choices = NULL, multiple = FALSE),
               actionButton("generateTableOne", "Generate Table"),
               tableOutput("tableOneOutput"),
               h2("R Code for TableOne"),
               verbatimTextOutput("tableoneRCode"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  dataset <- ggplot2::mpg
  
  observe({
    # Identify categorical variables for potential stratification
    categorical_vars <- names(dataset)[sapply(dataset, function(x) is.factor(x) | is.character(x) | length(unique(x)) < 10)]
    
    # Update UI elements for stratification choices
    updateSelectInput(session, "tableoneStrata", choices = categorical_vars, selected = NULL)
  })
  
  output$data <- renderTable({
    req(input$columns)  # ensure columns are selected
    head(dataset[ , input$columns], input$rows)
  })
  
  output$rcode <- renderText({
    req(input$columns)  # ensure columns are selected
    paste("data_subset <- mpg[ , c(", paste(sQuote(input$columns), collapse=", "), ")]\n",
          "head(data_subset, ", input$rows, ")", sep="")
  })
  
  observeEvent(input$generateTableOne, {
    req(input$tableoneVar)  # Ensure variables are selected
    
    # Create TableOne object
    tableOne <- CreateTableOne(vars = input$tableoneVar, strata = input$tableoneStrata, data = dataset, test = FALSE)
    
    # Convert the tableOne object to a matrix for rendering
    output$tableOneOutput <- renderTable({
      print(tableOne, text = TRUE)
    }, rownames = TRUE)
    
    # Generate R code for creating the TableOne object
    strata_part <- if(!is.null(input$tableoneStrata) && input$tableoneStrata != "") {
      paste(", strata = \"", input$tableoneStrata, "\"", sep = "")
    } else {
      ""
    }
    output$tableoneRCode <- renderText({
      req(input$tableoneVar)  # Ensure variables are selected
      strata_part <- if(!is.null(input$tableoneStrata) && input$tableoneStrata != "") {
        paste(", strata = \"", input$tableoneStrata, "\"", sep = "")
      } else {
        ""
      }
      paste("CreateTableOne(vars = c(", paste(sQuote(input$tableoneVar), collapse = ", "), ")",
            strata_part, ", data = mpg, test = FALSE)")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

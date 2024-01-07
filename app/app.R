# Load necessary libraries
library(shiny)
library(Publish)
library(jtools)
library(ggstance)
library(broom.mixed)
library(huxtable)
require("tableone")

# Load your data
load("NHANESanalytic.Rdata")
analytic.data2 <- as.data.frame(na.omit(analytic.data1))
rm(analytic.data1)
ui <- fluidPage(
  # Application title
  titlePanel("App for Research Question"),
  
  # Sidebar with a tabset for different analyses
  sidebarLayout(
    sidebarPanel(
      # Note or label about Diastolic
      p("Note: 'diastolic' is always included as an outcome. "),
      
      # Input widgets for variable selection
      checkboxGroupInput("varSelect", "Choose Variables:",
                         choices = list("Systolic blood pressure" = "systolic", 
                                        "Smoking habit" = "smoke", 
                                        "Race" = "race", 
                                        "Age (centred)" = "age.centred", 
                                        "Gender" = "gender", 
                                        "Marital status" = "marital", 
                                        "Alcohol consumption" = "alcohol"),
                         selected = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Regression", verbatimTextOutput("regressionModel"), plotOutput("regressionPlot")),
        tabPanel("Summary", tableOutput("summaryTable"))
      )
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Generate summary table based on selected variables
  output$summaryTable <- renderTable({
    # analytic.data2 <- as.data.frame(na.omit(analytic.data1))
    # Include 'diastolic' and selected variables
    varSelect1 <- c(input$varSelect, "diastolic")
    tabOne <- CreateTableOne(data = analytic.data2, includeNA = TRUE, vars = varSelect1)
    
    # Convert the tableOne object to a matrix for rendering
    matrixRepresentation <- print(tabOne, text = TRUE)
    matrixRepresentation
  }, rownames = TRUE)
  
  output$regressionModel <- renderPrint({
    # Check if any variables are selected; if none, default to intercept only model
    independentVars <- if(length(input$varSelect) > 0) paste(input$varSelect, collapse = " + ") else "1"
    formula <- as.formula(paste("diastolic ~", independentVars))
    fit23 <- glm(formula, data=analytic.data2)
    publish(fit23)  # Display a summary of the glm model
  })
  
  output$regressionPlot <- renderPlot({
    if(length(input$varSelect) > 0){
      # Check if any variables are selected; if yes, proceed with the plot
      independentVars <- paste(input$varSelect, collapse = " + ")
      formula <- as.formula(paste("diastolic ~", independentVars))
      
      # Fit the model using the dynamically constructed formula
      fit23 <- glm(formula, data=analytic.data2)
      
      # Plot summaries of the glm model using jtools::plot_summs
      plot_summs(fit23)
    } else {
      # If no variables are selected, don't plot anything
      plot.new()
      text(0.5, 0.5, "Select at least 1 variable.", cex = 1.5)
    }
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line
            tags$hr(),
            
            # Input: Checkbox if file has header
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line
            tags$hr(),
            
            # Input: Select number of rows to display
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Action button to trigger linear model plot
            actionButton("go", "Plot Linear Model")
        ),

        # Show plot of generated distribution


        # Main panel for displaying plots and tables
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
            tags$h3("Slope:"),
            verbatimTextOutput("slope"),
            tags$h3("Intercept:"),
            # verbatimTextOutput("intercept"),
            tags$h3("Correlation Coefficient:"),
           # verbatimTextOutput("correlation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

lmdata <- reactiveValues(model = NULL)

    
    # Reactive expression for reading uploaded data
    dataInput <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })

    # Reactive expression for updating linear model
    observeEvent(input$go, {
        lmdata$model <- lm(y ~ x, data = dataInput())
     lmdata$coef_vals <- coef(lmdata$model)
    lmdata$intercept <- lmdata$coef_vals[1]
    lmdata$slope <- lmdata$coef_vals[2]
    lmdata$correlation <- round(cor(dataInput()$x, dataInput()$y), 2)
    })


    # Render plot for original data
    output$origPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y)
    })

    # Render plot for linear model
    # output$lmPlot <- renderPlot({
      #  plot(dataInput()$x, dataInput()$y)
       # abline(lmdata$model)
   # })

    # Render plot for linear model
output$lmPlot <- renderPlot({
    data <- dataInput()
    plot(data$x, data$y)
    abline(lmdata$model)
    
    # Extract coefficients and correlation coefficient
   # coef_vals <- coef(lmdata$model)
    #intercept <- coef_vals[1]
    #slope <- coef_vals[2]
    #correlation <- round(cor(data$x, data$y), 2)
    
    # Construct the text to display on the plot
  #  text_pos <- min(data$x) + 0.05 * (max(data$x) - min(data$x))
    text_labels <- paste(
        "Slope:", round(lmdata$slope, 2), " ",
        "Intercept:", round(lmdata$intercept, 2), " ",
        "Correlation:", lmdata$correlation
    )
    
    # Add text annotation to the plot
    title(main = "Graph", sub = text_labels)
})

    # Render table for data display
    output$contents <- renderTable({
        if (input$disp == "head") {
            return(head(dataInput()))
        } else {
            return(dataInput())
        }
    })
    output$slope <- renderText(paste(
        "Slope:", round(lmdata$slope, 2), " ",
        "Intercept:", round(lmdata$intercept, 2), " ",
        "Correlation:", lmdata$correlation
    ))
}

# Run the application 
shinyApp(ui = ui, server = server)

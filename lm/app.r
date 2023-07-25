#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

                 # Add the button to model the data
      actionButton("model_button", "Model Data"),
),

    
    # Output slope, intercept, and correlation coefficient
    h3("Linear Model Results:"),
    h5("Slope:"),
    verbatimTextOutput("slope"),
    h5("Intercept:"),
    verbatimTextOutput("intercept"),
    h5("Correlation Coefficient:"),
    verbatimTextOutput("correlation"),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
output$distPlot <- renderPlot({
        data <- dataInput()
        plot(data[, 1], data[, 2], xlab = colnames(data)[1], ylab = colnames(data)[2], main = "Scatter Plot")
    })

    # Function to fit a linear model and return the model object and predictions
    fit_linear_model <- function() {
        data <- dataInput()
        lm_fit <- lm(data[, 2] ~ data[, 1])  # Fit a linear model using the first two columns of data
        data$predictions <- predict(lm_fit)  # Generate predictions from the linear model
        return(list(model = lm_fit, predictions = data$predictions))
    }

  # Click event for "Model Data" button
observeEvent(input$model_button, {
    model_result <- fit_linear_model()
    output$lmPlot <- renderPlot({
        data <- dataInput()
        plot(data[, 1], data[, 2], xlab = colnames(data)[1], ylab = colnames(data)[2], main = "Scatter Plot with Linear Model", col = "blue")
        lines(data[, 1], model_result$predictions, col = "red")  # Overlay the linear model predictions in red
    })

     # Calculate and output slope, intercept, and correlation coefficient
        model <- model_result$model
        if (!is.null(model)) {
            output$slope <- renderText({
                coef(model)[[2]]
            })

            output$intercept <- renderText({
                coef(model)[[1]]
            })

            output$correlation <- renderText({
                data <- dataInput()
                cor(data[, 2], model_result$predictions)
            })
        }
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)

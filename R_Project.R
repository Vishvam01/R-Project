# Load necessary libraries
library(shiny)
library(ggplot2)
library(caTools)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Regression Analysis - Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Salary Data (.csv)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("file2", "Upload 50 Startups Data (.csv)", accept = c(".csv")),
      fileInput("file3", "Upload Position Salaries Data (.csv)", accept = c(".csv")),
      sliderInput("split_ratio", "Training Set Split Ratio:", min = 0.5, max = 0.9, value = 0.67),
      actionButton("run_analysis", "Run Regression Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simple Linear Regression", 
                 h3("Training Set Plot"),
                 plotOutput("plot1"),
                 h3("Test Set Plot"),
                 plotOutput("plot2")),
        
        tabPanel("Multiple Regression", 
                 h3("Summary"),
                 verbatimTextOutput("mult_reg_summary")),
        
        tabPanel("Polynomial Regression", 
                 h3("Polynomial Plot"),
                 plotOutput("plot3"),
                 h3("Higher Resolution Plot"),
                 plotOutput("plot4"))
      )
    )
  )
)

# Define server logic required for regression analysis
server <- function(input, output) {
  
  # Reactive data input for Salary dataset
  salary_data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # Reactive data input for 50 Startups dataset
  startups_data <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath)
  })
  
  # Reactive data input for Position Salaries dataset
  position_data <- reactive({
    req(input$file3)
    read.csv(input$file3$datapath)
  })
  
  # Simple Linear Regression for Salary Dataset
  observeEvent(input$run_analysis, {
    dataset <- salary_data()
    
    # Split the dataset into the training set and test set
    set.seed(123)
    split <- sample.split(dataset$Salary, SplitRatio = input$split_ratio)
    training_set <- subset(dataset, split == TRUE)
    test_set <- subset(dataset, split == FALSE)
    
    # Fit Simple Linear Regression to the Training set
    regressor <- lm(formula = Salary ~ YearsExperience, data = training_set)
    
    # Visualize the training set results
    output$plot1 <- renderPlot({
      ggplot() +
        geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), color = 'black') +
        geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), color = 'red') +
        ggtitle("Salary vs Experience (Training Set)") +
        xlab("Years of Experience") +
        ylab("Salary")
    })
    
    # Visualize the test set results
    output$plot2 <- renderPlot({
      ggplot() +
        geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), color = 'hotpink') +
        geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), color = 'yellow') +
        ggtitle("Salary vs Experience (Test Set)") +
        xlab("Years of Experience") +
        ylab("Salary")
    })
  })
  
  # Multiple Regression for 50 Startups Dataset
  observeEvent(input$run_analysis, {
    dataset <- startups_data()
    
    # Encode categorical data (State variable)
    dataset$State <- factor(dataset$State, levels = c('New York', 'California', 'Florida'), labels = c(1, 2, 3))
    
    # Split the dataset into Training and Test sets
    set.seed(123)
    split <- sample.split(dataset$Profit, SplitRatio = 0.8)
    training_set <- subset(dataset, split == TRUE)
    test_set <- subset(dataset, split == FALSE)
    
    # Fit Multiple Linear Regression to the Training set
    regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = training_set)
    
    # Output the regression summary
    output$mult_reg_summary <- renderPrint({
      summary(regressor)
    })
  })
  
  # Polynomial Regression for Position Salaries Dataset
  observeEvent(input$run_analysis, {
    dataset <- position_data()
    
    # Keeping only Level and Salary columns
    dataset <- dataset[2:3]
    
    # Fit Linear Regression to the dataset
    lin_reg <- lm(formula = Salary ~ ., data = dataset)
    
    # Fit Polynomial Regression to the dataset
    dataset$Level2 <- dataset$Level^2
    dataset$Level3 <- dataset$Level^3
    poly_reg <- lm(formula = Salary ~ ., data = dataset)
    
    # Visualize the polynomial regression results
    output$plot3 <- renderPlot({
      ggplot() +
        geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red') +
        geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)), color = 'blue') +
        ggtitle('Polynomial Regression') +
        xlab('Level') +
        ylab('Salary')
    })
    
    # Higher resolution visualization for Polynomial Regression
    output$plot4 <- renderPlot({
      x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
      ggplot() +
        geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'yellow') +
        geom_line(aes(x = x_grid, y = predict(poly_reg, newdata = data.frame(Level = x_grid, Level2 = x_grid^2, Level3 = x_grid^3))), color = 'blue') +
        ggtitle('Polynomial Regression (High Resolution)') +
        xlab('Level') +
        ylab('Salary')
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
install.packages("shiny")

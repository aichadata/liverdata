# Load required libraries for Shiny app, visualization, and correlation analysis
library(shiny)
library(ggpubr)      # For creating polished plots
library(readr)       # For reading CSV files
library(corrplot)    # For correlation matrix visualization



# Load the dataset
data <- read_csv("cleaned_data.csv")

# Ensure the 'Result' column is treated as a character type if it exists
if ("Result" %in% colnames(data)) {
  data$Result <- as.character(data$Result)
}

# Define the User Interface (UI) of the Shiny application
ui <- fluidPage(
  titlePanel("Variable Visualization"),  # Application title
  
  # Tabs for navigating between different functionalities
  tabsetPanel(
    
    # Tab 1: Variable Visualization
    tabPanel(
      "Variable Visualization",
      sidebarLayout(
        sidebarPanel(
          # Dropdown to select a variable for visualization
          selectInput("variable", 
                      label = "Select a variable:", 
                      choices = colnames(data), 
                      selected = colnames(data)[1]),
          
          # Option to apply log transformation for numeric variables
          checkboxInput("log_transform", 
                        label = "Apply log transformation (for numeric variables)", 
                        value = FALSE)
        ),
        mainPanel(
          # Plot output for visualizing the selected variable
          plotOutput("plot")
        )
      )
    ),
    
    # Tab 2: Correlation Matrix
    tabPanel(
      "Correlation Matrix",
      sidebarLayout(
        sidebarPanel(
          # Dropdown to select numerical variables for the correlation matrix
          selectizeInput("corr_vars", 
                         label = "Select numerical variables for correlation matrix:", 
                         choices = colnames(data[sapply(data, is.numeric)]), 
                         selected = colnames(data[sapply(data, is.numeric)]), 
                         multiple = TRUE)
        ),
        mainPanel(
          # Plot output for displaying the correlation matrix
          plotOutput("correlation_plot")
        )
      )
    )
  )
)

# Define the server logic of the Shiny application
server <- function(input, output) {
  
  # Tab 1: Variable Visualization
  output$plot <- renderPlot({
    var_name <- input$variable  # Get the selected variable
    var_data <- data[[var_name]]  # Extract data for the selected variable
    var_class <- class(var_data)  # Identify the variable type
    
    # Handle numeric variables
    if (var_class == "numeric") {
      if (input$log_transform) {
        var_data <- log(var_data + 1)  # Apply log transformation if selected
      }
      
      # Plot density for numeric variables
      ggpubr::ggdensity(data.frame(var_data), 
                        x = "var_data",
                        fill = "lightblue",
                        title = paste("Density Plot of", var_name)) +
        xlab(var_name) +
        ylab("Density")
      
    } else if (var_class == "character" || is.logical(var_data)) {
      # Handle categorical or binary variables by creating a frequency bar chart
      freq_table <- as.data.frame(table(var_data))
      colnames(freq_table) <- c("Category", "Count")
      
      ggpubr::ggbarplot(freq_table, 
                        x = "Category", 
                        y = "Count", 
                        fill = "lightblue", 
                        title = paste("Bar Plot of", var_name)) +
        xlab(var_name) +
        ylab("Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      # Display a message for unsupported variable types
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Unsupported Variable Type")
      text(0.5, 0.5, labels = paste("Variable type", var_class, "is not supported."), cex = 1.2)
    }
  })
  
  # Tab 2: Correlation Matrix
  output$correlation_plot <- renderPlot({
    selected_vars <- input$corr_vars  # Get the selected variables
    
    if (length(selected_vars) >= 2) {
      num_data <- data[selected_vars]  # Subset selected variables
      cor_matrix <- cor(num_data, use = "pairwise.complete.obs")  # Calculate correlation matrix
      
      # Plot the correlation matrix
      corrplot(cor_matrix, method = "color", 
               col = colorRampPalette(c("blue", "white", "red"))(200),
               type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black")
    } else {
      # Message when fewer than two variables are selected
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Select at least two numerical variables")
      text(0.5, 0.5, labels = "Please select at least two numerical variables.", cex = 1.2)
    }
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)


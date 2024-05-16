################################################################################
# A simple example integrating descriptive, predictive, and prescriptive
# analytics into a shiny app
# Team & Member Names:Team 2 - Jinxin_Ren,Daniel_Logan,Chihyi_Yu
################################################################################
# https://shiny.posit.co/
# https://rstudio.github.io/shinythemes/
#install.packages("shinythemes")
library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  #theme = bs_theme(bootswatch = "minty"),
  shinythemes::themeSelector(),  # allows us to change theme
  
  # Add a boilermaker background image
  tags$style(HTML("
    body {
      background-image: url('https://upload.wikimedia.org/wikipedia/commons/8/8f/Purdue_Pete_modern.jpg');
      background-size: cover;
    }
  ")),
  # Add a dark background for better content readability
  tags$style(HTML("
    /* Adjusting text color for h3 and p elements under the Introduction tab */
    .tab-content .active h3, .tab-content .active p {
        color: #E0E0E0;  /* Light gray color */
    }
    
    /* Optional: Adding a slightly darker background for better contrast */
    .tab-content .active {
        background-color: rgba(0, 0, 0, 0.7);  /* Black with 70% opacity */
        padding: 15px;  /* Some padding for better spacing */
        border-radius: 5px;  /* Rounded corners */
    }
")),
  
  titlePanel("Using R for Analytics"),
  mainPanel(
    navbarPage(
      tabPanel("--"),
      tabPanel("Introduction",
               h3("Problem Description"),
               p("Stamina Glass is a local glass assembly company in Indianapolis and it has four subordinate glass suppliers. Recently, the owner of Stamina Glass wanted to
                  reduce the glass breakage rate to compete for the opportunity to become an exclusive windshield provider for a big local automobile company. So Stamina Glass
                  hired our team to find out what contributes to the glass breakage rate and to provide guidance for the technicians from the 4 different suppliers on how to 
                  further reduce the glass breakage rate."),
               p("<Refinement and Constraints> In our analysis of the glass breakage rate, we have chosen to focus on three primary aspects: the manufacturing process, suppliers,
                  and window frame specifications. While these are our primary areas of investigation, we acknowledge that there are other potential factors that could influence 
                  the glass breakage rate. These include variations in raw materials, human factors in the manufacturing process, and external conditions such as atmospheric pressure."),
               p("<Assumptions Made> For the purpose of our analysis, we have made the assumption that the influence of these additional factors is negligible. This allows us to concentrate 
                  on the data available and identify the most influential factors within our chosen areas of focus."),
               p("<Business Benefits> Stamina is expected to reduce the glass breakage rate by 5%."),
               
               h3("Analytics Problem Framing"),
               p("We utilized a linear regression model to address the problem because we wanted to analyze the relationship between several independent variables and the dependent variable - Breakage rate."),
               p("<Drivers> Common intuition suggests that varied manufacturing process would result in a different breakage rate. Our objective is to validate this assumption with concrete evidence. We also
                  aim to quantify the influence of supplier variability on the breakage rate and determine their significance in our analysis."),
                 p("<Key Assumptions>"),
                 p("H0: Manufacturing process has no significant influence on breakage rate | H1: Manufacturing process has significant influence on breakage rate"),
                 p("H0: Supplier variability has no significant influence on breakage rate | H1: Supplier variability has significant influence on breakage rate"),
                 p("<Key metrics of success>"),
                 p("Reduction in Breakage Rate: A measurable decrease in the breakage rate post-implementation of recommended changes, compared to historical data."),
                 p("High predictive Accuracy: We will deteremine how well the model predicts breakage rates based on the identified drivers"),
               
               h3("Data"),
               p("We utilized the data stored in Stamina's internal database to conduct this analysis."),
               p("The dataset is called 'Window_Manufacturing.csv' including the following columns:"),
               tableOutput("dataDictTable")
      ),
      
      
      tabPanel("Descriptive Analytics",
               plotOutput(outputId="multi_plot", height="800px", width="1000px")

      ),
      
      
      tabPanel("Predictive Analytics",
                 mainPanel(
                   fluidRow(
                     column(6, tableOutput(outputId="coeffTable")),       # Half the width (6/12) for the table
                     column(6, verbatimTextOutput(outputId="evalMetrics"))# Half the width (6/12) for the evaluation metrics
                   ),
                 )
        ),
      
      tabPanel("Prescriptive Analytics",
               sidebarPanel(
                 actionButton(inputId = "run2", label = "Run")
               ), 
               textOutput(outputId="optResult"), 
               textOutput(outputId="optDecisions"),
               sidebarPanel(sliderInput(inputId = "obs",label = "Ambient Temp",
                                        min = 8.393, max = 24.098, value = 10)) # Slider set within historic temperature range
      )
    ),

  tags$footer(style = "position: fixed; right: 0; bottom: 0; height: 80px; background-color: #f5f5f5; padding: 10px;",
              tags$div("Shiny App Demo © 2023 | Version 1.0.0 | Developed by [Team 2] | Last Updated: October 14, 2023"),
              tags$div("Disclaimer: This app is for demonstration purposes only. Use at your own risk.")
  )
),
)
server = function(input, output) {
  ################# Data Dictionary #######################################
  data_dictionary <- data.frame(
    variable_name = c("Breakage Rate", "Window Size", "Glass Thickness","Ambient Temp","Cut Speed","Edge Deletion rate",
                      "Spacer Distance","Window color","Window Type","Glass Supplier","Silicon Viscosity","Glass Supplier Location"),
    R_data_type = c("numeric", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","factor","factor",
                    "numeric","factor"),
    description = c("Breakage rate in percentage", "Area of glass", "Thickness in cm","Environment temperature","The speed to cut glass",
                    "The removal part around the perimeter","Space between the two glass plies","Color of glass","Window frame material",
                    "Supplier name","Stands for glass hardness","Supplier address")
  )
  
  # Render the data dictionary as a table
  output$dataDictTable <- renderTable({
    data_dictionary
  })
  
  
  ################# Default data uploading #######################################
  #install.packages("readxl")
  library(readxl)
  data <- read_excel("Window_Manufacturing.xlsx")
  names(data) <- c("Breakage_Rate","Window_Size","Glass_Thickness","Ambient_Temp",
                   "Cut_Speed","Edge_Deletion_Rate","Spacer_Distance","Window_color",
                   "Window_Type","Glass_Supplier","Silicon_Viscosity","Glass_Supplier_Location")
    
  
  #################  Data pre-processing #######################################
  
  #Check missing data
  missing_data <- sapply(data, function(x) sum(is.na(x)))
  print(missing_data)
  
  #Replace the missing value in continuous columns with mean value 
  numeric_vars <- c("Window_Size", "Ambient_Temp", "Cut_Speed", "Edge_Deletion_Rate", "Spacer_Distance", "Silicon_Viscosity")
  
  for (col_name in numeric_vars) {
    data[[col_name]][is.na(data[[col_name]])] <- mean(data[[col_name]], na.rm = TRUE)
  }
  
  #Replace the missing value in categorical columns with mode
  categorical_vars <- c("Window_Type", "Glass_Supplier", "Glass_Supplier_Location")
  
  for (col_name in categorical_vars) {
    mode_val <- as.character(names(sort(table(data[[col_name]]), decreasing = TRUE)[1]))
    data[[col_name]][is.na(data[[col_name]])] <- mode_val
  }
  
  
  ################# Descriptive Tab #######################################
  #install.packages('gridExtra')
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  output$multi_plot <- renderPlot({
    
      
      plot1 <- ggplot(data, aes(x=Window_Type, y=Breakage_Rate)) + 
        geom_boxplot(aes(fill=Window_Type)) +
        labs(title="Boxplot of Breakage Rate by Window Type", 
             x="Window Type", 
             y="Breakage Rate") +
        theme_minimal() +
        theme(legend.position="none")
      print(plot1)
      
      
      
      # Calculate the average breakage rate for each supplier
      avg_data <- data %>%
        group_by(Glass_Supplier) %>%
        summarise(avg_breakage = mean(Breakage_Rate, na.rm = TRUE))
      
      plot2 <- ggplot(avg_data, aes(x=Glass_Supplier, y=avg_breakage)) + 
        geom_col(aes(fill=Glass_Supplier)) + 
        labs(title="Average Breakage Rate by Glass Supplier", y="Average Breakage Rate")
      print(plot2)
      
      
      
      plot3 <- ggplot(data, aes(x=Glass_Thickness, y=Breakage_Rate)) +
        geom_point(aes(color=Glass_Thickness), size=3, alpha=0.7) +
        geom_smooth(method="lm", se=FALSE, color="blue") +
        labs(title="Linear Regression of Breakage Rate on Glass Thickness", 
             x="Glass Thickness", 
             y="Breakage Rate") +
        scale_color_gradient(low="blue", high="red") +
        theme_minimal() +
        theme(legend.position="bottom", 
              legend.title=element_text(face="bold"),
              panel.grid.major = element_line(color="grey80"),
              panel.grid.minor = element_blank(),
              strip.text = element_text(face="bold", size=12),
              panel.background = element_rect(fill="ivory"),
              axis.text = element_text(color="black", size=12))
      print(plot3)
      
      
      plot4 <- ggplot(data, aes(x=Window_Size, y=Breakage_Rate, color=Cut_Speed)) +
        geom_point(size=3) +
        scale_color_gradient(low="green", high="blue") +
        labs(title="Scatter plot of Breakage Rate vs Glass Thickness colored by Cut Speed",
             x="Window Size", y="Breakage Rate", color="Cut Speed") +
        theme_minimal()
      
      print(plot4)
      
      
      
      
       # Arrange in 2x2 grid
      grid.arrange(plot1, plot2,plot3,plot4, ncol=2)
      
  })
  
  ################# Predictive Tab #######################################
  library(caret)
  
  output$coeffTable <- renderTable({
    f <- lm(Breakage_Rate ~ . - Glass_Supplier_Location, data=data)
    coeff_names <- rownames(summary(f)[["coefficients"]])
    coeff_estimates <- summary(f)[["coefficients"]][, "Estimate"]
    coeff_pvalues <- summary(f)[["coefficients"]][, "Pr(>|t|)"]
    data.frame(Coefficient = coeff_names, Estimate = coeff_estimates, P_Value = coeff_pvalues)
  })
  
  output$evalMetrics <- renderPrint({
    inTrain <- createDataPartition(y = data$Breakage_Rate, p = .70, list = FALSE)
    train <- data[inTrain, ]
    test <- data[-inTrain, ]
    
    train_model <- lm(Breakage_Rate ~ . - Glass_Supplier_Location, data=train)
    
    # Predict on training set
    train_pred <- predict(train_model, newdata=train)
    train_res <- defaultSummary(data.frame(obs = train$Breakage_Rate, pred = train_pred))
    
    # Predict on test set
    test_pred <- predict(train_model, newdata=test)
    test_res <- defaultSummary(data.frame(obs = test$Breakage_Rate, pred = test_pred))
    
    # Print the metrics
    cat("Training Metrics:\n")
    print(train_res)
    
    cat("\nTest Metrics:\n")
    print(test_res)
    
    
  })
  
  #Define coeff_estimates as a reactive expression outside of the output$coeffTable block
  coeff_estimates <- reactive({
    f <- lm(Breakage_Rate ~ . - Glass_Supplier_Location, data=data)
    return(summary(f)[["coefficients"]][, "Estimate"])
  })


  ################# Prescriptive Tab #######################################
  #install.packages("lpSolve")
  #install.packages("lpSolveAPI")
  # formulate an optimization model 
  model2 <- eventReactive(input$run2, {
    library(lpSolveAPI)
    library(lpSolve)
    options(scipen=999)
    # there are 13 decision variables
    (lps.model <- make.lp(nrow=0, ncol=14))
    # real decision variables
    set.type(lps.model, columns=1, type="real")
    set.type(lps.model, columns=2, type="real")
    set.type(lps.model, columns=3, type="real")
    set.type(lps.model, columns=4, type="real")
    set.type(lps.model, columns=5, type="real")
    set.type(lps.model, columns=6, type="real")
    set.type(lps.model, columns=7, type="real")
    set.type(lps.model, columns=8, type="real")
    set.type(lps.model, columns=c(9,10), type="binary")
    set.type(lps.model, columns=c(11,12,13), type="binary")
    #set.type(lps.model, columns=10, type="binary")
    #set.type(lps.model, columns=12, type="binary")
    # set objective function
    lp.control(lps.model, sense="min")
    set.objfn(lps.model, obj=coeff_estimates())
    # define constraints
    # 1s and 0s are parameters
    # c(1,0,0,0,0,0,0,0,0,0,0,0,0,0) is y/intercept
    # c(0,1,0,0,0,0,0,0,0,0,0,0,0,0) is the Window Size
    # c(0,0,1,0,0,0,0,0,0,0,0,0,0,0) is the Glass Thickness
    # ...
    # c(0,0,0,0,0,0,0,0,0,0,0,0,0,1) is the Silicon Viscosity

    # Y Constraint 
    add.constraint(lps.model, c(1,0,0,0,0,0,0,0,0,0,0,0,0,0), "=", 1)
    # Ambient Temperature Slider
    add.constraint(lps.model, c(0,0,0,1,0,0,0,0,0,0,0,0,0,0), "=", input$obs)#slider constraint
    # The following are box constraints for the ranges of each variable:
    # Window size range (51.92, 75.57)
    add.constraint(lps.model, c(0,1,0,0,0,0,0,0,0,0,0,0,0,0), ">=", 51.92)
    add.constraint(lps.model, c(0,1,0,0,0,0,0,0,0,0,0,0,0,0), "<=", 75.57)
    # Glass thickness range (0.4797, 0.5231)
    add.constraint(lps.model, c(0,0,1,0,0,0,0,0,0,0,0,0,0,0), ">=", 0.4797)
    add.constraint(lps.model, c(0,0,1,0,0,0,0,0,0,0,0,0,0,0), "<=", 0.5231)
    # Cut speed range (0.2964, 3.2180)
    add.constraint(lps.model, c(0,0,0,0,1,0,0,0,0,0,0,0,0,0), ">=", 0.2964)
    add.constraint(lps.model, c(0,0,0,0,1,0,0,0,0,0,0,0,0,0), "<=", 3.2180)
    # Edge deletion rate (13.75, 17.75)
    add.constraint(lps.model, c(0,0,0,0,0,1,0,0,0,0,0,0,0,0), ">=", 13.75)
    add.constraint(lps.model, c(0,0,0,0,0,1,0,0,0,0,0,0,0,0), "<=", 17.75)
    # Spacer distance (2.690, 5.810)
    add.constraint(lps.model, c(0,0,0,0,0,0,1,0,0,0,0,0,0,0), ">=", 2.690)
    add.constraint(lps.model, c(0,0,0,0,0,0,1,0,0,0,0,0,0,0), "<=", 5.810)
    # Window color range (7.55, 98.05)
    add.constraint(lps.model, c(0,0,0,0,0,0,0,1,0,0,0,0,0,0), ">=", 7.55)
    add.constraint(lps.model, c(0,0,0,0,0,0,0,1,0,0,0,0,0,0), "<=", 98.05)
    # Silicon viscosity range (7.806, 16.196)
    add.constraint(lps.model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,1), ">=", 7.806)
    add.constraint(lps.model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,1), "<=", 16.196)
    # Type and Supplier binary variable constraint
    add.constraint(lps.model, c(0,0,0,0,0,0,0,0,1,1,0,0,0,0), "=", 1)
    add.constraint(lps.model, c(0,0,0,0,0,0,0,0,0,0,1,1,1,0), "=", 1)
    # Solve the model
    solve(lps.model)
    
    # final model that is ready to be solved
    lps.model 
  })
  
  output$optResult <- renderText({
    paste("Optimal Breakage Rate:",round(get.objective(model2()),2))
  }) 
  
  output$optDecisions <- renderText({
    paste("Decisions:", data.frame(lpSolveAPI::get.variables(model2())))
  })
  
  
}


shinyApp(ui = ui, server = server)
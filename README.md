# Window-Manufacturing-DSS

## Shiny App Demo and Presentation
[Shiny app link](https://jensenren.shinyapps.io/test/)  
[Presentation video link](https://drive.google.com/file/d/1madBGUTI6_453hEE7OGBcnkW1289v0_V/view?usp=sharing)

## General Description
This repository contains the final project for the "Using R for Analytics" (MGMT 590) course. The project involves developing a Shiny application that serves as a Decision Support System (DSS) to help identify optimal manufacturing process settings, suppliers, and customer window specifications to reduce the window breakage rate. The project demonstrates the integration of descriptive, predictive, and prescriptive analytics using R libraries and functions.

## Project Description
The Shiny app developed in this project aims to provide actionable insights and recommendations to window manufacturing technicians. It leverages interactive data analytics to support decision-making and improve manufacturing processes. The application includes the following components:

### Descriptive Analytics
- Utilizes the `ggplot2` library to create visually appealing graphs that highlight key relationships in the data.
- Includes various descriptive statistics and summaries to understand the current state of the manufacturing process.

### Predictive Analytics
- Implements linear regression models using the `lm()` and `caret` libraries to predict window breakage rates based on different process settings.
- Displays the estimated parameters and p-values, as well as model fit evaluation statistics.

### Prescriptive Analytics
- Uses the final predictive model as an objective function in an optimization model.
- Identifies controllable and non-controllable decision variables, allowing users to set values for non-controllable variables and solve for optimal settings.
- Adds relevant constraints to ensure the solution is practical and within the scope of the original data.

## Dataset
The dataset used in this project, `Window_Manufacturing.xlsx`, contains various parameters related to the manufacturing process of windows. It includes information such as supplier details, process settings, and breakage rates. The data is used to build and validate the descriptive, predictive, and prescriptive models in the Shiny app.

## Outcomes
The Shiny app provides the following outcomes:
- Visualizations and descriptive statistics that offer insights into the factors affecting window breakage rates.
- Predictive models that estimate the likelihood of window breakage based on different settings.
- Optimal recommendations for process settings to minimize window breakage rates, improving overall manufacturing efficiency.

## Usage
To run the Shiny app locally:
1. Clone this repository to your local machine.
2. Open the R project file in RStudio.
3. Install the required libraries using `install.packages()` if not already installed.
4. Run the app by executing `shiny::runApp('path_to_app')`.

## Conclusion
This project showcases the application of R for developing a comprehensive DSS that integrates various analytics methods. The Shiny app serves as a valuable tool for window manufacturing technicians to make data-driven decisions and enhance the efficiency and quality of the manufacturing process.

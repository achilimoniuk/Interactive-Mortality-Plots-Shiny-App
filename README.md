# Interactive Mortality Plot Shiny App
## Description
The Interactive Mortality Time Series Plot App is a project aimed at providing users with an interactive application that displays time series plots showcasing mortality rates among various social groups and regions. The app specifically focuses on presenting mortality data for Polish and EU citizens, allowing users to select their preferred social group based on factors such as sex, age, and more. As the user makes their selections, the plot dynamically updates to reflect the chosen parameters. Furthermore, the app provides the ability to generate mortality plots for specific time intervals of interest. Additionally, geographical plots are available for both Poland and the EU.
At the end it is possible to save the chosen plots as PDF Report.

## Purpose
The main purpose of this project is to create a user-friendly and interactive application that visualizes mortality trends among different social groups and regions. By providing accessible visual representations of mortality data, the app aims to enhance understanding and analysis of mortality patterns, particularly for Polish and EU citizens.

## Features
- Displays time series plots showcasing mortality rates among various social groups and regions.
- Allows users to select social groups based on factors such as sex, age, etc.
- Updates the plot dynamically based on user selections.
- Provides the option to generate mortality plots for specific time intervals.
- Offers geographical plots for both Poland and the EU.

## Files
- `app.R` - main app that is needed to be run.
- `downloading.R`- file used in `app.R` to download data.
- `Report.Rmd`- file used in `app.R` to generated the report from the analysis.

## Requirements 
To run this app, make sure you have the following packages installed:
- tidyverse
- ggplot2
- tseries
- lmtest
- knitr
- markdown
- rmarkdown
- googleVis
- plotly
- shiny
- DBI
- DT
- "rio"

## Usage
To use the Interactive Mortality Time Series Plot App, follow these steps:

1. Install the necessary dependencies and libraries specified in the project's requirements file.
2. Launch the app by running `app.R.` file and follow the given steps.
3. Once the app is running, navigate to the interactive interface.
4. Choose the desired social group by selecting the relevant parameters, such as sex and age.
5. Explore the dynamically updated time series plot showcasing the mortality rates for the selected social group.
6. To generate mortality plots for specific time intervals, specify the desired start and end dates.
7. Additionally, explore the geographical plots to visualize mortality trends in Poland and the EU.
8. Analyze the displayed plots to gain insights into mortality patterns among different social groups and regions.
9. Save chosen plots as PDF report.

## Contributors
Agnieszka Chilimoniuk

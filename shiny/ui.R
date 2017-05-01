library(shiny)

# Define UI for application that draws a histogram
#  by Jake Reeves

shinyUI(fluidPage(

  # Application title
  titlePanel("Heterosis-GRN-simulation"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
	h4("Simulation of Heterosis using GRN"),
      sliderInput("nodes",
                  "Number of genes and environmental factors (nodes):",
                  min = 5,
                  max = 100,
                  value = 30),
	sliderInput("adjust",
                  "Time for adjusting to initial environment (adjust):",
                  min = 20,
                  max = 2000,
                  value = 30),
	sliderInput("gaps",
                  "After evolution of the first world, gaps for each sampling (gaps):",
                  min = 1,
                  max = 100,
                  value = 5),
	sliderInput("rep",
                  "Repeat of sampling (rep):",
                  min = 1,
                  max = 100,
                  value = 30),
	sliderInput("pops",
                  "Number of total population during the simulation (pops):",
                  min = 2,
                  max = 10,
                  value = 2),
	sliderInput("max.popn",
                  "Maximum number of individuals in each population (max.popn):",
                  min = 5,
                  max = 100,
                  value = 20),
	actionButton("gametes", "New gametes", style='margin-bottom:20px;'),
	actionButton("environments", "Adjust to Env.", style='margin-bottom:20px;'),
	actionButton("heterosis", "Simulate heterosis", style='margin-bottom:20px;')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("networkOut")
    )
  )
))
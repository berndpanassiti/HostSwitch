# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Simulate parasite host switching"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton(inputId = "refresh",
                   label = "Refresh simulation", icon = icon("fa fa-refresh")),

      sliderInput(inputId = "mig",
                  label = "Migration probability:",
                  min = 0,
                  max = 1,
                  value = 0.01),
      sliderInput(inputId = "b",
                  label = "Birth rate:",
                  min = 0,
                  max = 50,
                  value = 10),
      sliderInput(inputId = "n_generation",
                  label = "Number of generations:",
                  min = 0,
                  max = 1000,
                  value = 200),
      sliderInput(inputId = "K",
                  label = "Carrying capacity:",
                  min = 0,
                  max = 1000,
                  value = 100),
      sliderInput(inputId = "sd",
                  label = "Standard deviation for mutation:",
                  min = 0,
                  max = 10,
                  value = 0.2,
                  step= 0.1),
      sliderInput(inputId = "sigma",
                  label = "Standard deviation for selection:",
                  min = 0,
                  max = 10,
                  value = 1,
                  step=0.1)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Shiny plot of host switches by parasites ----
      plotOutput(outputId = "HostSwitchPlot"),
      tableOutput("HostSwitchSummary")
    )
  )
)

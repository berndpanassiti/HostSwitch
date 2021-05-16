# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Simulate consumer host-switching"),

  div(id = "app_info", class = "collapse out",
      p("This visualisation is based on a Host Switch simulation model. See:"),
      #tags$a(href = "", ""),
      p(""),
      p("For further information, visit the project summary page:"),
      #tags$a(href = "", "")
      ),

  HTML("<button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#app_info'><span class='glyphicon glyphicon-collapse-down'></span> More Information</button>"),

  br(),  br(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton(inputId = "refresh",
                   label = "Refresh simulation", icon = icon("fa fa-refresh")),

      tippy::with_tippy(sliderInput(inputId = "mig",
                  label = "Dispersal probability:",
                  min = 0,
                  max = 1,
                  value = 0.01),
      "Migration rate"),
      tippy::with_tippy(sliderInput(inputId = "b",
                  label = "Birth rate:",
                  min = 0,
                  max = 50,
                  value = 10),
                  "Number of offspring in the next generation"),
      tippy::with_tippy(sliderInput(inputId = "n_generations",
                  label = "Number of generations:",
                  min = 0,
                  max = 1000,
                  value = 200),
                  "Number of generations"),
      tippy::with_tippy(sliderInput(inputId = "K",
                  label = "Carrying capacity:",
                  min = 0,
                  max = 1000,
                  value = 100),
                  "Upper limit allowed for consumer population"),
      tippy::with_tippy(sliderInput(inputId = "sd",
                  label = "Standard deviation of mutation:",
                  min = 0,
                  max = 10,
                  value = 0.2,
                  step= 0.1),
                  "Mutation of consumer phenotypes"),
      tippy::with_tippy(sliderInput(inputId = "sigma",
                  label = "Standard deviation of selection:",
                  min = 0,
                  max = 10,
                  value = 1,
                  step=0.1),
                  "Niche breadth"),
  tippy::with_tippy(selectInput(inputId = "jump_back",
                  label = "Jump back:",
                  selected="no",
                  choices=c("no","yes")),
                  "Let the consumer jump back to previous resource")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Shiny-plot of consumer host-switching ----
      plotOutput(outputId = "HostSwitchPlot",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE)
                 ),
      tableOutput(outputId = "HostSwitchSummary")
    )
  )
)

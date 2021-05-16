# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  #titlePanel("Simulate consumer host-switching"),
  HTML("<h3>Simulate consumer host-switching</h3>"),
  div(id = "app_info", class = "collapse out",
      p("This visualisation is based on a Host Switch simulation model. See:"),
      #tags$a(href = "", ""),
      p(""),
      p("For further information, visit the project summary page:")
      #tags$a(href = "", "")
      ),

  HTML("<button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#app_info'><span class='glyphicon glyphicon-collapse-down'></span> More Information</button>"),

  br(),  br(),

  tags$style(type = "text/css", "
{font-size: 6px;},
      .irs-all {top:-20px;}
             "),


  # Sidebar layout with input and output definitions ----
  fluidRow(
    column(3,

      # Sidebar panel for inputs ----
      sidebarPanel(width = 10,
                          actionButton(inputId = "refresh",
                            label = "Refresh simulation", icon = icon("fa fa-refresh")),

        tippy::with_tippy(sliderInput(inputId = "mig",
                                      label=HTML('<FONT size="2">Dispersal probability:</FONT>'),
                                      min = 0, max = 1,value = 0.01),
                        "Migration rate"),
        tippy::with_tippy(sliderInput(inputId = "b",
                                      label=HTML('<FONT size="2">Birth rate:</FONT>'),
                                      min = 0,max = 50,value = 10),
                          "Number of offspring in the next generation"),
        tippy::with_tippy(sliderInput(inputId = "n_generations",
                                      label=HTML('<FONT size="2">Number of generations:</FONT>'),
                                    min = 0,max = 1000,value = 200),
                          "Number of generations"),
        tippy::with_tippy(sliderInput(inputId = "K",
                  label=HTML('<FONT size="2">Carrying capacity:</FONT>'),
                  min = 0,
                  max = 1000,
                  value = 100),
                  "Upper limit allowed for consumer population"),
        tippy::with_tippy(sliderInput(inputId = "sd",
                                      label=HTML('<FONT size="2">Standard deviation of mutation:</FONT>'),
                                      min = 0,max = 10,value = 0.2,step= 0.1),
                          "Mutation of consumer phenotypes"),
        tippy::with_tippy(sliderInput(inputId = "sigma",
                 label=HTML('<FONT size="2">Standard deviation of selection:</FONT>'),
                  min = 0,max = 10,value = 1,step=0.1),
                  "Niche breadth"),
        tippy::with_tippy(radioButtons(inputId = "jump_back",
                                      label = "Jump back:",
                                      selected="no",choices=c("no","yes")),
                          "Let the consumer jump back to previous resource")

      )


    # Main panel for displaying outputs ----
    ),

  column(9,
         offset = -3,

      # Output: Shiny-plot of consumer host-switching ----

      plotOutput(outputId = "HostSwitchPlot",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE),
                 height = "500px"
                 ),
      tableOutput(outputId = "HostSwitchSummary")
    )
  )
)

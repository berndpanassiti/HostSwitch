# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$HostSwitchPlot <- renderPlot({

    HostSwitch_simulated_quantities= HostSwitch::simHostSwitch(K=input$K,b=input$b, mig=input$mig, sd=input$sd, sigma=1, pRes_min=1, pRes_max=10, n_generation=input$n_generation)

    HostSwitch::plotHostSwitch(HostSwitch_simulated_quantities)

  })

}

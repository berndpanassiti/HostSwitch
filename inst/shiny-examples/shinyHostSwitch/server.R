# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  HostSwitch_simulated_quantities <- reactive({
    # Make action button dependency
    input$refresh
    # but isolate input$sample
    isolate(HostSwitch::simHostSwitch(K=input$K,b=input$b, mig=input$mig, sd=input$sd, sigma=1, pRes_min=1, pRes_max=10, n_generation=input$n_generation))

  })


  output$HostSwitchPlot <- renderPlot({
    HostSwitch::plotHostSwitch(HostSwitch_simulated_quantities())
    })

}

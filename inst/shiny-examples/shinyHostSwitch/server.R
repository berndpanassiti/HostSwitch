# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  HostSwitch_simulated_quantities <- reactive({
    # Make action button dependency
    input$refresh
    # but isolate input$sample
    isolate(HostSwitch::simHostSwitch(K=input$K,b=input$b, mig=input$mig, sd=input$sd, sigma=input$sigma, pRes_min=1, pRes_max=10, n_generation=input$n_generation))

  })

  output$HostSwitchPlot <- renderPlot({
    HostSwitch::plotHostSwitch(HostSwitch_simulated_quantities())
    })
  output$HostSwitchSummary = renderTable({
    data.frame("Number of parasite jumps"= length(which(HostSwitch_simulated_quantities()$pInd_jump_sim>0)),
                "Number of successful host switches" = c(length(which(HostSwitch_simulated_quantities()$pRes_sim[-1]==HostSwitch_simulated_quantities()$pRes_new_sim)))
                                            , check.names=FALSE)
    },colnames = "TRUE")

}

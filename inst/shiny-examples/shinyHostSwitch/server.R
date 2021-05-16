# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  HostSwitch_simulated_quantities <- reactive({
    # Make action button dependency
    input$refresh
    # but isolate input$sample
    isolate(HostSwitch::simHostSwitch(K=input$K,b=input$b, mig=input$mig, sd=input$sd, sigma=input$sigma, pRes_min=1, pRes_max=10, n_generations=input$n_generations,jump_back=input$jump_back,n_sim=1))

  })




  output$HostSwitchSummary = renderTable({
    data.frame("Total events of dispersion (N dispersed consumers)"= paste(length(which(HostSwitch_simulated_quantities()$pInd_jump_sim[[1]]>0))," (",length(na.omit(unlist(HostSwitch_simulated_quantities()$pInd_whichjump_sim))),")" ,sep=""),
                "Number of successful host switches (N colonizing consumers)" = paste(c(length(which(HostSwitch_simulated_quantities()$pRes_sim[[1]][-1]==HostSwitch_simulated_quantities()$pRes_new_sim[[1]])))," (",length(na.omit(unlist(HostSwitch_simulated_quantities()$pInd_whichsurv_sim))),")" ,sep="")
                                            , check.names=FALSE)
    },colnames = "TRUE")


  # -------------------------------------------------------------------
  # Single zoomable plot
  ranges <- reactiveValues(x = NULL, y = NULL)



  output$HostSwitchPlot <- renderPlot({
    HostSwitch::plotHostSwitch(HostSwitch_simulated_quantities()) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      theme(plot.margin=unit(c(1,1,1,0),"cm"))
  } )


  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

#' Launch a shiny-app simulating the Ricker model
#' @details
#' Launch app for details
#' @examples
#' \dontrun{Ricker.app}
#' @export
ricker.app=shinyApp(
# This creates the User Interface (UI)
ui = pageWithSidebar(
headerPanel("Ricker Model"),
sidebarPanel(
sliderInput("r", "Growth rate (r):", 1,
              min = 0, max = 4, step=.1),
sliderInput("K", "Carrying capacity (K):", 100,
              min = 25, max = 200),
numericInput("X0", "Initial number:", 70,
              min = 1, max = 200),
numericInput("Tmax", "Tmax:", 20,
              min = 1, max = 500)
),

mainPanel(tabsetPanel(
  tabPanel("Simulation", plotOutput("plot1", height = 500)),
  tabPanel("Details",
    withMathJax(
                helpText("MODEL:"),
            helpText("$$X_{t+1} = X_t \\mbox{exp}(r (1- X_t/K))$$"),
            helpText("REFERENCE: Ricker WE (1954) Stock and recruitment. 
              Journal of Fishery Research Board Canada 11: 559-623"),
       helpText(eval(Attr))
)
)
)
)
),

# This creates the 'behind the scenes' code (Server)
server = function(input, output) {
 logist = function(r, K, length = 200, X0=70){
  X =  rep(NA, length) #set up the empty vector of the right length
  X[1] = X0 #setting the abundance at time 1 to N0

  for(i in 2:length){ #iteratively updating the growth model.
                    #next abundance is determined by previous abundance
    X[i] = X[i-1]*exp(r*(1-X[i-1]/K))
    }
  return(X) #returning the simulated vector
  }



  output$plot1 <- renderPlot({

    X= logist(r=input$r, K=input$K, length=input$Tmax, X0=input$X0)
    time = 1:input$Tmax
    par(mfrow=c(1,2))
     plot(X, xlab = "time", ylab = "abundance", type="b") # making a time series plot
    curve(x*exp(input$r*(1-x/input$K)),0,input$K*3, xlab = "Xt-1", ylab = "Xt")
abline(a=0, b=1) # adding the 1-to-1 line
points(X[1:(input$Tmax-1)],X[2:input$Tmax], col = "red") # adding the points
# from the simulation to the graph
lines(X[1:(input$Tmax-1)], X[2:input$Tmax], col = "red") # adding the line to connect the points
   })
  }
)


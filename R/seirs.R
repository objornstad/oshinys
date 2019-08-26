#' Launch a shiny-app simulating the SEIRS model
#' @details
#' Launch app for details
#' @examples
#' \dontrun{seirs.app}
#' @export
seirs.app=shinyApp(
# This creates the User Interface (UI)
ui <- pageWithSidebar(
headerPanel("SEIRS periodicity"),
sidebarPanel(
sliderInput("beta", "Transmission (yr^-1):", 500,
              min = 0, max = 3000),
sliderInput("oneoveromega", "Immune duration (years):", 4,
              min = 0, max = 100),
sliderInput("Ip", "Infectious period (days)", 5,
              min = 1, max = 100),
sliderInput("oneoversigma", "Latent period (days):", 8,
              min = 1, max = 100),
sliderInput("oneovermu", "Life expectancy (years):", 10,
              min = 1, max = 100),
sliderInput("T", "Time range:",
                  min = 0, max = 100, value = c(0,20)),
checkboxInput("lg", "un-Log", TRUE)
),

mainPanel(
  tabsetPanel(
      tabPanel("Time", plotOutput("plot1")), 
      tabPanel("Phase plane", plotOutput("plot2")),
       tabPanel("Equations", 
           withMathJax(
            helpText("Susceptible $$\\frac{dS}{dt} = \\mu (N - S) - \\frac{\\beta I S}{N} + \\omega R$$"),
            helpText("Exposed $$\\frac{dE}{dt} = \\frac{\\beta I S}{N} - (\\mu+\\sigma) E$$"),
            helpText("Infecitous $$\\frac{dI}{dt} = \\sigma E - (\\mu+\\gamma) I$$"),
           helpText("Removed $$\\frac{dR}{dt} = \\gamma I - \\mu R - \\omega R$$"),
           helpText("Reproductive ratio $$R_0 =  \\frac{\\sigma}{\\sigma +\\mu} \\frac{1}{\\gamma+\\mu} \\frac{\\beta N}{N}$$"),
           helpText(eval(Attr))             
           ))
  
  )
)
),

# This creates the 'behind the scenes' code (Server)
server <- function(input, output) {
seirsmod=function(t, x, params){
  S=x[1]
  E=x[2]
  I=x[3]
  R=x[4]

  mu=params["mu"]
  beta=params["beta"]
  omega=params["omega"]
  sigma=params["sigma"]
  gamma=params["gamma"]

  dS = mu * (1  - S)  - beta * S * I + omega * R
  dE = beta * S * I - (mu + sigma) * E
  dI = sigma * E - (mu + gamma) * I
  dR = gamma * I - mu * R - omega * R
  res=c(dS, dE, dI, dR)
  list(res)
} 

  output$plot1 <- renderPlot({

  times  = seq(0, input$T[2], by=1/100)
  paras  = c(mu = 1/input$oneovermu, beta =  input$beta, sigma = 365/input$oneoversigma, gamma = 365/input$Ip, omega=1/input$oneoveromega)
  xstart = c(S=0.539, E=0, I=0.001, R = 0.46)

  R0=with(as.list(paras),{
    beta*sigma/((mu+sigma)*(mu+gamma))
  })


Sstar=1/R0
Istar=paras["mu"]*(1-Sstar)/(paras["beta"]*Sstar - (paras["omega"]*paras["gamma"])/(paras["mu"]+paras["omega"]))
Estar=(paras["mu"]+paras["gamma"])*Istar/paras["sigma"]
Rstar=paras["gamma"]*Istar/(paras["mu"]+paras["omega"])

star=as.list(c(S=Sstar, E=Estar, I=Istar, R=Rstar, paras))
names(star)[1:4]=c("S", "E", "I", "R")

fns=list(quote(mu * (1  - S)  - beta * S * I  + omega * R), quote(beta * S * I - (mu + sigma) * E), quote(sigma * E - (mu + gamma) * I), quote(gamma * I - mu * R - omega * R))

aa=as.vector(c(sapply(fns, D, "S"),
sapply(fns, D, "E"),
sapply(fns, D, "I"),
sapply(fns, D, "R")))

JJ=matrix(sapply(aa, eval, star), ncol=4)

EE=eigen(JJ)$values
WW=which.max(Im(EE))
rp=2*pi/Im(EE[WW])


out=ode(y=xstart,
  times=times,
  func=seirsmod,
  parms=paras)

  out=as.data.frame(out)

  sel=out$time>input$T[1]&out$time<input$T[2]

par(mar = c(5,5,2,5))
plot(x=out$time[sel], y=out$I[sel], ylab="fraction", xlab="time", type="l",
ylim=range(out[sel,-c(1,2, 5)]), xlim=c(input$T[1], input$T[2]), log=ifelse(input$lg==TRUE, "y", ""), col="red")
 lines(x=out$time, y=out$E, col="blue")
 title(paste("R0=", round(R0, 1), ", Period=", round(rp,2)))

par(new=T)
plot(x=out$time, y=out$S, type="l", col="green", axes=FALSE, xlab=NA, ylab=NA, 
    ylim=range(out[sel,2]), xlim=c(input$T[1], input$T[2]), log=ifelse(input$lg==TRUE, "y", ""))
axis(side = 4, col="green")
mtext(side = 4, line = 4, "S", col="green")
  legend("right",
        legend=c("I", "E", "S"),
        lty=c(1,1,1),
         col=c("red", "blue", "green"))
   })
  
output$plot2 <- renderPlot({
  times  = seq(0, input$T[2], by=1/100)
  paras  = c(mu = 1/input$oneovermu, beta =  input$beta, sigma = 365/input$oneoversigma, gamma = 365/input$Ip, omega=1/input$oneoveromega)
  xstart = c(S=0.539, E=0, I=0.001, R = 0.46)

  R0=with(as.list(paras),{
    beta*sigma/((mu+sigma)*(mu+gamma))
  })
 

Sstar=1/R0
Istar=paras["mu"]*(1-Sstar)/(paras["beta"]*Sstar - (paras["omega"]*paras["gamma"])/(paras["mu"]+paras["omega"]))
Estar=(paras["mu"]+paras["gamma"])*Istar/paras["sigma"]
Rstar=paras["gamma"]*Istar/(paras["mu"]+paras["omega"])

star=as.list(c(S=Sstar, E=Estar, I=Istar, R=Rstar, paras))
names(star)[1:4]=c("S", "E", "I", "R")

fns=list(quote(mu * (1  - S)  - beta * S * I  + omega * R), quote(beta * S * I - (mu + sigma) * E), quote(sigma * E - (mu + gamma) * I), quote(gamma * I - mu * R - omega * R))

aa=as.vector(c(sapply(fns, D, "S"),
sapply(fns, D, "E"),
sapply(fns, D, "I"),
sapply(fns, D, "R")))

JJ=matrix(sapply(aa, eval, star), ncol=4)

EE=eigen(JJ)$values
WW=which.max(Im(EE))
rp=2*pi/Im(EE[WW])


  out=ode(y=xstart,
  times=times,
  func=seirsmod,
  parms=paras)

  out=as.data.frame(out)

  sel=out$time>input$T[1]&out$time<input$T[2]

  plot(out$S[sel], out$I[sel], log=ifelse(input$lg==TRUE, "xy", ""), type="l", xlab="fraction susceptible", ylab="fraction infected")
  title(paste("R0=", round(R0, 1), ", Period=", round(rp,2)))
  abline(v=1/R0, col="green")
  curve(paras["mu"]*(1-x)/(paras["beta"]*x - (paras["omega"]*paras["gamma"])/(paras["mu"]+paras["omega"])), min(out$S), max(out$S), add=TRUE, col="red")
    legend("topright",
        legend=c("I-socline", "S-isocline"),
        lty=c(1,1),
         col=c("red", "green"))
 
  })

  }
)


## ---- Load Package ----
library(shiny)
library(mvtnorm)
library(ellipse)
#Enable Slider Text Input
library(shinyWidgets)


# INPUT:
#   SR:  selection ratio
#   BR:  base rate
#   CV:   criterion validity
#   PPV: desired Positive Predictive Value

# ---- Draw TR plot ----
TRplot <- function(r1,xthresh,ythresh){
  plot(ellipse::ellipse(r1), type = "l",
       xlab = "Selection Test", cex.lab = 1.50,
       ylab = "Criterion Performance",
       col="red",
       #main = paste("r = ", r1, sep=""),
       main = bquote(paste(italic(r), " = ", .(r1))),
       cex.main=2,
       lwd=3)
  abline(v=xthresh)
  abline(h=ythresh)
  text(xthresh+0.5,ythresh+0.5,"TP", cex=2)
  text(xthresh+0.5,ythresh-0.5,"FP",cex=2)
  text(xthresh-0.5,ythresh-0.5,"TN",cex=2)
  text(xthresh-0.5,ythresh+0.5,"FN",cex=2)
}

## ---- Sensitivity Function ----
Sensitivity<-function(SR,
                      BR,
                      R){
  
  threshold.x <- -qnorm(SR)
  threshold.y <- -qnorm(BR)
  
  ## TP  
  A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                        upper = c(+Inf, +Inf),
                        sigma = matrix(c(1, R, R, 1),2,2))[1] 
  
  
  round(A/BR,3)
}

## ---- Specificity Function ----
Specificity<-function(SR,
                      BR,
                      R){
  
  threshold.x <- -qnorm(SR)
  threshold.y <- -qnorm(BR)
  
  
  ## FP 
  B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                        upper = c(+Inf, threshold.y),
                        sigma = matrix(c(1, R, R, 1),2,2))[1] 
  
  
  round((1-BR-B) / (1-BR), 3)
}

## ---- R Shiny Panels ----
## Four selection available, with three sliderbars for each selection
## Ticks are removed for aethestic purpose
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  # ---- _Base Rate ----
  shiny::tabPanel("Base Rate",
           sliderInput("sr1", "Selection Ratio",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001,
                       ticks = TRUE),
  
           sliderInput("r1", "Criterion Validity",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001,
                       ticks = TRUE),
           
           sliderInput("PPV1", "Positive Predictive Value",
                       min = 0,max = 1,
                       value = 0.5, step = 0.001,
                       ticks = TRUE),
           
           actionButton("reset1", "Reset")
  ),
  # ---- _Selection Ratio ----
  tabPanel("Selection Ratio",
           
           sliderInput("br2", "Base Rate",
                       min = 0,max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           sliderInput("r2", "Criterion Validity",
                       min = 0,max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           sliderInput("PPV2", "Positive Predictive Value",
                       min = 0,max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           actionButton("reset2", "Reset")
  ),
  # ---- _PPV ----
  tabPanel("Positive Predictive Value",
           
           sliderInput("br3", "Base Rate",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           sliderInput("sr3", "Selection Ratio",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001,
                       ticks = TRUE),
           
           sliderInput("r3", "Criterion Validity",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           actionButton("reset3", "Reset")
  ),
  # ---- _Criterion Validity ----
  tabPanel("Criterion Validity",
           
           sliderInput("br4", "Base Rate",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           sliderInput("sr4", "Selection Ratio",
                       min = 0, max = 1,
                       value = 0.5, step = 0.001,
                       ticks = TRUE),
           
           sliderInput("PPV4", "Positive Predictive Value",
                       min = 0,max = 1,
                       value = 0.5, step = 0.001, 
                       ticks = TRUE),
           
           actionButton("reset4", "Reset")
  )
)


## ---- R Shiny ui ----
ui <- fluidPage(
  ## Change Font of the entire page
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  tags$style(HTML("
    body {
            background-color: #FDF5E6;
            color: black;
            }")),
  ## Change theme
  theme = bslib::bs_theme(bootswatch="flatly"),
  ## Start Content
  sidebarLayout(
    sidebarPanel(
      selectInput("value", "Estimate the:",
                  choices = c("Base Rate",
                              "Selection Ratio", 
                              "Criterion Validity",
                              "Positive Predictive Value")),
      parameter_tabs,
    ),
    mainPanel(
      h3("Taylor-Russell \nClassification Results"),
      tableOutput("values"),
      plotOutput("plot")
    )
  )
)



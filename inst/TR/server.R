
## ---- Load Package ----
library(shiny)
library(mvtnorm)
library(ellipse)
#Enable Slider Text Input
library(shinyWidgets)

# INPUT:
#   SR:  selection ratio
#   BR:  base rate
#   R:   criterion validity
#   PPV: desired Positive Predictive Value

# ---- Draw TR plot ----
TRplot <- function(r1, xthresh, ythresh){
  
  plot(ellipse(r1), type="l",
       xlab = "Selection Test", cex.lab = 1.50,
       ylab = "Criterion Performance",
       col = "red",
       main  = bquote(paste(italic(r), " = ", .(r1))),
       cex.main = 2,
       lwd = 3)
  abline(v = xthresh)
  abline(h = ythresh)
  text(xthresh+0.5, ythresh+0.5, "TP", cex=2)
  text(xthresh+0.5, ythresh-0.5, "FP", cex=2)
  text(xthresh-0.5, ythresh-0.5, "TN", cex=2)
  text(xthresh-0.5, ythresh+0.5, "FN", cex=2)
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
  
  B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                        upper = c(+Inf, threshold.y),
                        sigma = matrix(c(1, R, R, 1),2,2))[1] 


  round((1-BR-B) / (1-BR), 3)
}


# ---- R shiny server ----
server <- function(input, output,session) {
  
## Update bars based on input selected
  observeEvent(input$value, {
    updateTabsetPanel(inputId = "params", selected = input$value)
  })

  observeEvent(input$reset1,{
    updateSliderInput(inputId = "sr1", value = 0.5)
    updateSliderInput(inputId = "r1", value = 0.5)
    updateSliderInput(inputId = "PPV1", value = 0.5)
  })  
  
  observeEvent(input$reset2,{
    updateSliderInput(inputId = "br2", value = 0.5)
    updateSliderInput(inputId = "r2", value = 0.5)
    updateSliderInput(inputId = "PPV2", value = 0.5)
  })  
  
  
  observeEvent(input$reset3,{
    updateSliderInput(inputId = "br3", value = 0.5)
    updateSliderInput(inputId = "sr3", value = 0.5)
    updateSliderInput(inputId = "r3", value = 0.5)
  })  
  
  observeEvent(input$reset4,{
    updateSliderInput(inputId = "br4", value = 0.5)
    updateSliderInput(inputId = "sr4", value = 0.5)
    updateSliderInput(inputId = "PPV4", value = 0.5)
  })  

  
  
# ----Table Output ----
## Reactive slider values 
  sliderValues <- reactive({
 
# ---- _TR Functions ----
# ---- __TR.sr ----  
## renamed br2, r2, ppv2 to avoid recalculation in output
    TR.sr <- function(sr){
      
      threshold.x <- -qnorm(sr)
      threshold.y <- -qnorm(as.numeric(input$br2))
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, as.numeric(input$r2), as.numeric(input$r2), 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, as.numeric(input$r2), as.numeric(input$r2), 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( as.numeric(input$PPV2) - A/(A + B) )^2  # return
      
    }# END TR function
    

    # ---- __TR.br ----  
    TR.br <- function(br){
      
      threshold.x <- -qnorm(as.numeric(input$sr1))
      threshold.y <- -qnorm(br)
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, as.numeric(input$r1), as.numeric(input$r1), 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, as.numeric(input$r1), as.numeric(input$r1), 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( as.numeric(input$PPV1) - A/(A + B) )^2  # return
      
    }# END TR.br function
    
    # ---- __TR.r ----  
    TR.r <- function(r){
      
      threshold.x <- -qnorm(as.numeric(input$sr4))
      threshold.y <- -qnorm(as.numeric(input$br4))
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, r, r, 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, r, r, 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( as.numeric(input$PPV4) - A/(A + B) )^2  # return
      
    }# END TR.r function
    
    
    # ---- __TR.ppv ----  
    TR.ppv <- function(PPV){
      
      threshold.x <- -qnorm(as.numeric(input$sr3))
      threshold.y <- -qnorm(as.numeric(input$br3))
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, as.numeric(input$r3), as.numeric(input$r3), 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, as.numeric(input$r3), as.numeric(input$r3), 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( PPV - A/(A + B) )^2  # return
      
    }# END TR.ppv function
    
# ---- _Generate Table Output ----    
## switch output results based on selected input   
## Generate output based on the selected variable 
## Ask Ziyu: have to modify code, otherwise does not fit in the structure
    switch(input$value,
           ## ---- __Table-sr ----
           "Selection Ratio" = data.frame(
             Name = if(round(optimize(f = TR.sr, 
                                      lower = 0.001, 
                                      upper = 0.999,
                                      maximum = FALSE)$min,3)<=0.001){c("Base Rate",
                                                                        "Criterion Validity",
                                                                        "<strong>**Selection Ratio (Smaller than)**</strong>",
                                                                        "Positive Predictive Value",
                                                                        "Sensitivity ",
                                                                        "Specificity")}
             else{c("Base Rate",
                    "Criterion Validity",
                    "<strong>**Selection Ratio**</strong>",
                    "Positive Predictive Value",
                    "Sensitivity",
                    "Specificity")},
             Value = if(input$br2==0.001 ||
                        input$r2==1.000){c("NA",
                                       "NA",
                                       "NA",
                                       "NA",
                                       "NA",
                                       "NA")}
             else if(input$br2 > input$PPV2){
               c(input$br2,
                 input$r2,
                 round(optimize(f = TR.sr, 
                                lower = 0.001, 
                                upper = .999,
                                maximum = FALSE)$min,3),
                 input$br2,
                 Sensitivity(round(optimize(f = TR.sr, 
                                            lower = 0.001, 
                                            upper = .999,
                                            maximum = FALSE)$min,3),input$br2,input$r2),
                 Specificity(round(optimize(f = TR.sr, 
                                            lower = 0.001, 
                                            upper = .999,
                                            maximum = FALSE)$min,3),input$br2,input$r2))
              
             }
                       else{c(input$br2,
                              input$r2,
                              round(optimize(f = TR.sr, 
                                             lower = 0.001, 
                                             upper = .999,
                                             maximum = FALSE)$min,3),
                              input$PPV2,
                              Sensitivity(round(optimize(f = TR.sr, 
                                                         lower = 0.001, 
                                                         upper = .999,
                                                         maximum = FALSE)$min,3),input$br2,input$r2),
                              Specificity(round(optimize(f = TR.sr, 
                                                         lower = 0.001, 
                                                         upper = .999,
                                                         maximum = FALSE)$min,3),input$br2,input$r2))},
             stringsAsFactors = FALSE),
           ## ---- __Table-br ----
          "Base Rate" = data.frame(
            Name = c("<strong>**Base Rate**</strong>",
                     "Criterion Validity",
                     "Selection Ratio",
                     "Positive Predictive Value",
                     "Sensitivity",
                     "Specificity"),
            Value = if(input$sr1<=1E-6 ||
                       input$PPV1==1 ||
                       input$r1==1){c("NA",
                                      "NA",
                                      "NA",
                                      "NA",
                                      "NA",
                                      "NA")
            }
            else{c(round(optimize(f = TR.br, 
                                     lower = 0.001, 
                                     upper = .999,
                                     maximum = FALSE)$min,3),
                   input$r1,
                   input$sr1,
                   input$PPV1,
                   Sensitivity(input$sr1,round(optimize(f = TR.br, 
                                                        lower = 0.001, 
                                                        upper = .999,
                                                        maximum = FALSE)$min,3),input$r1),
                   Specificity(input$sr1,round(optimize(f = TR.br, 
                                                        lower = 0.001, 
                                                        upper = .999,
                                                        maximum = FALSE)$min,3),input$r1))},
            stringsAsFactors = FALSE),
          ## ---- __Table-r ----
          "Criterion Validity" = data.frame(
            Name = c("Base Rate",
                     "<strong>**Criterion Validity**</strong>",
                     "Selection Ratio",
                     "Positive Predictive Value",
                     "Sensitivity",
                     "Specificity"),
            Value = if(input$sr4<=1E-6 ||
                       input$sr4==1 ||
                       input$br4==0){c("NA",
                                       "NA",
                                       "NA",
                                       "NA",
                                       "NA",
                                       "NA")
            }
            else{as.character(c(input$br4,
                                round(optimize(f = TR.r, 
                                                   lower = -1, 
                                                   upper = 1,
                                                   maximum = FALSE)$min,3),
                                input$sr4,
                                input$PPV4,
                                Sensitivity(input$sr4,input$br4,(round(optimize(f = TR.r, 
                                                                                lower = -1, 
                                                                                upper = 1,
                                                                                maximum = FALSE)$min,3))),
                                Specificity(input$sr4,input$br4,(round(optimize(f = TR.r, 
                                                                                lower = -1, 
                                                                                upper = 1,
                                                                                maximum = FALSE)$min,3)))))},
            stringsAsFactors = FALSE),
          ## ---- __Table-PPV ----
          "Positive Predictive Value" = data.frame(
            Name = c("Base Rate",
                     "Criterion Validity",
                     "Selection Ratio",
                     "<strong>**Positive Predictive Value**</strong>",
                     "Sensitivity",
                     "Specificity"),
            Value = if(input$sr3<=1E-6 ||
                       input$r3 == 1){c("NA",
                                        "NA",
                                        "NA",
                                        "NA",
                                        "NA",
                                        "NA")
              }
            else{as.character(c(input$br3,
                                input$r3,
                                input$sr3,
                                (round(optimize(f = TR.ppv, 
                                                lower = 0, 
                                                upper = 1,
                                                maximum = FALSE)$min,3)),
                                Sensitivity(input$sr3,input$br3,input$r3),
                                Specificity(input$sr3,input$br3,input$r3)))},
            stringsAsFactors = FALSE))
    
  })
  

# ---- Plot Output ----
## Generate output for plot; 
## Recalculate the function
  
  oldpar <- par(pty="s")
  
  Plot <- reactive({
    
    # ---- _TR Functions ----
    # ---- __TR.sr ----  
    TR.sr <- function(sr){
      
      threshold.x <- -qnorm(sr)
      threshold.y <- -qnorm(input$br2)
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, input$r2, input$r2, 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, input$r2, input$r2, 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * (input$PPV2 - A/(A + B) )^2  # return
      
    }# END TR function
    
    # ---- __TR.br ----  
    TR.br <- function(br){
      
      threshold.x <- -qnorm(as.numeric(input$sr1))
      threshold.y <- -qnorm(br)
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, as.numeric(input$r1), as.numeric(input$r1), 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, as.numeric(input$r1), as.numeric(input$r1), 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( as.numeric(input$PPV1) - A/(A + B) )^2  # return
      
    }# END TR.br function
    
    # ---- __TR.r ----  
    TR.r <- function(r){
      
      threshold.x <- -qnorm(as.numeric(input$sr4))
      threshold.y <- -qnorm(as.numeric(input$br4))
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, r, r, 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, r, r, 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( as.numeric(input$PPV4) - A/(A + B) )^2  # return
      
    }# END TR.r function
    
    
    # ---- __TR.ppv ----  
    TR.ppv <- function(PPV){
      
      threshold.x <- -qnorm(as.numeric(input$sr3))
      threshold.y <- -qnorm(as.numeric(input$br3))
      
      
      A <- mvtnorm::pmvnorm(lower = c(threshold.x,  threshold.y), 
                            upper = c(+Inf, +Inf),
                            sigma = matrix(c(1, as.numeric(input$r3), as.numeric(input$r3), 1),2,2))[1] 
      
      B <- mvtnorm::pmvnorm(lower = c(threshold.x, -Inf), 
                            upper = c(+Inf, threshold.y),
                            sigma = matrix(c(1, as.numeric(input$r3), as.numeric(input$r3), 1),2,2))[1] 
      
      # function to minimize
      Q <- 500 * ( PPV - A/(A + B) )^2  # return
      
    }# END TR.ppv function
    
    
    # ---- __Find sr  ----
    if(input$value=="Selection Ratio"){ 
      out <- optimize(f = TR.sr, 
                      lower = 0.001, 
                      upper = 0.999,
                      maximum = FALSE)
      
      sr = out$minimum    
      sr <- round(sr, 3)
      
      br<-input$br2
      r<-input$r2
      PPV<-input$PPV2
    }
    
    # ---- __Find br  ----
    if(input$value=="Base Rate"){ 
      out <- optimize(f = TR.br, 
                      lower = 0.001, 
                      upper = 0.999,
                      maximum = FALSE)
      
      br = out$minimum    
      br <- round(br, 3)
      
      
      sr<-input$sr1
      r<-input$r1
      PPV<-input$PPV1
    } #END if br == Null
    
    # ---- __Find r  ----
    if(input$value=="Criterion Validity"){ 
      out <- optimize(f = TR.r, 
                      lower = -1, 
                      upper = 1,
                      maximum = FALSE)
      
      r = out$minimum    
      r <- round(r, 3)
      
      
      br<-input$br4
      sr<-input$sr4
      PPV<-input$PPV4
      
    } #END if r == Null
    
    # ---- __Find PPV  ----
    if(input$value=="Positive Predictive Value"){ 
      out <- optimize(f = TR.ppv, 
                      lower = 0, 
                      upper = 1,
                      maximum = FALSE)
      
      PPV = out$minimum    
      PPV <- round(PPV, 3)
      
      
      br<-input$br3
      r<-input$r3
      sr<-input$sr3
    }
    
  
    
## ---- convert sr/br ----
# Change the values to z-score
    srZ<-qnorm(1-sr)
    brZ<-qnorm(1-br)
    

    
 ## ---- _Draw plot ----
    TRplot(as.numeric(r),as.numeric(srZ),as.numeric(brZ))
  })

# ---- Outputs ----
  ## Outputs
  output$values <- renderTable({
    sliderValues()
  },sanitize.text.function=function(x){x},digits=3)
  
  output$plot <- renderPlot({
    Plot()
  })
  
  ## November 30, 2023
  par(oldpar)
 
}

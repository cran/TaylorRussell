## Taylor Russell Demonstration Program 
## Authors: Niels Waller and Ziyu Ren
## September 15, 2023
##
#' A 'shiny' Taylor-Russell demonstration program 
#'
#' \code{TRDemo()} is a 'shiny' program for demonstrating the 
#' classic Taylor-Russell model for personnel selection.   
#' 
#' @description{
#' \code{TRDemo()} is an R 'shiny' function for illustrating the bivariate 
#' (i.e., single predictor) Taylor-Russell model for 
#' personnel selection. The function can be called 
#' from the command prompt (\code{>}) by typing:
#' 
#' \code{TRDemo()}
#' }
#' 
#' After calling the function, users are asked to enter values for 3 of the 
#' following 4 variables:
#' \itemize{
#'  \item The Base Rate (BR) of successful criterion performance 
#'  (i.e., within the target population, the proportion of individuals who 
#'  successfully execute the job demands). 
#'  \item The Selection Ratio (SR; i.e., the proportion of hired candidates within 
#'  the target population).
#'  \item The Criterion Validity (CV; the correlation between the selection test 
#'  and an operationalized measure of job performance).
#'  \item The Positive Predicted Value (PPV; The probability that a hired 
#'  individual can successfully execute the 
#'  job demands).
#'  }
#'  
#' @details
#'  \code{TRDemo()} calculates the model-implied value for the omitted 
#'  variable and prints all model parameters (e.g., BR, SR, 
#'  CV, and PPV) to 
#'  screen. Given these parameters, the function also reports the test Sensitivity 
#'  (defined as the probability that a qualified individual will be hired) and 
#'  test Specificity (defined as the probability that an unqualified individual 
#'  will not be hired). Finally, the function plots a 
#'  correlation ellipse (associated with the CV) showing the relative 
#'  proportions of True Positives (TP; hired individuals who are 
#'  qualified for the job), False Positives (FP; hired individuals who are not 
#'  qualified for the job), True Negatives (TN; non hired individuals who are 
#'  not qualified for the job), and  
#'  False Negatives (FN; non hired individuals who are qualified for the job).
#' 
#' @return 
#'  No return value.
#'  
#' @author
#' \itemize{
#'   \item Niels G. Waller (nwaller@umn.edu)
#'   \item Ziyu Ren
#' }
#' 
#' @references
#' \itemize{
#'   \item Taylor, H. C. & Russell, J. (1939). The relationship of validity 
#'   coefficients to the practical effectiveness of tests in selection: 
#'   Discussion and tables. \emph{Journal of Applied Psychology, 23}, 565--578.
#'  }

#' 
#' @keywords stats 
#' @import shiny
#' @import mvtnorm
#' @import shinyWidgets
#' @import graphics
#' @import stats 
#' @export
#' 
#' @examples 
#' 
#' # Type TRDemo() from the command prompt (>) to run the 'shiny' app
#' # in interactive mode.
#'  
#'  
#' 
  
  TRDemo <- function(){
	 if(interactive()){
        appDir = system.file("TR", package = "TaylorRussell")
        shiny::runApp(appDir, display.mode = "normal")
     }
  }


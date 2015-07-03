#' Load data from Megellan ASCI export files
#' 
#' To use this read function to read data from files generated using Tecan's 
#' Magellen suite, use the following settings for ASCI export files in your
#' method:
#' 
#' \describe{
#'   \item{Direction}{Horizontal}
#'   \item{Result}{Table (well data in columns)}
#'   \item{Add kinetic time stamps}{(checked)}
#' }
#' 
#' @param file Path to the file to read.
#' 
#' @return A data.frame in which the first column contains time stamps and 
#'   subsequent columns readings from wells, row-wise across the plate (eg
#'   A1..A12 then B1..B12, etc.).
#'   
#' @export
read.magellan <- function(file) {
  
  df <- read.table( file
                  , sep              = "\t"
                  , header           = FALSE
                  , stringsAsFactors = FALSE
                  )
  
  df[, 1]        <- as.numeric(gsub("s", "", df[, 1]))
  df[, ncol(df)] <- NULL
  df
  
}

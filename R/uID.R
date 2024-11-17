#' Create unique ID
#'
#' Creates a unique ID given a data frame and a string separated by ;.
#'
#' @param x a data frame.
#' @param uID a string separated by ;.
#'
#' @returns A character vector.
#'




uID <- function(x,uID){
  do.call(paste, c(as.data.frame(x[,strsplit(uID,split = ";")[[1]]]), sep="_"))
}
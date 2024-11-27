#' cBinds tibbles
#'
#'
#' @param x tibble.
#' @param y data frame or vector.
#' @param xy a logical vector indicating if the order should be \code{cbind(x,y)}.
#'
#' @returns A tibble.
#' @noRd


cbindtb <- function(x,y){
  
  if(inherits(x,"tbl")){
    kl <- class(x)
  }else{
    kl <- class(y)
  }
 
  out <- cbind(x,y)
  
  class(out) <- kl
  out
}
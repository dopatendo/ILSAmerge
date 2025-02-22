#' Untibble
#'
#' Converts a tibble into a plain data frame with no column attributes.
#' 
#' @param tibble a tibble object or a list of tibbles.
#' @param mistoNAs a logical value indicating if missing values should be 
#' converted into NAs. Default is \code{FALSE}.
#'
#' @returns A tibble.
#' 
#' @keywords tibblemanagement
#' 
#' @examples
#' # Path where raw 'SPSS' files are
#' input <- system.file("extdata/reds", package = "ILSAmerge")
#' 
#' # Load complete data
#' fulllist <- justload(inputdir = input, population = "BCGV1", justattributes = FALSE)
#' 
#' # Untibble first element
#' unt1 <- untibble(fulllist[[1]])
#' 
#' # Untibble all list
#' unt2 <- untibble(fulllist)
#' 
#'
#' @export
#

untibble <- function(tibble, mistoNAs = FALSE){
  
  

# Checks ------------------------------------------------------------------

  if(!inherits(tibble,"list")){
    if(!inherits(tibble, "tbl_df"))
      stop(c("\nInvalid input for 'tibble'.",
             "\nIt should be a tibble or a list of tibbles."),call. = FALSE)
  }else{
    if(!all(sapply(tibble, inherits, what = "tbl_df")))
      stop(c("\nInvalid input for 'tibble'.",
             "\nIt should be a tibble or a list of tibbles."),call. = FALSE)
    
  }
    
 
  if(!(isTRUE(mistoNAs)|isFALSE(mistoNAs)))
    stop(c("\nInvalid input for 'mistoNAs'.",
           "\nIt should be a logical value."),call. = FALSE)


# Process & Output --------------------------------------------------------

    
  
  if(!inherits(tibble,"list"))
    return(.untibble(tibble, mistoNAs = mistoNAs))
  
  out <- vector("list",length(tibble))
  for(i in 1:length(out)){
    out[[i]] <- .untibble(tibble[[1]], mistoNAs = mistoNAs)
  }
  return(out)
  
}

.untibble <- function(tibble, mistoNAs = FALSE){
  

  otibble <- as.data.frame(tibble)
  for(i in 1:ncol(tibble)){
    otibble[,i] <- as.vector(otibble[,i,drop = TRUE])
  }
  
  if(mistoNAs){
    
    otibble[is.na(tibble)] <- NA
    
  }
  
  
  return(otibble)
  
}






#' Check available ILSA data
#'
#' Checks which 'SPSS' data from different International Large-Scale Assessments (ILSA).
#' are available.
#'
#' @param print a logical value indicating if results should be printed or not.
#' @param FOR a string indicating the availability of ILSA data for different 
#' purposes. Valid strings are \code{"download"}, \code{"combine.students"},
#' and \code{add.schools}.
#' 
#'
#' @returns A list with the names of the ILSA and the available years.
#'
#' @examples
#' 
#' availableILSA(print = TRUE, )
#'
#' @export

availableILSA <- function(print = TRUE, FOR = "download"){
  
  # Checks ----
  
  if(!(isTRUE(print)|isFALSE(print)))
    stop(c("\nInvalid input for 'print'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  # Process ----
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/combinestudents/data/ILSAlinks.csv"
  
  
  ILSAlinks <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSAlinks)){
    stop(paste0("Could not read ILSAlinks file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }

  
  if(FOR=="download"){
    ilsas <- unique(ILSAlinks[,1:2])
    ilsas <- ilsas[!ilsas$Name%in%"Other",]
    ilsas <- ilsas[order(ilsas$Year),]
    ilsasU <- sort(unique(ilsas$Name))
    
    
    out <- lapply(ilsasU,function(i){
      as.numeric(ilsas$Year[ilsas$Name%in%i])
    })
    names(out) <- ilsasU
  }

  # if(FOR=="combine.students"){
  #   
  # }


  # Output ----
  
  if(print){
    for(i in 1:length(out)){
      cat(paste0(names(out)[i],": ",
                 paste0(out[[i]],collapse = ", "),".\n"))
    }
    return(invisible(out))
  }else{
    return(out)
  }
  
  
 
  
 
}




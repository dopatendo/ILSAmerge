#' Check available ILSA data
#'
#' Checks which 'SPSS' data from different International Large-Scale Assessments (ILSA).
#' are available.
#'
#' @param print a logical value indicating if results should be printed or not.
#' @param FOR a string indicating the availability of ILSA data for different 
#' purposes. Valid strings are \code{"download"}, \code{"combine.students"},
#' \code{add.schools}, and \code{"ILSAready"}.
#' 
#'
#' @returns A list with the names of the ILSA and the available years.
#'
#' @examples
#' 
#' availableILSA(print = TRUE)
#'
#' @export

availableILSA <- function(print = TRUE, 
                          FOR = c("download","combine.students","add.schools","ILSAready")){
  
  # Checks ----
  
  if(!(isTRUE(print)|isFALSE(print)))
    stop(c("\nInvalid input for 'print'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  FORv <- c("download","combine.students","add.schools","ILSAready")
  
  if(!(is.vector(FOR)&&is.character(FOR)))
    stop(c("\nInvalid input for 'FOR'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  
  
  FOR <- try(match.arg(FOR,FORv),silent = TRUE)
  
  if("try-error"%in%class(FOR))
    stop(c("\nInvalid input for 'FOR'."),call. = FALSE)
  
 
  
  # Process ----
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv"
  
  
  ILSAlinks <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSAlinks)){
    stop(paste0("Could not read ILSAlinks file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }

  
  if(FOR=="download"){
    ilsas <- unique(ILSAlinks[,1:2])
    ilsas <- ilsas[!ilsas$Name%in%"Other",]
  }
  
  if(FOR=="combine.students"){
    ilsas <- ILSAlinks[ILSAlinks$combineSTU%in%1,1:2]
  }
  
  if(FOR=="add.schools"){
    ilsas <- ILSAlinks[ILSAlinks$addSCH%in%1,1:2]
  }
  
  if(FOR=="ILSAready"){
    ilsas <- ILSAlinks[ILSAlinks$ILSAready%in%1,1:2]
  }
  
  
  
  
  ilsas <- ilsas[order(ilsas$Year),]
  ilsasU <- sort(unique(ilsas$Name))
  
  
  out <- lapply(ilsasU,function(i){
    sort(unique(as.numeric(ilsas$Year[ilsas$Name%in%i])))
  })
  names(out) <- ilsasU



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




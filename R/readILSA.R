#' Read ILSA
#'
#' Reads files created with ILSAmerge().
#'
#' @param x a string
#'
#' @returns A data frame.
#' @noRd




readILSA <- function(x){
  ext <- substring(x,max(gregexpr("\\.",x)[[1]])+1)
  
  if(ext=="rds"){
    return(readRDS(x))
  }
  
  if(ext%in%c("zsav","sav")){
    out <- try(haven::read_spss(file = x,
                                user_na = TRUE,
                                col_select = NULL,
                                skip = 0,
                                n_max = Inf,
                                .name_repair = "unique"),
               silent = TRUE)
    
    if("try-error"%in%class(out)){
      out <- haven::read_sav(file = x,
                             user_na = TRUE,
                             col_select = NULL,
                             skip = 0,
                             n_max = Inf,
                             .name_repair = "unique",
                             encoding = "latin1")
    }
    
    return(out)
    
  }
}




#' Read ILSA data
#'
#' Reads files created with ILSAmerge().
#'
#' @param file a path to an '.rds', '.sav', or '.zsav' file.
#' @param untibble a logical value indicating if data should be converted into
#' a plain data frame with no column attributes.
#' @inheritParams untibble
#' 
#' 
#' @examples
#' # Path where raw 'SPSS' files are
#' input <- system.file("extdata/reds", package = "ILSAmerge")
#' 
#' # Path where merged files will be saved
#' unlink(file.path(tempdir(),"ILSAmerge"),recursive = TRUE)
#' dir.create(file.path(tempdir(),"ILSAmerge"))
#' output <- file.path(tempdir(),"ILSAmerge")
#' 
#' # Merging 'REDS' 2021, as .rds file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#' 
#' # Read student file
#' readILSA(file = file.path(output,"BSGV1.rds"))
#'
#' @returns A tibble or a data frame.
#' 
#' @export



readILSA <- function(file, mistoNAs = FALSE, untibble = FALSE){
  

# Checks ------------------------------------------------------------------

  if(!(is.vector(file)&&is.character(file)&&length(file)==1))
    stop(c("\nInvalid input for 'file'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(file))
    stop(c("\nInvalid input for 'file'.",
           "\nFile does not exist."),call. = FALSE)
  
  ext <- tolower(substring(file,max(gregexpr("\\.",file)[[1]])+1))
  if(!ext%in%c("rds","sav","zsav"))
    stop(c("\nInvalid input for 'file'.",
           "\nFile extension should be 'rds', 'sav', or 'zsav'."),call. = FALSE)
  
  
  if(!(isTRUE(mistoNAs)|isFALSE(mistoNAs)))
    stop(c("\nInvalid input for 'mistoNAs'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  
  if(!(isTRUE(untibble)|isFALSE(untibble)))
    stop(c("\nInvalid input for 'untibble'.",
           "\nIt should be a logical value."),call. = FALSE)
  

# Process & Output --------------------------------------------------------

  file <- .readILSA(x = file)
  
  if(untibble)
    return(untibble(tibble = file, mistoNAs = mistoNAs))
  
  if(mistoNAs)
    return(mistoNAs(tibble = file))
  
  return(file)
}


.readILSA <- function(x,n_max = Inf){
  ext <- tolower(substring(x,max(gregexpr("\\.",x)[[1]])+1))
  

  
  if(ext=="rds"){
    return(readRDS(x))
  }
  
  if(ext%in%c("zsav","sav")){
    out <- try(haven::read_spss(file = x,
                                user_na = TRUE,
                                col_select = NULL,
                                skip = 0,
                                n_max = n_max,
                                .name_repair = "unique"),
               silent = TRUE)
    
    if("try-error"%in%class(out)){
      out <- haven::read_sav(file = x,
                             user_na = TRUE,
                             col_select = NULL,
                             skip = 0,
                             n_max = n_max,
                             .name_repair = "unique",
                             encoding = "latin1")
    }
    
    return(out)
    
  }
}





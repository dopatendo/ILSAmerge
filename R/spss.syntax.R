#' 'SPSS' merge syntax
#'
#' Produces and saves an 'SPSS' merge syntax given a list of files.
#'
#' @param filelist a character vector with the list of files to be merged.
#' @param name a string with the name of the merged file (without any
#' extension).
#' @param outputdir the directory where the \code{.sps} file and the
#' merged file will be saved.
#' @param zsav a logical value indicating if the the merged file should
#' be compressed with zsav. Default is \code{TRUE}.
#' @param SPSSlimit a numerical value indicating the limit of files per command
#' of 'SPSS', typically 50.
#'
#' @returns Saves an \code{.sps} file with the 'SPSS' syntax for merging the
#' desired files.
#'
#' @examples
#' # Path were raw 'SPSS' files are
#' input <- system.file("extdata", package = "ILSAmerge")
#' 
#' # Path were merged files will be saved
#' output <- tempdir()
#' 
#' # List of BCGV1 files to be merged
#' files <- list.files(path = input, pattern = "BCG.+V1|bcg.+v1")
#' 
#' # Create 'SPSS' syntax
#' spss.syntax(filelist = files, name = "BCGV1", outputdir = output, zsav = TRUE)
#'
#' @export






spss.syntax <- function(filelist, name, outputdir = getwd(), zsav = TRUE, SPSSlimit = 50){


  # Checks ----

  if(!(is.vector(filelist)&&is.character(filelist)&&length(filelist)>1))
    stop(c("\nInvalid input for 'filelist'.",
           "\nIt should be a character vector of length bigger than 1."),call. = FALSE)


  if(!(is.vector(name)&&is.character(name)&&length(name)==1))
    stop(c("\nInvalid input for 'name'.",
           "\nIt should be a character vector."),call. = FALSE)

  ## outputdir
  if(!(is.vector(outputdir)&&is.character(outputdir)&&length(outputdir)==1))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nPath does not exist."),call. = FALSE)

  if(!(is.vector(zsav)&&is.logical(zsav)&&length(zsav)==1))
    stop(c("\nInvalid input for 'zsav'.",
           "\nIt should be a logical value."),call. = FALSE)

  if(!(is.vector(SPSSlimit)&&length(SPSSlimit)==1&&is.numeric(SPSSlimit)))
    stop(c("\nInvalid input for 'SPSSlimit'.",
           "\nIt should be a numeric value."),call. = FALSE)

  if(SPSSlimit>50)
    warning("Be aware SPSS tipically can only work with 50 files at a time.",call. = FALSE)

  # Process & Output ----

  nam <- paste0(name,ifelse(zsav,'.zsav','.sav'))

  inp <- c("* .sps file produced by IEAmerge.",
           "",
           "OUTPUT NEW.",
           "OUTPUT NAME IEAmerge.",
           "",
           "GET",
           "File = 'INSERTFIRST'.",
           "DATASET NAME IEAmerge WINDOW=FRONT.",
           "")


  out <- gsub("INSERTFIRST",filelist[1],inp)

  out <- c(out,unlist(lapply(split(filelist[-1],
                                   ceiling(seq_along(filelist[-1])/(SPSSlimit-1))),
                             function(i){

                               c("DATASET ACTIVATE IEAmerge.",
                                 "ADD FILES /FILE=*",
                                 paste0(paste0("/FILE='",i,collapse = "'\n"),"'."),
                                 "EXECUTE.")

                             })))



  if(zsav){
    out <- c(out,"","SAVE OUTFILE='INSERTOUT' /ZCOMPRESSED.")
  }else{
    out <- c(out,"","SAVE OUTFILE='INSERTOUT' /ENCODING='LOCALE'.")
  }
  out <- c(out,"DATASET ACTIVATE IEAmerge.","",
           "DATASET CLOSE IEAmerge.",
           "OUTPUT CLOSE IEAmerge.")

  out <- gsub("INSERTOUT",file.path(outputdir,nam),out)

  writeLines(text = out,file.path(outputdir,paste0(name,".sps")))
}

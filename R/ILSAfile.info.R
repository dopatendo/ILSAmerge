#' ILSA data files information
#'
#' Aggregates ILSA data files information by population.
#'
#' @param inputdir a string indicating the path were ILSA SPSS files are stored.
#'
#' @returns A data frame with the number of files and MBs per population.
#'
#' @examples
#' \dontrun{
#' # For example, after downloading TIMSS 1995 G4 data:
#'
#' # Path were original SPSS files are stored
#' input <- 'C:/TIMSS1995_IDB_SPSS_G4/Data/'
#'
#' # Get file information
#' ILSAfile.info(inputdir = input)
#'
#' }
#'
#' @export

ILSAfile.info <- function(inputdir){

  # Checks ----

  if(!is.vector(inputdir)&&!is.character(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a character vector."),call. = FALSE)

  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)

  # Process & Output ----

  ark <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZAV")
  erk <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZAV",full.names = TRUE)

  if(length(ark)==0)
    stop(c("\nNo files found."),call. = FALSE)



  pop <- substr(ark,1,3)
  cou <- substr(ark,4,6)

  ext <- lapply(ark,function(i){
    out <- substring(i,7)
    c(substr(out,1,gregexpr('\\.',out)[[1]]-1),
      substring(out,gregexpr('\\.',out)[[1]]+1))
  })

  stu <- unlist(lapply(ext,function(i) i[[1]]))
  ext <- unlist(lapply(ext,function(i) i[[2]]))

  popstu <- paste0(pop,stu)
  upopstu <- unique(popstu)

  siz <- file.size(erk)
  siz <- stats::aggregate(siz,by = list(popstu),sum)

  out <- as.data.frame(table(popstu))
  colnames(out) <- c('Population','Files')
  out <- cbind.data.frame(out,MB = round(siz[,2]/1000/1000,1))

  # Output ----
  return(out)
}



#' Loading ILSA data into a list
#'
#' Load 'SPSS' data from different International Large-Scale Assessments (ILSA),
#' including: 'TIMSS', 'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS', 'CIVED', 'REDS', 'RLII',
#' and 'SITES' into a list.
#'
#' @param inputdir a string indicating the path were ILSA 'SPSS' files are stored.
#' @param population a character value indicating which files should be merged. 
#' For more information on available populations, run \code{ILSAfile.info()} first.
#' @param justattributes a logical value indicating if 0 rows should be loaded.
#' This can be used when we just need to check column attributes. Default is 
#' \code{FALSE}.
#'
#' @returns A list of tibbles.
#'
#' @examples
#' \donttest{
#' # For example, after downloading 'REDS' 2021 G4 data:
#'
#' # Downloading 'REDS' 2021 and unzipping files
#' ILSAdownload(study = "REDS", year = 2021, outputdir = tempdir(), unzip = TRUE, agreeLicense = TRUE)
#'
#' # Path were raw 'SPSS' files are
#' input <- file.path(tempdir(),"REDS2021_IDB_SPSS/Data")
#' 
#' # Load only attributes
#' justload(inputdir = input, population = "bcgv1", justattributes = TRUE)
#' 
#' # Load complete data
#' justload(inputdir = input, population = "bsgv1", justattributes = FALSE)
#' }
#'
#' @export



justload <- function(inputdir, population, justattributes = FALSE){
  
  # Example & Test ----
  
  # inputdir <- "/Users/andreschristiansen/Downloads/TIMSS1995_IDB_SPSS_G4/Data"
  # ILSAfile.info(inputdir)
  # population <- "ASAm1"
  # justattributes = FALSE
  
  
  # Checks ----
  
  if(!(is.vector(inputdir)&&is.character(inputdir)))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  if(!(is.vector(justattributes)&&is.logical(justattributes)&&length(justattributes==1)))
    stop(c("\nInvalid input for 'justattributes'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  ## Process ----
  
  
  ark <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZSAV")
  erk <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZSAV",full.names = TRUE)
  
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
  
  
  if(!(is.vector(population)&&is.character(population)&&length(population==1)))
    stop(c("\nInvalid input for 'population'.",
           "\nIt should be a character vector of length 1."),call. = FALSE)
  
  
  if(!population%in%upopstu)
    stop(c("\nInvalid input for 'population'.",
           "\nIt is not contained in the path. Check with ILSAfile.info()."),call. = FALSE)
  
  
  upopstu <- upopstu[upopstu%in%population]
  
  
  erki <- erk[popstu%in%upopstu]
  
  out <- lapply(1:length(erki),function(j){
    
    outj <- haven::read_spss(file = erki[j], user_na = TRUE, col_select = NULL,
                             skip = 0, n_max = ifelse(justattributes, 0, Inf),
                             .name_repair = "unique")
    
    outj
  })
  
  # Output ----
  
  return(out)
  
}





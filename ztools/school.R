#' School ILSA data
#'
#' Renames or converts school ILSA data merged by....
#'
#' @param inputdir a string indicating the path were **MERGED** ILSA 'SPSS' files are stored.
#' @param filetype a string indicating the type of file to be saved, it can
#' be \code{"rds"}, \code{"zsav"}, or \code{"sav"}.
#' @param quiet a logical value indicating if status of progress should be
#' shown. Default is \code{FALSE}.
#'
#' @returns Renames or converts merged school ILSA data.
#'
#' @examples
#' # For example, after downloading 'RLII' 1991 G4 data:
#'
#' # Downloading 'RLII' 1991 and unzipping files
#' ILSAdownload(study = "RLII", year = 1991, outputdir = tempdir(), unzip = TRUE, agreeLicense = TRUE)
#' 
#' # Path were raw 'SPSS' files are
#' input <- file.path(tempdir(),"RLII1991_IDB_SPSS/Data")
#' 
#' # Path were merged files will be saved
#' output <- file.path(tempdir(),"RLII1991_IDB_SPSS")
#' 
#' # Merging 'RLII' 1991, as .rds file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#'
#' @export
#' 

school <- function(inputdir, filetype = "rds", quiet = FALSE){
  
  
  # Checks ----
  
  ## ILSApops
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv"
  
  
  ILSApops <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSApops)){
    stop(paste0("Could not read ILSApops file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  
  ## inputdir
  if(!(is.vector(inputdir)&&is.character(inputdir)))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  ## quiet
  if(!(is.vector(quiet)&&is.logical(quiet)&&length(quiet==1)))
    stop(c("\nInvalid input for 'quiet'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  ## filetype
  if(!(is.vector(filetype)&&is.character(filetype)))
    stop(c("\nInvalid input for 'filetype'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  if(min(filetype%in%c("rds", "zsav", "sav"))!=1)
    stop(c("\nInvalid input for 'filetype'.",
           "\nIt should be a 'rds', 'zsav', or 'sav'."),call. = FALSE)
  
  # Process ----
  
  filedirtot <- list.files(inputdir,pattern = ".rds|.zsav|.sav")
  filedir <- filedirtot
  filetyp <- gsub(".rds|.zsav|.sav","",filedir)
  
  avaipop <- ILSApops[ILSApops$Population%in%filetyp,]
  avaipop <- avaipop[avaipop$Respondent1%in%"school",]
  if(nrow(avaipop)==0)
    stop("\nPath does not contain school files of available ILSA.",call. = FALSE)
  
  filedir <- filedir[filetyp%in%avaipop$Population]
  filetyp <- filetyp[filetyp%in%avaipop$Population]
  
  
  
  i=1
  for(i in 1:nrow(avaipop)){
    popi <- subset(avaipop,Population%in%filetyp[i])
    
    
    
    fext <- .getext(filedir[i])
    
    # Read and save
    if(filetype==fext){
      cf <- file.copy(from = file.path(inputdir,filedir[i]),
                      to = file.path(inputdir,paste0(.getname(popi),".",filetype)),
                      overwrite = TRUE)
      if(cf&!quiet){
        message(paste0("School file created for ",filedir[i],"."))
      }
      if(!cf){
        message(paste0("School file could not be created for ",filedir[i],"."))
      }
    }else{
      
      # Read
      if(fext=="rds"){
        cf <- readRDS(file.path(inputdir,filedir[i]))
      }else{
        cf <- try(haven::read_spss(file = file.path(inputdir,filedir[i]),
                                   user_na = TRUE,
                                   col_select = NULL,
                                   skip = 0,
                                   n_max = Inf,
                                   .name_repair = "unique"),
                  silent = TRUE)
        
        if("try-error"%in%class(cf)){
          cf <- haven::read_sav(file = file.path(inputdir,filedir[i]),
                                user_na = TRUE,
                                col_select = NULL,
                                skip = 0,
                                n_max = Inf,
                                .name_repair = "unique",
                                encoding = "latin1")
        }
        
      }
      # Save
      
      if(filetype%in%"zsav"){
        haven::write_sav(data = cf,compress = "zsav",
                         path = file.path(inputdir,paste0(.getname(popi),".zsav")))
      }
      
      if(filetype%in%"sav"){
        haven::write_sav(data = cf,compress = "byte",
                         path = file.path(inputdir,paste0(.getname(popi),".sav")))
      }
      
      if(filetype%in%"rds"){
        saveRDS(cf,file = file.path(inputdir,paste0(.getname(popi),".rds")))
      }
      
      if(!quiet){
        message(paste0("School file created for ",filedir[i],"."))
      }
      
      
    }
  }
}
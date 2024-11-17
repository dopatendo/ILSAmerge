#' Download and prepare ILSA data
#'
#' Downloads ILSA data, merges it, combines students and add school information.
#' This function is a wrapper for \code{\link{ILSAdownload}}, \code{\link{ILSAmerge}},
#' \code{\link{ILSArename}}, \code{\link{combineStudents}}, and \code{\link{addSchools}}. 
#' To see which ILSA are available for this function use \code{\link{availableILSA}}.
#'
#' @param study a string indicating the name of the study. For available studies
#' check the description of this function.
#' @param year a numeric value indicating the year of the study.
#' @param outputdir the directory where the data will be saved.
#' @param filetype a string indicating the type of file to be saved, it can
#' be \code{"rds"}, \code{"zsav"}, or \code{"sav"}.
#' @param quiet a logical value indicating if status of progress should be
#' shown. 
#' Default is \code{FALSE}.
#' @param agreeLicense a logical value indicating if you agree with
#' the Disclaimer and License Agreement file from www.iea.nl. If \code{FALSE},
#' you will be prompted to agree with it or else data will not be downloaded.
#' Default is \code{FALSE}.
#' 
#'
#' @returns Saves merged and renamed ILSA data.
#'
#' @examples
#'
#' # Path were files will be saved
#' dir.create(file.path(tempdir(),"ILSAready"))
#' output <- file.path(tempdir(),"ILSAready")
#'
#' # Downloading and preparing 'RLII' 1991 data
#' ILSAready("RLII",1991,outputdir = output, agreeLicense = TRUE)
#' 
#'
#' @export
#' 



ILSAready <- function(study, year, outputdir = getwd(), 
                      filetype = c("rds", "zsav", "sav"),
                      quiet = FALSE, agreeLicense = FALSE){
  
  # Read external ----
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/combinestudents/data/ILSAlinks.csv"
  
  
  ILSAlinks <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSAlinks)){
    stop(paste0("Could not read ILSAlinks file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  
  ILSAlinks <- ILSAlinks[ILSAlinks$ILSAready%in%1,]
  
  
  
  
  
  
  
  # Checks ----
  
  ## study
  if(!(is.vector(study)&&is.character(study)&&length(study)==1))
    stop("\nInvalid input for 'study'.\nIt should be a character value.",call. = FALSE)
  
  study <- gsub(' ','',toupper(study))
  
  STUDY <- try(match.arg(study,sort(unique(ILSAlinks[,"Name"]))),silent = TRUE)
  
  if("try-error"%in%class(STUDY))
    stop(paste0("\n",study," not found.",
                "\nIt should be one of ",
                paste0(sort(unique(ILSAlinks[,"Name"])),collapse = ", "),"."))
  
  
  ## year
  if(!(is.vector(year)&&is.numeric(year)&&length(year)==1))
    stop(paste0("\nInvalid input for 'year'.",
                "\nIt should be a numeric value."))
  
  todownload <- ILSAlinks[ILSAlinks[,"Name"]==STUDY&ILSAlinks[,"Year"]==year,]
  avai <- sort(unique(ILSAlinks[ILSAlinks[,"Name"]==STUDY,"Year"]))
  
  if(nrow(todownload)<1)
    stop(paste0("\nNo files found for ",STUDY," ",year,".",
                "\nFor ",STUDY," the available ",
                ifelse(length(avai)==1,"year is: ","years are: "),
                paste0(avai,collapse = ", "),".",
                "\nCheck availability using availableILSA(FOR = \"ILSAready\")"),call. = FALSE)
  
  
  # Download ----------------------------------------------------------------
  
  
  dir.create(file.path(tempdir(),"forDownload"),showWarnings = FALSE)
  ILSAdownload(study, year, 
               outputdir = file.path(tempdir(),"forDownload"), 
               unzip = TRUE, maxtime = Inf, 
               quiet = quiet, agreeLicense = agreeLicense)
  
  
  # Identify ----------------------------------------------------------------
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/combinestudents/data/ILSApops.csv"
  
  ILSApops <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSApops)){
    stop(paste0("Could not read ILSApops file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  
  
  fls <- list.files(file.path(tempdir(),"forDownload"),recursive = TRUE,full.names = F)
  flsU <- toupper(fls)
  psb <- paste0(paste0(substring(ILSApops$Pop,1,3),".+",substring(ILSApops$Pop,4,5)),collapse = "|")
  
  if(!quiet){cat("Files downloaded.\n")}
  
  # Copy --------------------------------------------------------------------
  
  dir.create(file.path(tempdir(),"forCopy"),showWarnings = FALSE)
  cf <- file.copy(file.path(tempdir(),"forDownload",fls[grepl(psb,flsU)]),
                  file.path(tempdir(),"forCopy",basename(fls[grepl(psb,flsU)])))
  unlink(file.path(tempdir(),"forDownload"),recursive = TRUE)
  
  
  
  # Merge -------------------------------------------------------------------
  
  dir.create(file.path(tempdir(),"forMerge"),showWarnings = FALSE)
  ILSAmerge(inputdir = file.path(tempdir(),"forCopy"),
            outputdir = file.path(tempdir(),"forMerge"),
            MBlimit = NULL,
            filetype = filetype,
            quiet = quiet)
  unlink(file.path(tempdir(),"forCopy"),recursive = TRUE)
  if(!quiet){cat("Files merged.\n")}
  
  
  
  # Rename ------------------------------------------------------------------
  
  ILSArename(inputdir = file.path(tempdir(),"forMerge"),quiet = TRUE)
  if(!quiet){cat("Files renamed.\n")}
  
  
  # Combine students --------------------------------------------------------
  
  tr <- try(combineStudents(inputdir = file.path(tempdir(),"forMerge"), 
                            outputdir = file.path(tempdir(),"forMerge"), 
                            quiet = quiet),silent = TRUE)
  if(!"try-error"%in%class(tr)){
    if(!quiet){cat("Students combined.\n")}
  }
  
  
  # Add schools -------------------------------------------------------------
  
  tr <- try(addSchools(inputdir = file.path(tempdir(),"forMerge"), 
                       outputdir = file.path(tempdir(),"forMerge"), 
                       quiet = quiet),silent = TRUE)
  if(!"try-error"%in%class(tr)){
    if(!quiet){cat("Schools added.\n")}
  }
  
  
  # Copy to outputdir -------------------------------------------------------
  
  fls <- list.files(file.path(tempdir(),"forMerge"),full.names = TRUE)
  cf <- file.copy(fls,
                  file.path(outputdir,basename(fls)))
  unlink(file.path(tempdir(),"forMerge"),recursive = TRUE)
  if(!quiet){cat("Files copied.\n")}
  
  
  if(!quiet){cat("Data ready.\n")}
  
}
#' Download and prepare ILSA data
#'
#' Downloads ILSA data, merges it, combines students and adds school information.
#' This function is a wrapper for \code{\link{ILSAdownload}}, \code{\link{ILSAmerge}},
#' \code{\link{ILSArename}}, \code{\link{combineStudents}}, and \code{\link{addSchools}}. 
#' To see which ILSA are available for this function use \code{\link{availableILSA}}.
#' If data is already downloaded you can use \code{\link{ILSAreadylocal}}.
#'
#' @inheritParams ILSAmerge
#' @inheritParams ILSAdownload
#' 
#' 
#' @examples
#' dir.create(file.path(tempdir(),"timssadv"),showWarnings = FALSE)
#' output <- file.path(tempdir(),"timssadv")
#' 
#' input <- system.file("extdata/timssadv", package = "ILSAmerge")
#' 
#' ILSAreadylocal(inputdir = input, outputdir = output, filetype = "zsav")
#' 
#' @returns Saves merged and renamed ILSA data.
#' 
#' @export
#' 
#' 

#' @rdname ILSAready
#' @export
ILSAready <- function(study, year, outputdir = getwd(), 
                      filetype = c("rds", "zsav", "sav"), 
                      MBlistlimit = 200, quiet = FALSE, 
                      agreeLicense = FALSE, maxtime = 999){
  # Read external ----
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv"


  ILSAlinks <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))

  if("try-error"%in%class(ILSAlinks)){
    stop(paste0("Could not read ILSAlinks file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  

  ILSAlinks <- ILSAlinks[ILSAlinks$ILSAready%in%1,]

  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSApops.csv"
  
  where <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(where)){
    warning(paste0("Could not read population information from 'GitHub'.",
                   "\nInternal data will be used to rename files.",
                   "\nPlease be aware, these data may not be the lastest one."),call. = FALSE)
    
    ILSApops <- utils::read.csv(file.path(system.file("extdata/ilsainfo", package = "ILSAmerge"),"ILSApops.csv"))
  }else{
    ILSApops <- where
  }
  
  
  
  # Checks ----
  
  if(min(filetype%in%c("rds", "zsav", "sav"))!=1)
    stop(c("\nInvalid input for 'filetype'.",
           "\nIt should be a 'rds', 'zsav', or 'sav'."),call. = FALSE)
  
  filetype = match.arg(filetype, c("rds", "zsav", "sav"))  
  
  ## study
  if(!(is.vector(study)&&is.character(study)&&length(study)==1))
    stop("\nInvalid input for 'study'.\nIt should be a character value.",call. = FALSE)
  
  study <- gsub(' ','',toupper(study))
  
  STUDY <- try(match.arg(study,sort(unique(ILSAlinks[,"Name"]))),silent = TRUE)
  
  if("try-error"%in%class(STUDY))
    stop(paste0("\n",study," not found.",
                "\nCheck availability using availableILSA(FOR = \"ILSAready\").",
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
  
  
  ## outputdir
  if(!(is.vector(outputdir)&&is.character(outputdir)&&length(outputdir)==1))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  ## quiet
  if(!(isTRUE(quiet)|isFALSE(quiet)))
    stop(c("\nInvalid input for 'quiet'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  ## agreeLicense
  if(!(isTRUE(agreeLicense)|isFALSE(agreeLicense)))
    stop(c("\nInvalid input for 'agreeLicense'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  if(!(is.vector(MBlistlimit)&&length(MBlistlimit)==1&&is.numeric(MBlistlimit)))
    stop(c("\nInvalid input for 'MBlistlimit'.",
           "\nIt should be a numeric value."),call. = FALSE)
  
  
  # Download ----------------------------------------------------------------
  
  
  ptime <- proc.time()
  
  
  ILSAdownload(study = study, year = year, outputdir = outputdir, unzip = TRUE,
               maxtime = maxtime, quiet = quiet, agreeLicense = agreeLicense)
  
  
  
  
  ndir <- file.path(outputdir,paste(study,year,"Merged",sep = "_"))
  
  dir.create(ndir, showWarnings = F)
  
  Name = NULL; Year = NULL
  whdownloaded <- subset(ILSAlinks,Name%in%study&Year%in%year)
  whdownloaded <- gsub(".zip","",basename(whdownloaded$Data_SPSS))
  whdownloaded <- gsub("%26","&",whdownloaded)
  
  
  
  
  # Merge -------------------------------------------------------------------
  
  
  
  
  i=1
  for(i in 1:length(whdownloaded)){
    dirs <- list.files(file.path(outputdir,whdownloaded[i]),recursive = TRUE,
                       pattern = ".SAV$|.ZSAV$",ignore.case = TRUE,full.names = TRUE)
    dirs <- dirname(dirs)
    
    j=1
    dirs <- unique(dirs)
    dirs <- dirs[unlist(lapply(dirs,function(j){all(ILSAfile.info(j)$Population%in%ILSApops$Pop)}))]

    for(j in 1:length(dirs)){
      .ILSAreadylocal(inputdir = dirs[j], outputdir = ndir, 
                      quiet = quiet, MBlistlimit = MBlistlimit,filetype = filetype)
      
    }
  }
  
  
  
  

  
  # End ---------------------------------------------------------------------
  
  
  if(!quiet){
    
    cat(paste0("Downloading and preparing data took ",round((proc.time()-ptime)[3]),
               " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))
  }
}

#' @rdname ILSAready
#' @export
ILSAreadylocal <- function(inputdir = getwd(), outputdir = getwd(), 
                           filetype = c("rds", "zsav", "sav"),
                           quiet = FALSE, MBlistlimit = 200){
  
  

# Checks ------------------------------------------------------------------

filetype = match.arg(filetype, c("rds", "zsav", "sav"))  
  

# Process -----------------------------------------------------------------

  
  
  ptime <- proc.time()
  
  .ILSAreadylocal(inputdir = inputdir,
                  outputdir = outputdir, filetype = filetype,
                  quiet = quiet, MBlistlimit = MBlistlimit)
  
  if(!quiet){
    
    cat(paste0("Preparing data took ",round((proc.time()-ptime)[3]),
               " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))
  }
}


.ILSAreadylocal <- function(inputdir = getwd(), outputdir = getwd(), 
                            filetype = c("rds", "zsav", "sav"), 
                            quiet = FALSE, MBlistlimit = 200){
  # Merge -------------------------------------------------------------------
  
  try(ILSAmerge(inputdir = inputdir, outputdir = outputdir, filetype = filetype,
                MBlistlimit = MBlistlimit,quiet = quiet),silent = TRUE)

  inf <- try(ILSAfile.info(inputdir = inputdir),silent = TRUE)
  inf <- try(file.path(paste0(inf$Population, ".",filetype)),silent = TRUE)
  
  
  # Combine students --------------------------------------------------------
  
  try(.combineStudents(inputdir = outputdir,
                                   inpfiles = inf,
                                   outputdir = outputdir,
                                   quiet = quiet,
                                   save = TRUE),silent = TRUE)
  

  # Add schools -------------------------------------------------------------
  
  try(.addSchools(inputdir = outputdir,
                                   inpfiles = inf,
                                   outputdir = outputdir,
                                   quiet = quiet,
                                   save = TRUE),silent = TRUE)
  
  # .addSchools(inputdir = outputdir,
  #                 inpfiles = inf,
  #                 outputdir = outputdir,
  #                 quiet = quiet, 
  #                 save = TRUE)
  
  # Rename ------------------------------------------------------------------
  
  try(ILSArename(inputdir = outputdir, quiet = quiet),silent = TRUE)
  
}




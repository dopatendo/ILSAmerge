#' Download ILSA data
#'
#' Downloads SPSS data from different International Large-Scale Assessments (ILSA).
#' This functions supports the following ILSA: PISA, TIMSS, TIMSS Advanced, PIRLS, 
#' ICCS, ICILS, CIVED, REDS, RLII, and SITES. Depending on the study, 
#' you will need to decide which data to download, and
#' read and accept its terms and conditions to proceed with the download.
#'
#' @param study a string indicating the name of the study. For available studies
#' check the description of this function.
#' @param year a numeric value indicating the year of the study.
#' @param outputdir the directory where the merged data will be saved.
#' @param unzip a logical value indicating if files should be unzipped.
#' Default is \code{FALSE}.
#' @param timeout a numeric value indicating the maximum time allowed for 
#' downloading a file. Default is \code{999}.
#'
#' @returns Saves SPSS ILSA data locally.
#'
#' @examples
#' \dontrun{
#' # For example, to download TIMSS 1995 data:
#'
#' # Path were files will be saved
#' output <- 'C:/'
#'
#' # Downloading TIMSS 1995
#' ILSAdownload(study = "TIMSS", year = 1995, outputdir = output)
#' 
#' # Downloading TIMSS 1995 and unzipping files
#' ILSAdownload(study = "TIMSS", year = 1995, outputdir = output, unzip = TRUE)
#' }
#'
#' @export

ILSAdownload <- function(study, year, outputdir, unzip = FALSE, timeout = 999){
  
  # Examples & Tests ----
  # study = "PISA"
  # year = 2009
  # outputdir = tests
  # unzip = TRUE
  
  # Read external ----
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv"
  
  ILSAlinks <- utils::read.csv(where)
  
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
  
  ## outputdir
  if(!(is.vector(outputdir)&&is.character(outputdir)))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  if(!file.exists(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  # unzip
  if(!(is.vector(unzip)&&is.logical(unzip)&&length(unzip==1)))
    stop(c("\nInvalid input for 'unzip'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  # timeout
  if(!(is.vector(timeout)&&is.numeric(timeout)&&length(timeout)==1))
    stop(c("\nInvalid input for 'timeout'.",
           "\nIt should be a numeric value."),call. = FALSE)
  
  # Institution
  
  inst <- NULL
  
  if(STUDY%in%c("PISA","TALIS"))
    inst <- "OECD"
  
  
  if(STUDY%in%c('CIVED', 'ICCS', 'ICILS', 'PIRLS', 'REDS', 'RLII', 'SITES', 'TIMSS', 'TIMSSADVANCED'))
    inst <- "IEA"
  
  if(is.null(inst))
    stop("\nStudy not yet supported.",call. = FALSE)
  
  
  # Process & Output ----
  
  todownload <- ILSAlinks[ILSAlinks[,"Name"]==STUDY&ILSAlinks[,"Year"]==year,]
  avai <- sort(unique(ILSAlinks[ILSAlinks[,"Name"]==STUDY,"Year"]))
  
  if(nrow(todownload)<1)
    stop(paste0("\nNo files found for ",STUDY," ",year,".",
                "\nFor ",STUDY," the available ",
                ifelse(length(avai)==1,"year is: ","years are: "),
                paste0(avai,collapse = ", "),"."),call. = FALSE)
  
  ## Conditional prompts by study ----
  
  if(paste0(STUDY,year)=="PISA2009"){
    cat(paste0("Which data from PISA 2009 do you wish to download?",
               "\nType 1 for PISA 2009",
               "\nType 2 for PISA 2009 Electronic Reading Assessment (ERA)"))
    
    whichdata <- readline(prompt="")
    
    if(!whichdata%in%c(1,2))
      stop("\nInvalid input for PISA 2009.",call. = FALSE)
    
    if(whichdata==1){STUDY <- "PISA"}
    if(whichdata==2){
      STUDY <- "PISA_ERA"
      warning("\nSPSS syntax not yet available for PISA ERA 2009.")
      
      }
    

  
    todownload <- ILSAlinks[ILSAlinks[,"Name"]==STUDY&ILSAlinks[,"Year"]==year,]
    
  }
  
  if(paste0(STUDY,year)=="PISA2012"){
    cat(paste0("Which data from PISA 2012 do you wish to download?",
               "\nType 1 for PISA 2012",
               "\nType 2 for PISA 2012 Financial Literacy",
               "\nType 3 for PISA 2012 Computer-Based Administration (CBA)"))
    
    whichdata <- readline(prompt="")
    
    
    if(!whichdata%in%c(1,2,3))
      stop("\nInvalid input for PISA 2012.",call. = FALSE)
    
    if(whichdata==1){STUDY <- "PISA"}
    if(whichdata==2){STUDY <- "PISA_FINANCIAL"}
    if(whichdata==3){STUDY <- "PISA_CBA"}
    
    
    todownload <- ILSAlinks[ILSAlinks[,"Name"]==STUDY&ILSAlinks[,"Year"]==year,]
    
  }
  
  ## End of conditional ----
  
  repo <- todownload[,"Repository"][1]
  ark <- todownload[,"Data_SPSS"]
  create <- as.logical(as.numeric(todownload[,"Create_folder"][1]))
  zip <- as.logical(as.numeric(todownload[,"ZIP"]))
  
  ## Download IEA ----
  if(inst=="IEA"){
    # mainurl <- "https://www.iea.nl/sites/default/files/data-repository"
    mainurl <- ""
    agree <- "https://www.iea.nl/sites/default/files/2019-05/Disclaimer%20and%20License%20Agreement.pdf"
    
    
    utils::download.file(agree,file.path(outputdir,
                                         "DisclaimerAndLicenseAgreement.pdf"),
                         quiet = TRUE)
    
    cat(paste0("By accessing the Data Repository, IDB Analyzer and Data visualizer, you indicate that you agree to the terms and conditions associated with their use. Please read the Disclaimer and License Agreement for full details.\n"))
    
    cat(paste0("Licencese Agreement downloaded.\n",
               "Do you agree with these terms and conditions?\n",
               "Please answer TRUE or FALSE."))
    
    doyou <- readline(prompt="")
    
    if(!isTRUE(as.logical(doyou)))
      stop("\nYou did not agree with the terms and conditions.",
           "\nNo data will be downloaded.", call. = FALSE)
    
    cat(paste0(length(ark)," files found for ",STUDY," ",year,".\n"))
    for(i in 1:length(ark)){
      i = ark[i]
      nm <- substring(i,max(gregexpr("/",i)[[1]])+1)
      utils::download.file(url = file.path(i),
                           destfile = file.path(outputdir,nm),
                           options = options(timeout=timeout))
      
      if(unzip){
        utils::unzip(zipfile = file.path(outputdir,nm),exdir = outputdir)
      }
      
    }
  }
  
  
  ## Download OECD ----
  if(inst=="OECD"){
    mainurl <- ""
    

    
    if(create){
      dir.create(path = file.path(outputdir,paste0(STUDY,year)),showWarnings = FALSE)
      savedir <- file.path(outputdir,paste0(STUDY,year))
    }else{
      savedir <- outputdir
    }
    
     

    cat(paste0(length(ark)," files found for ",STUDY," ",year,".\n"))
    for(i in 1:length(ark)){
      # i = ark[i]
      nm <- substring(ark[i],max(gregexpr("/",ark[i])[[1]])+1)
      utils::download.file(url = file.path(ark[i]),
                           destfile = file.path(savedir,nm),
                           options = options(timeout=timeout))
      
      if(unzip&zip[i]){
        utils::unzip(zipfile = file.path(savedir,nm),exdir = file.path(savedir))
      }

    }
  }
  
  
  cat(paste0("Visit ",repo," to know how to use and cite these datasets."))
}
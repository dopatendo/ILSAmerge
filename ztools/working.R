study = "REDS"
year = 2021
outputdir = "/Users/andreschristiansen/Downloads/old"


ILSAdownload <- function(study, year, outputdir){
  
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
                "\nIt should be a numerica value."))
  
  ## outputdir
  if(!(is.vector(outputdir)&&is.character(outputdir)))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nIt should be a character vector."),call. = FALSE)
  
  if(!file.exists(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nPath does not exist."),call. = FALSE)
  

  
  # Process & Output ----
  
  todownload <- ILSAlinks[ILSAlinks[,"Name"]==STUDY&ILSAlinks[,"Year"]==year,]
  avai <- sort(unique(ILSAlinks[ILSAlinks[,"Name"]==STUDY,"Year"]))
  
  if(nrow(todownload)<1)
    stop(paste0("\nNo files found for ",STUDY," ",year,".",
                "\nFor ",STUDY," the available ",
                ifelse(length(avai)==1,"year is: ","years are: "),
                paste0(avai,collapse = ", "),"."),call. = FALSE)
  
  repo <- todownload[,"Repository"]
  ark <- todownload[,"Data_SPSS"]
  
  
  
  mainurl <- "https://www.iea.nl/sites/default/files/data-repository"
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
    utils::download.file(url = file.path(mainurl,i),
                         destfile = file.path(outputdir,nm))
  }
  
  cat(paste0("Visit ",repo," to know how to cite these datasets."))
}



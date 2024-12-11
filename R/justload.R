#' Loading ILSA data into a list
#'
#' Load 'SPSS' data from different International Large-Scale Assessments (ILSA),
#' including: 'TIMSS', 'TIMSS Advanced', 'PIRLS', 'ICCS', 'ICILS', 'CIVED', 'REDS', 'RLII',
#' and 'SITES' (2006) into a list.
#'
#' @inheritParams ILSAmerge
#' @param population a character value indicating which files should be loaded. 
#' For more information on available populations, run \code{ILSAfile.info()} first.
#' @param justattributes a logical value indicating if 0 rows should be loaded.
#' This can be used when we just need to check column attributes. Default is 
#' \code{FALSE}.
#' @param addcountries a logical value indicating if country information should
#' be added to the elements of the list. This means adding the variable \code{CNTRY}
#' where needed and adding labels for \code{IDCNTRY} where needed. If \code{FALSE}
#' (the default), data will be loaded as is.
#' Country information will be retrieved from 'GitHub' if possible. If not, it will
#' use the package internal data.
#' 
#'
#' @returns A list of tibbles.
#'
#' @examples
#' # Path where raw 'SPSS' files are
#' input <- system.file("extdata/reds", package = "ILSAmerge")
#' 
#' # Load only attributes
#' emptylist <- justload(inputdir = input, population = "BCGV1", justattributes = TRUE)
#' 
#' # Load complete data
#' fulllist <- justload(inputdir = input, population = "BCGV1", justattributes = FALSE)
#' 
#' # Load complete data and add country labels
#' withcou <- justload(inputdir = input, population = "BCGV1", addcountries = TRUE)
#'
#' @export



justload <- function(inputdir = getwd(), population, justattributes = FALSE,
                     addcountries = FALSE){
  
  # Example & Test ----
  
  # inputdir <- "/Users/andreschristiansen/Downloads/TIMSS1995_IDB_SPSS_G4/Data"
  # ILSAfile.info(inputdir)
  # population <- "ASAm1"
  # justattributes = FALSE
  
  
  # Checks ----
  
  ## inputdir
  if(!(is.vector(inputdir)&&is.character(inputdir)&&length(inputdir)==1))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  if(!(is.vector(justattributes)&&is.logical(justattributes)&&length(justattributes==1)))
    stop(c("\nInvalid input for 'justattributes'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  ## Process ----
  
  
  ark <- list.files(path = inputdir,pattern = ".SAV$|.ZSAV$",ignore.case = TRUE)
  erk <- list.files(path = inputdir,pattern = ".SAV$|.ZSAV$",ignore.case = TRUE, full.names = TRUE)
  
  pop <- substr(ark,1,3)
  cou <- substr(ark,4,6)
  
  ext <- lapply(ark,function(i){
    out <- substring(i,7)
    c(substr(out,1,gregexpr('\\.',out)[[1]]-1),
      substring(out,gregexpr('\\.',out)[[1]]+1))
  })
  
  stu <- unlist(lapply(ext,function(i) i[[1]]))
  ext <- unlist(lapply(ext,function(i) i[[2]]))
  
  
  popstu <- toupper(paste0(pop,stu))
  upopstu <- unique(popstu)
  
  
  if(!(is.vector(population)&&is.character(population)&&length(population==1)))
    stop(c("\nInvalid input for 'population'.",
           "\nIt should be a character vector of length 1."),call. = FALSE)
  
  
  if(!population%in%upopstu)
    stop(c("\nInvalid input for 'population'.",
           "\nIt is not contained in the path. Check with ILSAfile.info()."),call. = FALSE)
  
  
  upopstu <- upopstu[upopstu%in%population]
  
  
  erki <- erk[popstu%in%upopstu]
  
  if(addcountries){
    where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAcou.csv"
    
    
    where <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
    
    if("try-error"%in%class(where)){
      warning(paste0("Could not read country information from 'GitHub'.",
                     "\nInternal data will be used for adding country labels.",
                     "\nPlease be aware, these data may not be the lastest one."),call. = FALSE)
      
      ILSAcou <- utils::read.csv(file.path(system.file("extdata/ilsainfo", package = "ILSAmerge"),"ILSAcou.csv"))
    }else{
      ILSAcou <- where
    }
    
    
    ILSAcou <- ILSAcou[ILSAcou$N3code!=0,]
    couL <- ILSAcou$IEAcode
    names(couL) <- ILSAcou$Name
    couLS <- sort(couL[ILSAcou$toLabel%in%1])
    couL <- sort(couL)
  }
  
  
  out <- vector(mode = "list",length = length(erki))
  
  for(j in 1:length(erki)){
    
    # outj <- try(haven::read_spss(file = erki[j], user_na = TRUE, col_select = NULL,
    #                              skip = 0, n_max = ifelse(justattributes, 0, Inf),
    #                              .name_repair = "unique"),silent = TRUE)
    # 
    # if("try-error"%in%class(outj)){
    #   outj <- haven::read_sav(file = erki[j], user_na = TRUE, col_select = NULL,
    #                           skip = 0, n_max = ifelse(justattributes, 0, Inf),
    #                           .name_repair = "unique",
    #                           encoding = 'latin1')
    # }
    
    outj <- .readILSA(x = erki[j])
    
    
    if(addcountries){
      
  
      # Fix IDCNTRY
      nav <- attr(outj$IDCNTRY,"na_values")
      lbl <- attr(outj$IDCNTRY,"labels")
      vlb <- attr(outj$IDCNTRY,"label")
      attr(outj$IDCNTRY,"format.spss") <- paste0("F",max(nchar(c(nav,lbl,couLS))),".0")
      attr(outj$IDCNTRY,"labels") <- c(couLS,lbl)
      
      
      # # add labels to IDCNTRY
      # attr(outj$IDCNTRY,'labels') <- couLS
      
      # Add country string
      if(!"IDCNTRY_STR"%in%colnames(outj)){
        cl <- class(outj)
        outj <- cbind(IDCNTRY_STR = names(couL)[match(as.numeric(outj$IDCNTRY),couL)], outj)
        class(outj) <- cl
      }
      
      if(!"CNTRY"%in%colnames(outj)){
        cl <- class(outj)
        outj <- cbind(CNTRY = toupper(cou[j])[!justattributes], outj)
        class(outj) <- cl
      }
      
    }
    
    
    
    out[[j]] <- outj
  }
  
  # Output ----
  
  return(out)
  
}





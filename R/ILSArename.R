#' Rename \code{\link{ILSAmerge}} files
#'
#' Renames files produced by \code{\link{ILSAmerge}} from name codes to comprehensible names 
#' including the study name, year and respondent.
#' This function has been tested to behave correctly for: 'TIMSS', 'TIMSS Advanced', 
#' 'PIRLS', 'ICCS', 'ICILS', 'CIVED', 'REDS', 'RLII', and 'SITES' (2006).
#' 
#' @inheritParams ILSAmerge
#' @param codeTOname a logical value indicating if files should be renamed from
#' codes to names (\code{TRUE}) or from names to codes (\code{FALSE}).
#' Default is \code{TRUE}.
#' @param overwrite a logical value indicating if files should be overwritten.
#' If \code{FALSE}, files will be copied with the new names. Default is \code{TRUE}.
#'
#' @returns Renames or copies files produced by \code{\link{ILSAmerge}}.
#' 
#'
#' @examples
#' 
#' # Merge files
#' dir.create(file.path(tempdir(),"REDS2021"),showWarnings = FALSE)
#' ILSAmerge(inputdir = system.file("extdata/reds", package = "ILSAmerge"), 
#' outputdir = file.path(tempdir(),"REDS2021"))
#' 
#' # Show files with raw names
#' list.files(file.path(tempdir(),"REDS2021"))
#' 
#' # Rename files
#' ILSArename(inputdir = file.path(tempdir(),"REDS2021"))
#' 
#' # Show files new names 
#' list.files(file.path(tempdir(),"REDS2021"))
#'
#' @export


ILSArename <- function(inputdir = getwd(), codeTOname = TRUE, overwrite = TRUE, quiet = FALSE){
  # Checks ------------------------------------------------------------------
  
  ##inputdir
  if(!(is.vector(inputdir)&&is.character(inputdir)&&length(inputdir)==1))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  # codeTOname
  if(!(isTRUE(codeTOname)|isFALSE(codeTOname)))
    stop(c("\nInvalid input for 'codeTOname'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  # overwrite
  if(!(isTRUE(overwrite)|isFALSE(overwrite)))
    stop(c("\nInvalid input for 'overwrite'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  # quiet
  if(!(isTRUE(quiet)|isFALSE(quiet)))
    stop(c("\nInvalid input for 'quiet'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  
  # Process -----------------------------------------------------------------
  
  
  
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
  

  
  if(codeTOname){
    inpname <- ILSApops$Pop
    outname <- ILSApops$Name
  }else{
    inpname <- ILSApops$Name
    outname <- ILSApops$Pop
  }
  
  inpfiles <- list.files(inputdir)
  
  
  ext <- lapply(inpfiles,function(i){
    dot <- max(gregexpr("\\.",i)[[1]])
    list(substr(i,1,dot-1),substring(i,dot))
    
  })
  pop <- sapply(ext,function(i) i[[1]])
  ext <- sapply(ext,function(i) i[[2]])
  
  
  inpfiles <- inpfiles[pop%in%inpname]
  ext <- ext[pop%in%inpname]
  pop <- pop[pop%in%inpname]
  
  if(length(inpfiles)==0)
    stop("\nNo ILSAmerge() files found in 'inputdir'.",call. = FALSE)
  
  if(!quiet){
    cat(length(inpfiles)," ILSAmerge() file(s) found.\n")
  }
  
  
  outfiles <- paste0(outname[match(pop,inpname)],ext)
  
  
  # Output ------------------------------------------------------------------
  
  
  
  if(overwrite){
    cf <- file.rename(from = file.path(inputdir,inpfiles),
                      to = file.path(inputdir,outfiles))
    
    if(!quiet){
      cat(sum(cf)," ILSAmerge() file(s) renamed.\n")
    }
  }else{
    cf <- file.copy(from = file.path(inputdir,inpfiles),
                    to = file.path(inputdir,outfiles),
                    overwrite = TRUE)
    
    if(!quiet){
      cat(sum(cf)," ILSAmerge() file(s) copied.\n")
    }
  }
}



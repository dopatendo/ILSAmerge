#' Rename \code{\link{ILSAmerge}} files
#'
#' Renames files produced by \code{\link{ILSAmerge}} from name codes to comprehensible names 
#' including the study name, year and respondent.
#' This function has been tested to behave correctly for: 'TIMSS', 'TIMSS Advanced', 
#' 'PIRLS', 'ICCS', 'ICILS', 'CIVED', 'REDS', 'RLII', and 'SITES' (2006).
#' 
#' @param inputdir a string indicating the path were \code{\link{ILSAmerge}} files are stored.
#' @param codeTOname a logical value indicating if files should be renamed from
#' codes to names (\code{TRUE}) or from names to codes (\code{FALSE}).
#' Default is \code{TRUE}.
#' @param overwrite a logical value indicating if files should be overwritten.
#' If \code{FALSE}, files will be copied with the new names. Default is \code{TRUE}.
#' @param quiet a logical value if progress should be
#' shown. Default is \code{FALSE}.
#'
#' @returns Renames or copies files produced by \code{\link{ILSAmerge}}.
#'
#' @examples
#' 
#' # Merge files
#' dir.create(file.path(tempdir(),"REDS2021"),showWarnings = FALSE)
#' ILSAmerge(inputdir = system.file("extdata", package = "ILSAmerge"), 
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


ILSArename <- function(inputdir, codeTOname = TRUE, overwrite = TRUE, quiet = FALSE){
  # Checks ------------------------------------------------------------------
  
  # inputdir
  if(!(is.vector(inputdir)&&is.character(inputdir)))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a character vector."),call. = FALSE)
  
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
  
  ILSApops <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSApops)){
    stop(paste0("Could not read ILSApops file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
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



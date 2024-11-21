rm(list = ls())


input= "/Users/andreschristiansen/Downloads/merged2"
# codeTOname = T
# overwrite = T
# quiet = FALSE

# function ----------------------------------------------------------------


rename <- function(inputdir, codeTOname = TRUE, overwrite = TRUE, quiet = FALSE){
  # Checks ------------------------------------------------------------------
  
  
  
  # Process -----------------------------------------------------------------
  
  
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/rename/data/ILSApops.csv"
  
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





rename(input,F)


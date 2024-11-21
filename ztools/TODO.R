## VIGNETTE combineStudents, addSchools, ILSAready
### LINK addSchools
### LINK combineStudents
### LINK availableILSA
### LINK ILSAready




setwd("/Users/andreschristiansen/Downloads/merged3")
inputdir = getwd()
quiet = FALSE


addSchools <- function(inputdir = getwd(), outputdir = getwd(), quiet = FALSE){
  # Load data ---------------------------------------------------------------
  
  
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/combinestudents/data/ILSApops.csv"
  
  ILSApops <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSApops)){
    stop(paste0("Could not read ILSApops file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  
  head(ILSApops)
  
  
  
  
  
  # Identify populations ----------------------------------------------------
  
  ptime <- proc.time()
  
  inpfiles <- list.files(path = inputdir,pattern = ".rds|.zsav|.sav")
  ext <- lapply(inpfiles,function(i){
    dot <- max(gregexpr("\\.",i)[[1]])
    list(substr(i,1,dot-1),substring(i,dot+1))
    
  })
  pop <- sapply(ext,function(i) i[[1]])
  ext <- sapply(ext,function(i) i[[2]])
  
  
  # ILSApops <- ILSApops[ILSApops$R1%in%"student",]
  
  inpfiles <- inpfiles[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  ext <- ext[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  pop <- pop[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  pop <- unlist(lapply(pop, function(i) ILSApops$Pop[ILSApops$Pop%in%i|ILSApops$Name%in%i]))
  ILSApops <- ILSApops[ILSApops$Pop%in%pop,] # all student files
  ILSApops <- ILSApops[match(pop,ILSApops$Pop),] # order like pop
  bindto <- ILSApops[!(ILSApops$SCHcbind%in%"-"),] # bind to which files
  popto <- pop[pop%in%bindto$Pop]
  inpto <- inpfiles[pop%in%bindto$Pop]
  extto <- ext[pop%in%bindto$Pop]
  
  
  
  
  ## Not a single file
  if(nrow(bindto)==0)
    stop(paste0("\nNo ILSAmerge() files for combining found in 'inputdir'.",
                "\nCheck availability using availableILSA(for = \"add.schools\")"),call. = FALSE)
  
  ## Missing files
  necpops <- unlist(strsplit(x = bindto$SCHcbind,split = ";"))
  
  
  if(!all(necpops%in%pop)){
    stop(paste0("\nNot all needed files are in 'inputdir'.",
                " Check your data."),call. = FALSE)
    
  }
  
  if(!quiet){
    cat((nrow(bindto)+length(necpops))," ILSAmerge()/ILSArename() file(s) found. ")
    cat(paste0("Adding schools to ",nrow(bindto)," file(s).\n"))
  }
  
  
  i=1
  for(i in 1:nrow(bindto)){
    
    # Precombine students
    
    tocom <- c(bindto[i,c("STUDcbind","STUDrbind")])
    tocom <- as.vector(unlist(lapply(tocom, function(j) strsplit(j,split = ";"))))
    tocom <- setdiff(tocom,"-")
    
    # Combining students if needed --------------------------------------------
    if(length(tocom)>0){
      
      if(!all(tocom%in%pop))
        stop(paste0("\nNo ILSAmerge() student files for combining found in 'inputdir'.",
                    "\nCheck your data."),call. = FALSE)
      
      if(!quiet){
        cat(paste0("Combining students for ",bindto$Pop[i],".\n"))
      }
      
      addto <- ILSAmerge:::.combineStudents(inpfiles = inpfiles[pop%in%tocom|pop%in%bindto$Pop[i]],
                                            outputdir = getwd(),
                                            quiet = TRUE, save = FALSE)
      
    }
    
    
    # Not combine students ----------------------------------------------------
    
    if(length(tocom)==0){
      addto <- readILSA(file.path(inputdir,inpto[i]))
    }
    
    
    # Add schools -------------------------------------------------------------
    
    if(!quiet){
      cat(paste0("Adding schools to ",bindto$Pop[i],".\n"))
    }
    
    klass <- class(addto)
    schoo <- readILSA(file.path(inputdir,inpfiles[pop%in%bindto$SCHcbind[i]][1]))
    uids <- ILSApops$uID[ILSApops$Pop%in%bindto$SCHcbind[i]]
    addto <- cbind(uID = uID(x = addto, uID = uids), addto)
    uID <- uID(x = schoo, uID = uids)
    schoo <- schoo[,-which(colnames(schoo)%in%intersect(colnames(addto),colnames(schoo)))]
    schoo <- cbind(uID, schoo)
    
    addto <- merge(addto,schoo,by = "uID",all = TRUE,sort = FALSE)
    
    
    addto <- addto[,-1]
    class(addto) <- klass
    
    
    
    if(extto[i]%in%"zsav"){
      haven::write_sav(data = addto,compress = "zsav",
                       path = file.path(outputdir,
                                        paste0(bindto$preName[i],"_",bindto$R1[i],"&","school",".zsav")))
    }
    
    if(extto[i]%in%"sav"){
      haven::write_sav(data = addto,compress = "byte",
                       path = file.path(outputdir,
                                        paste0(bindto$preName[i],"_",bindto$R1[i],"&","school",".sav")))
    }
    
    if(extto[i]%in%"rds"){
      saveRDS(addto,file = file.path(outputdir,
                                   paste0(bindto$preName[i],"_",bindto$R1[i],"&","school",".rds")))
    }
    
  }
  
}





addSchools()



addSchools(input,output)

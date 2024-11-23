#' Add school data
#'
#' Add school data to student and teacher files merged by \code{\link{ILSAmerge}}.
#' It will run \code{\link{combineStudents}} internally.
#' To see which ILSA are available for adding school data use \code{\link{availableILSA}}.
#'
#' @inheritParams combineStudents
#'
#' @returns Saves combined student data and teacher data with added school data.
#'
#' @examples
#' # Path where raw 'SPSS' files are
#' input <- system.file("extdata/timssadv", package = "ILSAmerge")
#' 
#' # Path where merged files will be saved
#' dir.create(file.path(tempdir(),"addSchools"))
#' output <- file.path(tempdir(),"addSchools")
#' 
#' # Merging 'TIMSS' Advanced 1995, as .rds file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#' 
#' # Check file names
#' list.files(output,pattern = ".rds")
#' 
#' # Add school data
#' addSchools(inputdir = output, outputdir = output)
#' 
#' # Check file names
#' list.files(output,pattern = ".rds")
#'
#' @export
#' 




addSchools <- function(inputdir = getwd(),
                        outputdir = getwd(),
                        quiet = FALSE){
  
  ## inputdir
  if(!(is.vector(inputdir)&&is.character(inputdir)&&length(inputdir)==1))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a string."),call. = FALSE)
  
  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)
  
  inpfiles <- list.files(path = inputdir,pattern = ".rds$|.zsav$|.sav$",recursive = FALSE)
  
  
  
  .addSchools(inputdir = inputdir,
              inpfiles = inpfiles,
              outputdir = outputdir,
              quiet = quiet,
              save = TRUE)
  
}

.addSchools <- function(inputdir = getwd(), inpfiles, 
                        outputdir = getwd(), quiet = FALSE, save = TRUE){
  
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
  
  
  ## save
  if(!(isTRUE(save)|isFALSE(save)))
    stop(c("\nInvalid input for 'save'.",
           "\nIt should be a logical value."),call. = FALSE)
  
  # Load data ---------------------------------------------------------------
  
  
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSApops.csv"
  
  where <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(where)){
    warning(paste0("Could not read population information from 'GitHub'.",
                "\nInternal data will be used to combine respondents.",
                "\nPlease be aware, these data may not be the lastest one."),call. = FALSE)
    
    ILSApops <- utils::read.csv(file.path(system.file("extdata/ilsainfo", package = "ILSAmerge"),"ILSApops.csv"))
  }else{
    ILSApops <- where
  }
  
  
  
  
  
  # Identify populations ----------------------------------------------------
  
  ptime <- proc.time()
  
  
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
                "\nCheck availability using availableILSA(FOR = \"add.schools\")"),call. = FALSE)
  
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
  
  # rownames(bindto) <- NULL
  
  i=1
  for(i in 1:nrow(bindto)){
    
    # print(i)
    
    # Precombine students
    
    tocom <- c(bindto[i,c("STUDcbind","STUDrbind")])
    tocom <- as.vector(unlist(lapply(tocom, function(j) strsplit(j,split = ";"))))
    tocom <- setdiff(tocom,"-")
    # tocom <- c(tocom,bindto$Pop[i])
    
    
    # Combining students if needed --------------------------------------------
    if(length(tocom)>0){
      
      
      
      srch <- paste0(bindto$preName[i],"_",bindto$R1[i],".",extto[i])
      if(srch%in%list.files(outputdir)){
        
        if(!quiet){
          cat(paste0("Combined students found for ",bindto$Pop[i],".\n"))
        }
        
        addto <- readILSA(file.path(outputdir,srch))
      }else{
        if(!all(tocom%in%pop))
          stop(paste0("\nNo ILSAmerge() student files for combining found in 'inputdir'.",
                      "\nCheck your data."),call. = FALSE)
        
        if(!quiet){
          cat(paste0("Combining students for ",bindto$Pop[i],".\n"))
        }
        
        addto <- .combineStudents(inpfiles = inpfiles[pop%in%tocom|pop%in%bindto$Pop[i]],
                                  inputdir = inputdir,
                                  outputdir = outputdir,
                                  quiet = TRUE, save = FALSE)
      }
      
      
      
      
      
    }
    
    
    # Not combine students ----------------------------------------------------
    
    if(length(tocom)==0){
      addto <- readILSA(file.path(inputdir,inpto[i]))
    }
    
    
    # Add schools -------------------------------------------------------------
    
    if(!quiet){
      cat(paste0("Adding schools to ",bindto$Pop[i],", file ",i," of ",nrow(bindto),".\n"))
    }
    
    klass <- class(addto)
    schoo <- readILSA(file.path(inputdir,inpfiles[pop%in%bindto$SCHcbind[i]][1]))
    uids <- ILSApops$uID[ILSApops$Pop%in%bindto$SCHcbind[i]]
    addto <- cbind(uIDs = uID(x = addto, uID = uids), addto)
    uIDs <- uID(x = schoo, uID = uids)
    schoo <- schoo[,-which(colnames(schoo)%in%intersect(colnames(addto),colnames(schoo)))]
    schoo <- cbind(uIDs, schoo)
    
    addto <- merge(addto,schoo,by = "uIDs",all = TRUE,sort = FALSE)
    
    
    addto <- addto[,-1]
    class(addto) <- klass
    
    if(bindto$R1[i]%in%"student"){
      fname <- paste0(bindto$preName[i],"_",bindto$R1[i])
    }else{
      fname <- paste0(bindto$Name[i])
    }
    
    if(!save)
      return(addto)
    
    if(extto[i]%in%"zsav"){
      haven::write_sav(data = addto,compress = "zsav",
                       path = file.path(outputdir,
                                        paste0(fname,"&","school",".zsav")))
    }
    
    if(extto[i]%in%"sav"){
      haven::write_sav(data = addto,compress = "byte",
                       path = file.path(outputdir,
                                        paste0(fname,"&","school",".sav")))
    }
    
    if(extto[i]%in%"rds"){
      saveRDS(addto,file = file.path(outputdir,
                                     paste0(fname,"&","school",".rds")))
    }
    
  }
  
  if(!quiet){
    cat(paste0("Adding schools took ",round((proc.time()-ptime)[3]),
               " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))
  }
  
}



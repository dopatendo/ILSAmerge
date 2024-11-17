#' Add school data
#'
#' Add school data to student and teacher files merged by \code{\link{ILSAmerge}}.
#' It will run \code{\link{combineStudents}} internally.
#' To see which ILSA are available for adding school data use \code{\link{availableILSA}}.
#'
#' @param inputdir a string indicating the path were \code{\link{ILSAmerge}} files are stored.
#' @param outputdir a string indicating where the combined data will be saved.
#' @param quiet a logical value indicating if status of progress should be
#' shown. Default is \code{FALSE}.
#'
#' @returns Saves combined student data and teacher data with added school data.
#'
#' @examples
#' # Path were raw 'SPSS' files are
#' input <- system.file("extdata/timssadv", package = "ILSAmerge")
#' 
#' # Path were merged files will be saved
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




addSchools <- function(inputdir = getwd(), outputdir = getwd(), quiet = FALSE){
  # Load data ---------------------------------------------------------------
  
  
  
  where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSApops.csv"
  
  ILSApops <- suppressWarnings(try(utils::read.csv(where),silent = TRUE))
  
  if("try-error"%in%class(ILSApops)){
    stop(paste0("Could not read ILSApops file from 'GitHub'.",
                "\nPlease be sure that you are connected to the Internet.",
                "\nIf you are and this message persists, please contact the mantainer to solve this issue."),call. = FALSE)
  }
  

  
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
  
  
  i=1
  for(i in 1:nrow(bindto)){
    
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
  
  if(!quiet){
    cat(paste0("Adding schools took ",round((proc.time()-ptime)[3]),
               " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))
  }
  
}



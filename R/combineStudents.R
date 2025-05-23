#' Combine student data
#'
#' Combines achievement and background student data merged by \code{\link{ILSAmerge}}.
#' To see which ILSA are available for combining use \code{\link{availableILSA}}.
#'
#' @param inputdir a string indicating the path where \code{\link{ILSAmerge}} files are stored.
#' @param outputdir a string indicating where the combined data will be saved.
#' @inheritParams ILSAmerge
#'
#' @returns Saves combined student data produced by \code{\link{ILSAmerge}}.
#'
#' @examples
#' # Path where raw 'SPSS' files are
#' input <- system.file("extdata/timssadv", package = "ILSAmerge")
#' 
#' # Path where merged files will be saved
#' dir.create(file.path(tempdir(),"combineStudents"))
#' output <- file.path(tempdir(),"combineStudents")
#' 
#' # Merging 'TIMSS' Advanced 1995, as .rds file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "rds", quiet = FALSE)
#' 
#' # Rename files
#' ILSArename(output)
#' 
#' # Check file names
#' list.files(output,pattern = ".rds")
#' 
#' # Combine student files
#' combineStudents(inputdir = output, outputdir = output)
#' 
#' # Check file names
#' list.files(output,pattern = ".rds")
#'
#' @export


combineStudents <- function(inputdir = getwd(),
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
  
  
  .combineStudents(inputdir = inputdir,
                   inpfiles = inpfiles,
                   outputdir = outputdir,
                   quiet = quiet,
                   save = TRUE)

}



.combineStudents <- function(inputdir = inputdir,
                             inpfiles = inpfiles,
                             outputdir = getwd(),
                             quiet = FALSE,
                             save = TRUE){
  # Checks ------------------------------------------------------------------
  
  
  
  
 
  
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
  
  ## Selection of students
  ILSApops <- ILSApops[ILSApops$R1%in%"student",]
  
  inpfiles <- inpfiles[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  ext <- ext[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  pop <- pop[pop%in%ILSApops$Pop|pop%in%ILSApops$Name]
  pop <- unlist(lapply(pop, function(i) ILSApops$Pop[ILSApops$Pop%in%i|ILSApops$Name%in%i]))
  ILSApops <- ILSApops[ILSApops$Pop%in%pop,] # all student files
  ILSApops <- ILSApops[match(pop,ILSApops$Pop),] # order like pop
  bindto <- ILSApops[!(ILSApops$STUDcbind%in%"-"&ILSApops$STUDrbind%in%"-"),] # bind to which files
  popto <- pop[pop%in%bindto$Pop]
  inpto <- inpfiles[pop%in%bindto$Pop]
  extto <- ext[pop%in%bindto$Pop]
  
  
  ## Not a single file
  if(nrow(bindto)==0)
    stop(paste0("\nNo ILSAmerge() student files for combining found in 'inputdir'.",
                "\nCheck availability using availableILSA(FOR = \"combine.students\")"),call. = FALSE)
  
  ## Missing files
  necpops <- unlist(strsplit(x = bindto$STUDcbind,split = ";"))
  necpops <- c(necpops,unlist(strsplit(x = bindto$STUDrbind,split = ";")))
  necpops <- setdiff(necpops,"-")
  
  if(!all(necpops%in%pop)){
    stop(paste0("\nNot all needed student files are in 'inputdir'.",
                " Check your data."),call. = FALSE)
    
  }
  
  if(!quiet){
    cat((nrow(bindto)+length(necpops))," ILSAmerge()/ILSArename() student file(s) found. ")
    cat(paste0("Combining into ",nrow(bindto)," file(s).\n"))
  }
  
  
  
  
  # Binding -----------------------------------------------------------------
  
  
  
  i=1
  for(i in 1:nrow(bindto)){
    
    if(!quiet){
      cat(paste0("Combining student ",i," of ",nrow(bindto),".\n"))
    }
    
    ach <- .readILSA(file.path(inputdir,inpto[i]))
    klass <- class(ach)
    ach <- cbind(uID = uID(x = ach, uID = bindto$uID[i]), ach)
    
    
    cb <- strsplit(bindto$STUDcbind,split = ';')[[1]]
    rb <- strsplit(bindto$STUDrbind,split = ';')[[1]]
    
    ## cbind ----
    j=1
    for(j in 1:length(cb)){
      bgr <- .readILSA(file.path(inputdir,inpfiles[pop%in%cb[j]][1]))
      uID <- uID(x = bgr, uID = bindto$uID[i])
      bgr <- bgr[,-which(colnames(bgr)%in%intersect(colnames(ach),colnames(bgr)))]
      bgr <- cbind(uID, bgr)
      
      ach <- merge(ach,bgr,by = "uID",all = TRUE,sort = FALSE)
      
    }
    ach <- ach[,-1]
    class(ach) <- klass
    
    
    ## rbind ----
    
    if(length(rb)>0){
      one <- .readILSA(file.path(inputdir,inpfiles[pop%in%rb[1]][1]))
      
      if(length(rb)>1){
        for(j in 2:length(rb)){
          two <- .readILSA(file.path(inputdir,inpfiles[pop%in%rb[j]][1]))
          com <- intersect(colnames(one),colnames(two))
          nc1 <- setdiff(colnames(one),colnames(two))
          nc2 <- setdiff(colnames(two),colnames(one))
          
          atrc <- cbind(one[1,nc1],two[1,nc2])
          out <- rbind(one[,com],two[,com])
          
          one <- cbind(one[,nc1],matrix(NA,
                                        ncol = length(nc2),
                                        nrow = nrow(one),
                                        dimnames = list(NULL,nc2)))
          one <- rbind(one,cbind(two[,nc2],matrix(NA,
                                                  ncol = length(nc1),
                                                  nrow = nrow(two),
                                                  dimnames = list(NULL,nc1))))
          one <- rbind(atrc,one)[-1,]
          one <- cbind(out,one)
          class(one) <- class(out)
          
        }
        
      }
      
      
      uID <- uID(x = one, uID = bindto$uID[i])
      oneN <- one[,-which(colnames(one)%in%intersect(colnames(ach),colnames(one)))]
      oneN <- cbind(uID, oneN)
      
      klass <- class(ach)
      ach <- cbind(uID = uID(x = ach, uID = bindto$uID[i]), ach)
      ach <- merge(ach,oneN,by = "uID",all = TRUE,sort = FALSE)
      
      ach <- ach[,-1]
      class(ach) <- klass
      
      
      
    }
    
    
    ## save ----
    
    if(!save)
      return(ach)
    
    if(extto[i]%in%"zsav"){
      haven::write_sav(data = ach,compress = "zsav",
                       path = file.path(outputdir,
                                        paste0(bindto$preName[i],"_student",".zsav")))
    }
    
    if(extto[i]%in%"sav"){
      haven::write_sav(data = ach,compress = "byte",
                       path = file.path(outputdir,
                                        paste0(bindto$preName[i],"_student",".sav")))
    }
    
    if(extto[i]%in%"rds"){
      saveRDS(ach,file = file.path(outputdir,
                                   paste0(bindto$preName[i],"_student",".rds")))
    }
    
  }
  
  if(!quiet){
    cat(paste0("Combining students took ",round((proc.time()-ptime)[3]),
               " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))
  }
  
  
  
  
  
  
  
  
  
  
}


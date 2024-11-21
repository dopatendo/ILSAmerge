ILSApop <- readxl::read_xlsx("/Users/andreschristiansen/RandA Dropbox/Andrés Christiansen/khipuverse/ILSAmerge/build/Populations.xlsx")
ILSApop <- as.data.frame(ILSApop)
inputdir = "/Volumes/Sin título/try"
filetype = "rds"
quiet = FALSE



mergeToTeachers <- function(inputdir,filetype = "rds",quiet = FALSE){
  filedirtot <- list.files(inputdir,pattern = ".rds|.zsav|.sav")
  filedir <- filedirtot
  filetyp <- gsub(".rds|.zsav|.sav","",filedir)
  
  avaipop <- ILSApop[ILSApop$Population%in%filetyp,]
  avaipop <- avaipop[avaipop$Respondent1%in%"teacher",]
  
  if(nrow(avaipop)==0)
    stop("\nPath does not contain teacher files of available ILSA.",call. = FALSE)
  
  
  
  filedir <- filedir[filetyp%in%avaipop$Population]
  filetyp <- filetyp[filetyp%in%avaipop$Population]
  
  i=1
  for(i in 1:length(filedir)){
    sch <- subset(ILSApop,Population%in%filetyp[i])[,c("Study","Year","Grade")]
    sch <- subset(ILSApop,Study%in%sch$Study&Year%in%sch$Year&Grade%in%sch$Grade)
    sch <- subset(sch,Respondent1%in%"school")
    
    if(nrow(sch)==0){
      warning(paste0("No school files found in GitHub for ",filedir[i],"."))
    }else{
      schfile <- filedirtot[grepl(sch$Population,filedirtot)][1]
      
      if(is.na(schfile)){
        warning(paste0("No school files found in path for ",filedir[i],"."))
      }else{
        ## Load school
        if(grepl(".rds",schfile)){
          sch <- readRDS(file.path(inputdir,schfile))
        }else{
          sch <- try(haven::read_spss(file = file.path(inputdir,schfile),
                                      user_na = TRUE,
                                      col_select = NULL,
                                      skip = 0,
                                      n_max = Inf,
                                      .name_repair = "unique"),
                     silent = TRUE)
          
          if("try-error"%in%class(sch)){
            sch <- haven::read_sav(file = file.path(inputdir,schfile),
                                   user_na = TRUE,
                                   col_select = NULL,
                                   skip = 0,
                                   n_max = Inf,
                                   .name_repair = "unique",
                                   encoding = "latin1")
          }
          
        }
        
        ## Load teacher
        if(grepl(".rds",filedir[i])){
          tea <- readRDS(file.path(inputdir,filedir[i]))
        }else{
          tea <- try(haven::read_spss(file = file.path(inputdir,filedir[i]),
                                      user_na = TRUE,
                                      col_select = NULL,
                                      skip = 0,
                                      n_max = Inf,
                                      .name_repair = "unique"),
                     silent = TRUE)
          
          if("try-error"%in%class(tea)){
            tea <- haven::read_sav(file = file.path(inputdir,filedir[i]),
                                   user_na = TRUE,
                                   col_select = NULL,
                                   skip = 0,
                                   n_max = Inf,
                                   .name_repair = "unique",
                                   encoding = "latin1")
          }
          
        }
        
        ## merged teacher + school
        out <- .mschool(tea,sch)
        
        
        ## Name
        nam <- (avaipop[avaipop$Population%in%filetyp[i],c("Study","Year","Grade")])
        nam <- paste0(nam[1],nam[2],ifelse(nam[3]=='-',"",paste0("_G",nam[3])))
        nam <- paste0(nam,"_","teachers_&_schools")
        
        
        # Saving ------------------------------------------------------------------
        
        if(filetype%in%"zsav"){
          haven::write_sav(data = out,compress = "zsav",
                           path = file.path(inputdir,paste0(nam,".zsav")))
        }
        
        if(filetype%in%"sav"){
          haven::write_sav(data = out,compress = "byte",
                           path = file.path(inputdir,paste0(nam,".sav")))
        }
        
        if(filetype%in%"rds"){
          saveRDS(out,file = file.path(inputdir,paste0(nam,".rds")))
        }
        
        if(!quiet){
          message(paste0("School data merged for ",filedir[i],"."))
        }
        
        
      }
      
    }
  }

  
}










# Internal ----------------------------------------------------------------



.schID <- function(x){
  do.call(paste, c(as.data.frame(x[, c("IDCNTRY","IDSCHOOL")]), sep="_"))
}

.mschool <- function(x,y){
  X <- cbind(NEWID = .schID(x),x)
  Y <- cbind(NEWID = .schID(y),y)
  
  if(length(unique(Y[,"NEWID"]))!=nrow(Y))
    stop("asdas")
  
  Y <- Y[!colnames(Y)%in%intersect(colnames(X),colnames(Y))[-1]]
  
  
  
  neu <- merge(X,Y,by = "NEWID",all.x = TRUE)[,-1]
  class(neu) <- c("tbl_df","tbl","data.frame")
  neu
}
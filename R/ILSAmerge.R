#' Merge ILSA data
#'
#' Merges SPSS data from different International Large-Scale Assessments (ILSA),
#' including: TIMSS, TIMSS Advanced, PIRLS, ICCS, ICILS, CIVED, REDS, RLII,
#' and SITES.
#'
#' @param inputdir a string indicating the path were ILSA SPSS files are stored.
#' @param outputdir the directory where the merged data will be saved.
#' @param filetype a string indicating the type of file to be saved, it can
#' be \code{"rds"}, \code{"zsav"}, or \code{"sav"}.
#' @param population a character vector indicating which files should be merged.
#' If \code{NULL} (the default), all files will be merged. For more information
#' on available populations, run \code{ILSAfile.info()} first.
#' @param MBlimit a numerical value indicating the allowed limit of the combined
#' storage of the files of one type (see \code{ILSAfile.info()}).
#' For type files that go over the limit, files will not be merged in R,
#' but an SPSS syntax will be produced via \code{spss.syntax()}.
#' If set to \code{NULL}, no limit will be used and all files will be merged
#' within R. If speed is a problem, we recommend
#' that this number should not be over \code{200} and merge the rest in SPSS.
#' @param MBlistlimit a numerical value indicating the allowed limit of the
#' combined storage of the files of one type for merging through a list.
#' Values over the limit will be merged through a matrix, which will be slower
#' but uses less memory. Default is \code{200}.
#' @param SPSSlimit a numerical value indicating the limit of files per command
#' of SPSS, typically \code{50}.
#'
#' @returns Saves merged ILSA data or \code{.sps} syntax for merging ILSA data.
#'
#' @examples
#' \dontrun{
#' # For example, after downloading TIMSS 1995 G4 data:
#'
#' # Path were original SPSS files are stored
#' input <- 'C:/TIMSS1995_IDB_SPSS_G4/Data/'
#'
#' # Path were merged files will be saved
#' output <- 'C:/TIMSS1995_IDB_SPSS_G4/merged/'
#'
#' # Merging TIMSS 1995, as .sav file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "sav")
#'
#' # Merging TIMSS 1995, as .zsav file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "zsav")
#'
#' # Merging TIMSS 1995, as .rds file
#' ILSAmerge(inputdir = input, outputdir = output, filetype = "rds")
#'
#' # Increasing the limit for R merge
#' ILSAmerge(inputdir = input, outputdir = output, MBlimit = 200)
#'
#' # Removing the limit for R merge
#' ILSAmerge(inputdir = input, outputdir = output, MBlimit = NULL)
#'
#' # Selecting only some populations
#' ## Check which populations are available
#' ILSAfile.info(inputdir = input)
#' ## Merge
#' ILSAmerge(inputdir = input, outputdir = output, population = c("ASAm1","ASGm1"))
#' }
#'
#' @export






ILSAmerge <- function(inputdir, outputdir, population = NULL,
                      filetype = c("rds", "zsav", "sav"),
                      MBlimit = NULL, MBlistlimit = 200, SPSSlimit = 50){

  # Checks ----

  if(!is.vector(inputdir)&&!is.character(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nIt should be a character vector."),call. = FALSE)

  if(!file.exists(inputdir))
    stop(c("\nInvalid input for 'inputdir'.",
           "\nPath does not exist."),call. = FALSE)

  if(!is.vector(outputdir)&&!is.character(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nIt should be a character vector."),call. = FALSE)

  if(!file.exists(outputdir))
    stop(c("\nInvalid input for 'outputdir'.",
           "\nPath does not exist."),call. = FALSE)

  if(!is.vector(filetype)&&!is.character(filetype))
    stop(c("\nInvalid input for 'filetype'.",
           "\nIt should be a character vector."),call. = FALSE)

  if(min(filetype%in%c("rds", "zsav", "sav"))!=1)
    stop(c("\nInvalid input for 'filetype'.",
           "\nIt should be a 'rds', 'zsav', or 'sav'."),call. = FALSE)

  if(!is.vector(MBlimit)&&length(MBlimit)!=1&&is.numeric(MBlimit))
    stop(c("\nInvalid input for 'MBlimit'.",
           "\nIt should be a numeric valued."),call. = FALSE)

  if(!is.vector(MBlistlimit)&&length(MBlistlimit)!=1&&is.numeric(MBlistlimit))
    stop(c("\nInvalid input for 'MBlistlimit'.",
           "\nIt should be a numeric valued."),call. = FALSE)

  if(!is.vector(SPSSlimit)&&length(SPSSlimit)!=1&&is.numeric(SPSSlimit))
    stop(c("\nInvalid input for 'SPSSlimit'.",
           "\nIt should be a numeric valued."),call. = FALSE)

  if(SPSSlimit>50)
    warning("Be aware SPSS tipically can only work with 50 files at a time.",call. = FALSE)

  ## Process & Output ----

  ### Premerge ----

  filetype <- match.arg(filetype,c("rds", "zsav", "sav"))

  ark <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZSAV")
  erk <- list.files(path = inputdir,pattern = ".sav|.zsav|.SAV|.ZSAV",full.names = TRUE)

  pop <- substr(ark,1,3)
  cou <- substr(ark,4,6)



  ext <- lapply(ark,function(i){
    out <- substring(i,7)
    c(substr(out,1,gregexpr('\\.',out)[[1]]-1),
      substring(out,gregexpr('\\.',out)[[1]]+1))
  })

  stu <- unlist(lapply(ext,function(i) i[[1]]))
  ext <- unlist(lapply(ext,function(i) i[[2]]))



  popstu <- paste0(pop,stu)
  upopstu <- sort(unique(popstu))

  if(!is.null(population)){
    if(!is.vector(population)&&!is.character(population))
      stop(c("\nInvalid input for 'population'.",
             "\nIt should be a character vector."),call. = FALSE)


    if(min(population%in%upopstu)!=1)
      stop(c("\nInvalid input for 'population'.",
             "\nIt is not contained in the path. Check with ILSAfile.info()."),call. = FALSE)


    upopstu <- upopstu[upopstu%in%population]

  }


  cat(paste0(length(ark)," files detected. Merging into ",
               length(upopstu)," files.\n"))


  inf <- ILSAfile.info(inputdir)
  inf[,3] <- inf[,3]+0.01



  if(is.null(MBlimit)){
    MBlimit <- max(inf[,3]+1)
  }


  ### Merge ----


  ptime <- proc.time()
  for(i in 1:length(upopstu)){

    ptime2 <- proc.time()

    cat(paste0("Merging ",upopstu[i],". Type ",i," of ",length(upopstu),".\n"))

    erki <- erk[popstu%in%upopstu[i]]
    mbs <- inf[,3][inf[,1]%in%upopstu[i]]

    if(mbs<=MBlimit){

      if(mbs<=(MBlistlimit+0.01)){

        out <- try(.mergebylist(files = erki,verbose = TRUE),silent = TRUE)

        if("try-error"%in%class(out)){
          out <- .mergebymatrix(files = erki,verbose = TRUE)
        }

      }else{
        out <- .mergebymatrix(files = erki,verbose = TRUE)
      }

      if(filetype%in%"zsav"){
        haven::write_sav(data = out,compress = "zsav",
                         path = file.path(outputdir,paste0(upopstu[i],".zsav")))
      }

      if(filetype%in%"sav"){
        haven::write_sav(data = out,compress = "byte",
                         path = file.path(outputdir,paste0(upopstu[i],".sav")))
      }

      if(filetype%in%"rds"){
        saveRDS(out,file = file.path(outputdir,paste0(upopstu[i],".rds")))
      }



      cat(paste0("Merging ",upopstu[i]," took ",round((proc.time()-ptime2)[3]),
                 " seconds or ",round((proc.time()-ptime2)[3]/60,2)," minutes.\n"))

    }else{
      out <- NULL

      spss.syntax(filelist = erki, name = upopstu[i],
                  zsav = ifelse(filetype=="zsav",TRUE,FALSE),
                  outputdir = outputdir,SPSSlimit = SPSSlimit)

      cat(paste0("SPSS syntax produced for ",upopstu[i],". Type ",i," of ",
                 length(upopstu),".\n"))
    }



  }
  cat(paste0("Merging took ",round((proc.time()-ptime)[3]),
             " seconds or ",round((proc.time()-ptime)[3]/60,2)," minutes.\n"))

}



.mergebymatrix <- function(files,verbose = FALSE){
  # first file to load attributes

  if(verbose)
    cat(paste0("Merging dataset ",1," of ",length(files),".\n"))





  # colnames
  coln <- lapply(files,function(j){

    load <- try(haven::read_spss(file = j, user_na = TRUE, col_select = NULL,
                                 skip = 0, n_max = 0, .name_repair = "unique"),
                silent = TRUE)
    if("try-error"%in%class(load)){
      load <- haven::read_sav(file = j, user_na = TRUE, col_select = NULL,
                                   skip = 0, n_max = 0, .name_repair = "unique",
                              encoding = 'latin1')
    }

    colnames(load)
  })

  repcoln <- as.data.frame(table(unlist(coln)))

  # same columns, which has the most variables
  whtoload <- which.max(unlist(lapply(coln,length)))
  colN <- toupper(coln[[whtoload]])


  out1 <- try(haven::read_spss(file = files[whtoload], user_na = TRUE, col_select = NULL,
                               skip = 0, n_max = 0, .name_repair = "unique"),
              silent = TRUE)
  if("try-error"%in%class(out1)){
    out1 <- haven::read_sav(file = files[whtoload], user_na = TRUE, col_select = NULL,
                            skip = 0, n_max = 0, .name_repair = "unique",
                            encoding = 'latin1')
  }



  # count rows
  last <- lapply(1:length(files),function(j){

    load <- try(haven::read_spss(file = files[j],col_select = coln[[j]][[1]]),
                silent = TRUE)
    if("try-error"%in%class(load)){
      load <- haven::read_sav(file = files[j],col_select = coln[[j]][[1]],
                              encoding = 'latin1')
    }


    nrow(load)
  })


  # last <- lapply(files[-1],function(j){
  #   nrow(haven::read_spss(file = j,col_select = 'IDCNTRY'))
  # })

  last <- cumsum(unlist(last))
  first <- c(1,(last+1))[-(length(last)+1)]

  # fill matrix
  out <- (matrix(NA,nrow = max(last),ncol = ncol(out1)))
  colnames(out) <- toupper(colnames(out1))



  lapply(1:length(files),function(j){

    if(verbose)
      cat(paste0("Merging dataset ",j," of ",length(files),".\n"))



    unt <- try(haven::read_spss(file = files[j],
                                user_na = TRUE,
                                col_select = NULL,
                                skip = 0,
                                n_max = Inf,
                                .name_repair = "unique"),
               silent = TRUE)

    if("try-error"%in%class(unt)){
      unt <- haven::read_sav(file = files[j],
                              user_na = TRUE,
                              col_select = NULL,
                              skip = 0,
                              n_max = Inf,
                              .name_repair = "unique",
                              encoding = "latin1")
    }




    cunt <- toupper(colnames(unt))
    unt <- lapply(1:ncol(unt), function(X) {
      as.vector(unt[, X, drop = TRUE])
    })
    unt <- do.call(cbind.data.frame, unt)

    out[first[j]:last[j],match(cunt,colN)] <- unt })




  colnames(out) <- colnames(out1)


  out <- rbind(out1,out)

  return(out)

}


.mergebylist <- function(files,verbose = FALSE){



  out <- lapply(1:length(files),function(j){

    if(verbose)
      cat(paste0("Merging dataset ",j," of ",length(files),".\n"))

    outj <- haven::read_spss(file = files[j], user_na = TRUE, col_select = NULL,
                     skip = 0, n_max = Inf, .name_repair = "unique")
    colnames(outj) <- toupper(colnames(outj))
    outj
  })

  out <- do.call(rbind,out)

  return(out)

}


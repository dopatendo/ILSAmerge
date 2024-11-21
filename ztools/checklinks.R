rm(list = ls())
# needed: httr


where <- "https://raw.githubusercontent.com/dopatendo/ILSAmerge/refs/heads/main/data/ILSAlinks.csv"

ILSAlinks <- utils::read.csv(where)

statok <- 200



links <- ILSAlinks$Data_SPSS

ptm = proc.time()
isok <- lapply(1:length(links),function(i){
  hd <- httr::HEAD(links[i])
  out <- hd$status_code%in%statok
  print(paste0(i," of ",length(links)))
  return(out)
})
proc.time()-ptm


if(min(unlist(isok))==1){
  cat("All links work")
}else{
  
  isnotok <- lapply(1:length(links[!unlist(isok)]),function(i){
    hd <- httr::HEAD(links[!unlist(isok)][i])
    hd$status_code
  })
  
  cat(paste0(sum(!unlist(isok))," link(s) do(es)nt work.\n"))
  
 print( ILSAlinks[!unlist(isok),c("Name","Year","Data_SPSS")])
  
  unlist(isnotok)
}




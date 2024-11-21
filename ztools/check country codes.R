# rm(list = ls())
# setwd("/Users/andreschristiansen/Downloads/merged")
# 
# ark <- list.files()
# 
# i=1
# ccs <- vector(mode = "list",length = length(ark))
# for(i in 1:length(ark)){
#   ccs[[i]] <- as.vector(unique(readRDS(ark[i])$IDCNTRY))
#   print(i)
# }
# bb = "/Users/andreschristiansen/RandA Dropbox/Andrés Christiansen/khipuverse/ILSAmerge/data/ILSApops.csv"

pops <- read.csv(bb)

kodes <- sort(sort(as.numeric(unique(unlist(ccs)))))
sw(iso3c <- countrycode((kodes),origin = "iso3n",destination = "iso3c"))
sw(name <- countrycode((kodes),origin = "iso3n",destination = "country.name"))

out <- cbind.data.frame(IEAcode = kodes,ISO3N = iso3c,name,
                        apps = as.data.frame(table(as.numeric(unlist(ccs))))[,2])

# sin <- out[is.na(out$ISO3N),]
# con <- out[!is.na(out$ISO3N),]
sin <- out

sin0 <- sin
sin0$where <- NA
sin1 <- NULL

while(nrow(sin0)>0){
  where <- lapply(1:nrow(sin0), function(i){
   gsub(".rds","",ark[sapply(ccs,function(j) sin0[i,1]%in%j)])
  })
  
  
  mx <- table(unlist(where))
  mx <- names(which.max(mx))
  print(mx)
  sin0$where[sapply(where,function(i) mx%in%i)] <- mx
  sin1 <- rbind(sin1,sin0[!is.na(sin0$where),])
  sin0 <- sin0[is.na(sin0$where),]
}

out <- sin1
out$data <- pops$Name[match(sin1$where,pops$Pop)]
out$data[is.na(out$data)] <- "SITES1998"
rm(list = ls())
setwd("/Users/andreschristiansen/Downloads/merged")

ark="/Users/andreschristiansen/RandA Dropbox/Andrés Christiansen/khipuverse/ILSAmerge/build/ilsapops.xlsx"
pops <- as.data.frame(readxl::read_xlsx(ark))


popi <- pops[!pops$uID%in%"-",]

popi <- popi[popi$R1%in%c("school"),]


i=1
uniq <- vector("logical",nrow(popi))
for(i in 1:nrow(popi)){
  rds <- readRDS(paste0(popi$Pop[i],".rds"))
  rds <- as.data.frame(rds[,strsplit(popi$uID[i],split = ";")[[1]]])
  rds <- do.call(paste, c(rds, sep="_"))
  uniq[i] <- length(unique(rds))==length(rds)
  print(paste0(i," of ",nrow(popi)))
}
table(uniq)


# rds <- readRDS(paste0(popi$Pop[i],".rds"))
# rds <- as.data.frame(rds[,strsplit(popi$uID[i],split = ";")[[1]]])
# ids = do.call(paste, c(rds, sep="_"))
# reps = as.data.frame(table(ids))
# reps = as.character(reps[reps$Freq>1,1])
# rds <- readRDS(paste0(popi$Pop[i],".rds"))
# rds[ids%in%reps,]

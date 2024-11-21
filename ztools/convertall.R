library(ILSAmerge)

input = "/Volumes/Sin título"
output = "/Users/andreschristiansen/Downloads/merged2"





dirs <- list.files(input)

total <- 1:length(dirs)
notworking <- 0

did <- total[total>max(notworking)]
did <- setdiff(did,notworking)

# did = 40

# did=3
ptm <- proc.time()
for(i in did){
  ii <- ILSAfile.info(file.path(input,dirs[i],"Data"))
  # cual <- ii$Population[2]
  ILSAmerge(inputdir = file.path(input,dirs[i],"Data"),
            outputdir = output, 
            # population = cual,
            MBlistlimit = 300,
            MBlimit = Inf,
            quiet = FALSE,filetype = "rds",SPSSlimit = 50)
  print(dirs[i])
  print(i)
  print(proc.time()-ptm)
}




list.files(output)[!gsub(".rds","",list.files(output))%in%where$Pop]
where$Pop[!where$Pop%in%gsub(".rds","",list.files(output))]

#2120
# i=39
# inputdir = file.path(input,dirs[i],"Data")
# outputdir = output
# population = "CS_F2"
# MBlistlimit = 0
# MBlimit = Inf
# quiet = FALSE
# filetype = "rds"
# SPSSlimit = 50
# i=1

# 2757.552, 46min slow
# 

i=1
ark <- list.files(output,full.names = TRUE)
lbl <- NULL
cls <- vector("list",length(ark))
for(i in 1:length(ark)){
  inp <- (readRDS(ark[i]))
  lbli <- length(attributes(inp$IDCNTRY)$labels)
  lbl <- c(lbl,lbli)
  cls[[i]] <- class(inp)
  print(i)
}

# beepr::beep(3)

lbl

do.call(rbind,cls)

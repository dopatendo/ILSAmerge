
what <- 'main/'
# what <- 'det'

ark <- list.files("R")
erk <- list.files("R",full.names = TRUE)

res <- NULL
for(i in 1:length(ark)){

  red <- readLines(erk[i])
  res <- c(res,max(grepl(what,red))==1)
}

ark[res]


# Sacar aggregates

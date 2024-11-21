rm(list = ls())
library(ILSAmerge)
library(khipu)


user <- 2
mod <- '1.2.5'
VIG = T

setwd('/Users/andreschristiansen/RandA Dropbox/Andrés Christiansen/khipuverse')
setwd("ILSAmerge")
versions <- file.path(getwd(),"..","versions")
tests <- file.path("ztests")

# mod <- '0.8.3.5.5' # CTT for review


# Data --------------------------------------------------------------------

# ILSAcou <- as.data.frame(readxl::read_xlsx("build/ilsacou.xlsx",col_types = "text"))
# ILSAcou <- ILSAcou[ILSAcou$EXPORT%in%1,]
# ILSAcou <- ILSAcou[,c('IEAcode','N3code','Name','toLabel')]
# write.csv(ILSAcou,file = "data/ILSAcou.csv",row.names = FALSE)
# write.csv(ILSAcou,file = "inst/extdata/ilsainfo/ILSAcou.csv",row.names = FALSE)

# ILSApops <- as.data.frame(readxl::read_xlsx("build/ilsapops.xlsx",col_types = "text"))
# ILSApops <- ILSApops[,c("S1","S2","Y","G","Pop","preName","Name","R1","uID",
#                         "STUDcbind","STUDrbind","SCHcbind")]
# write.csv(ILSApops,file = "data/ILSApops.csv",row.names = FALSE)
# write.csv(ILSApops,file = "inst/extdata/ilsainfo/ILSApops.csv",row.names = FALSE)

# ILSAlinks <- as.data.frame(readxl::read_xlsx("build/ilsalinks.xlsx",col_types = "text"))
# ILSAlinks <- ILSAlinks[,c("Name","Year","Repository","Data_SPSS",
#                           "Create_folder","ZIP","combineSTU","addSCH","ILSAready")]
# # setwd("ILSAmerge")
# # usethis::use_data(ILSAlinks,internal = TRUE,overwrite = TRUE)
# write.csv(ILSAlinks,file = "data/ILSAlinks.csv",row.names = FALSE)
# # setwd("..")
# 



# Build ---------------------------------------------------------------


# save(ILSAlinks,file = "ILSAmerge/data/ILSAlinks.RData")

detach("package:ILSAmerge", unload = TRUE)
os3 <- function(iswindows = FALSE){
  sys <- Sys.info()[1]

  if(sys=='Darwin')
    ret <- 'macos'

  if(sys=='Linux')
    ret <- 'linux'

  if(sys=='Windows')
    ret <- 'windows'

  if(iswindows){
    ret <- (ret=='windows')
  }

  ret
}

# if(os3()=='windows'){setwd("D:\\RandA Dropbox\\Andrés Christiansen\\004. scaling package")}
# # if(os()=='linux'){setwd('/home/randauser01/IEA/004. scaling package/')}
#
#
# if(os3()=='linux'){setwd(paste0('/home/randauser0',user,'/RandA Dropbox/Andrés Christiansen/004. scaling package'))}


# documentation
devtools::document()

# build
devtools::build(path = versions,vignettes = VIG)

devtools::install(build_vignettes = VIG) 

install.packages(paste0(file.path(versions,"ILSAmerge_"),mod,".tar.gz"),
                 repos = NULL, type = "source")
.rs.restartR()

# aa=mini.data[,-(1:2)]

# Load --------------------------------------------------------------------
devtools::check(cran = TRUE,vignettes = VIG)



# .rs.restartR()



# Manual ------------------------------------------------------------------
devtools::check(cran = TRUE,vignettes = VIG)


devtools::build_manual(path = versions)
# devtools::build_manual(classicaltest)


devtools::check('ILSAmerge',build_args = c('--resave-data'),cran = TRUE)

devtools::check('ILSAmerge',build_args = c('--as-cran'))


library(classicaltest)
dat <- trans(minitalis,info = mininfo,items=minitem)


####

load('S_USEINF.RData')
aa=S_USEINF


# fix problems in rho handling small data with poly


#### producir frecuencias de todo y tablas de correspondencia y correlaciones
#### codigo para colapsar


# T_TEAAPPTCI_A
# no funciona porque hay una variable completamente vacia,
# el programa para cuando psych quiere hacer la correlacion


devtools::check('khiputools',build_args = c('--resave-data'))



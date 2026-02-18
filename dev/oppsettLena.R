##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nakke16c3f936f.sql.gz__20260112_082700.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")


library(nger)
source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)

rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)

AlleVarNum <- AlleVarNum(datoFra = '2025-01-01')
NgerData <- nger:: NGERRegDataSQL()

tictoc::tic('start gml')
RegDataGml <- NGERRegDataSQL(#datoFra = '2025-01-01', datoTil = Sys.Date(),
                             medPROM=1, gml=1)
tictoc::toc()
tictoc::tic('start ny')
RegDataNy <- NGERRegDataSQL(# datoFra = '2025-01-01', datoTil = Sys.Date(),
                          medPROM=1, gml=0)
tictoc::toc()

setdiff(sort(names(RegDataNy)), sort(names(RegDataGml)))
setdiff(sort(names(RegDataGml)), sort(names(RegDataNy)))

RegData <- NGERPreprosess(RegDataNy)

rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase@standardEmail')

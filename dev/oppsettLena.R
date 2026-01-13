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

RegData <- NGERRegDataSQL()
RegData <- NGERPreprosess(RegData)

rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase@standardEmail')

##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nger1702cb1d5.sql.gz__20260219_092951.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/nger1702cb1d5.sql;

library(nger)
source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)

source("dev/sysSetenv.R")
RegData <- nger::NGERRegDataSQL(datoFra = '2023-01-01', medPROM = 1)

RegData <- NGERPreprosess(RegData = RegData)
#ProsedyreGr, Prosedyrer, Diagnoser, DiagnoseGr
NGERFigAndeler(RegData=RegDataNy, valgtVar='Prosedyrer')
RegData <- NGERPreprosess(RegDataNy)


rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)



rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase@standardEmail')

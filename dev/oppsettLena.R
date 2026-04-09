##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nger136cd92e2.sql.gz__20260409_091146.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/nger136cd92e2.sql;

library(nger)
source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)

source("dev/sysSetenv.R")
RegData <- nger::NGERRegDataSQL(datoFra = '2020-01-01', medPROM = 1)
RegData <- NGERPreprosess(RegData = RegData)

unique(RegData[order(RegData$ShNavn),c("ShNavn","ReshId")])

rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)


På Rapporteket holder det med data fra 2020
rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase@standardEmail')

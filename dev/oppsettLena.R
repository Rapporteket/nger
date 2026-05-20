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


shiny::shinyApp(
  ui = nger::ui_ngerOK,
  server = nger::server_ngerOK)

source("dev/sysSetenv.R")
RegDataRaa <- nger::NGERRegDataSQL(datoFra = '2025-01-01', datoTil = '2025-12-31' ,medPROM = 0)
RegData <- NGERPreprosess(RegData = RegDataRaa)

table(RegDataRaa$LapRobotKirurgi, useNA = 'a')
table(RegData$LapRobotKirurgi, useNA = 'a')



unique(RegData[order(RegData$ShNavn),c("ShNavn","ReshId")])

rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)


rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase')

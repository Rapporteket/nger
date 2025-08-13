##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")

source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)

rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)

NgerData <- nger:: NGERRegDataSQL()
RegData <- NGERRegDataSQL()
RegData <- NGERPreprosess(RegData)
rm('RegData')


#devtools::install("../rapbase/.")
remotes::install_github('Rapporteket/rapFigurer', ref = 'lint')

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")


remotes::install_github('Rapporteket/nger', ref = 'v4.0.19')

#For å teste alle
devtools::test()

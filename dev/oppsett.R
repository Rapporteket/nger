devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)

Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nger/data-raw/config")
Sys.setenv(MYSQL_DB_DATA="NGERReportDataStaging")

nger::kjor_NGERapp()



dum <- NGERRegDataSQL(datoFra = '2022-01-01')
RegData <- NGERPreprosess(dum)
rm('RegData')

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

nordicscir::kjor_NSapper(register='nordicscir', browser = TRUE)

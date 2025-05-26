##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

nger::kjor_NGERapp(browser = TRUE)


NgerData <- nger:: NGERRegDataSQL()

sum(is.na(AlleVarNum$OpDato))
head(AlleVarNum$OpDato)

RegData <- NGERRegDataSQL(datoFra = '2024-01-01')
RegData <- NGERPreprosess(dum)
rm('RegData')


devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")



# Kevin:
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


nger::kjor_NGERapp()


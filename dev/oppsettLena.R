##############################
## Kjøring på mobilt kontor ##
##############################

# Lena :
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")
setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nger15f1192c2.sql.gz__20260728_134517.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/nger15f1192c2.sql;

library(nger)
source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)


shiny::shinyApp(
  ui = nger::ui_ngerOK,
  server = nger::server_ngerOK)

source("dev/sysSetenv.R")
RegDataRaa <- nger::NGERRegDataSQL(datoFra = '2026-01-01', datoTil = '2026-12-31' ,medPROM = 1)
RegData <- NGERPreprosess(RegData = RegDataRaa)

NGERFigAndelerGrVar(RegData=NGERData, valgtVar='KomplIntra', preprosess=0,
                    reshID=reshID, outfile = '')
print(p)
knitr::knit2pdf('../inst/NGERmndRapp.Rnw')

NGERFigFordeling(RegData = RegData, valgtVar='Opf0hvor', preprosess = 0)

AndelerShus <-
  NGERFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar='PREMSnakke')
tabAndelerShus <- cbind(Antall=AndelerShus$Ngr,
                        Andeler = AndelerShus$AggVerdier$Hoved)

NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar='PREMSnakke',
                                   #OpMetode=OpMetode,
                                   figurtype='andelTid')
NGERVarSpes$varTxt
fig <- NGERFigAndelTid(RegData = RegData, valgtVar = 'PREMSnakke', tidsenhet = 'Mnd')
plot(fig)


table(RegDataRaa$LapRobotKirurgi, useNA = 'a')
table(RegData$LapRobotKirurgi, useNA = 'a')



unique(RegData[order(RegData$ShNavn),c("ShNavn","ReshId")])

rapbase::runAutoReport(group = "nger",
                       dato = Sys.Date()+1, dryRun = TRUE)


rm('RegData')

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)


remotes::install_github('Rapporteket/rapbase')


#--------------------------------------SamleRAPPORTER-----------------------------------

rm(list=ls())
library(nger)
library(knitr)
library(tools)
NGERAlleVarNum <- read.table('C:/Registre/NGER/data/AlleVarNum2016-06-08.csv', sep=';', header=T, encoding = 'UTF-8') #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-06-08.csv', sep=';', header=T, encoding = 'UTF-8')
NGERRegData <- merge(NGERForlop, NGERAlleVarNum, by.x = "ForlopsID", by.y = "MCEID", all = FALSE)

reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
setwd('C:/ResultattjenesteGIT/nger/inst/')
#knit('NGERmonthlyReport.Rnw')
#tools:: texi2pdf('NGERmonthlyReport.tex')
knit('NGERSamleRapp.Rnw', encoding = 'UTF-8')
tools::texi2pdf('NGERSamleRapp.tex')


#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------
rm(list=ls())
library(nger)
#NGERData <- read.table('C:/Registre/NGER/data/NGER2015-03-03NyeNavn.csv', sep=';', header=T) #,
NGERAlleVarNum <- read.table('C:/Registre/NGER/data/alleVarNum2016-08-22.csv', sep=';', header=T) #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-08-22.csv', sep=';', header=T)
NGERData <- merge(NGERForlop, NGERAlleVarNum, by = "ForlopsID", suffixes = c('','xx'), all = FALSE)
RegData <- NGERData
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- TRUE
OpMetode <- 99
Hastegrad <- ''
AlvorlighetKompl <- ''#c('2','3')
hentData <- 0
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'Alder'	#Må velge... Alder, Education, FollowupSeriousness, HysCompleteness, KomplPost, KomplPostUtd, KomplReopUtd,
        #KomplHys, LapIntraAbdominal, KomplLap, LapComplications
        #LapAccessMethod, LapEkstrautstyr,LapNumHjelpeinnstikk
        #MaritalStatus, OpMetodeOpAnesthetic, PatientNorwegian, OpAnesthetic
				#, OpASA,
		#OpBMICategory, Opcat, OpDaySurgery, OpEarlierVaginal, OpEarlierLaparoscopy, OpEarlierLaparatomy,
		#OpOpcatOutsideDaytime, OpType

outfile <- paste(valgtVar, '_ford.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")

NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, OpMetode = OpMetode,
  minald=minald, maxald=maxald, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)


#Teste variable
variable <- c('Alder', 'Education', 'FollowupSeriousness', 'HysCompleteness', 'HysComplications',
              'KomplPost', 'KomplPostUtd', 'KomplReopUtd', 'LapAccessMethod', 'LapComplications',
              'LapEkstrautstyr', 'LapIntraAbdominal', 'LapNumHjelpeinnstikk',
              'MaritalStatus', 'OpMetode', 'PatientNorwegian', 'OpAnesthetic', 'OpASA',
              'OpBMICategory', 'Opcat', 'OpDaySurgery', 'OpEarlierVaginal', 'OpEarlierLaparoscopy',
              'OpEarlierLaparatomy', 'OpOpcatOutsideDaytime', 'OpType')

KomplLapIntraOp

for (valgtVar in variable) {
	outfile <- paste(valgtVar, '_ford.png', sep='')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
		minald=minald, maxald=maxald)
}

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------
rm(list=ls())
library(nger)
#NGERData <- read.table('C:/Registre/NGER/data/NGER2015-03-03NyeNavn.csv', sep=';', header=T) #,
NGERAlleVarNum <- read.table('C:/Registre/NGER/data/alleVarNum2016-06-08.csv', sep=';', header=T) #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-06-08.csv', sep=';', header=T)
NGERData <- merge(NGERForlop, NGERAlleVarNum, by.x = "ForlopsID", by.y = "MCEID", all = FALSE)
RegData <- NGERData
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-02-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- Sys.Date() #'2016-03-01'
preprosess <- TRUE
hentData <- 0
OpMetode <- 99
tidsenhet <- 'Aar'
Hastegrad <- ''
AlvorlighetKompl <- ''
enhetsUtvalg <- 1 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'StatusFollowup' #
outfile <- paste(valgtVar, '_', tidsenhet, '.png', sep='')

NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
            minald=minald, maxald=maxald, OpMetode=OpMetode, Hastegrad=Hastegrad,
            AlvorlighetKompl=AlvorlighetKompl, tidsenhet=tidsenhet, preprosess=1)



#Teste variable
variable <- c('Alder', 'ComplAfterBleed', 'ComplEquipment', 'ComplInfection', 'ComplOrgan', 'ComplReop',
              'FollowupSeriousness', 'KomplIntra', 'KomplPostop', 'OpAntibioticProphylaxis',
              'OpASA', 'OpBMI', 'StatusFollowup')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.png')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, OpMetode=OpMetode, Hastegrad=Hastegrad,
              AlvorlighetKompl=AlvorlighetKompl, tidsenhet=tidsenhet, preprosess=TRUE)
}


#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------
rm(list=ls())
library(nger)
#NGERData <- read.table('C:/Registre/NGER/data/NGER2015-03-03NyeNavn.csv', sep=';', header=T) #,
NGERAlleVarNum <- read.table('C:/Registre/NGER/data/AlleVarNum2016-02-17.csv', sep=';', header=T) #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-02-17.csv', sep=';', header=T)
NGERData <- merge(NGERForlop, NGERAlleVarNum, by.x = "ForlopsID", by.y = "MCEID", all = FALSE)
RegData <- NGERData
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-02-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-10-01'
preprosess <- TRUE
if (preprosess){RegData <- NGERPreprosess(RegData=RegData, reshID=reshID)}

OpMetode <- 1
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'OpBMI'	#Må velge... Alder, ComplReop, Education, FollowupSeriousness, KomplIntra, KomplPostop,
      #OpAntibioticProphylaxis, OpASA, OpBMI, StatusFollowup


outfile <- paste(valgtVar, '_Shus.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")


NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, OpMetode=OpMetode,
            minald=minald, maxald=maxald, Hastegrad = Hastegrad)


#Teste variable
variable <- c('Alder', 'ComplReop', 'Education', 'FollowupSeriousness', 'KomplIntra', 'KomplPostop',
      'OpAntibioticProphylaxis', 'OpASA', 'OpBMI', 'StatusFollowup')

for (valgtVar in variable) {
  outfile <- paste(valgtVar, '_Shus.png', sep='')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald)
}


######################### LITT LEKING ##############################
setwd("C:/ResultattjenesteGIT/nger/")

NGERAlleVarNum <- read.table('C:/Registre/NGER/data/AlleVarNum2016-06-08.csv', sep=';', header=T, encoding = 'UTF-8') #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-06-08.csv', sep=';', header=T, encoding = 'UTF-8')
NGERRegData <- merge(NGERForlop, NGERAlleVarNum, by.x = "ForlopsID", by.y = "MCEID", all = FALSE)
RegData <- NGERRegData

data.frame(lapply(df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)
chartr("a-cX", "D-Fw", x)
tolower(x)
toupper(x)

ProcHys <- c('HysProc1', 'HysProc2', 'HysProc3')
ProcLap <- c('LapProc1', 'LapProc2', 'LapProc3')
DiagLap <- c('LapDiag1', 'LapDiag2', 'LapDiag3')
DiagHysLap <- c('HysDiag1', 'HysDiag2', 'HysDiag3')

table(as.vector(RegData[,c(ProcHys, ProcLap)]))
Alleproc <- table(toupper(as.vector(as.matrix(RegData[,c(ProcHys, ProcLap)]))))
sort(Alleproc, decreasing = TRUE)[1:20]
a <- names(Alleproc)










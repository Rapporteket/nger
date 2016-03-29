
#--------------------------------------SAMLERAPPORT-----------------------------------

rm(list=ls())
library(knitr)
setwd('C:/Registre/Hjerneslag/trunk/RSamleDok')

SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21.csv', sep=';', header=T) #HjerneSlag2014-04-07
RegData <- SlagData
reshID <- 106340 #StOlav: 106340, Harstad sykehus: 700741, Narvik sykehus: 700742, Tromsø sykehus: 601159

libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
libkatTex <- libkat

source("../RAndeler/SlagFigAndeler.R", encoding="UTF-8")

knit('SlagSamleDok.Rnw')

#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------
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
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- TRUE
MCEType <- 99
Hastegrad <- ''
AlvorlighetKompl <- ''
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'LapComplications'	#Må velge... Alder, Education, FollowupSeriousness, HypCompleteness, KomplPost, KomplPostUtd, KomplReopUtd,
        #KomplHyp, LapIntraAbdominal, KomplLap, LapComplications
        #LapAccessMethod, LapEkstrautstyr,LapNumHjelpeinnstikk
        #MaritalStatus, MCETypeOpAnesthetic, PatientNorwegian, OpAnesthetic
				#, OpASA,
		#OpBMICategory, Opcat, OpDaySurgery, OpEarlierVaginal, OpEarlierLaparoscopy, OpEarlierLaparatomy,
		#OpOpcatOutsideDaytime, OpType

outfile <- paste(valgtVar, '_ford.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")


FigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, MCEType = MCEType,
  minald=minald, maxald=maxald, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)


#Teste variable
variable <- c('Alder', 'Education', 'FollowupSeriousness', 'HypCompleteness', 'HypComplications',
              'KomplPost', 'KomplPostUtd', 'KomplReopUtd', 'LapAccessMethod', 'LapComplications',
              'LapEkstrautstyr', 'LapIntraAbdominal', 'LapNumHjelpeinnstikk',
              'MaritalStatus', 'MCEType', 'PatientNorwegian', 'OpAnesthetic', 'OpASA',
              'OpBMICategory', 'Opcat', 'OpDaySurgery', 'OpEarlierVaginal', 'OpEarlierLaparoscopy',
              'OpEarlierLaparatomy', 'OpOpcatOutsideDaytime', 'OpType')

KomplLapIntraOp

for (valgtVar in variable) {
	outfile <- paste(valgtVar, '_ford.png', sep='')
	FigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
		minald=minald, maxald=maxald)
}

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------
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
datoTil <- '2016-03-01'
preprosess <- TRUE
if (preprosess){RegData <- NGERPreprosess(RegData=RegData, reshID=reshID)}

MCEType <- 1
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten


#Teste variable
variable <- c('Alder', 'KomplPostop', 'FollowupSeriousness', 'KomplIntra', 'OpAntibioticProphylaxis',
      'OpBMI', 'Reop', 'StatusFollowup')

for (valgtVar in variable) {
  outfile <- paste(valgtVar, '_Aar.png', sep='')
  FigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
             reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
             minald=minald, maxald=maxald)
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

MCEType <- 1
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'OpBMI'	#Må velge... Alder, ComplReop, Education, FollowupSeriousness, KomplIntra, KomplPostop,
      #OpAntibioticProphylaxis, OpASA, OpBMI, StatusFollowup


outfile <- paste(valgtVar, '_Shus.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")


FigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, MCEType=MCEType,
            minald=minald, maxald=maxald, Hastegrad = Hastegrad)


#Teste variable
variable <- c('Alder', 'ComplReop', 'Education', 'FollowupSeriousness', 'KomplIntra', 'KomplPostop',
      'OpAntibioticProphylaxis', 'OpASA', 'OpBMI', 'StatusFollowup')

for (valgtVar in variable) {
  outfile <- paste(valgtVar, '_Shus.png', sep='')
  FigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald)
}

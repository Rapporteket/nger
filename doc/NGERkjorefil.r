
#--------------------------------------SamleRAPPORTER-----------------------------------

rm(list=ls())
library(nger)
library(knitr)
library(tools)
library(plyr)

reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
setwd('C:/ResultattjenesteGIT/nger/inst/')
#knitr::knit('NGERSamleRapp.Rnw', encoding = 'UTF-8')
#tools::texi2pdf('NGERSamleRapp.tex')

knit('NGERmonthlyReport.Rnw')
tools:: texi2pdf('NGERmonthlyReport.tex')

#--------------------------------Datakobling--------------------------
#Vil "snart" endre spørringa slik at det i hvert tilfelle spørres etter de variablene man trenger.

rm(list=ls())
NGERBasis <- read.table('C:/Registre/NGER/data/AlleVarNum2017-02-15.csv', sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2017-02-15.csv', sep=';', header=T, fileEncoding = 'UTF-8')
#NGEROppf <- read.table('C:/Registre/NGER/data/FollowupsNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
#NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('','xx'), all = FALSE)
#NGERData <- merge(NGERData, NGEROppf, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE)
NGERData <- merge(NGERBasis, NGERForlop, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE, all.y=FALSE)
#write.table(NGERData, file = "NGERData.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
RegData <- NGERData

library(nger)
#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'
preprosess <- 1
MCEType <- ''
Hastegrad <- ''
AlvorlighetKompl <- ''#c('2','3')
hentData <- 0
enhetsUtvalg <- 1 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'OpTid'	#Må velge... Alder,... Diagnoser, Prosedyrer, NY: OpTid

outfile <- paste0(valgtVar, '_ford.png')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")

NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, MCEType = MCEType, outfile=outfile, preprosess = preprosess,
  minald=minald, maxald=maxald, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)


#Teste variable
variable <- c('Alder','Diagnoser',  'HysGjforingsGrad', 'HysKomplikasjoner','KomplPost',
              'KomplPostUtd', 'KomplReopUtd', 'LapEkstrautstyr', 'LapKomplikasjoner','LapTeknikk',
               'LapIntraabdominell', 'LapNumHjelpeinnstikk',
              'Sivilstatus', 'OpMetode', 'Norsktalende', 'OpAnestesi', 'OpASA',
              'OpBMI', 'OpKategori', 'OpDagkirurgi','Opf0AlvorlighetsGrad',
              'OpTidlVagInngrep', 'OpTidlLapsko',
              'OpTidlLaparotomi', 'OpIVaktTid', 'OpType', 'Prosedyrer', 'Utdanning')

for (valgtVar in variable) {
	outfile <- paste(valgtVar, '_ford.png', sep='')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,preprosess = 1,
		minald=minald, maxald=maxald)
}

#Opf0KomplUtstyr - LapAdherProfylakse

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-02-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- Sys.Date() #'2016-03-01'
preprosess <- 1
hentData <- 0
MCEType <- 1
tidsenhet <- 'Aar'
Hastegrad <- ''
AlvorlighetKompl <- ''
enhetsUtvalg <- 1 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'KomplIntra' #
outfile <- paste(valgtVar, '_', tidsenhet, '.png', sep='')

NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
            minald=minald, maxald=maxald, MCEType=MCEType, Hastegrad=Hastegrad,
            AlvorlighetKompl=AlvorlighetKompl, preprosess=1) #



#Teste variable
variable <- c('Alder', 'Opf0KomplBlodning', 'Opf0KomplUtstyr', 'Opf0KomplInfeksjon', 'Opf0KomplOrgan',
              'Opf0Reoperasjon','Opf0AlvorlighetsGrad', 'KomplIntra', 'KomplPostop', 'OpAntibProfylakse',
              'OpASA', 'OpBMI', 'Opf0Status')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.png')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, MCEType=MCEType, Hastegrad=Hastegrad,
              AlvorlighetKompl=AlvorlighetKompl, tidsenhet=tidsenhet, preprosess=TRUE)
}


#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-02-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-10-01'
preprosess <- 1
Hastegrad <- ''
#if (preprosess){RegData <- NGERPreprosess(RegData=RegData, reshID=reshID)}

MCEType <- ''
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'Alder'	#Må velge... Alder, Opf0Reoperasjon, Education, Opf0AlvorlighetsGrad, KomplIntra, KomplPostop,
      #OpAntibProfylakse, OpASA, OpBMI, Opf0Status


outfile <- paste(valgtVar, '_Shus.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/nger/")


NGERFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, MCEType=MCEType,
            minald=minald, maxald=maxald, Hastegrad = Hastegrad, preprosess = 1)


#Teste variable
variable <- c('Alder', 'KomplIntra', 'KomplPostop', 'Opf0Reoperasjon', 'Opf0AlvorlighetsGrad',
              'OpAntibProfylakse', 'OpASA', 'OpBMI', 'Opf0Status', 'Utdanning')

for (valgtVar in variable) {
  outfile <- paste(valgtVar, '_Shus.png', sep='')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, preprosess = 1)
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










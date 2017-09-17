
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
dato <- '2017-09-11'
NGERBasis <- read.table(paste0('A:/NGER/AlleVarNum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table(paste0('A:/NGER/ForlopsOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
#NGEROppf <- read.table('C:/Registre/NGER/data/FollowupsNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
#NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('','xx'), all = FALSE)
#NGERData <- merge(NGERData, NGEROppf, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE)
NGERData <- merge(NGERBasis, NGERForlop, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE, all.y=FALSE)
#write.table(NGERData, file = "NGERData.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
RegData <- NGERData

library(nger)
#-----------------------------------PARAMETRE------------------------------
setwd("C:/ResultattjenesteGIT/nger/")
# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2016-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- 1
MCEType <- ''
Hastegrad <- ''
AlvorlighetKompl <- ''#c('2','3')
hentData <- 0
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten

tidsenhet <- 'Aar'
grVar <- 'ShNavn'
Ngrense <- 10
valgtMaal <- 'Gjsn'

#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------


valgtVar <- 'R0ScoreGeneral'	#Må velge... Alder,... Diagnoser, Prosedyrer, OpTid
                    #Tss2Mott	Tss2Behandling	Tss2Lytte	Tss2Behandlere	Tss2Enighet	Tss2Generelt
                    #R0ScorePhys,	R0ScoreRoleLmtPhy,	R0ScoreRoleLmtEmo,	R0ScoreEnergy,	R0ScoreEmo,
                    #R0ScoreSosial,	R0ScorePain,	R0ScoreGeneral

outfile <- '' #paste0(valgtVar, '_ford.png')	#Navn angis av Jasper

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

variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.png')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,preprosess = 1,
		minald=minald, maxald=maxald)
}

#Opf0KomplUtstyr - LapAdherProfylakse

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------
valgtVar <- 'KomplPostop' #
outfile <- paste0(valgtVar, '_', tidsenhet, '.png')

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
valgtVar <- 'Tss2Generelt' #Må velge... Alder, Opf0Reoperasjon, Education, Opf0AlvorlighetsGrad,
      #KomplIntra, KomplPostop, OpAntibProfylakse, OpASA, OpBMI, Opf0Status,
      #Tss2Mott, Tss2Behandling,	Tss2Lytte, Tss2Behandlere, Tss2Enighet,	Tss2Generelt

outfile <- '' #paste0(valgtVar, '_Shus.png')	#Navn angis av Jasper

NGERFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, MCEType=MCEType,
            minald=minald, maxald=maxald, Hastegrad = Hastegrad, preprosess = 1)


#Teste variable
variable <- c('Alder', 'KomplIntra', 'KomplPostop', 'Opf0Reoperasjon', 'Opf0AlvorlighetsGrad',
              'OpAntibProfylakse', 'OpASA', 'OpBMI', 'Opf0Status', 'Utdanning')
variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.png')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, preprosess = 1)
}


#------------------------------ Sentralmål per sykehus --------------------------
#------------------------------ (GjsnGrVar) --------------------------

valgtVar <- 'R0ScoreGeneral'	#Må velge... R0ScorePhys,	R0ScoreRoleLmtPhy,	R0ScoreRoleLmtEmo,	R0ScoreEnergy,
                            #R0ScoreEmo, R0ScoreSosial,	R0ScorePain,	R0ScoreGeneral

outfile <- '' #paste0(valgtVar, '_ford.png')	#Navn angis av Jasper


NGERFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                             MCEType=MCEType, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad,
                             valgtMaal='Gjsn', hentData=0, preprosess=1, grVar=grVar, Ngrense=10,
                             medKI=1, reshID=0, outfile='')

variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_ShusMed.png')
  NGERFigGjsnGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                      valgtMaal='Med',outfile=outfile)
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










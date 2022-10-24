#Hente inn fra kodebok:
library(tidyverse)
klokebok = read_delim(mappeplassering_til_klokebok)
kodebok_kategorier = klokebok %>% filter(type == 'Listevariabel')

#"Opf0UtstyrInstrumenter", "Opf0UtstyrNett" og "Opf0UtstyrSutur"

#--------------------------------------SamleRAPPORTER-----------------------------------
TEST
rm(list=ls())
library(nger)
library(knitr)
library(tools)
library(dplyr)
library(lubridate)

dev.off()
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
#reshID <- 8
datoFra <- '2019-01-01'
datoTil <- Sys.Date()
sluttDato <- datoTil
#setwd('C:/ResultattjenesteGIT/nger/inst/')
setwd('~/nger/inst/')
data('NGERtulledata', package = 'nger')
load('A:/NGER/NGER2019-09-03.Rdata')
RegData <- NGERPreprosess(RegData=RegData) #I App'en preprosesseres data

src <- normalizePath(system.file('NGERSamleRapp.Rnw', package='nger'))
knitr::knit(src <- normalizePath(system.file('NGERSamleRapp.Rnw', package='nger')))
knitr::knit('NGERSamleRapp.Rnw', encoding = 'UTF-8')
tools::texi2pdf('NGERSamleRapp.tex')

knitr::knit2pdf('NGERmndRapp.Rnw')

#--Vil undersøke variabelen Opf0Status nærmere
RegData <- NGERPreprosess(NGERRegDataSQL(datoFra = '2021-01-01', datoTil = '2021-10-31'))

table(RegData$Opf0Status, useNA = 'a')
#Alle med Opf0Status=NA har også NA for 'Opf0metode', 'Opf0BesvarteProm', 'Opf0Komplikasjoner'
RegData[is.na(RegData$Opf0Status), c('Opf0metode', 'Opf0BesvarteProm', 'Opf0Komplikasjoner')]
prop.table(table(RegData$Opf0Komplikasjoner, useNA = 'a'))
prop.table(table(RegData$Opf0InfOpSaar, useNA = 'a'))

table(RegData[which(RegData$Opf0Status==1 & RegData$Opf0metode %in% 1:2), c('Opf0Komplikasjoner', 'Opf0BesvarteProm')], useNA = 'a')

addmargins(table(RegData[which(RegData$Opf0Status==1), c('Opf0metode', 'Opf0BesvarteProm')], useNA = 'a'))
addmargins(table(RegData[which(RegData$Opf0Status==1), c('Opf0metode', 'Opf0Komplikasjoner')], useNA = 'a'))
addmargins(table(RegData[which(RegData$Opf0Status==1), c('Opf0metode', 'Opf0InfOpSaar')], useNA = 'a'))


table(RegData$Opf0metode, useNA = 'a')

sum(RegData$Opf0metode %in% 1:2 | (RegData$Opf0metode == 3 & RegData$Opf0BesvarteProm==1), na.rm = T)
prop.table(table(RegData$Opf0metode %in% 1:2 | (RegData$Opf0metode == 3 & RegData$Opf0BesvarteProm==1), useNA = 'a'))
sum(RegData$Opf0Komplikasjoner %in% 0:1)

RegData$Variabel <- 0
RegData$Variabel[RegData$Opf0metode %in% 1:3 ] <- 1 # Må fjerne de som ikke har svart på PROM, 950 8364
RegData$Variabel[RegData$Opf0Komplikasjoner %in% 0:1] <- 1 #  0 1: 2333 6981
table(RegData$Variabel)

table(RegData[,c('Opf0Komplikasjoner', 'Opf0metode', 'Opf0Status')], useNA = 'a')
# , , Opf0Status = 1
# Opf0Komplikasjoner / Opf0metode
#         1    2    3    9 <NA>
# 0    1681   38 4964    0    0
# 1      78    2  218    0    0
# <NA>    0    0 1383  781    0
#NB: Mulig ikke bør bruke Komplikasjoner for å sjekke...
table(RegData[which(is.na(RegData$Opf0Status)), c('Opf0metode', 'Aar')], useNA = 'a')

#--------------------------------Datakobling--------------------------

rm(list=ls())
dato <- '2019-09-03'
#dato <- '2019-03-18Aarsrapp18'
NGERBasis <- read.table(paste0('A:/NGER/AlleVarNum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table(paste0('A:/NGER/ForlopsOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
NGERSkjema <- read.table(paste0('A:/NGER/SkjemaOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
#NGEROppf <- read.table('C:/Registre/NGER/data/FollowupsNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
#NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('','xx'), all = FALSE)
#NGERData <- merge(NGERData, NGEROppf, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE)
NGERData <- merge(NGERBasis, NGERForlop, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE, all.y=FALSE)
#write.table(NGERData, file = paste0("NGER", dato), row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
 RegData <- NGERData
# RegData <- NGERPreprosess(NGERData)
# RegData <- NGERUtvalgEnh(RegData, datoFra = '2016-01-01', datoTil = '2018-12-31')$RegData
# save(RegData, file=paste0('A:/NGER/Aarsrapp2018', dato, '.Rdata'))
# write.table(RegData, file = "A:/NGER/Aarsrapp2018.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
SkjemaOversikt <- NGERSkjema
save(RegData, SkjemaOversikt, file=paste0('A:/NGER/NGER', dato, '.RData'))
save(RegData, SkjemaOversikt, file=paste0('A:/NGER/NGERtestdata.RData'))
save(NGERData, file=paste0('A:/NGER/Aarsrapp', dato, '.RData'))

RegData <- NGERData[which(NGERData$OpDato >= as.Date('2016-01-01')) & which(NGERData$OpDato <= as.Date('2018-12-31')),]
load(paste0('A:/NGER/NGER', dato, '.Rdata'))
load(paste0('A:/NGER/Aarsrapp20182019-03-18.Rdata'))


library(nger)
#----------------------------------- Lage tulledata ------------------------------
#Denne inneholder ingen id'er og trenger ikke
SkjemaOversikt <- NGERSkjema[ ,c("Skjemanavn", "SkjemaRekkeflg",  "SkjemaStatus",  "OpprettetDato", "HovedDato")]

SkjemaOversikt<- lageTulleData(RegData=SkjemaOversikt, antSh=26, antObs=20000)

varBort <- c('PasRegDato', 'PersonNr', 'PersonNrType', 'FodselsDato', 'Morsmaal', 'MorsmaalAnnet',
           'Tss2ScoreAVG', 'AvdRESHYY', 'ShNavn', 'PasientIDYY', 'PostNr', 'PostSted', 'Kommune', 'Kommunenr',
           'Fylke', 'Fylkenr', 'KryptertFnr', 'Fodselsdato', 'NorsktalendeYY', 'SivilStatusYY', 'UtdanningSSB',
           'AvdodYY', 'InnDato', 'Mnd', 'Kvartal', 'Halvaar', 'Aar',
           'SykehusNavn', 'AvdRESH',
           grep('Opf1', names(RegData)), grep('RY1', names(RegData)))
RegData <- lageTulleData(RegData=RegData, varBort=varBort, antSh=26, antObs=20000)

save(SkjemaOversikt, RegData, file = 'A:/NGER/NGERtulledata.Rdata')

#----------------------------------- PARAMETRE ------------------------------
RegData <- NGERRegDataSQL()
RegData <- NGERPreprosess(NGERRegDataSQL())

setwd("C:/ResultattjenesteGIT/nger/")
# Inndata til funksjon:
reshID <- 8 #110734 # 110734 (Tønsberg)  	700399
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2018-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2018-12-31'
preprosess <- 1
OpMetode <- 99 #1: Laparoskopi, 2: Hysteroskopi, 3: Begge,  99: Alle
#'                 4: LCD01 eller LCD04 (total laparoskopisk hysterektomi)
#'                 5: LCC11 (laparoskopisk subtotal hysterektomi)
#'                 6: LCD11 (laparoskopisk assistert vaginal hysterektomi)
velgDiag <- 0
dagkir <- 9
Hastegrad <- ''
AlvorlighetKompl <- ''#c('2','3')
hentData <- 0
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten

tidsenhet <- 'Mnd'
grVar <- 'ShNavn'
Ngrense <- 10
valgtMaal <- 'med'
velgAvd <- 0
outfile <- ''

RegDatatest <- NGERPreprosess(RegDataAlle)
RegData <-NGERUtvalgEnh(RegDatatest, datoFra = '2019-08-01')$RegData
setdiff(sort(names(RegDataAlle)), sort(names(RegData)))

tapply(RegDatatest$R0Spm2, RegDatatest$Aar, FUN = 'mean', na.rm=T)
tapply(RegDatatest$R0Spm2, RegDatatest$Aar, FUN = 'length')
tapply(RegDatatest$R0Spm9d, RegDatatest$Aar, FUN = function(x){sum(!is.na(x))})
tapply(RegDatatest$RY1Spm4c, RegDatatest$Aar, FUN = function(x){sum(!is.na(x))})

#------------------------------ Andeler flere var (tilsvarer Fordelinger)--------------------------

valgtVar <- 'Alder'	#Må velge... Alder,... Diagnoser, Prosedyrer, , Opf0metode, OpTid, Tss2Sumskaar

outfile <- '' #paste0(valgtVar, '_ford.png')
enhetsUtvalg <- 1


UtDataFraFig <- NGERFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, OpMetode = OpMetode, outfile=outfile, preprosess = preprosess,
  minald=minald, maxald=maxald, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad, velgDiag = velgDiag,
	velgAvd = velgAvd)

ind <- which(RegData$InnDato<as.Date('2017-01-01') & RegData$InnDato>as.Date('2015-12-31'))

#Teste variable
variable <- c('Alder','HysGjforingsGrad', 'HysKomplikasjoner','KomplPostopType',
              'KomplPostUtd', 'KomplReopUtd', 'LapEkstrautstyr', 'LapKomplikasjoner','LapTeknikk',
              'LapIntraabdominell', 'LapNumHjelpeinnstikk',
              'SivilStatus', 'Opf0metode', 'OpMetode', 'Norsktalende', 'OpAnestesi', 'OpASA',
              'OpBMI', 'OpKategori', 'OpDagkirurgi','Opf0AlvorlighetsGrad',
              'OpTidlVagInngrep', 'OpTidlLapsko',
              'OpTidlLaparotomi', 'OpIVaktTid', 'OpMetode', 'Prosedyrer', 'RegForsinkelse', 'Utdanning')

variable <- c('Utdanning','Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.png')
	NGERFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,preprosess = 1)
}

#Opf0KomplUtstyr - LapAdherProfylakse


#------------------------------ Andeler per tidsenhet (AndelTid)  --------------------------
valgtVar <- 'Alder' #LapKonvertert, OpAnestesi, OpDagkirurgi, OpTid
outfile <- '' #paste0(valgtVar, '_', tidsenhet, '.png')

test <- NGERFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, enhetsUtvalg=1, tidsenhet = tidsenhet,
            #minald=minald, maxald=maxald, OpMetode=OpMetode, Hastegrad=Hastegrad, AlvorlighetKompl=AlvorlighetKompl,
            outfile=outfile) #


#Teste variable
variable <- c('Alder', 'LapKonvertert', 'OpDagkirurgi', 'Opf0KomplBlodning', 'Opf0KomplUtstyr',
              'Opf0KomplInfeksjon', 'Opf0KomplOrgan',
              'Opf0Reoperasjon','Opf0AlvorlighetsGrad', 'KomplIntra', 'KomplPostop', 'OpAntibProfylakse',
              'OpASA', 'OpBMI', 'Opf0Status')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.png')
  NGERFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=1, outfile=outfile,
              minald=minald, maxald=maxald, OpMetode=OpMetode, Hastegrad=Hastegrad,
              AlvorlighetKompl=AlvorlighetKompl, tidsenhet=tidsenhet, preprosess=TRUE)
}



#------------------------------ Andeler per sykehus (AndelGrVar) --------------------------
 #Ok?: Opf0KomplOrgan, Opf0Status, RegForsinkelse, Tss2Mott
load('A:/NGER/Aarsrapp2018_2019-08-05.Rdata')
RegData <- NGERData

valgtVar <- 'KomplIntra'  #Opf0AlvorlighetsGrad #Må velge... OpAnestesi, OpDagkirurgi,OpTid
      #Alder, Opf0Reoperasjon, Education, Opf0AlvorlighetsGrad,
      #KomplIntra, KomplPostop, OpAntibProfylakse, OpASA, OpBMI, Opf0Status, RegForsinkelse
      #Tss2Mott, Tss2Behandling,	Tss2Lytte, Tss2Behandlere, Tss2Enighet,	Tss2Generelt

outfile <- '' #paste0(valgtVar, '_Shus.png')	#Navn angis av Jasper
#velgAvd <- '' #c(108048, 111180, 700404)

UtDataFraFig <-NGERFigAndelerGrVar(RegData=NGERRegDataSQL(),  datoFra='2018-01-01', #datoTil = '2018-12-31',
                                   valgtVar='Opf0AlvorlighetsGrad',  outfile='',OpMetode = 1)

NGERFigAndelerGrVar(RegData=RegData, datoFra='2018-01-01', valgtVar=valgtVar, datoTil=datoTil,
            reshID=reshID, outfile=outfile, OpMetode=OpMetode,
            minald=minald, maxald=maxald, Hastegrad = Hastegrad, preprosess = 1, velgAvd='' )


#Teste variable
variable <- c('Alder', 'KomplIntra', 'KomplPostop', 'Opf0Reoperasjon', 'Opf0AlvorlighetsGrad',
              'OpAntibProfylakse', 'OpASA', 'OpBMI', 'Opf0metode', 'Opf0Status', 'RegForsinkelse', 'Utdanning',
              'Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.png')
  NGERFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,Ngrense=20,
              minald=minald, maxald=maxald, preprosess = 1)
}


#------------------------------ Sentralmål per sykehus/Tid --------------------------
#------------------------------ (GjsnGrVar/GjsnTid) --------------------------

valgtVar <- 'Tss2Sumskaar'	#Må velge... Alder, R0ScorePhys,	R0ScoreRoleLmtPhy,	R0ScoreRoleLmtEmo,	R0ScoreEnergy,
                            #R0ScoreEmo, R0ScoreSosial,	R0ScorePain,	R0ScoreGeneral, RegForsinkelse, OpTid
                          #'Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
                          #'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt', 'Tss2Sumskaar'

outfile <- ''
#outfile <- paste0(valgtVar, '_sh.png')

testGjsn <- NGERFigGjsnGrVar(RegData=RegData,valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                 OpMetode=OpMetode, valgtMaal='gjsn', hentData=0,  grVar=grVar, Ngrense=10,
                 medKI=1, outfile=outfile, velgAvd = '')#AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad,

testGjsnTid <- NGERFigGjsnTid(RegData=RegData,valgtVar=valgtVar, datoFra='2017-01-01', datoTil=datoTil, minald=minald, maxald=maxald,
                 valgtMaal='gjsn', hentData=0, tidsenhet = 'Kvartal', enhetsUtvalg = enhetsUtvalg, #OpMetode=OpMetode, medKI=1, , velgAvd = ''
                 outfile=outfile, reshID = reshID)#AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad,

variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
              'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_ShGjsn.png')
  NGERFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra,datoTil=datoTil,
                      valgtMaal='Gjsn',outfile=outfile)
}

#-----------------------------Ant. registereringer------------------------------

NGERFigAntReg(RegData=0, datoTil=Sys.Date(),
                           minald=0, maxald=130, erMann='', outfile='',
                           reshID=0, enhetsUtvalg=2, hentData=1)



#-----------------------------Kvalietsindikatorer------------------------------
#valgtVar <- 'kvalInd' #RAND0, TSS0, kvalInd
outfile <- '' #paste0(valgtVar, '_kvalInd.png')
data("NGERtulledata")
UtKval <- NGERFigKvalInd(RegData=RegData, datoFra='2017-10-01', datoTil='3000-01-02', valgtVar='kvalInd', OpMetode=99,
               Hastegrad=99, preprosess=0, Ngrense=10, enhetsUtvalg=1, reshID = 8, velgAvd = 0,
               outfile=outfile)

RegData <- RegData[which(as.Date(RegData$HovedDato)>= as.Date('2017-01-01')), ]
ind <- which(as.Date(RegData$HovedDato) < '2017-03-30')
sum(table(RegData$Opf0metode[ind], useNA = 'a')[3:4])/length(ind)
table(RegData$Opf0Status[ind], useNA = 'a')
table(RegData$Opf0metode[ind], useNA = 'a')


#--------Data til Resultatportalen-----------------
rm(list=ls())
library(nger)
setwd('P:/Registerinfo og historie/NGER/Resultatportalen')
aar <- 2016:2019
#load(paste0('A:/NGER/Aarsrapp2018_2019-08-05.Rdata'))

NGERData <- NGERPreprosess(NGERRegDataSQL(datoFra = 2019))
RegData <- NGERData
#KomplIntra, KomplPostop, Opf0AlvorlighetsGrad,
#' HysKonvertert, LapKonvertert, Tss2Generelt, Tss2Sumskaar
test <- dataTilResPort(RegData = RegData, valgtKI = 'Tss2Sumskaar', aar=2019)

# For registeret Fil, enhetsID: EnhetsID	Enhetsnavn	HF navn	RHF navn (Toril komplettere med HF-navn)
Enheter <- unique(NGERPreprosess(RegData=NGERData)[ ,c('ReshId','ShNavn')])
write.table(Enheter, file = 'NGERenheter.csv', row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')

# Intraop kompl. ved laparoskopi
tab <- dataTilResPort(RegData=NGERData, valgtKI='KomplIntra', OpMetode = 1, aar = aar)

# Intraop kompl. ved hysteroskopi
dataTilResPort(RegData=NGERData, valgtKI='KomplIntra', OpMetode = 2, aar=aar)
#PostopKompl. ved laparoskopi/hysteroskopi
dataTilResPort(RegData=NGERData, valgtKI='KomplPostop', OpMetode = 1, aar=aar)
dataTilResPort(RegData=NGERData, valgtKI='KomplPostop', OpMetode = 2, aar=aar)

# Opf0AlvorlighetsGrad (>2) Lap/Hys,
dataTilResPort(RegData=NGERData, valgtKI='Opf0AlvorlighetsGrad', OpMetode = 1, aar=aar)
dataTilResPort(RegData=NGERData, valgtKI='Opf0AlvorlighetsGrad', OpMetode = 2, aar=aar)

# Konvertering hysteroskopi-> lapraskopi/lapratomi,
dataTilResPort(RegData=NGERData, valgtKI='HysKonvertert', aar=aar)

# lapraskopi -> lapratomi,
dataTilResPort(RegData=NGERData, valgtKI='LapKonvertert', aar=aar)

# TSS2 - sumskår, gj.sn.
dataTilResPort(RegData=NGERData, valgtKI='Tss2Sumskaar', aar=aar)

# TSS2 - positiv + svært positiv.
dataTilResPort(RegData=NGERData, valgtKI='Tss2Generelt', aar=aar)


# # Laparoskopi: Komplikasjoner etter operasjon Middels, alvorlig, død
# #(KomplPostop)
# dataTilResultatPort(RegData=NGERData, valgtKI='Opf0AlvorlighetsGrad', figurtype='andelGrVar', OpMetode = 1, aar=aar)
#
# # Hysteroskopi: Komplikasjoner etter operasjon Middels, alvorlig, død
# dataTilResultatPort(RegData=NGERData, valgtKI='Opf0AlvorlighetsGrad',  OpMetode = 2, aar=aar)

# Laparoskopi: Komplikasjoner etter operasjon
dataTilResultatPort(RegData=NGERData, valgtKI='KomplPostop', figurtype='andelGrVar', OpMetode = 1, aar=aar)

# Hysteroskopi: Komplikasjoner etter operasjon
dataTilResultatPort(RegData=NGERData, valgtKI='KomplPostop',  OpMetode = 2, aar=aar)

# test <- function(x) {sum(x, na.rm = T)/mean(x, na.rm = T)}
# test(RegData$Aar)



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



################################## REGISTRERINGSFORSINKELSE###########################
#	PasRegDato, Leveringsdato, OpDato
RegData$PasRegDato
RegData$Leveringsdato
RegData$OpDato
attach(RegData)
PasRegDato <- as.Date(PasRegDato)
Leveringsdato <- as.Date(Leveringsdato)
OpDato <- as.Date(OpDato)
diff <-
  plot(PasRegDato-OpDato)
plot(Leveringsdato-OpDato)
barplot(tapply(Leveringsdato-OpDato, SykehusNavn, FUN=mean, na.rm=T), horiz = T)

help(barplot)



a <- c(1:4,0:3,3:6)
b <- matrix(a,3,4)
colnames(b) <- c('rod', 'gronn','blaa','kvit')
c <- apply(b, 1,FUN=unique)
unlist(c)


############## Signifikant endring? --------------------------------------------------
#Signifikant endring i andel postoperative komplikasjoner for Sandnessjøen 2016 vs 2017?
RegData <- NGERPreprosess(RegData=RegData)
table(RegData$Aar)
RegData <- RegData[which(RegData$Aar %in% 2016:2017),]
'%i%' <- intersect
RegData <- RegData[which(RegData$Opf0Komplikasjoner %in% 0:1) %i%
                             which(RegData$Opf0Status == 1) %i%
                             which(RegData$OpMetode == 1) %i% which(RegData$ShNavn=='Sandessjøen'), ]
table(RegData$Opf0Komplikasjoner, RegData$Aar)
#2016 2017
#0   42   38
#1    9    4
prop.test(x=c(9,3), n=c(51, 42), alternative = "greater",correct = FALSE) #
prop.table(table(RegData$Opf0Komplikasjoner, RegData$Aar), margin = 2)
prop.test(x=table(RegData$Aar, RegData$Opf0Komplikasjoner), correct=FALSE)


RegData <- NGERPreprosess(RegData = NGERRegDataSQL())
RegData$tss2



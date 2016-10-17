
#--------------------------------Datakobling--------------------------

rm(list=ls())
NGERBasis <- read.table('C:/Registre/NGER/data/AlleVarNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
NGEROppf <- read.table('C:/Registre/NGER/data/FollowupsNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('forl',''), all = FALSE)
NGERData <- merge(NGERData, NGEROppf, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE)
RegData <- NGERData

# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2015-12-31'
preprosess <- 1
hentData <- 0
MCEType <- 99
tidsenhet <- 'Aar'
Hastegrad <- ''
AlvorlighetKompl <- ''
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
setwd("C:/ResultattjenesteGIT/nger/aarsrapp/")
library(nger)
#------------------------------ Andeler flere var --------------------------

variable <- c('Alder', 'KomplPost','KomplPostUtd','LapIntraabdominell', 'Norsktalende', 'OpBMI',
              'Opf0AlvorlighetsGrad','SivilStatus', 'Utdanning')

for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.pdf')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,preprosess = 1,
		minald=minald, maxald=maxald)
}


#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------

variable <- c('KomplIntra','KomplPostop','Opf0AlvorlighetsGrad','Opf0Reoperasjon')
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, MCEType=MCEType, Hastegrad=Hastegrad,
              AlvorlighetKompl=AlvorlighetKompl, tidsenhet=tidsenhet, preprosess=TRUE)
}


#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------

variable <- c('Alder', 'Opf0Status', 'OpBMI' )

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile,
              minald=minald, maxald=maxald, preprosess = 1)
}


#------------------------------Tabeller-----------------------------------
library(xtable)
RegData <- NGERPreprosess(RegData)
NGERUtvalg <- NGERUtvalg(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra,
                         datoTil = datoTil, MCEType = MCEType, Hastegrad=Hastegrad)
RegData <- NGERUtvalg$RegData


TabPasKar <- NGERtabVI(RegData)

#print(xtable::xtable(TabPasKar, digits=0, align=c('l', rep('r', max(c(1,ncol(TabPasKar)), na.rm=T))),
#                     caption='Pasientkarakterisika og operasjonstid.',
#                     label='tab:TabPasKar'),
#      include.rownames=TRUE, include.colnames=TRUE, check.names = FALSE)
cap <- "Gjennomsnittlig BMI, fødsler, graviditeter og knivtid. Verdiene
er gitt samlet for alle typer inngrep og splittet for laparoskopi,
hysteroskopi og der begge prosedyrer er brukt. Datagrunnlaget er tatt fra
en tre-års periode."

tab <- xtable::xtable(TabPasKar, align=c("l", "l", rep("r", ncol(TabPasKar)-1)),
                      digits=c(0,0,rep(1, ncol(TabPasKar)-1)),
                      caption=cap, label="tab:pasKarakteristika")


print(tab, include.rownames=FALSE, sanitize.text.function = function(x){x})

write.table(tab, file="TabPasienkarakteristika.csv", row.names=F, sep=';')


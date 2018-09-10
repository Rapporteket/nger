
#--------------------------------Data og parametrekobling--------------------------

rm(list=ls())
# NGERBasis <- read.table('C:/Registre/NGER/data/AlleVarNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8') #,
# NGERForlop <- read.table('C:/Registre/NGER/data/ForlopsOversikt2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
# NGEROppf <- read.table('C:/Registre/NGER/data/FollowupsNum2016-10-14.csv', sep=';', header=T, fileEncoding = 'UTF-8')
# NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('forl',''), all = FALSE)
# NGERData <- merge(NGERData, NGEROppf, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE)
# RegData <- NGERData

load(paste0('A:/NGER/NGER2018-09-10.Rdata'))
NGERData <- RegData


# Inndata til funksjon:
datoFra<- '2014-01-01'
datoFra1aar <- '2017-01-01'
datoTil <- '2017-12-31'
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
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
}

valgtVar <- 'Diagnoser'
outfile <- paste0(valgtVar, '_ford.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
               reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
MCEType <- 1
outfile <- paste0(valgtVar, '_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, MCEType = MCEType,
               reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
MCEType <- 2
outfile <- paste0(valgtVar, '_fordHys.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,MCEType = MCEType,
               reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

for (valgtVar in c('Diagnoser', 'Prosedyrer')) {
  for (MCEType in c(1:2,99)) {
  outfile <- paste0(valgtVar, '_fordOpType',MCEType, '.pdf')
  NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
					MCEType = MCEType, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
  }
}

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------

variable <- c('OpAnestesi', 'OpDagkirurgi', 'KomplIntra','KomplPostop',
					'Opf0AlvorlighetsGrad','Opf0Reoperasjon', 'OpTid')
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              enhetsUtvalg=0, outfile=outfile, MCEType=MCEType, Hastegrad=Hastegrad,
              AlvorlighetKompl=AlvorlighetKompl, tidsenhet='Aar')
}


#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------

variable <- c('Alder', 'OpAnestesi', 'OpDagkirurgi', 'Opf0Status', 'OpBMI', 'OpTid' )

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
}

#------------------------------ Sentralmål per sykehus --------------------------
valgtVar <- 'OpTid'	#Må velge... Alder, R0ScorePhys,	R0ScoreRoleLmtPhy,	R0ScoreRoleLmtEmo,	R0ScoreEnergy,
                            #R0ScoreEmo, R0ScoreSosial,	R0ScorePain,	R0ScoreGeneral, OpTid
                          #'Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
                          #'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt'

variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
              'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
variable <- 'OpTid'
valgtMaal <- 'Med'
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_' ,valgtMaal,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                      valgtMaal=valgtMaal,outfile=outfile)
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


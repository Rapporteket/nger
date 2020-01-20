
#--------------------------------Data og parametrekobling--------------------------

rm(list=ls())
dato <- '2019-08-05Aarsrapp18'
NGERBasis <- read.table(paste0('A:/NGER/AlleVarNum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table(paste0('A:/NGER/ForlopsOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
NGERSkjema <- read.table(paste0('A:/NGER/SkjemaOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('forl',''), all = FALSE)
NGERData <- NGERData[(as.Date(NGERData$OpDato) >= as.Date('2016-01-01')) &
                       (as.Date(NGERData$OpDato) <= as.Date('2018-12-31')), ]

#--Til registerleder
library(nger)
#RegData <- NGERPreprosess(NGERData)
#Ikke nødv: RegData <- NGERUtvalgEnh(RegData, datoFra = '2016-01-01', datoTil = '2018-12-31')$RegData
# write.table(RegData, file = "A:/NGER/Aarsrapp2018.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
#save(NGERData, file=paste0('A:/NGER/Aarsrapp2018', dato, '.RData'))

load(paste0('A:/NGER/Aarsrapp2018_2019-08-05.Rdata'))


# Inndata til funksjon:
datoFra<- '2016-01-01'
datoFra1aar <- '2018-01-01'
datoTil <- '2018-12-31'
OpMetode <- 99
tidsenhet <- 'Aar'
Hastegrad <- ''
AlvorlighetKompl <- ''
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
setwd("P:/Registerinfo og historie/NGER/aarsrapp/")
library(nger)

#--------------------Offentliggjøring--------------------------

NGERFigAndelTid(RegData=NGERData, valgtVar='KomplIntra', datoFra=datoFra, datoTil=datoTil,
                OpMetode=1, tidsenhet='Aar',
                outfile='Fig1 Andel intraoperative komplikasjoner siste år, laparoskopi.png')

NGERFigAndelTid(RegData=NGERData, valgtVar='LapKonvertert', datoFra=datoFra, datoTil=datoTil,
                tidsenhet='Aar', OpMetode = 1,
                outfile='Fig3 Utvikling konvertering til laparotomi.png')

NGERFigAndelTid(RegData=NGERData, valgtVar='KomplPostop', datoFra=datoFra, datoTil=datoTil,
                tidsenhet='Aar', OpMetode = 4, Hastegrad = 1,
                outfile='Fig4 antall postopr kompl ved TLH per aar.png')


NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='LapIntraabdominell', datoTil=datoTil,
               outfile='Fig2 Intraabdominelle kompl ved laparoskopi.png')

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil,
               OpMetode = 4, outfile='Fig5 Alvorlighetsgrad ved TLH.png')

NGERFigAndeler(RegData=NGERData, valgtVar='Tss2Generelt', datoFra=datoFra1aar, datoTil=datoTil,
               outfile='Fig6 TSS2 Oppfatning om gynekologisk avdeling.png')

NGERFigKvalInd(RegData=NGERData, valgtVar='TSS0', datoFra=datoFra1aar, datoTil=datoTil,
               outfile='Fig7 TSS2 samlet.png')

NGERFigGjsnGrVar(RegData=NGERData, valgtVar='Tss2Generelt', datoFra=datoFra1aar, datoTil=datoTil,
                 Hastegrad = 1, outfile='Fig8 Gjsn sumskår, oppfatning om gyn avd generelt.png')

#------------------------------ Andeler flere var --------------------------

variable <- c('Alder', 'KomplPostopType','KomplPostUtd','LapIntraabdominell', 'Norsktalende', 'OpBMI',
              'Opf0AlvorlighetsGrad','SivilStatus', 'Utdanning')
variable <- 'RegForsinkelse'
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.pdf')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
}

valgtVar <- 'Diagnoser'
outfile <- paste0(valgtVar, '_ford.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
OpMetode <- 1
outfile <- paste0(valgtVar, '_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, OpMetode = OpMetode,
               outfile=outfile)
OpMetode <- 2
outfile <- paste0(valgtVar, '_fordHys.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,OpMetode = OpMetode,
               outfile=outfile)

for (valgtVar in c('Diagnoser', 'Prosedyrer')) {
  for (OpMetode in c(1:2,99)) {
  outfile <- paste0(valgtVar, '_fordOpMetode',OpMetode, '.pdf')
  NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
					OpMetode = OpMetode, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
  }
}

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------

variable <- c('KomplIntra','KomplPostop',
					'Opf0AlvorlighetsGrad','Opf0Reoperasjon', 'OpTid')
variable <- 'RegForsinkelse'

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', tidsenhet, '.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              outfile=outfile, tidsenhet='Aar')
}

NGERFigAndelTid(RegData=NGERData, valgtVar='OpDagkirurgi', datoFra=datoFra, datoTil=datoTil,
                OpMetode=1, Hastegrad=1, tidsenhet='Aar', outfile='OpDagkirLapEl_aar.pdf')

NGERFigAndelTid(RegData=NGERData, valgtVar='OpAnestesi',   datoFra='2013-01-01', datoTil=datoTil, #datoFra
                OpMetode=2, Hastegrad=1, tidsenhet='Aar', outfile='OpAnestesiHysEl_aar.pdf')

#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------

variable <- c('Alder', 'OpAnestesi', 'OpDagkirurgi', 'Opf0Status', 'OpBMI', 'OpTid', 'RegForsinkelse' )

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
}

#------------------------------ Sentralmål per sykehus --------------------------
variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
              'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
              'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
variable <- 'OpTid'
variable <- 'RegForsinkelse'

valgtMaal <- 'med'
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_' ,valgtMaal,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                      valgtMaal=valgtMaal,outfile=outfile)
}



#------------------------------Tabeller-----------------------------------
library(xtable)
library(nger)
load(paste0('A:/NGER/Aarsrapp2018_2019-08-05.Rdata'))
RegData <- NGERData
RegData <- NGERPreprosess(RegData)

#Antall registreringer siste 5 år
tabOpph <- tabAntOpphSh5Aar(RegData=RegData, datoTil=datoTil)
AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]), margin = 1)
xtable::xtable(tabAvdAarN, digits=0, align=c('l', rep('r',ncol(tabAvdAarN))),
               caption = 'Antall registrerte opphold',
               label = 'tab:AntRegAar')


#Tabell med antall registreringer for hvert sykehus, splittet på lap, hys og begge
tab <- table(RegData[ ,c('ShNavn', "OpMetode", 'Aar')])
dimnames(tab)$OpMetode <- c('Lap', 'Hys', 'Begge')
tab <- addmargins(tab, margin = 1)
#colSums(tab)

tabell <- cbind(tab[,,'2016'],
                ' ',
                tab[,,'2017'],
                ' ',
                tab[,,'2018'])

xtable::xtable(tabell, align=c('l', rep('r',ncol(tabell))),)

ggplot(RegData, aes(OpMetode)) +
  geom_histogram(bins = 3) +
  facet_wrap(~ShNavn, ncol=5) +
  ggtitle("Eksempel")


#----
tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
grtxt <-UtDataGjsnTid$grtxt
if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
  grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
rownames(tabGjsnTid) <- grtxt

antKol <- ncol(tabGjsnTid)
navnKol <- colnames(tabGjsnTid)
if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}

output$gjsnTidTab <- function() {
  kableExtra::kable(tabGjsnTid, format = 'html'
                    , full_width=F
                    , digits = 1 #c(0,1,1,1)[1:antKol]
  ) %>%
    add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
    #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
    column_spec(column = 1, width_min = '7em') %>%
    column_spec(column = 2:(antKol+1), width = '7em') %>%
    row_spec(0, bold = T)
}


# Karakteristikker
RegData <- NGERData
RegData <- NGERPreprosess(RegData)
NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra1aar,
                         datoTil = datoTil, OpMetode = OpMetode, Hastegrad=Hastegrad)
RegData <- NGERUtvalg$RegData


TabPasKar <- NGERtabVI(RegData)
cap <- "Gjennomsnittlig BMI, fødsler, graviditeter og knivtid for pasienter operert i 2018. Verdiene
er gitt samlet for alle typer inngrep og splittet for laparoskopi,
hysteroskopi og der begge prosedyrer er brukt. ."

tab <- xtable::xtable(TabPasKar, align=c("l", "l", rep("r", ncol(TabPasKar)-1)),
                      digits=c(0,0,rep(1, ncol(TabPasKar)-1)),
                      caption=cap, label="tab:pasKarakteristika")


print(tab, include.rownames=FALSE, sanitize.text.function = function(x){x})
write.table(tab, file="TabPasienkarakteristika.csv", row.names=F, sep=';')


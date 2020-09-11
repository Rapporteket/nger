
#--------------------------------Data og parametrekobling--------------------------

rm(list=ls())
dato <- 'Aarsrapp2019_2020-04-28'
NGERBasis <- read.table(paste0('A:/NGER/AlleVarNum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
NGERForlop <- read.table(paste0('A:/NGER/ForlopsOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
NGERSkjema <- read.table(paste0('A:/NGER/SkjemaOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
NGERData <- merge(NGERForlop, NGERBasis, by = "ForlopsID", suffixes = c('forl',''), all = FALSE)
NGERData <- NGERData[(as.Date(NGERData$OpDato) >= as.Date('2016-01-01')) &
                       (as.Date(NGERData$OpDato) <= as.Date('2019-12-31')), ]

#--Til registerleder
library(nger)
#RegData <- NGERPreprosess(NGERData)
#Ikke nødv: RegData <- NGERUtvalgEnh(RegData, datoFra = '2016-01-01', datoTil = '2018-12-31')$RegData
# write.table(RegData, file = "A:/NGER/Aarsrapp2018.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
#save(NGERData, file=paste0('A:/NGER/Aarsrapp2018', dato, '.RData'))

#load(paste0('A:/NGER/Aarsrapp2018_2019-08-05.Rdata'))


# Inndata til funksjon:
datoFra<- '2016-01-01'
datoFra1aar <- '2019-01-01'
datoTil <- '2019-12-31'
#OpMetode <- 99
#tidsenhet <- 'Aar'
#Hastegrad <- ''
#AlvorlighetKompl <- ''
#enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
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


#-------------------------------Årsrapport 2020-------------------------------
#Figurer med RAND?

#--Tabeller:
#   Antall registreringer per år 2016-19
# Endres? Opphold per sykehus og operasjonstype (Lap Hys Begge) per år


#------------------------------ Fordelingsfigurer --------------------------
# 'BMI-kategori' = 'OpBMI',
# 'Diagnoser, hyppigste' = 'Diagnoser', hys/lap/tot
# 'Gjennomføringsgrad av hysteroskopi' = 'HysGjforingsGrad',
# 'Prosedyrer, hyppigste' = 'Prosedyrer', hys/lap/tot
# 'Laparaskopisk tilgang, teknikk og metode' = 'LapTeknikk',
# 'Operasjonstid (minutter)' = 'OpTid', (lap/tot. lap. hysrektomi/hysteroskopi)
# 'Laparaskopisk ekstrautstyr' = 'LapEkstrautstyr',
# 'Laparoskopiske intrapoerative komplikasjoner' = 'LapKomplikasjoner',
# 'Laparoskopiske intraabdominale komplikasjoner' = 'LapIntraabdominell', (Alle / Tot. lap. hysrektomi)
# 'Hysteroskopi intrapoerative komplikasjoner' = 'HysKomplikasjoner'
# 'Alvorlighetsgrad, postop. kompl.' = 'Opf0AlvorlighetsGrad',   (Alle / laparoskopi/hysteroskopi/ tot.lap hysrektomi)
# 'Komplikasjoner, postoperativt' = 'KomplPostopType',(Alle/laparoskopi/tot.lap hysrektomi/hysteroskopi)
# 'Registreringsforsinkelse' =  'RegForsinkelse',
# 'TSS2, sp.6 Generell oppfatning av avdelinga' = 'Tss2Generelt',

#For alle
variable <- c('OpBMI', 'Diagnoser','HysGjforingsGrad','HysKomplikasjoner', 'KomplPostopType', 'LapEkstrautstyr',
              'LapIntraabdominell', 'LapKomplikasjoner', 'LapTeknikk', 'Opf0AlvorlighetsGrad',
              'Prosedyrer', 'RegForsinkelse', 'Tss2Generelt')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.pdf')
	NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
}

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Diagnoser', datoTil=datoTil, OpMetode = 1,
               outfile='Diagnoser_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Diagnoser', datoTil=datoTil, OpMetode = 2,
               outfile='Diagnoser_fordHys.pdf')

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Prosedyrer', datoTil=datoTil, OpMetode = 1,
               outfile='Prosedyrer_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Prosedyrer', datoTil=datoTil, OpMetode = 2,
               outfile='Prosedyrer_fordHys.pdf')

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 1,
               outfile='OpTid_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 2,
               outfile='OpTid_fordHys.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 4,
               outfile='OpTid_fordTotLapHys.pdf')

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='LapIntraabdominell', datoTil=datoTil, OpMetode = 4,
               outfile='LapIntraabdominell_fordTotLapHys.pdf')

NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 1,
               outfile='Opf0AlvorlighetsGrad_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 2,
               outfile='Opf0AlvorlighetsGrad_fordHys.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 4,
               outfile='Opf0AlvorlighetsGrad_fordTotLapHys.pdf')

# Postoperative komplikasjoner Laparoskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 1,
               outfile='KomplPostopType_fordLap.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 1, AlvorlighetKompl = 1, outfile='KomplPostopType_fordLapAlv1.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 1, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordLapAlv234.pdf')

# Postoperative komplikasjoner Hysteroskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 2,
               outfile='KomplPostopType_fordHys.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 2, AlvorlighetKompl = 1, outfile='KomplPostopType_fordHysAlv1.pdf')
NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 2, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordHysAlv234.pdf')


NGERFigAndeler(RegData=NGERData, datoFra=datoFra1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 4,
               outfile='KomplPostopType_fordTotLapHys.pdf')



#----------------Kvalitetsindikatorsamlinger----------------
# 'TSS2, oppfølging' = 'TSS0', TSS2, alle spørsmål (Alle/tot.lap hysterektomi/hysteroskopi)
NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1aar, valgtVar='TSS0', datoTil=datoTil,
                           outfile='TSS0_ford.pdf')
NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1aar, valgtVar='TSS0', datoTil=datoTil, OpMetode = 2,
                             outfile='TSS0_fordHys.pdf')
NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1aar, valgtVar='TSS0', datoTil=datoTil, OpMetode = 4,
                               outfile='TSS0_fordTotLapHys.pdf')

#------------------------------ Andeler per år --------------------------
#------------------------------ (AndelTid) --------------------------

# 'Dagkirurgiske inngrep' = 'OpDagkirurgi', (lapraroskopi, elektiv)
# Lokalbedøvelse = OpAnestesi (hysteroskopi, elektiv)
# 'ASA-grad > II' = 'OpASA', (Alle / tot.lap hysrektomi)
# 'Konvertert til laparoromi?' = 'LapKonvertert',
# 'Komplikasjoner under operasjon' = 'KomplIntra',
# 'Postop. komplikasjon: Alle' = 'KomplPostop',
# 'Postop. komplikasjon: Reoperasjon' = 'Opf0Reoperasjon', (Alle/laparoskopi/tot.lap.hysrektomi)

variable <- c('KomplIntra','KomplPostop','LapKonvertert', 'OpASA',
              'Opf0Reoperasjon')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_', 'Aar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              outfile=outfile, tidsenhet='Aar')
}

NGERFigAndelTid(RegData=NGERData, valgtVar='OpDagkirurgi', datoFra=datoFra, datoTil=datoTil,
                OpMetode=1, Hastegrad=1, tidsenhet='Aar', outfile='OpDagkirLapEl_aar.pdf')

NGERFigAndelTid(RegData=NGERData, valgtVar='OpAnestesi',   datoFra=datoFra, datoTil=datoTil, #datoFra
                OpMetode=2, Hastegrad=1, tidsenhet='Aar', outfile='OpAnestesiHysEl_aar.pdf')

NGERFigAndelTid(RegData=NGERData, valgtVar='OpASA',   datoFra=datoFra, datoTil=datoTil, #datoFra
                OpMetode=4, tidsenhet='Aar', outfile='OpASATotLapHys_aar.pdf')



#--Laparoskopi
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad234',
                   'Opf0Reoperasjon','LapKonvertert')) {
  outfile <- paste0(valgtVar, '_', 'LapAar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                  OpMetode=1, outfile=outfile, tidsenhet='Aar')
}
#Postoperative komplikasjoner Laparoskopi, lite alvorlige, utvikling siste 4 år
NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar='Opf0AlvorlighetsGrad1', datoTil=datoTil,
                OpMetode=1, outfile='Opf0AlvorlighetsGrad1_LapAar.pdf', tidsenhet='Aar')

#--Hysteroskopi
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad234',
                   'Opf0Reoperasjon')) {
  outfile <- paste0(valgtVar, '_', 'HystAar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                  OpMetode=2, outfile=outfile, tidsenhet='Aar')
}
NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar='Opf0AlvorlighetsGrad1', datoTil=datoTil,
                OpMetode=4, outfile='KomplPostop_TLHaar.pdf', tidsenhet='Aar')

#Postoperative komplikasjoner Hysteroskopi, lite alvorlige, utvikling siste 4 år
NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar='Opf0AlvorlighetsGrad234', datoTil=datoTil,
                OpMetode=2, outfile='Opf0AlvorlighetsGrad234_HystAar.pdf', tidsenhet='Aar')



#------------------------------ Andeler per sykehus --------------------------
#------------------------------ (AndelGrVar) --------------------------
# 'Fedme (BMI>30)' = 'OpBMI',
# 'Dagkirurgiske inngrep' = 'OpDagkirurgi',
# 'Komplikasjoner under operasjon' = 'KomplIntra', (Laparoskopi, valgte sykehus..)
# 'Postop. komplikasjon: Alle' = 'KomplPostop', (Alle, valgte sykehus)
# 'TSS2: Møtet med gyn. avd. var svært godt' = 'Tss2Mott',
# 'TSS2: Behandlingsopplegg/-innhold passet svært bra' = 'Tss2Behandling',
# 'TSS2: Behandlerne lyttet- og forsto i svært stor grad' = 'Tss2Lytte',
# 'TSS2: Pasienten hadde svært stor tillit til sine behandlere' = 'Tss2Behandlere',
# 'TSS2: Pasient og behandlere svært enige om målsetn. for behandlinga' = 'Tss2Enighet',
# 'TSS2: Positiv oppfatning om gyn. avd.' = 'Tss2Generelt'
# 'Registreringsforsinkelse' = 'RegForsinkelse',Mer enn 4 uker fra op. til reg.


variable <- c( 'OpBMI', 'KomplPostop', 'OpDagkirurgi', 'RegForsinkelse',
               'Tss2Mott', 'Tss2Behandling', 'Tss2Lytte', 'Tss2Behandlere', 'Tss2Enighet', 'Tss2Generelt')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
}

#Laparoskopi
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad234',
                   'Opf0Reoperasjon','LapKonvertert', 'OpDagkirurgi')) {
  outfile <- paste0(valgtVar, '_LapShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
                      OpMetode=1, outfile=outfile)
}


#--Hysteroskopi
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad234',
                   'Opf0Reoperasjon')) {
  outfile <- paste0(valgtVar, '_HystShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData, datoFra=datoFra1aar, valgtVar=valgtVar, datoTil=datoTil,
                      OpMetode=2, outfile=outfile)
}



#------------------------------ Sentralmål per sykehus --------------------------
# variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
#               'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
# variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
#               'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
# variable <- 'OpTid'
# variable <- 'RegForsinkelse'

#'TSS2, sumskår' = 'Tss2Sumskaar'
for (valgtVar in c('Tss2Sumskaar')) {
  outfile <- paste0(valgtVar, '_' ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                      outfile=outfile)
}

for (OpMetode in c(1,2,4)) {
  outfile <- paste0('OpTid_', c('Lap','Hyst','', 'TLH')[OpMetode] ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData, valgtVar='OpTid', datoFra=datoFra1aar, datoTil=datoTil,
                   OpMetode = OpMetode, outfile=outfile)
}

#Skal bare ha med Haugesund 701437, Bodø 706220, Trondheim 107644, Ullevål 700399 og Tønsberg 110734.
NGERDataUtvSh <- NGERData[which(NGERData$AvdRESH %in% c(701437, 706220, 107644, 700399, 110734)),]
NGERFigGjsnGrVar(RegData=NGERDataUtvSh, valgtVar='R0ScorePhys', datoFra=datoFra1aar, datoTil=datoTil,
                  outfile='R0ScorePhys_UtvalgteShGjsn.pdf')


#KvalInd
for (valgtVar in c('kvalInd', 'RAND0', 'TSS0')) {
outfile <- paste0(valgtVar, '_' ,'KI.pdf')
NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1aar, datoTil=datoTil,
                           valgtVar=valgtVar, outfile=outfile)
}



#------------------------------Tabeller-----------------------------------
library(xtable)
library(nger)
#load(paste0('A:/NGER/Aarsrapp2018_2019-08-05.Rdata'))
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
                tab[,,'2018'],
                ' ',
                tab[,,'2019'])

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

#-----------Resultater for enkeltsykehus--------------------------
#Sandnessjøen, resh 103162. Postop blødning. Hysterektomi, 2015-2019

NGERFigAndelTid(RegData=NGERData, datoFra='2015-01-01', valgtVar='Opf0KomplBlodning', datoTil='2019-12-31',
                reshID = 103162, enhetsUtvalg = 2, OpMetode=4, outfile='PostOpBlod_Sandnessj.pdf', tidsenhet='Aar')


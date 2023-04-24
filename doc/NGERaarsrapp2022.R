
#--------------------------------Data og parametrekobling--------------------------
# Inndata til funksjon:
library(nger)
datoFra <- '2016-01-01'
rappAar <- 2022
datoFra1aar <- paste0(rappAar, '-01-01')
datoTil <- paste0(rappAar, '-12-31')
datoFra1Yoppf <- paste0(rappAar-1, '-01-01')#'2020-01-01'
datoTil1Yoppf <- paste0(rappAar-1, '-12-31') #'2020-12-31'

NGERData <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil))
NGERData1aar <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))

setwd('/home/rstudio/speiler_.ssh/aarsrapp/NGER' ) #"P:/Registerinfo og historie/NGER/aarsrapp/")


Mangler figurene:
Opf0AlvorlighetsGrad_HystAar
Opf0AlvorlighetsGradFord
Opf0KomplAlvorInfeksjonTid
Opf0KomplAlvorInfeksjonShus

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

#Fjernet, 2021: 'Diagnoser','KomplPostopType', 'LapEkstrautstyr', 'Prosedyrer',
variabler <- c('OpBMI', 'HysGjforingsGrad','HysKomplikasjoner',
              'LapIntraabdominell', 'LapKomplikasjoner', 'LapTeknikk',
              'Opf0AlvorlighetsGrad', 'RegForsinkelse', 'Tss2Generelt')
variabler <- 'Opf0AlvorlighetsGrad'
for (valgtVar in variabler) {
	outfile <- paste0(valgtVar, '_ford.pdf')
	NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar, outfile=outfile)
}
#NGERFigAndeler(RegData=RegData, preprosess=0, valgtVar='KomplAlvorligPostopType')

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Diagnoser', OpMetode = 1,
               outfile='Diagnoser_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Diagnoser', OpMetode = 2,
               outfile='Diagnoser_fordHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Prosedyrer', OpMetode = 1,
               outfile='Prosedyrer_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Prosedyrer', OpMetode = 2,
               outfile='Prosedyrer_fordHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='LapIntraabdominell', OpMetode = 4,
               outfile='LapIntraabdominell_fordTotLapHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Opf0AlvorlighetsGrad', OpMetode = 1,
               outfile='Opf0AlvorlighetsGrad_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Opf0AlvorlighetsGrad', OpMetode = 2,
               outfile='Opf0AlvorlighetsGrad_fordHys.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Opf0AlvorlighetsGrad', OpMetode = 4,
               outfile='Opf0AlvorlighetsGrad_fordTotLapHys.pdf')

# Postoperative komplikasjoner Laparoskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplPostopType',
               OpMetode = 1, AlvorlighetKompl = 1, outfile='KomplPostopType_fordLapAlv1.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplPostopType',
               OpMetode = 1, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordLapAlv234.pdf')

# Postoperative komplikasjoner Hysteroskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplPostopType',
               OpMetode = 2, AlvorlighetKompl = 1, outfile='KomplPostopType_fordHysAlv1.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplPostopType',
               OpMetode = 2, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordHysAlv234.pdf')

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplPostopType', OpMetode = 4,
               outfile='KomplPostopType_fordTotLapHys.pdf')
#Postop, alvorlige og middels alvorlige komplikasjoner:
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='KomplAlvorPostopType', OpMetode = 4,
               outfile='KomplAlvorPostopType_fordTotLapHys.pdf')

#Fordelingsfigurer: alder  og BMI på Laparoskopi, og på Hysteroskopi og på TLH (total laparaskopisk hysrektomi).

NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Alder', OpMetode = 1,
               outfile='Alder_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Alder', OpMetode = 2,
               outfile='Alder_fordHys.pdf')
NGERFigAndeler(RegData=NGERData1aar, preprosess=0, valgtVar='Alder', OpMetode = 4,
               outfile='Alder_fordTLH.pdf')


#----------------Kvalitetsindikatorsamlinger----------------
# 'TSS2, oppfølging' = 'TSS0', TSS2, alle spørsmål (Alle/tot.lap hysterektomi/hysteroskopi)
NGERFigKvalInd(RegData=NGERData1aar, preprosess=0, valgtVar='TSS0',
                           outfile='TSS0_ford.pdf')

#------------------------------ Andeler per år (AndelTid)--------------------------
# 'Dagkirurgiske inngrep' = 'OpDagkirurgi', (lapraroskopi, elektiv)
# Lokalbedøvelse = OpAnestesi (hysteroskopi, elektiv)
# 'ASA-grad > II' = 'OpASA', (Alle / tot.lap hysrektomi)
# 'Konvertert til laparoromi?' = 'LapKonvertert',
# 'Komplikasjoner under operasjon' = 'KomplIntra',
# 'Postop. komplikasjon: Alle' = 'KomplPostop',
# 'Postop. komplikasjon: Reoperasjon' = 'Opf0Reoperasjon', (Alle/laparoskopi/tot.lap.hysrektomi)

#Fjernet 2021: 'KomplIntra', 'KomplPostop', 'OpASA', 'Opf0Reoperasjon'
NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar='LapKonvertert',
              outfile='LapKonvertert_Aar.pdf', tidsenhet='Aar')

NGERFigAndelTid(RegData=NGERData, valgtVar='OpDagkirurgi', preprosess = 0,
                OpMetode=1, Hastegrad=1, tidsenhet='Aar', outfile='OpDagkirLapEl_aar.pdf')


#--Laparoskopi
#Fjernet 2021: 'KomplPostop',
variabler <- c('KomplIntra', 'Opf0AlvorlighetsGrad', 'Opf0AlvorlighetsGrad1',
               'Opf0KomplAlvorInfeksjon', 'KomplPostopAlvor',
               'Opf0Reoperasjon','LapKonvertert')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_', 'LapAar.pdf')
  NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar=valgtVar,
                  OpMetode=1, outfile=outfile, tidsenhet='Aar')
}
#Postoperative komplikasjoner Laparoskopi, lite alvorlige, utvikling siste 4 år
NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar='Opf0AlvorlighetsGrad1',
                OpMetode=1, outfile='Opf0AlvorlighetsGrad1_LapAar.pdf', tidsenhet='Aar')

#--Hysteroskopi
#Fjernet 2021: 'KomplPostop',
variabler <- c('KomplIntra', 'Opf0AlvorlighetsGrad', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
               'Opf0Reoperasjon')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_', 'HystAar.pdf')
  NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar=valgtVar,
                  OpMetode=2, outfile=outfile, tidsenhet='Aar')
}
NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar='Opf0AlvorlighetsGrad1',
                OpMetode=4, outfile='KomplPostop_TLHaar.pdf', tidsenhet='Aar')




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

#Fjernet 2021: 'OpDagkirurgi',
variabler <- c(# 'OpBMI', 'KomplPostop', 'Opf0KomplAlvorInfeksjon', 'RegForsinkelse',
               'Tss2Mott', 'Tss2Behandling', 'Tss2Lytte', 'Tss2Behandlere',
               'Tss2Enighet', 'Tss2Generelt')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,  outfile=outfile)
}

#valgtVar <- 'OpDagkirurgi'
#Laparoskopi
#Fjernet 2021: 'Alder', 'KomplPostop', 'OpBMI',
variabler <- c( 'KomplIntra', 'Opf0AlvorlighetsGrad', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                'Opf0Reoperasjon','LapKonvertert', 'LapKonvertertUventet', 'OpDagkirurgi')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_LapShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,
                      OpMetode=1, outfile=outfile)
}


#--Hysteroskopi
#Fjernet 2021: 'Alder', 'OpBMI',
variabler <- c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
               'Opf0Reoperasjon')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_HystShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,
                      OpMetode=2, outfile=outfile)
}

#TLH
#Fjernet 2021: 'Alder',
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar='OpBMI',
                      OpMetode=4, outfile='OpBMI_TLHShus.pdf')

#------------------------------ Sentralmål per sykehus --------------------------
# variabler <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
#               'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
# variabler <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
#               'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
# variabler <- 'OpTid'
# variabler <- 'RegForsinkelse'

#'TSS2, sumskår' = 'Tss2Sumskaar'
for (valgtVar in c('Tss2Sumskaar')) {
  outfile <- paste0(valgtVar, '_' ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra1aar,
                      outfile=outfile)
}

for (OpMetode in c(1,2,4)) {
  outfile <- paste0('OpTid_', c('Lap','Hyst','', 'TLH')[OpMetode] ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData1aar, valgtVar='OpTid', preprosess = 0,
                   OpMetode = OpMetode, outfile=outfile)
}

#KvalInd
for (valgtVar in c('kvalInd', 'RAND0')) {
outfile <- paste0(valgtVar, '_' ,'KI.pdf')
NGERFigKvalInd(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar, outfile=outfile)
}

#Ønsker også Rand etter 1 år
 NGERFigKvalInd(RegData=NGERData, preprosess=0, datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                valgtVar='RAND1', outfile='RAND1_KI.pdf')


#------------------------------Tabeller-----------------------------------
library(xtable)
library(nger)
RegData <- NGERData

#Antall registreringer siste 5 år
tabOpph <- tabAntOpphSh5Aar(RegData=RegData, datoTil=datoTil)
AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]), margin = 1)
xtable::xtable(tabAvdAarN, digits=0, align=c('l', rep('r',ncol(tabAvdAarN))),
               caption = 'Antall registrerte opphold',
               label = 'tab:AntRegAar')


#Tabell med antall registreringer for hvert sykehus, splittet på lap, hys og begge
RegData1aar <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))
tab <- table(RegData1aar[ ,c('ShNavn', "OpMetode")]) #, 'Aar'
dimnames(tab)$OpMetode <- c('Lap', 'Hys', 'Begge')
tab <- addmargins(tab, margin = 1)

xtable::xtable(tab, align=c('l', rep('r',ncol(tab))), digits=0)

% latex table generated in R 4.2.2 by xtable 1.8-4 package
% Thu Apr 20 08:42:53 2023
\begin{table}[ht]
\centering
\begin{tabular}{lrrr}
\hline
& Lap & Hys & Begge \\
\hline
Ahus & 463 & 205 & 3 \\
Aleris Frogner & 288 & 78 & 4 \\
Arendal & 116 & 49 & 1 \\
Betanien & 105 & 137 & 0 \\
Bodø & 175 & 48 & 0 \\
Bærum & 198 & 155 & 5 \\
DNR & 118 & 0 & 0 \\
Drammen & 264 & 373 & 7 \\
Elverum & 260 & 108 & 0 \\
Flekkefjord & 58 & 35 & 1 \\
Førde & 78 & 31 & 1 \\
Gjøvik & 139 & 186 & 5 \\
Hammerfest & 67 & 44 & 3 \\
Harstad & 105 & 98 & 3 \\
Haugesund & 160 & 134 & 0 \\
Haukeland & 276 & 34 & 0 \\
Kirkenes & 37 & 15 & 0 \\
Kongsberg & 76 & 22 & 0 \\
Kongsvinger & 69 & 9 & 0 \\
Kristiansand & 103 & 121 & 0 \\
Levanger & 76 & 69 & 2 \\
Lillehammer & 182 & 77 & 0 \\
Lofoten & 26 & 0 & 0 \\
Mo i Rana & 37 & 25 & 0 \\
Molde & 62 & 31 & 1 \\
Namsos & 62 & 35 & 0 \\
Narvik & 65 & 22 & 2 \\
Ringerike & 142 & 40 & 0 \\
Sandnessjøen & 84 & 43 & 1 \\
Skien & 241 & 188 & 0 \\
Stavanger & 285 & 168 & 3 \\
Stord & 103 & 73 & 0 \\
Tromsø & 101 & 80 & 0 \\
Trondheim & 342 & 368 & 5 \\
Tønsberg & 408 & 268 & 1 \\
Ullevål & 1530 & 549 & 1 \\
Vesterålen & 26 & 20 & 1 \\
Volda & 25 & 30 & 0 \\
Volvat & 2 & 0 & 0 \\
Voss & 82 & 67 & 0 \\
Østfold & 186 & 167 & 0 \\
Ålesund & 144 & 90 & 5 \\
Sum & 7366 & 4292 & 55 \\
\hline
\end{tabular}
\end{table}


# ggplot::ggplot(RegData, aes(OpMetode)) +
#   geom_histogram(bins = 3) +
#   facet_wrap(~ShNavn, ncol=5) +
#   ggtitle("Eksempel")


# Karakteristikker
RegData <- NGERData
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



#--------------------Data til SKDE interaktive nettsider------------------
#KomplIntra, KomplPostop, KomplPostopAlvor
#OpMetode  1: Laparoskopi #2: Hysteroskopi, 2019: 5954 3146
library(nger)
setwd('/home/rstudio/speil/aarsrapp/NGER' )
RegData <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = '2016-01-01'))
                                                   #,datoTil = '2022-12-31'))
lastNedFil <- 0

dataTilSKDE_Flere <- dataTilOffVisning(RegData=RegData,
                                 valgtVar = 'KomplIntra',
                                 OpMetode = 1,
                                 lastNedFil = lastNedFil,
                                 indID = 'nger_kompl_intra_lap', filUt='KomplIntraLap')

dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplIntra',
                                 OpMetode = 2,
                                 lastNedFil = lastNedFil,
                                 indID = 'nger_kompl_intra_hys', filUt='KomplIntraHys')
dataTilSKDE_Flere <- rbind(dataTilSKDE_Flere, dataTilSKDE)

#Forekomsten av middels og alvorlige komplikasjoner etter  inngrep.
#Lap Hys
dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor',
                                 OpMetode = 1,
                                 lastNedFil = lastNedFil,
                                 indID = 'nger_kompl_postop_lap', filUt='KomplPostopLap')
dataTilSKDE_Flere <- rbind(dataTilSKDE_Flere, dataTilSKDE)

dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor',
                                 OpMetode = 2,
                                 lastNedFil = lastNedFil,
                                 indID = 'nger_kompl_postop_hys', filUt='KomplPostopHys')
dataTilSKDE_Flere <- rbind(dataTilSKDE_Flere, dataTilSKDE)

#Generell pasienttilfredshet
dataTilSKDE <- dataTilOffVisning(RegData=RegData,
                                 valgtVar = 'Tss2Sumskaar',
                                 aggData = 1,
                                 lastNedFil = lastNedFil,
                                 indID = 'nger_pasient_tilfredshet',
                                 filUt='Tss2Sumskaar')

dataTilSKDE_Flere <- rbind(dataTilSKDE_Flere, dataTilSKDE)

sum(is.na(dataTilSKDE_Flere$orgnr))
write.table(dataTilSKDE_Flere, file = 'dataTilSKDE_Flere.csv', sep = ';', row.names = F)

#tapply(dataTilSKDE$var, dataTilSKDE$year, FUN='mean')*100

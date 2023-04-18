
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

#Fjernet, 2021: 'Diagnoser','KomplPostopType', 'LapEkstrautstyr', 'Opf0AlvorlighetsGrad','Prosedyrer',
variabler <- c('OpBMI', 'HysGjforingsGrad','HysKomplikasjoner',
              'LapIntraabdominell', 'LapKomplikasjoner', 'LapTeknikk',
               'RegForsinkelse', 'Tss2Generelt')
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
for (valgtVar in c('KomplIntra', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                   'Opf0Reoperasjon','LapKonvertert')) {
  outfile <- paste0(valgtVar, '_', 'LapAar.pdf')
  NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar=valgtVar,
                  OpMetode=1, outfile=outfile, tidsenhet='Aar')
}
#Postoperative komplikasjoner Laparoskopi, lite alvorlige, utvikling siste 4 år
NGERFigAndelTid(RegData=NGERData, preprosess = 0, valgtVar='Opf0AlvorlighetsGrad1',
                OpMetode=1, outfile='Opf0AlvorlighetsGrad1_LapAar.pdf', tidsenhet='Aar')

#--Hysteroskopi
#Fjernet 2021: 'KomplPostop',
for (valgtVar in c('KomplIntra','Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                   'Opf0Reoperasjon')) {
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
variabler <- c( 'OpBMI', 'KomplPostop', 'RegForsinkelse',
               'Tss2Mott', 'Tss2Behandling', 'Tss2Lytte', 'Tss2Behandlere', 'Tss2Enighet', 'Tss2Generelt')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,  outfile=outfile)
}

#valgtVar <- 'OpDagkirurgi'
#Laparoskopi
#Fjernet 2021: 'Alder', 'KomplPostop', 'OpBMI',
for (valgtVar in c( 'KomplIntra','Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                   'Opf0Reoperasjon','LapKonvertert', 'LapKonvertertUventet', 'OpDagkirurgi')) {
  outfile <- paste0(valgtVar, '_LapShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,
                      OpMetode=1, outfile=outfile)
}


#--Hysteroskopi
#Fjernet 2021: 'Alder', 'OpBMI',
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                   'Opf0Reoperasjon')) {
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
 NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
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

% latex table generated in R 4.2.2 by xtable 1.8-4 package
% Tue Apr 18 16:10:21 2023
\begin{table}[ht]
\centering
\begin{tabular}{lrrrrr}
\hline
& 2018 & 2019 & 2020 & 2021 & 2022 \\
\hline
Ahus & 637 & 458 & 628 & 614 & 671 \\
Aleris Frogner & 0 & 0 & 162 & 211 & 370 \\
Arendal & 137 & 163 & 144 & 167 & 166 \\
Betanien & 237 & 240 & 235 & 274 & 242 \\
Bodø & 187 & 259 & 201 & 229 & 223 \\
Bærum & 274 & 233 & 267 & 340 & 358 \\
DNR & 132 & 135 & 146 & 107 & 118 \\
Drammen & 483 & 544 & 567 & 658 & 644 \\
Elverum & 105 & 254 & 243 & 376 & 368 \\
Flekkefjord & 33 & 16 & 86 & 70 & 94 \\
Førde & 63 & 121 & 116 & 134 & 110 \\
Gjøvik & 308 & 296 & 258 & 279 & 330 \\
Hammerfest & 82 & 148 & 99 & 99 & 114 \\
Harstad & 0 & 65 & 132 & 214 & 206 \\
Haugesund & 325 & 294 & 318 & 250 & 294 \\
Haukeland & 292 & 388 & 388 & 354 & 310 \\
Kirkenes & 38 & 48 & 6 & 9 & 52 \\
Kongsberg & 6 & 112 & 92 & 101 & 98 \\
Kongsvinger & 63 & 60 & 89 & 107 & 78 \\
Kristiansand & 212 & 228 & 230 & 265 & 224 \\
Levanger & 0 & 0 & 87 & 81 & 147 \\
Lillehammer & 73 & 190 & 177 & 161 & 259 \\
Lofoten & 0 & 14 & 23 & 21 & 26 \\
Mo i Rana & 76 & 68 & 53 & 72 & 62 \\
Molde & 58 & 111 & 104 & 95 & 94 \\
Namsos & 0 & 0 & 3 & 80 & 97 \\
Narvik & 0 & 0 & 96 & 63 & 89 \\
Ringerike & 80 & 89 & 154 & 193 & 182 \\
Sandnessjøen & 98 & 109 & 117 & 126 & 128 \\
Skien & 183 & 291 & 463 & 450 & 429 \\
Stavanger & 285 & 392 & 416 & 483 & 456 \\
Stord & 0 & 196 & 205 & 168 & 176 \\
Tromsø & 132 & 137 & 152 & 190 & 181 \\
Trondheim & 292 & 612 & 819 & 879 & 715 \\
Tønsberg & 708 & 755 & 694 & 729 & 677 \\
Ullevål & 1380 & 1571 & 1891 & 1932 & 2080 \\
Vesterålen & 1 & 46 & 49 & 64 & 47 \\
Volda & 19 & 21 & 51 & 55 & 55 \\
Volvat & 0 & 0 & 41 & 4 & 2 \\
Voss & 67 & 120 & 142 & 132 & 149 \\
Østfold & 74 & 295 & 265 & 363 & 353 \\
Ålesund & 50 & 92 & 270 & 237 & 239 \\
Sum & 7190 & 9171 & 10679 & 11436 & 11713 \\
\hline
\end{tabular}
\caption{Antall registrerte opphold}
\label{tab:AntRegAar}
\end{table}


#Tabell med antall registreringer for hvert sykehus, splittet på lap, hys og begge
RegData1aar <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))
tab <- table(RegData[ ,c('ShNavn', "OpMetode")]) #, 'Aar'
dimnames(tab)$OpMetode <- c('Lap', 'Hys', 'Begge')
tab <- addmargins(tab, margin = 1)

xtable::xtable(tab, align=c('l', rep('r',ncol(tab))), digits=0)

% latex table generated in R 4.2.2 by xtable 1.8-4 package
% Tue Apr 18 16:10:55 2023
\begin{table}[ht]
\centering
\begin{tabular}{lrrr}
\hline
& Lap & Hys & Begge \\
\hline
Ahus & 2515 & 1286 & 14 \\
Aleris Frogner & 555 & 182 & 6 \\
Arendal & 722 & 229 & 4 \\
Betanien & 647 & 777 & 20 \\
Bodø & 1002 & 418 & 5 \\
Bærum & 978 & 667 & 14 \\
DNR & 850 & 0 & 0 \\
Drammen & 1790 & 2081 & 40 \\
Elverum & 1051 & 378 & 2 \\
Flekkefjord & 247 & 82 & 3 \\
Førde & 413 & 182 & 3 \\
Gjøvik & 1161 & 916 & 24 \\
Hammerfest & 344 & 245 & 11 \\
Harstad & 308 & 301 & 8 \\
Haugesund & 1106 & 979 & 8 \\
Haukeland & 1994 & 302 & 10 \\
Kirkenes & 213 & 45 & 1 \\
Kongsberg & 310 & 97 & 2 \\
Kongsvinger & 469 & 73 & 1 \\
Kristiansand & 818 & 569 & 10 \\
Levanger & 147 & 165 & 3 \\
Lillehammer & 673 & 371 & 8 \\
Lofoten & 84 & 0 & 0 \\
Mo i Rana & 321 & 128 & 5 \\
Molde & 334 & 231 & 51 \\
Namsos & 138 & 42 & 0 \\
Narvik & 170 & 73 & 5 \\
Ringerike & 495 & 199 & 4 \\
Sandnessjøen & 544 & 215 & 11 \\
Skien & 1504 & 689 & 7 \\
Stavanger & 1661 & 817 & 19 \\
Stord & 473 & 264 & 8 \\
Tromsø & 552 & 489 & 0 \\
Trondheim & 1635 & 2141 & 22 \\
Tønsberg & 2750 & 2156 & 34 \\
Ullevål & 8404 & 2799 & 46 \\
Vesterålen & 79 & 124 & 4 \\
Volda & 149 & 101 & 6 \\
Volvat & 36 & 10 & 1 \\
Voss & 452 & 248 & 0 \\
Østfold & 834 & 650 & 2 \\
Ålesund & 667 & 318 & 22 \\
Sum & 39595 & 22039 & 444 \\
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
RegData <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = '2016-01-01',
                                                   datoTil = '2021-12-31'))

dataTilSKDE <- dataTilOffVisning(RegData=RegData,
                                 valgtVar = 'KomplIntra',
                                 OpMetode = 1,
                                 lastNedFil=1,
                                 indID = 'nger_kompl_intra_lap', filUt='KomplIntraLap')

dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplIntra',
                                 OpMetode = 2,
                                 lastNedFil=1,
                                 indID = 'nger_kompl_intra_hys', filUt='KomplIntraHys')

#Forekomsten av middels og alvorlige komplikasjoner etter  inngrep.
#Lap Hys
dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor',
                                 OpMetode = 1,
                                 lastNedFil=1,
                                 indID = 'nger_kompl_postop_lap', filUt='KomplPostopLap')

dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor',
                                 OpMetode = 2,
                                 lastNedFil=1,
                                 indID = 'nger_kompl_postop_hys', filUt='KomplPostopHys')

#Andel pasienter som har positiv eller svært positiv oppfatning av gynekologisk avdeling - UTGÅR!
# dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'Tss2Generelt',
#                                  lastNedFil=1,
#                                  indID = 'nger_pasient_tilfredshet', filUt='Tss2Generelt')

#Generell pasienttilfredshet
dataTilSKDE <- dataTilOffVisning(RegData=RegData,
                                 valgtVar = 'Tss2Sumskaar',
                                 aggData = 1,
                                 lastNedFil=1,
                                 indID = 'nger_pasient_tilfredshet',
                                 filUt='Tss2Sumskaar')


tapply(dataTilSKDE$var, dataTilSKDE$year, FUN='mean')*100


#--------------------------------Data og parametrekobling--------------------------
# Inndata til funksjon:
library(nger)
datoFra <- '2016-01-01'
datoFra1aar <- '2021-01-01'
datoTil <- '2021-12-31'
datoFra1Yoppf <- '2020-01-01'
datoTil1Yoppf <- '2020-12-31'

NGERData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
NGERData1aar <- NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil)

setwd('/home/rstudio/speil/aarsrapp/NGER' ) #"P:/Registerinfo og historie/NGER/aarsrapp/")

###### NB KAN VÆRE MYE TULL HER. FIKK MERGE-KONFLIKT!!!!!!!!!!!!!!!!!!

#-------------------------------Årsrapport 2020 og 21-------------------------------
#Figurer med RAND?

#--Tabeller:
#   Antall registreringer per år 2016-20
# Endres? Opphold per sykehus og operasjonstype (Lap Hys Begge) per år


NGERData <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil))
NGERData1aar <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))

setwd('/home/rstudio/speil/aarsrapp/NGER' ) #"P:/Registerinfo og historie/NGER/aarsrapp/")

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
#Fjernet for 2021: 'Diagnoser', 'KomplPostopType', 'LapEkstrautstyr', 'Opf0AlvorlighetsGrad',
variabler <- c('OpBMI', 'HysGjforingsGrad','HysKomplikasjoner',
              'LapIntraabdominell', 'LapKomplikasjoner', 'LapTeknikk',
              'Prosedyrer', 'RegForsinkelse', 'Tss2Generelt')
for (valgtVar in variabler) {
	outfile <- paste0(valgtVar, '_ford.pdf')
	NGERFigAndeler(RegData=NGERData1aar, valgtVar=valgtVar, datoTil=datoTil, outfile=outfile)
}

NGERFigAndeler(RegData=NGERData1aar, valgtVar='Diagnoser', datoTil=datoTil, OpMetode = 1,
               outfile='Diagnoser_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='Diagnoser', datoTil=datoTil, OpMetode = 2,
               outfile='Diagnoser_fordHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, valgtVar='Prosedyrer', datoTil=datoTil, OpMetode = 1,
               outfile='Prosedyrer_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='Prosedyrer', datoTil=datoTil, OpMetode = 2,
               outfile='Prosedyrer_fordHys.pdf')

# NGERFigAndeler(RegData=NGERData1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 1,
#                outfile='OpTid_fordLap.pdf')
# NGERFigAndeler(RegData=NGERData1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 2,
#                outfile='OpTid_fordHys.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='OpTid', datoTil=datoTil, OpMetode = 4,
               outfile='OpTid_fordTotLapHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, valgtVar='LapIntraabdominell', datoTil=datoTil, OpMetode = 4,
               outfile='LapIntraabdominell_fordTotLapHys.pdf')

NGERFigAndeler(RegData=NGERData1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 1,
               outfile='Opf0AlvorlighetsGrad_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 2,
               outfile='Opf0AlvorlighetsGrad_fordHys.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='Opf0AlvorlighetsGrad', datoTil=datoTil, OpMetode = 4,
               outfile='Opf0AlvorlighetsGrad_fordTotLapHys.pdf')

# Postoperative komplikasjoner Laparoskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
# NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 1,
#                outfile='KomplPostopType_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 1, AlvorlighetKompl = 1, outfile='KomplPostopType_fordLapAlv1.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 1, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordLapAlv234.pdf')

# Postoperative komplikasjoner Hysteroskopi, fordeling: *--lite alvorlige, *--moderat/ alvorlig
# NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 2,
#                outfile='KomplPostopType_fordHys.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 2, AlvorlighetKompl = 1, outfile='KomplPostopType_fordHysAlv1.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil,
               OpMetode = 2, AlvorlighetKompl = 2:4, outfile='KomplPostopType_fordHysAlv234.pdf')

NGERFigAndeler(RegData=NGERData1aar, valgtVar='KomplPostopType', datoTil=datoTil, OpMetode = 4,
               outfile='KomplPostopType_fordTotLapHys.pdf')

#Fordelingsfigurer: alder  og BMI på Laparoskopi, og på Hysteroskopi og på TLH (total laparaskopisk hysrektomi).

NGERFigAndeler(RegData=NGERData1aar, valgtVar='Alder', datoTil=datoTil, OpMetode = 1,
               outfile='Alder_fordLap.pdf')
NGERFigAndeler(RegData=NGERData1aar, valgtVar='Alder', datoTil=datoTil, OpMetode = 2,
               outfile='Alder_fordHys.pdf')
#NGERFigAndeler(RegData=NGERData1aar, valgtVar='Alder', datoTil=datoTil, OpMetode = 4,
#Fjernet, 2021: 'Diagnoser','KomplPostopType', 'LapEkstrautstyr', 'Opf0AlvorlighetsGrad','Prosedyrer',
variable <- c('OpBMI', 'HysGjforingsGrad','HysKomplikasjoner',
              'LapIntraabdominell', 'LapKomplikasjoner', 'LapTeknikk',
               'RegForsinkelse', 'Tss2Generelt')
for (valgtVar in variable) {
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
NGERFigKvalInd(RegData=NGERData1aar, valgtVar='TSS0', datoTil=datoTil,
                           outfile='TSS0_ford.pdf')
NGERFigKvalInd(RegData=NGERData1aar, valgtVar='TSS0', datoTil=datoTil, OpMetode = 2,
                             outfile='TSS0_fordHys.pdf')
NGERFigKvalInd(RegData=NGERData1aar, valgtVar='TSS0', datoTil=datoTil, OpMetode = 4,
                               outfile='TSS0_fordTotLapHys.pdf')
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

#FJeret for 2021: 'KomplIntra', 'KomplPostop', 'OpASA', 'Opf0Reoperasjon'
variable <- c('LapKonvertert')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Aar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
              outfile=outfile, tidsenhet='Aar')
}

NGERFigAndelTid(RegData=NGERData, valgtVar='OpDagkirurgi', datoFra=datoFra, datoTil=datoTil,
                OpMetode=1, Hastegrad=1, tidsenhet='Aar', outfile='OpDagkirLapEl_aar.pdf')

# NGERFigAndelTid(RegData=NGERData, valgtVar='OpAnestesi',   datoFra=datoFra, datoTil=datoTil, #datoFra
#                 OpMetode=2, Hastegrad=1, tidsenhet='Aar', outfile='OpAnestesiHysEl_aar.pdf')

# NGERFigAndelTid(RegData=NGERData, valgtVar='OpASA',   datoFra=datoFra, datoTil=datoTil, #datoFra
#                 OpMetode=4, tidsenhet='Aar', outfile='OpASATotLapHys_aar.pdf')



#--Laparoskopi
#Fjernet foir 2021: 'KomplPostop',
for (valgtVar in c('KomplIntra', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad',
                   'Opf0Reoperasjon','LapKonvertert')) {
  outfile <- paste0(valgtVar, '_', 'LapAar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                  OpMetode=1, outfile=outfile, tidsenhet='Aar')
}
#Postoperative komplikasjoner Laparoskopi, lite alvorlige, utvikling siste 4 år
NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar='Opf0AlvorlighetsGrad1', datoTil=datoTil,
                OpMetode=1, outfile='Opf0AlvorlighetsGrad1_LapAar.pdf', tidsenhet='Aar')

#--Hysteroskopi
#Fjernet foir 2021: 'KomplPostop',
for (valgtVar in c('KomplIntra', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad',
                   'Opf0Reoperasjon')) {
  outfile <- paste0(valgtVar, '_', 'HystAar.pdf')
  NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                  OpMetode=2, outfile=outfile, tidsenhet='Aar')
}
#NGERFigAndelTid(RegData=NGERData, datoFra=datoFra, valgtVar='Opf0AlvorlighetsGrad1', datoTil=datoTil,

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

#Fjernet for 2021:  'KomplPostop','OpDagkirurgi',
variable <- c( 'OpBMI', 'RegForsinkelse',
               'Tss2Mott', 'Tss2Behandling', 'Tss2Lytte', 'Tss2Behandlere', 'Tss2Enighet', 'Tss2Generelt')
for (valgtVar in variable) {
  outfile <- paste0(valgtVar, '_Shus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, valgtVar=valgtVar, outfile=outfile)
}

#Laparoskopi
#Fjernet for 2021: 'Alder', 'KomplPostop', 'OpBMI',
for (valgtVar in c('KomplIntra', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad', #
                   'Opf0Reoperasjon','LapKonvertert', 'LapKonvertertUventet', 'OpDagkirurgi')) {
  outfile <- paste0(valgtVar, '_LapShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,
                      OpMetode=1, outfile=outfile)
}


#--Hysteroskopi
#Fjernet for 2021: 'Alder', 'OpBMI',
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'Opf0AlvorlighetsGrad',
                   'Opf0Reoperasjon')) {
  outfile <- paste0(valgtVar, '_HystShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, valgtVar=valgtVar,
                      OpMetode=2, outfile=outfile)
}
#Fjernet 2021: 'Alder', 'OpBMI',
for (valgtVar in c('KomplIntra','KomplPostop', 'Opf0AlvorlighetsGrad1', 'KomplPostopAlvor',
                   'Opf0Reoperasjon')) {
  outfile <- paste0(valgtVar, '_HystShus.pdf')
  NGERFigAndelerGrVar(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar,
                      OpMetode=2, outfile=outfile)
}

#TLH
  NGERFigAndelerGrVar(RegData=NGERData1aar, valgtVar=valgtVar,
                      OpMetode=4, outfile='OpBMI_TLHShus.pdf')

#------------------------------ Sentralmål per sykehus --------------------------
# variable <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
#               'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
# variable <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
#               'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
# variable <- 'OpTid'
# variable <- 'RegForsinkelse'

<<<<<<< HEAD
#NGERData <- NGERRegDataSQL(datoFra = datoFra1Yoppf, datoTil = datoTil)
#'TSS2, sumskår' = 'Tss2Sumskaar'
for (valgtVar in c('Tss2Sumskaar')) {
  outfile <- paste0(valgtVar, '_' ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData1aar, valgtVar=valgtVar,
=======
#'TSS2, sumskår' = 'Tss2Sumskaar'
for (valgtVar in c('Tss2Sumskaar')) {
  outfile <- paste0(valgtVar, '_' ,'ShGjsn.pdf')
  NGERFigGjsnGrVar(RegData=NGERData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra1aar,
>>>>>>> rel
                      outfile=outfile)
}

for (OpMetode in c(1,2,4)) {
  outfile <- paste0('OpTid_', c('Lap','Hyst','', 'TLH')[OpMetode] ,'ShGjsn.pdf')
<<<<<<< HEAD
  NGERFigGjsnGrVar(RegData=NGERData1aar, valgtVar='OpTid',
=======
  NGERFigGjsnGrVar(RegData=NGERData1aar, valgtVar='OpTid', preprosess = 0,
>>>>>>> rel
                   OpMetode = OpMetode, outfile=outfile)
}

#Skal bare ha med Haugesund 701437, Bodø 706220, Trondheim 107644, Ullevål 700399 og Tønsberg 110734.
<<<<<<< HEAD
NGERDataUtvSh <- NGERData[which(NGERData$AvdRESH %in% c(701437, 706220, 107644, 700399, 110734)),]
NGERDataUtvSh <- NGERPreprosess(RegData = NGERDataUtvSh)

#Ønsker R0 og R1 for 2020
NGERFigGjsnGrVar(RegData=NGERDataUtvSh, valgtVar='R0ScoreGeneral', datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                 preprosess=0, outfile=paste0('R0ScoreGeneral_UtvalgteShGjsn.pdf'))
NGERFigGjsnGrVar(RegData=NGERDataUtvSh, valgtVar='R1ScoreGeneral', datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                 preprosess=0, outfile=paste0('R1ScoreGeneral_UtvalgteShGjsn.pdf'))



#KvalInd
for (valgtVar in c('kvalInd', 'RAND0', 'TSS0')) {
outfile <- paste0(valgtVar, '_' ,'KI.pdf')
NGERFigKvalInd(RegData=NGERData1aar, datoTil=datoTil,
                           valgtVar=valgtVar, outfile=outfile)
=======
NGERDataUtvSh1aar <- NGERData[which(NGERData1aar$ReshId %in% c(701437, 706220, 107644, 700399, 110734)),]

NGERFigGjsnGrVar(RegData=NGERDataUtvSh, valgtVar='R0ScoreGeneral', datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                 outfile=paste0('R0ScoreGeneralForrige_UtvalgteShGjsn.pdf'))
NGERFigGjsnGrVar(RegData=NGERDataUtvSh, valgtVar='R1ScoreGeneral', datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                  outfile=paste0('R1ScoreGeneral_UtvalgteShGjsn.pdf'))


#KvalInd
for (valgtVar in c('kvalInd', 'RAND0')) {
outfile <- paste0(valgtVar, '_' ,'KI.pdf')
NGERFigKvalInd(RegData=NGERData1aar, preprosess=0, valgtVar=valgtVar, outfile=outfile)
>>>>>>> rel
}

#Ønsker også Rand etter 1 år
 NGERFigKvalInd(RegData=NGERData, datoFra=datoFra1Yoppf, datoTil=datoTil1Yoppf,
                valgtVar='RAND1', outfile='RAND1_KI.pdf')


#------------------------------Tabeller-----------------------------------
library(xtable)
library(nger)
<<<<<<< HEAD
aarsRappAar <- 2021
datoFra1aar <- paste0(aarsRappAar, '-01-01')
datoTil <- paste0(aarsRappAar, '-12-31')
RegData <- NGERPreprosess(NGERRegDataSQL(datoFra = '2016-01-01', datoTil = datoTil))
RegData1aar <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))
=======
RegData <- NGERData
RegData <- NGERPreprosess(RegData)
>>>>>>> rel

#Antall registreringer siste 5 år
tabOpph <- tabAntOpphSh5Aar(RegData=RegData, datoTil=datoTil)
AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]), margin = 1)
xtable::xtable(tabAvdAarN, digits=0, align=c('l', rep('r',ncol(tabAvdAarN))),
               caption = 'Antall registrerte opphold',
               label = 'tab:AntRegAar')


#Tabell med antall registreringer for hvert sykehus, splittet på lap, hys og begge
<<<<<<< HEAD
tab <- table(RegData1aar[ ,c('ShNavn', "OpMetode")]) #, 'Aar'
dimnames(tab)$OpMetode <- c('Lap', 'Hys', 'Begge')
tab <- addmargins(tab, margin = 1)
xtable::xtable(tab, align=c('l', rep('r',ncol(tab))), digits=0)






# tabell <- cbind(tab[,,'2016'],
#                 ' ',
#                 tab[,,'2017'],
#                 ' ',
#                 tab[,,'2018'],
#                 ' ',
#                 tab[,,'2019'],
#                 ' ',
#                 tab[,,'2020'])

=======
RegData1aar <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil))
tab <- table(RegData[ ,c('ShNavn', "OpMetode")]) #, 'Aar'
dimnames(tab)$OpMetode <- c('Lap', 'Hys', 'Begge')
tab <- addmargins(tab, margin = 1)

xtable::xtable(tab, align=c('l', rep('r',ncol(tab))), digits=0)
>>>>>>> rel

# ggplot::ggplot(RegData, aes(OpMetode)) +
#   geom_histogram(bins = 3) +
#   facet_wrap(~ShNavn, ncol=5) +
#   ggtitle("Eksempel")


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
    kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
    #kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
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

NGERFigAndelTid(RegData=NGERData, datoFra='2015-01-01', valgtVar='Opf0KomplBlodning', datoTil=datoTil,
                reshID = 103162, enhetsUtvalg = 2, OpMetode=4, outfile='PostOpBlod_Sandnessj.pdf', tidsenhet='Aar')

#--------------------Data til SKDE/Resultaportalen------------------
#KomplIntra, KomplPostop, Opf0AlvorlighetsGrad
#OpMetode  1: Laparoskopi #2: Hysteroskopi, 2019: 5954 3146
library(nger)
RegData <- NGERPreprosess(RegData = NGERRegDataSQL(datoFra = '2016-01-01',
                                                   datoTil = '2020-12-31'))

# dataTilOffVisning(RegData = RegData, valgtVar, datoFra = '2014-01-01', aar=0,
#                               OpMetode=0,
#                               indID = 'indDummy', ResPort=0, lastNedFil=1, filUt='dummy')
#Forekomsten av komplikasjoner ved  inngrep.
#Lap Hys
# dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplIntra',
#                                  OpMetode = 1,
#                                  ResPort=0, lastNedFil=1,
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
                                 ResPort=0, lastNedFil=1,
                                 lastNedFil=1,
                                 indID = 'nger_kompl_intra_hys', filUt='KomplIntraHys')

#Forekomsten av middels og alvorlige komplikasjoner etter  inngrep.
#Lap Hys
dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor',
                                 OpMetode = 1,
                                 ResPort=0, lastNedFil=1,
                                 indID = 'nger_kompl_postop_lap', filUt='KomplPostopLap')

dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'KomplPostopAlvor', #'Opf0AlvorlighetsGrad', #
                                 OpMetode = 2,
                                 ResPort=0, lastNedFil=1,
                                 indID = 'nger_kompl_postop_hys', filUt='KomplPostopHys')

#Andel pasienter som har positiv eller svært positiv oppfatning av gynekologisk avdeling.
dataTilSKDE <- dataTilOffVisning(RegData=RegData, valgtVar = 'Tss2Generelt',
                                 #aar=2016:2019,
                                 ResPort=0, lastNedFil=1,
                                 indID = 'nger_pasient_tilfredshet', filUt='Tss2Generelt')
                                 # lastNedFil=1,
                                 # indID = 'nger_kompl_postop_lap', filUt='KomplPostopLap')

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

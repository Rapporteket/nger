#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERUtvalgEnh
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#' @param ind indekser fra enhetsUtvalg. Benyttes normalt ikke her, men trengs for
#' valgtVar Diagnoser og Prosedyrer
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NGERVarTilrettelegg  <- function(RegData, valgtVar, grVar='', ind=0, figurtype='andeler'){


  "%i%" <- intersect

  #----------- Figurparametre ------------------------------
  cexgr <- 1	#Kan endres for enkeltvariable
  retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
  flerevar <- 0
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  #grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  #grNavn <- ''
  varTxt <- ''
  xAkseTxt <- ''	#Benevning
  #yAkseTxt <- ''
  subtxt <- ''
  #txtEtiketter  <- ''	#legend
  #verdier <- ''	#AggVerdier, gjennomsnitt, ...
  #verdiTxt <- '' 	#pstTxt, ...
  #strIfig <- ''		#cex
  sortAvtagende <- TRUE  #Sortering av resultater
  KImaal <- NA
  tittel <- 'Mangler tittel'
  variable <- 'Ingen'
  #deltittel <- ''
  #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller
  #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
  #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???


  #--------------- Definere variable ------------------------------
  #Variabeltyper: Numeriske, kategoriske, indikator
  # For hver valgtVar:
  # Definer og gjør utvalg for variabelen
  # tittel, xAkseTxt, sortAvtagende (standard: TRUE)

  tittel <- '' #I andelGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen

  #-------------------------------------
  RegData$Variabel <- 0

  TSS0var <- c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',	'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')
  Rand0var <- c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',	'R0ScoreEmo',
                'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')
  if (valgtVar %in% c(TSS0var, Rand0var)) {
   RegData <- RegData[RegData$InnDato >= '2016-01-01', ]}


  if (valgtVar=='Alder') {	#Andeler, , #andelGrVar, GjsnGrVar, GjsnTid
    RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
    xAkseTxt <- 'alder (år)'
    tittel <- switch(figurtype,
                     andeler = 'Aldersfordeling',
                     andelGrVar = 'Pasienter over 70 år',
                     andelTid = 'Pasienter over 70 år',
                     gjsnGrVar = 'alder ved innleggelse',
                     gjsnTid = 'alder ved innleggelse')
    gr <- c(0, seq(15, 80, 5), 120)
    RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('<15', levels(RegData$VariabelGr)[2:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper (år)'
	varTxt <- 'pasienter >=70år'
    RegData$Variabel[which(RegData$Alder >= 70)] <- 1
    if (figurtype == 'gjsnGrVar') {
      RegData$Variabel <- RegData$Alder}
    retn <- 'V'
    sortAvtagende <- FALSE
    cexgr <- 0.8
  }

  if (valgtVar=='HysGjforingsGrad') {   #Andeler
    #Gjennomføringsgrad av hysteroskopi
    #Kode •	1-Fullstendig, 2-Ufullstendig, 3-Mislykket
    RegData <- RegData[which(RegData$OpMetode == 2), ]
    grtxt <- c('Fullstendig', 'Ufullstendig', 'Mislykket')
    tittel <- 'Gjennomføringsgrad av hysteroskopi'
    koder <- 1:3
    RegData <- RegData[which(RegData$HysGjforingsGrad %in% koder), ]
    RegData$VariabelGr <- factor(RegData$HysGjforingsGrad, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }

    if (valgtVar=='HysKonvertert') { #andelGrVar, andelTid
    RegData <- RegData[intersect(which(RegData$HysKonvertert %in% 0:1), which(RegData$HysStatus == 1)), ]
    RegData$Variabel <- RegData$HysKonvertert
    mean(RegData$Variabel, na.rm = T)*100
    varTxt <- 'konverterte'
    tittel <- 'Konvertert hysteroskopi til laparotomi/-skopi'
  }

  if (valgtVar=='KomplIntra') { #andelGrVar, andelTid
    # Komplikasjoner ved operasjon. Må kombinere HysKomplikasjoner og LapKomplikasjoner
    # Noen få Hys-skjema ikke ferdigstilt ved begge (OpMetode=3). Lar disse stå.
    # Bare ferdigstilte skjema ellers, så filtrerer ikke på ferdigstilte.
    #Kode 0: Nei, 1:Ja, tomme
    indVar <- which((RegData$LapKomplikasjoner==1) | (RegData$HysKomplikasjoner==1))
    #Alle med en NA faller bort. RegData$KomplIntra <- with(RegData, HysKomplikasjoner + LapKomplikasjoner, na.rm=T) #Får mange tomme!!!
    # if (OpMetode %in% c(1,4:6)) {
    #   indMed <- which(RegData$LapKomplikasjoner %in% 0:1)
    #   indVar <- which(RegData$LapKomplikasjoner == 1)
    # } else {
    #   indMed <- switch(as.character(OpMetode),
    #                    '2' = which(RegData$HysKomplikasjoner %in% 0:1),
    #                    '3' = which(RegData$KomplIntra %in% 0:1),	#Få tomme for dette valget
    #                    '99' = union(which(is.finite(RegData$HysKomplikasjoner)), which(is.finite(RegData$LapKomplikasjoner))))
    #   indVar <- switch(as.character(OpMetode),
    #                    '2' = which(RegData$HysKomplikasjoner == 1),
    #                    '3' = which(RegData$KomplIntra %in% 1:2),
    #                    '99' = union(which(RegData$HysKomplikasjoner == 1), which(RegData$LapKomplikasjoner==1)))
    # }
    RegData$Variabel[indVar] <- 1
    #RegData <- RegData[indMed, ]
    varTxt <- 'komplikasjoner'
    tittel <- 'Komplikasjoner, intraoperativt'
  }
  if (valgtVar=='KomplPostop') { #andelGrVar, andelTid
    # Andel postoperative komplikasjoner
    #Kode 0: Nei, 1:Ja, tomme
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    RegData$Variabel <- RegData$Opf0Komplikasjoner
	varTxt <- 'komplikasjoner'
    tittel <- 'Komplikasjoner, postoperativt'
  }
  if (valgtVar=='LapKonvertert') { #andelTid
    RegData <- RegData[intersect(which(RegData$LapKonvertert %in% 0:1), which(RegData$LapStatus == 1)), ]
    RegData$Variabel <- RegData$LapKonvertert
    varTxt <- 'konverterte'
    tittel <- 'Konvertert lapraskopi til laparotomi'
  }
  if (valgtVar == 'LapNumHjelpeinnstikk') {   #Andeler
    # Velge antall fra 0 til 6
    #IKKE gjort noen utvalg. (StatusLap==1?, LapHjelpeinnstikk==1?)
    tittel <- 'Antall hjelpeinnstikk, laparaskopi'
    grtxt <- 0:6
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = grtxt)
  }

  if (valgtVar == 'Norsktalende') {   #Andeler
    # 0:Nei, 1:Ja, 2:Delvis, 9:Ukjent
    tittel <- 'Pasient forstår og gjør seg forstått på norsk'
    grtxt <- c('Nei', 'Ja', 'Delvis', 'Ukjent')
    koder <- c(0:2,9)
    RegData <- RegData[which(RegData$Norsktalende %in% koder), ]
    RegData$VariabelGr <- factor(RegData$Norsktalende, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }
  if (valgtVar == 'OpAnestesi') {   #Andeler, andelGrVar, andelTid
    # 1-Ingen, 2-Lokal, 3-Generell, 4-Spinal, 5-Annet
    tittel <- 'Anestesitype ved endoskopiske inngrep'
    grtxt <- c('Ingen', 'Lokal', 'Generell', 'Spinal', 'Annet')
    koder <- 1:5
    retn <- 'H'
    RegData <- RegData[RegData$OpAnestesi %in% koder, ]
    RegData$VariabelGr <- factor(RegData$OpAnestesi, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      tittel <- 'Lokalbedøvelse'
      varTxt <- 'som har fått lokalbedøvelse'
      RegData$Variabel[RegData$OpAnestesi == 2] <- 1
    }
  }
  if (valgtVar=='OpAntibProfylakse') { #andelGrVar #andelTid
    #Andel som får antibiotika
    #Kode 0,1: Nei, Ja (ingen tomme per 22.feb.2016)
    RegData <- RegData[which(RegData$OpAntibProfylakse %in% 0:1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
	varTxt <- 'profylakser'
    tittel <- 'Andel som får antibiotikaprofylakse'
  }

  if (valgtVar == 'OpASA') {   #Andeler, andelGrVar, andelTid
    koder <- 1:5
    grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende')
    subtxt <- 'Sykdomsgrad'
    tittel <-  switch(figurtype,
                      andeler ='ASA-gruppe',
                      andelGrVar = 'ASA-grad > II',
					  andelTid = 'ASA-grad > II')
    retn <- 'H'
    RegData <- RegData[RegData$OpASA %in% koder, ]
    RegData$VariabelGr <- factor(RegData$OpASA, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    #Andel med ASA-grad>2
    RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
  }

  if (valgtVar == 'OpDagkirurgi') {   #Andeler, AndelGrVar, AndelTid
    #0: Nei, 1: Ja Manglende:Ukjent
    tittel <- 'Dagkirurgiske Inngrep'
    grtxt <- c('Nei', 'Ja')
    koder <- 0:1
    RegData <- RegData[which(RegData$OpDagkirurgi %in% koder), ]
    RegData$VariabelGr <- factor(RegData$OpDagkirurgi, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    varTxt <- 'dagkirurgiske inngrep'
    RegData$Variabel <- RegData$OpDagkirurgi
  }
  if (valgtVar=='Opf0AlvorlighetsGrad') {   #Andeler, andelGrVar
    #Postoperative komplikasjoner
    #Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
    RegData <- RegData[(which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner %in% 0:1)), ]
    grtxt <- c('Ingen kompl.', 'Lite alvorlig', 'Middels alvorlig', 'Alvorlig', 'Dødelig')
    koder <- 1:4
    retn <- 'H'
    if (figurtype == 'andeler') {
      indVar <- which(RegData[ ,valgtVar] %in% koder)
      RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
      RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,koder), labels = grtxt) #levels=c(nivaa,9)
      tittel <- 'Alvorlighetsgrad av komplikasjoner'
    }
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      #Andel av postoperative komplikasjoner som var moderate 2 eller alvorlige (3 og 4)
      RegData$Variabel[which(RegData$Opf0AlvorlighetsGrad %in% 2:4)] <- 1
	  varTxt <- 'komplikasjoner grad 2-4'
      tittel <- 'Postop. komplikasjon, moderat/alvorlig'
    }
  }

  if (valgtVar=='Opf0KomplBlodning') { #andelGrVar, andelTid
    #Kode 0: Nei, 1:Ja
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    RegData$Variabel[which(RegData$Opf0KomplBlodning == 1)] <- 1
	varTxt <- 'blødninger'
    tittel <- 'Postop. komplikasjon: Blødning'
  }
  if (valgtVar=='Opf0KomplUtstyr') { #andelGrVar, andelTid
    #Kode 0: Nei, 1:Ja
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    RegData$Variabel[which(RegData$Opf0KomplUtstyr == 1)] <- 1
	varTxt <- 'tilfeller av problem med utstyr'
    tittel <- 'Postop. komplikasjon: Problemer med ustyr'
  }
  if (valgtVar=='Opf0KomplOrgan') { #andelGrVar, andelTid
    #Kode 0: Nei, 1:Ja
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    RegData$Variabel[which(RegData$Opf0KomplOrgan == 1)] <- 1
	varTxt <- 'organskader'
    tittel <- 'Postop. komplikasjon: Organskade'
  }

  if (valgtVar=='Opf0Reoperasjon') { #andelGrVar
	#Reoperasjon som følge av komplikasjon
    #Kode 0: Nei, 1:Ja
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    RegData$Variabel[which(RegData$Opf0Reoperasjon == 1)] <- 1
	varTxt <- 'reoperasjoner'
    tittel <- 'Postop. komplikasjon: Reoperasjon'
  }
  if (valgtVar == 'OpIVaktTid') {   #Andeler
    #0: Nei, 1: Ja Manglende:Ukjent
    tittel <- 'Operasjon i vakttid'
    grtxt <- c('Nei', 'Ja')
    koder <- 0:1
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
    RegData$VariabelGr <- factor(RegData$OpIVaktTid, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }
  if (valgtVar == 'OpKategori') {   #Andeler
    RegData$Variabel <- 99
    # 1:Elektiv, 2:Akutt, 3:Øyeblikkelig hjelp
    tittel <- 'Operasjonskategori'
    grtxt <- c('Elektiv', 'Akutt', 'Ø-hjelp')
    koder <- 1:3
    indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
    RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }
  if (valgtVar == 'OpMetode') {   #Andeler
    #1:Laparoskopi, 2:Hysteroskopi, 3:Begge
    tittel <- 'Operasjonsmetode'
    koder <- 1:3
    grtxt <- c('Laparoskopi', 'Hysteroskopi', 'Begge')
    retn <- 'V'
    RegData <- RegData[RegData$OpMetode %in% koder, ]
    RegData$VariabelGr <- factor(RegData$OpMetode, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }
  if (valgtVar %in% c('OpTidlVagInngrep', 'OpTidlLapsko', 'OpTidlLaparotomi')) {   #Andeler
    # 0: Nei, 1: Ja, 9: Vet ikke
    RegData$Variabel <- 99
    tittel <- sprintf('Tidligere %s', switch(as.character(valgtVar),
                                             'OpTidlVagInngrep' = 'vaginale inngrep',
                                             'OpTidlLapsko' = 'laparoskopiske inngrep',
                                             'OpTidlLaparotomi' = 'laparatomi'))
    grtxt <- c('Nei', 'Ja', 'Vet ikke/Ukjent')
    koder <- 0:1
    indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
    RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
  }

  if (valgtVar == 'OpType') {   #Andeler
    # 1:Primærinngrep, 2:Reoperasjon
    RegData$Variabel <- 99
    tittel <- 'Operasjonstype'
    grtxt <- c('Primærinngrep', 'Reoperasjon') #, 'Ukjent')
    koder <- 1:2
    indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
    RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=koder, labels = grtxt) #levels=c(nivaa,9)
  }

  if (valgtVar == 'SivilStatus') {   #Andeler
    #    # 1:Enslig, 2:Særboer, 3:Samboer, 4:Gift, 5:Skilt, 6:Enke, 9:Ukjent
    tittel <- 'Sivilstatus'
    grtxt <- c('Enslig', 'Særboer', 'Samboer', 'Gift', 'Skilt', 'Enke', 'Ukjent')
    koder <- c(1:6,9)
    RegData <- RegData[RegData$SivilStatus %in% koder, ]
    RegData$VariabelGr <- factor(RegData$SivilStatus, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    retn <- 'H'
  }

  ### Numeriske variable:

  if (valgtVar == 'OpBMI') {   #Andeler, #andelGrVar, andelTid
    # 1:Alvorlig undervekt,2:moderat undervekt, 3:mild undervekt, 4:normal vekt, 5:overvekt,
    # 6:fedme kl.I, 7:fedme kl.II, 8:fedme kl.III
    tittel <- 'BMI-kategorier' #, Slå sammen undervekt, fedme 2 og 3.
    #grtxtAlle <- c('Undervekt','Undervekt','Undervekt','Normal vekt', 'Overvekt', 'Fedme kl.I',
    #	'Fedme kl.II&III', 'Fedme kl.II&III' 'Ukjent')
    #mapvalues(RegData$OpBMIKategori, from = 1:8, to = grtxtAlle)
    #       RegData$OpBMIKategori <- plyr::revalue(as.character(RegData$OpBMIKategori), c('1'='1', '2'='1', '3'='1', '4'='2', '5'='3', '6'='4', '7'='5', '8'='5'))
    gr <- c(-1, 0, 18.5, 25, 30, 35, 1000)
    ind <- which(RegData$OpBMI>0)
    RegData$Dummy <- -1
    RegData$Dummy[ind] <- RegData$OpBMI[ind]
    RegData$VariabelGr <- cut(RegData$Dummy, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('Ukjent', 'Undervekt','Normalvekt', 'Overvekt', 'Fedme kl.I', 'Fedme kl.II&III')
		if (figurtype %in% c('andelGrVar','andelTid')) {
		  #BMI > 30
		  RegData <- RegData[which(RegData[,valgtVar] >10), ]
		  RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
		  varTxt <- 'med BMI>30'
		  tittel <- 'Pasienter med fedme (BMI > 30)'
		}
    retn <- 'V'
  }

  if (valgtVar == 'Opf0metode') {   #Andeler, andelGrVar
    # Oppfølging Post, telefon, ikke mulig
    tittel <- switch(figurtype,
                     andeler= 'Oppfølgingsmetode',
                     andelGrVar = 'Oppfølging: Ikke mulig')
    gr <- c(1,2,9)
    grtxt <- c('post', 'telefon', 'ikke mulig')
    RegData <- RegData[which(RegData$Opf0Status==1) %i% which(RegData$Opf0metode %in% gr), ]
    RegData$Variabel[RegData$Opf0metode==9] <- 1
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = gr)
  }

  if (valgtVar=='Opf0Status') { #andelGrVar, andelTid
    #Andel med RegData$Opf0metode %in% 1:2 (av samtlige, også tomme reg. 9-oppf. ikke mulig)
    #Kode: tomme, -1,0,1
    #Tar ut hendelser siste 8 uker:
    datoTil <- min(as.POSIXlt(datoTil), as.POSIXlt(Sys.Date() - 8*7))

    RegData$Variabel[RegData$Opf0metode %in% 1:2] <- 1 #Opf0Status==1
    varTxt <- 'av postoperativ oppfølging'
    tittel <- 'Pasienter som har svart på oppfølging'
  }
  #Lag figur for ett års oppfølging

  if (valgtVar == 'OpTid') {   #Andeler, andelTid, andelGrVar, gjsnGrVar,
    #0-20, 21-40, 41-60, 61-80, 81-100, 101-120, 121-140, 141-160, 161-180, 181-200, 201-220, 221-240, > 240
    tittel <- 'Operasjonstid'
    gr <- c(seq(0, 180, 20), 1000) #c(seq(0, 180, 30), 1000) #
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '180+')
    if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
      tittel <- 'operasjonstid'
      RegData$Variabel <- RegData$OpTid
    }
    sortAvtagende <- F
    subtxt <- 'minutter'
    cexgr <- 0.9
    retn <- 'V'
    if (figurtype %in% c('andelGrVar','andelTid')) {
      #BMI > 30
      RegData$Variabel[which(RegData[ ,valgtVar] > 60)] <- 1
      varTxt <- 'med op.tid > 60 min.'
      tittel <- 'Operasjonstid over 60 minutter'
    }
  }
  if (valgtVar == 'RegForsinkelse') {  #Andeler, GjsnGrVar
    #!!!!!!!!!!!Leveringsdato vil oppdateres ved reåpning og kan derfor ikke brukes
    #Verdier: 0-3402
    RegData$Diff <- as.numeric(as.Date(RegData$Leveringsdato) - as.Date(RegData$InnDato)) #difftime(RegData$InnDato, RegData$Leveringsdato) #
    RegData <- RegData[which(RegData$OpStatus==1) %i% which(RegData$Diff > -1), ]
    tittel <- switch(figurtype,
                     andeler='Tid fra operasjon til ferdigstilt registrering',
                     andelGrVar = 'Mer enn 4 uker fra operasjon til ferdig registrering')
    RegData$Variabel[RegData$Diff > 4*7] <- 1
    subtxt <- 'døgn'
    gr <- c(0,1,7,14,30,90,365,5000) #gr <- c(seq(0, 90, 10), 1000)
    RegData$VariabelGr <- cut(RegData$Diff, breaks = gr, include.lowest = TRUE, right = TRUE)
    #plot(RegData$VariabelGr)
    grtxt <- c('<= 1', '(1-7]', '(7-14]', '(14-30]', '(30-90]', '(90-365]', '>365')
    #grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '>90')
    cexgr <- 0.9
  }

  if (valgtVar == 'R0ScorePhys') {  #Andeler, #GjsnGrVar
    #Verdier: 0:5:100
    RegData$Variabel <- RegData$R0ScorePhys
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData$R0ScorePhys > -1), ] #Evt. R0Metode in 1:2
    tittel <- 'Fysisk funksjon'
    if (figurtype == 'gjsnGrVar') {tittel <- 'fysisk funksjon'}
    subtxt <- 'sumskår (høyest er best)'
    gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
    RegData$VariabelGr <- cut(RegData$R0ScorePhys, breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
  }
  if (valgtVar == 'R0ScoreRoleLmtPhy') { #Andeler, #GjsnGrVar
    #Verdier: 0:25:100
    RegData$Variabel <- RegData[ ,valgtVar]
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData$Variabel > -1), ]
    tittel <- 'Rollebegrensning grunnet fysisk helse'
    gr <- c(0,25,50,75,100) #seq(0, 100, 25) #c(seq(0, 100, 25), 100) #c(seq(0, 180, 30), 1000) #
    grtxt <- gr #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
    subtxt <- 'sumskår (høyest er best)'
    RegData$VariabelGr <- factor(RegData$Variabel, levels=grtxt) #cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
  }
  if (valgtVar == 'R0ScoreRoleLmtEmo') {  #Andeler, #GjsnGrVar
    #Verdier: 0:33.3:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Følelsesmessig rollebegrensning'
    subtxt <- 'sumskår (høyest er best)'
    gr <-c(0, 30, 65, 70, 100) #seq(0, 100, 33) #c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
    grtxt <- c(0,33,67,100) #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = TRUE, right = TRUE)
    #RegData$VariabelGr <- as.factor(RegData$R0ScoreRoleLmtEmo) #, levels=grtxt) #cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
  }

  if (valgtVar == 'R0ScoreEnergy') { #GjsnGrVar, andeler
    #Verdier: 0:5:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData$R0ScoreEnergy > -1), ]
    RegData$Variabel <- RegData$R0ScoreEnergy
    tittel <- 'Energinivå/vitalitet'
    gr <- seq(0, 100, 20)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- levels(RegData$VariabelGr)
    subtxt <- 'sumskår (høyest er best)'
  }
  if (valgtVar == 'R0ScoreEmo') { #Andeler#GjsnGrVar
    #Verdier: 0:4:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Mental helse'
    subtxt <- 'sumskår (høyest er best)'
    gr <- seq(0, 100, 20)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
  }
  if (valgtVar == 'R0ScoreSosial') { #Andeler#GjsnGrVar
    #Verdier: 0:12.5:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Sosial funksjon'
    #gr <- c(seq(0, 75, 25), 100) #c(seq(0, 180, 30), 1000) #
    #RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    subtxt <- 'sumskår (høyest er best)'
    gr <- seq(0, 100, 25)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- levels(RegData$VariabelGr)
    #grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
  }
  if (valgtVar == 'R0ScorePain') { #Andeler#GjsnGrVar
    #Verdier: 0:2.5?:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Smerte'
    #gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
    #grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
    subtxt <- 'sumskår (høyest er best)'
    gr <- seq(0, 100, 20)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- levels(RegData$VariabelGr)
  }
  if (valgtVar == 'R0ScoreGeneral') { #Andeler, #GjsnGrVar
    #Verdier: 0:5:100
    RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Generell helsetilstand'
    #gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
    #RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    #grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
    subtxt <- 'sumskår (høyest er best)'
    gr <- seq(0, 100, 20)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
    grtxt <- levels(RegData$VariabelGr)

  }

  #Tss2Type  FOLLOWUP_TYPE	Oppfølgingsmetode	["Oppfølging pr post/brev","Oppfølging pr telefonintervju","Oppfølging ikke mulig"]
  if (valgtVar == 'Tss2Mott') {   #Andeler, andelGrVar
    #Spm.1, Sverige
    #0:Mindre godt, 1:Ingen mening, 2:Ganske godt, 3:Svært godt
    tittel <- switch(figurtype,
                     andeler = 'Hvordan ble du møtt på gynekologisk avdeling?',
                     gjsnGrVar = 'Hvordan ble du møtt på gynekologisk avdeling?',
                     andelGrVar = 'Møtet med gynekologisk avdeling var svært godt')
    grtxt <- c('Mindre godt','Ingen mening','Ganske godt','Svært godt')
    koder <- 0:3
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ] #Evt: Tss2Status == 1, Tss2Type in 1:2
    RegData$VariabelGr <- factor(RegData$Tss2Mott, levels=koder, labels = grtxt)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Mott == 3)] <- 1 }
      if (figurtype == 'gjsnGrVar') {
        RegData$Variabel <- RegData$Tss2Mott}
  }

  if (valgtVar == 'Tss2Behandling') {   #Andeler#andelGrVar
    #Spm.2, Sverige
    #0:Passet ikke, 1:Verken eller, 2:Ganske bra, 3:Svært bra
    tittel <- switch(figurtype,
                     andeler = 'Hvordan passet behandlingens opplegg og innhold for deg?',
                     gjsnGrVar = 'Hvordan passet behandlingens opplegg og innhold for deg?',
                     andelGrVar = 'Behandlingens opplegg/innhold passet svært bra')
    grtxt <- c('Passet ikke','Verken eller','Ganske bra','Svært bra')
    koder <- 0:3
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder) %i% which(RegData$Tss2Status == 1), ]
    RegData$VariabelGr <- factor(RegData$Tss2Behandling, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Behandling == 3)] <- 1}
    if (figurtype == 'gjsnGrVar') {
      RegData$Variabel <- RegData$Tss2Behandling}
  }

  if (valgtVar == 'Tss2Lytte') {   #Andeler, #andelGrVar
    #Spm.3, Sverige
    #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
    tittel <- switch(figurtype,
                     andeler = 'Lyttet og forsto dine behandlere det du tok opp?',
                     gjsnGrVar = 'Lyttet og forsto dine behandlere det du tok opp?',
                     andelGrVar = 'I svært stor grad lyttet og forsto behandlerne')
    grtxt <- c("Nei","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
    koder <- 0:3
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder) %i% which(RegData$Tss2Status == 1), ]
    RegData$VariabelGr <- factor(RegData$Tss2Lytte, levels=koder, labels = grtxt)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Lytte == 3)] <- 1}
    if (figurtype == 'gjsnGrVar') {
      RegData$Variabel <- RegData$Tss2Lytte}
  }
  if (valgtVar == 'Tss2Behandlere') {   #Andeler, andelGrVar
    #Spm.4, Sverige
    #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
    tittel <- switch(figurtype,
                     andeler = 'Hadde du tillit til dine behandlere på gynekologisk avd.?',
                     gjsnGrVar = 'Hadde du tillit til dine behandlere på gynekologisk avd.?',
                     andelGrVar = 'Pasienten hadde svært stor tillit til sine behandlere')
    grtxt <- c("Nei, ikke tilstrekkelig","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
    koder <- 0:3
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder) %i% which(RegData$Tss2Status == 1), ]
    RegData$VariabelGr <- factor(RegData$Tss2Behandlere, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Behandlere == 3)] <- 1 }
      if (figurtype == 'gjsnGrVar') {
        RegData$Variabel <- RegData$Tss2Behandlere}
  }
if (valgtVar == 'Tss2Enighet') {   #Andeler, #andelGrVar
  #Spm.5, Sverige
  #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
    tittel <- switch(figurtype,
                     andeler = 'Var du og dine behandlere enige om målsettingen for din behandling?',
                     gjsnGrVar = 'Var du og dine behandlere enige om målsettingen for din behandling?',
                     andelGrVar = 'Pasient og behandlere svært enige om målsetn. for behandlinga')
    grtxt <- c("Nei","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
    koder <- 0:3
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder) %i% which(RegData$Tss2Status == 1), ]
    RegData$VariabelGr <- factor(RegData$Tss2Enighet, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Enighet == 3)] <- 1 }
    if (figurtype == 'gjsnGrVar') {
      RegData$Variabel <- RegData$Tss2Enighet}
  }
  if (valgtVar == 'Tss2Generelt') {   #Andeler, andelGrVar
    #Spm.6, Sverige
    #0:Svært negativ, 1:Negativ, 2:Nøytral, 3:Positiv, 4:Svært positiv
    tittel <- switch(figurtype,
                     andeler = 'Hvilken oppfatning har du om gynekologisk avdeling generelt?',
                     gjsnGrVar = 'skår, oppfatning om gynekologisk avdeling generelt',
                     andelGrVar = 'Svært positiv oppfatning om gyn. avd.')
    grtxt <- c("Svært negativ","Negativ","Nøytral","Positiv","Svært positiv")
    koder <- 0:4
    retn <- 'H'
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder) %i% which(RegData$Tss2Status == 1), ]
    RegData$VariabelGr <- factor(RegData$Tss2Generelt, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype == 'andelGrVar') {
      RegData$Variabel[which(RegData$Tss2Generelt == 4)] <- 1 }
    if (figurtype == 'gjsnGrVar') {
      RegData$Variabel <- RegData$Tss2Generelt-1}
  }

  if (valgtVar == 'Tss2Sumskaar') {   #Andeler, #gjsnGrVar
    #Stort sett: 0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
    #Alle variable må besvares for å kunne ferdigstille skjema.
    RegData <- RegData[which(RegData$Tss2Status == 1) %i% which(RegData$Tss2Type %in% 1:2), ]
    #TSSsumskaar
    #RegData$Test <- (RegData$Tss2Score-1)/6 OK
    RegData$Variabel <- (rowSums(RegData[ ,c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte',
                                          'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt')])-1)/6
#   head(RegData[, c('Tss2Mott',	'Tss2Behandling',	'Tss2Lytte', 'Tss2Behandlere',	'Tss2Enighet',	'Tss2Generelt', 'Variabel')])
    gr <- c(-1:3)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=F, right=T)
    grtxt <- c('0', levels(RegData$VariabelGr)[2:(length(gr)-1)])
    #retn <- 'H'
    tittel <- 'TSS2, gjennomsnittlig sumskår'
  }

  if (valgtVar == 'Utdanning') {   #Andeler
    # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
    #PasientSkjema. Andel med Utdanning 4 el 5

    tittel <- switch(figurtype,
                     andeler = 'Utdanningsnivå',
                     andelGrVar = 'Andel uten høyere utdanning',
					 andelTid = 'Andel uten høyere utdanning')
    grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år',
               'Ukjent')
    koder <- c(1:5,9)
    retn <- 'H'
    RegData <- RegData[which(RegData$Utdanning %in% koder), ]
    RegData$VariabelGr <- factor(RegData$Utdanning, levels=koder, labels = grtxt) #levels=c(nivaa,9)
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[which(RegData$Utdanning %in% 1:5), ]		#which(RegData$PasientSkjemaStatus ==1)), ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:3)] <- 1
      varTxt <- 'uten høyere utdanning'
	  }
  }


  #-------------- SAMMENSATTE variable
  #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
  #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
  # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
  # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
  # som 0.
  #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
  #(Alternativt kan vi gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen)

  if (valgtVar=='Diagnoser') { #Tilfelle hvor man heller endrer format på variablene...?
    #Gammel kommentar?: PER NÅ FEIL. SAMME DIAGNOSE KAN VÆRE FØRT OPP FLERE GANGER FOR SAMME PASIENT.
    #Tar unique for hver rad. Antar dette er for å ta høyde for at sm. diag oppf. flere ganger.
    tittel <- 'Hyppigst forekommende diagnoser'
    diagLap <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3')
    diagHys <- c('HysDiagnose1', 'HysDiagnose2', 'HysDiagnose3')
    var <- c(diagLap, diagHys)
    ant <- 20
    cexgr <- 1-0.005*ant
    #Når bare utført lap el hys:
    ind1 <- which(RegData$OpMetode %in% 1:2)
    ind2 <- which(RegData$OpMetode == 3)
    AlleDiag1 <- unlist(apply(as.matrix(RegData[intersect(ind$Hoved, ind1), c(diagLap, diagHys)]),
                              1, FUN=unique))
    AlleDiag2L <- unlist(apply(as.matrix(RegData[intersect(ind$Hoved, ind2), diagLap]), 1, FUN=unique))
    AlleDiag2H <- unlist(apply(as.matrix(RegData[intersect(ind$Hoved, ind2), diagHys]), 1, FUN=unique))
    AlleDiag <- c(AlleDiag1, AlleDiag2L, AlleDiag2H)
    AlleDiagSort <- sort(table(AlleDiag[which(AlleDiag != '')]), decreasing = TRUE)
    grtxt <- names(AlleDiagSort)[1:min(length(AlleDiagSort), ant)]	#Alle diagnoser som skal være med. Kan benyttes til å lage indeks...
    variable <- grtxt
    nymatr <- as.data.frame(matrix(0,dim(RegData)[1],ant))
    names(nymatr) <- grtxt
    for (k in grtxt) {
      nymatr[rowSums(RegData[ ,var]== k, na.rm = T)>0, k] <- 1
    }
    RegData <- data.frame(RegData,nymatr)
  }
  if (valgtVar=='Prosedyrer') {
    tittel <- 'Hyppigst forekommende prosedyrer'
    #RegData$Opf0Status == 1 OK
    #Hver prosedyre skal telles bare en gang per operasjon - unique på hver linje.
    prosVar <- c('HysProsedyre1', 'HysProsedyre2', 'HysProsedyre3', 'LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
    #AllePros <- toupper(as.vector(as.matrix(RegData[ , prosVar])))
    AllePros <- toupper(unlist(apply(as.matrix(RegData[ind$Hoved, prosVar]), 1,FUN=unique)))
    #Må fjerne tomme. Tomme behandles som tomme lokalt, men NA på server.
    AlleProsSort <- sort(table(AllePros[which(AllePros != '')]), decreasing = TRUE)
    ant <- 20
    cexgr <- 1-0.005*ant
    grtxt <- names(AlleProsSort)[1:min(length(AlleProsSort), ant)]
    variable <- grtxt
    nymatr <- as.data.frame(matrix(0,dim(RegData)[1],ant))
    names(nymatr) <- grtxt
    for (k in grtxt) {
      nymatr[rowSums(RegData[ ,prosVar]== k, na.rm = T)>0, k] <- 1
    }
    RegData <- data.frame(RegData,nymatr)

    #AntVar <- AlleProsSort[1:ant]
    #NVar <- dim(RegData)[1]
    #N <- NVar
  }
  if (valgtVar == 'inklKrit' ) {   # Andeler
    tittel <- 'Inklusjonskriterier, Rygg'
    #RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2016-01-01')), ]
    sortAvtagende <- T
    retn <- 'H'
    flerevar <- 1
    variable <- c('MoreThan24Hours',  'MechanicalRespirator', 'DeadPatientDuring24Hours',
                  'MovedPatientToAnotherIntensivDuring24Hours', 'VasoactiveInfusion' )
    #retn <- 'H'
    grtxt <- c('Liggetid over 24t', 'Mekanisk \nrespirasjonsstøtte', 'Død innen 24t',  'Overflyttet innen 24t',
               'Kontinuerlig \nvasoaktiv infusjon')
    ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
    ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
    RegData[ ,variable] <- NA
    RegData[ ,variable][ind01] <- 0
    RegData[ ,variable][ind1] <- 1
    xAkseTxt <- 'Andel opphold (%)'
    #Beregne direkte:
    #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
  }
  #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
  if (valgtVar %in% c('Diagnoser', 'KomplPostopType', 'HysKomplikasjoner', 'LapKomplikasjoner',
                      'KomplPostUtd', 'KomplReopUtd', 'LapEkstrautstyr',
                      'LapIntraabdominell', 'LapTeknikk', 'Prosedyrer')){
    flerevar <- 1
    retn <- 'H'}

  #   for (teller in 1:(medSml+1)) {
  #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
  #RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]



  #  ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
  #	ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
  #	RegData[ ,variable] <- NA
  #	RegData[ ,variable][ind01] <- 0
  #	RegData[ ,variable][ind1] <- 1

  if (valgtVar=='HysKomplikasjoner') {
    #Hysteroskopi intrapoerative komplikasjoner:
    variable <- c('HysTilgang',
                  'HysPerforasjon',
                  'HysTeknisk',
                  'HysFluidOverload',
                  'HysBlodning')
    grtxt <- c('Ved tilgang', 'Perforasjon', 'Teknisk/utstyr',
               'Fluid overload', 'Blødning')
    tittel <- 'Intraoperative komplikasjoner ved hysteroskopi'
    RegData <- RegData[RegData$HysKomplikasjoner %in% 0:1,]	#Velger ikke ut på OpMetode=2 siden ønsker også de som har begge
    #ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
    #RegData[ ,variable] <- 0
    #RegData[ ,variable][ind1] <- 1
    #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
    #NVar <- length(indMed)
    #N <- NVar
  }
  if (valgtVar=='KomplPostopType') {
    #Postoperative komplikasjoner. Bare registreringer hvor Opf0Komplikasjoner er 0 el. 1
    tittel <- 'Postoperative komplikasjoner'
    RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
    grtxt <- c('Blødning', 'Med utstyr', 'Infeksjon', 'Organskade')
    variable <- c('Opf0KomplBlodning', 'Opf0KomplUtstyr', 'Opf0KomplInfeksjon', 'Opf0KomplOrgan')
    xAkseTxt <- 'Andel operasjoner (%)'
    ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
    RegData[ ,variable] <- 0
    RegData[ ,variable][ind1] <- 1
    #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
    #NVar <- length(indMed)
    #N <- NVar
  }

  if (valgtVar == 'KomplPostUtd') {		#Evt. ReopUtd
    #Postoperative komplikasjoner for ulike utdanningsgrupper
    #Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper.
    ####!!!Usikker på hvilke variable som skal inngå, dvs. om tomme burde være med i N.
    # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 9:Ukjent
    tittel <- 'Postop. komplikasjon i utdanningsgrupper'
    grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år')
    #RegData <- RegData[which(RegData$Utdanning %in% 1:5), ] #Antar at tomme Opf0Reoperasjon er nei. & which(RegData$Opf0Reoperasjon %in% 0:1)
    RegData <- RegData[which(RegData$Opf0Status == 1) %i% which(RegData$Utdanning %in% 1:5)
                       %i% which(RegData$Opf0Komplikasjoner %in% 0:1), ] #Antar at tomme Opf0Reoperasjon er nei. & which(RegData$Opf0Reoperasjon %in% 0:1)
    RegData$Utdanning <- factor(RegData$Utdanning, levels=1:5)
    ind01 <- cbind(1:length(RegData$Utdanning), RegData$Utdanning) #which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
    ind1 <- cbind(which(RegData$Opf0Komplikasjoner == 1), RegData$Utdanning[which(RegData$Opf0Komplikasjoner == 1)]) #
    variable <- paste0('k',1:5)
    RegData[ ,variable] <- NA
    RegData[ ,variable][ind01] <- 0
    RegData[ ,variable][ind1] <- 1
    #AntVar <- table(RegData$Utdanning[which(RegData$Opf0Komplikasjoner ==1)])
    #NVar <- table(RegData$Utdanning)
    #N <- sum(NVar, na.rm = T)
  }
  if (valgtVar == 'KomplReopUtd') {		#Evt. ReopUtd
    #Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper.
    ####!!!Usikker på hvilke variable som skal inngå. Eks Opf0Reoperasjon=1, OpType=2, tomme?
    # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
    tittel <- 'Reoperasjon (grunnet komplikasjon) i utdanningsgrupper'
    grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år')
    RegData <- RegData[which(RegData$Utdanning %in% 1:5) %i% which(RegData$Opf0Komplikasjoner %in% 0:1), ] #Antar at tomme Opf0Reoperasjon er nei.
    RegData$Utdanning <- factor(RegData$Utdanning, levels=1:5)
    ind01 <- cbind(1:length(RegData$Utdanning), RegData$Utdanning) #which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
    ind1 <- cbind(which(RegData$Opf0Reoperasjon == 1), RegData$Utdanning[which(RegData$Opf0Reoperasjon == 1)]) #
    variable <- paste0('k',1:5)
    RegData[ ,variable] <- NA
    RegData[ ,variable][ind01] <- 0
    RegData[ ,variable][ind1] <- 1

    #AntVar <- table(RegData$Utdanning[which(RegData$Opf0Reoperasjon ==1)])
    #NVar <- table(RegData$Utdanning)
    #N <- sum(NVar, na.rm = T)
    retn <- 'H'
  }
  if (valgtVar=='LapEkstrautstyr') {
    #Laparaskopisk ekstrautstyr
    #OpMetode=1 el 3 (Laparoskopi eller begge)
    #29.08.2016: Thunderbeat -> To nye variable: Kob.bipolar og ultralys. Legg til «Bipolar koagulasjon og klipping»
    #LapThunderbeat -> LapIntKombo

    retn <- 'H'
    variable <- c('LapAdherProfylakse',
                  'LapBipolarDiatermi',
                  'LapClips',
                  'LapHarmonicS',
                  'LapMorcellatorUtenPose',
                  'LapMorcellatorMedPose',
                  'LapNett',
                  'LapPreparatopose',
                  'LapUterusmanipulator',
                  'LapRobotKirurgi',
                  'LapSingelPort',
                  'LapStaplerEndogia',
                  'LapSutur',
                  'LapIntKombo',
                  'LapIntKoagOgKlipp',
                  'LapUnipolarDiatermi')
    grtxt <- c('Adheranseprofylakse', 'Bipolar Diatermi', 'Clips', 'Harmonic skalpell',
               'Morc. u/pose [1/3-16]', 'Morc. m/pose [1/3-16]',
               'Nett', 'Preparatpose', 'Uterusmanipulator', 'Robotkirurgi', 'Singel port',
               'Stapler/endoGIA', 'Sutur', 'Bipolar og ultralyd', 'Bipolar koag. og klipping',
               'Unipolar Diatermi')
    cexgr <- 0.8
    tittel <- 'Laparaskopisk ekstrautstyr'
    RegData <- RegData[RegData$OpMetode %in% c(1,3), ]
    #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
    indInnfDato <- which(as.Date(RegData$HovedDato) >= as.Date('2016-03-01'))
    varInnfind <- which(variable %in% c('LapMorcellatorUtenPose', 'LapMorcellatorMedPose'))
    RegData[which(as.Date(RegData$HovedDato) < as.Date('2016-03-01')), variable[varInnfind]] <- NA

    #AntVar[varBytt] <- colSums(RegData[indInnfDato,varBytt], na.rm=T)
    #N <- length(indMed)
    #NVar <- rep(N, length(variable))
    #NVar[varByttind] <- length(indInnfDato)
  }
  if (valgtVar=='LapKomplikasjoner') {
    #Laparoskopiske intrapoerative komplikasjoner:
    #Andel komplikasjoner ved bruk av de ulike utstyrstypene? OK. Variablene angir komplikasjonsårsak.
    #		LAPAROSCOPY_PORTIOADAPTER_CHECK: LapUterusmanipulator
    #		LAPAROSCOPY_TILGANG_CHECK: LapKompTilgang
    #		LAPAROSCOPY_HJELPEINSTIKK_CHECK: LapHjelpeinnstikk
    #		LAPAROSCOPY_INTRAABDOMINAL_CHECK: LapIntraabdominell
    #		LAPAROSCOPY_TEKNISK_UTSTYR_CHECK: LapTekniskUtstyr
    #		LAPAROSCOPY_POSTOPERATIV_CHECK: LapPostoperativ
    variable <- c('LapUterusmanipulator', #0,1
                  'LapKompTilgang',	#
                  'LapHjelpeinnstikk', #0,1
                  'LapIntraabdominell',  #0,1
                  'LapTekniskUtstyr', #0,1
                  'LapPostoperativ') #0,1 Hører denne med?
    grtxt <- c('Uterusmanipulator', 'Tilgangsmetode', 'Hjelpeinnstikk',
               'Intraabdominal', 'Utstyr', 'Postoperativ')
    cexgr <- 0.85
    tittel <- 'Intraoperative komplikasjoner ved laparoskopi'
    RegData <- RegData[(RegData$LapKomplikasjoner %in% 0:1), ]
    #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
    #NVar <- length(indMed)
    #N <- NVar
  }
  if (valgtVar=='LapIntraabdominell') {
    #Laparoskopiske intraabdominale komplikasjoner:
    variable <- c('LapNerv',
                  'LapUreter',
                  'LapTarm',
                  'LapBlare',
                  'LapKarBlodning')
    grtxt <- c('Nerve', 'Ureter', 'Tarm', 'Blære', 'Kar')
    tittel <- 'Intraabdominelle komplikasjoner ved laparoskopi'
    RegData <- RegData[RegData$LapIntraabdominell %in% 0:1, ]	#
    #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
    #NVar <- length(indMed)
    #N <- NVar
  }
  if (valgtVar == 'LapTeknikk') { #Tidl: LapTilgangsMetode
    #		LapTilgangsMetode: LAPAROSCOPY_ACCESS_METHOD_TEXT: 1,2,NA
    #		LapTilgang: LAPAROSCOPY_ACCESS_TEXT
    #LapTilgangsMetode 0: Åpent, 1: Veress-nål, 2: Annet
    #LapTilgang, fra 1/3-16: 1-Venstre Palmers point
    #Bare laparoskopi og begge
    #Ny kategori, dvs. ny variabel: Palmers point, neste prod.setting, etterreg. fra 1.3.2016. Mangler noen og disse filtreres bort.
    RegData <- RegData[which(RegData$OpMetode %in% c(1,3)), ]
    tittel <- 'Laparoskopisk tilgang, teknikk og metode' #  'Teknikk for laparaskopisk tilgang'
    grtxt <- c('Åpent', 'Veress-nål', 'Annet','Palmers point [1/3-16]', 'Navlen [1/3-16]') #LapTilgangsMetode
    indMar16 <- which(as.Date(RegData$HovedDato)>='2016-03-01')
    indMet <- which(RegData$LapTilgangsMetode %in% 0:2)
    indTilg <- which(RegData$LapTilgang %in% 1:2)

    variable <- c(paste0('met',0:2), paste0('tilg', 1:2))
    ind1met <- cbind(indMet, RegData$LapTilgangsMetode[indMet]+1) #which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
    ind01tilg <- intersect(indMar16,indTilg)
    ind1tilg <- cbind(ind01tilg, RegData$LapTilgang[ind01tilg]) #which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei

    RegData[ ,variable] <- NA
    RegData[ ,variable[1:3]] <- 0
    RegData[ ,variable[1:3]][ind1met] <- 1
    RegData[ind01tilg ,variable[4:5]] <- 0
    RegData[ ,variable[4:5]][ind1tilg] <- 1

    #AntVar <- c(table(RegData$LapTilgangsMetode), table(RegData$LapTilgang[indMar16]))
    #N <- dim(RegData)[1]
    #NVar <- c(rep(N,3), rep(length(indMar16),2))
  }
  if (valgtVar == 'Opf0KomplInfeksjon') {   #Andeler, andelGrVar, andelTid
    retn <- 'H'
    flerevar <- 1
    #Opf0metode in 1:2 #9 angir "ikke mulig"
    #Opf0metode in 1:2 ekvivalent med Opf0Komplikasjoner %in% 0:1
    RegData <- RegData[(which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner %in% 0:1)), ]
    variable <- c('Opf0InfOpSaar', 'Opf0InfIntraabdominal', 'Opf0InfEndometritt', 'Opf0InfUVI', 'Opf0InfAnnen')
    tittel <- switch(figurtype,
                     andeler = 'Type infeksjon, postoperativt',
                     andelGrVar = 'Postop. komplikasjon: Infeksjon',
                     andelTid = 'Postop. komplikasjon: Infeksjon' )
    grtxt <- c('Operasjonssår', 'Intraabdominal', 'Endometritt/Saplingitt', 'UVI', 'Annet') #, 'Ukjent')
    varTxt <- 'infeksjoner'
    RegData$Variabel[which(RegData$Opf0KomplInfeksjon == 1)] <- 1
  }


  #Generell beregning for alle figurer med sammensatte variable:
  #      if (teller == 1) {
  #        AntHoved <- AntVar
  #        NHoved <- N #sum(NVar, na.rm=T)	#feil: max(NVar, na.rm=T)
  #        Andeler$Hoved <- 100*AntVar/NVar
  #      }
  #      if (teller == 2) {
  #        AntRest <- AntVar
  #        NRest <- N #sum(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
  #        Andeler$Rest <- 100*AntVar/NVar
  #      }

  #} #end medSml (med sammenligning)
  #}	#end begrensning til valgtVar som inneholder flere variable



  UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt,
                 subtxt=subtxt, KImaal=KImaal, retn=retn,
                 tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
  #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
  return(invisible(UtData))

}

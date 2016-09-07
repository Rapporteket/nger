#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Pasientens alder, 5-årige aldersgrupper
#'     \item Diagnoser: Hyppigst forekommende diagnoser
#'     \item HysGjforingsGrad: Gjennomføringsgrad av hysteroskopi
#'    		Koder:	1-Fullstendig, 2-Ufullstendig, 3-Mislykket
#'     \item HysKomplikasjoner: Hysteroskopi intrapoerative komplikasjoner
#'     \item KomplPost: Postoperative komplikasjoner
#'     \item KomplPostUtd: Postoperative komplikasjoner for ulike utdanningsgrupper
#'     \item KomplReopUtd: Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper
#'     \item LapKomplikasjoner: Laparoskopiske intrapoerative komplikasjoner
#'     \item LapEkstrautstyr: Laparaskopisk ekstrautstyr - Kommer, NY variabel: koagulasjon og klipping
#'     \item LapIntraabdominell: Laparoskopiske intraabdominale komplikasjoner
#'     \item LapNumHjelpeinnstikk: Antall hjelpeinnstikk
#'     \item LapTilgangsMetode: Teknikk for laparaskopisk tilgang
#'     \item Norsktalende: Pasientens norskkunnskaper
#'     \item OpAnestesi: Anestesitype
#'     \item OpASA: ASA-grad
#'     \item OpBMI: BMI-kategori
#'     \item Opf0AlvorlighetsGrad: Alvorlighetsgrad, postoperative komplikasjoner
#'			  Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
#'     \item OpDagkirurgi: Dagkirurgiske inngrep
#'     \item OpIVaktTid: Operasjon i legens vakttid
#'     \item OpKategori: Hastegrad av operasjon
#'     \item OpMetode: Operasjonsmetode
#'     \item OpTidlVagInngrep: Tidligere vaginale inngrep
#'     \item OpTidlLapsko: Tidligere laparoskopi
#'     \item OpTidlLaparotomi: Tidligere laparatomi
#'     \item OpType: Primæroperasjon eller reoperasjon
#'     \item Prosedyrer: Hyppigst forekommende prosedyrer
#'     \item Sivilstatus: Sivilstand
#'     \item Utdanning: Pasientens utdanning (1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent)
#'    }
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel som skal visualiseres
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#' @param preprosess Preprosesser data
#'                 0: Nei (Standard)
#'                 1: Ja
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param MCEType  1: Laparoskopi
#'                 2: Hysteroskopi
#'                 3: Begge
#'                 99: Alle
#' @param Hastegrad Hastegrad av operasjon.
#'                1: Elektiv
#'                2: Akutt
#'                3: Ø-hjelp
#' @param AlvorlighetKompl  Alvorlighetsgrad for postoperative komplikasjoner (Flervalg)
#'                          Angis som en vektor av tall som tekst, f.eks. c('1','2')
#'                          1: Lite alvorlig
#'                          2: Middels alvorlig
#'                          3: Alvorlig
#'                          4: Dødelig
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export
#'
NGERFigAndeler  <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='2050-12-31', minald=0, maxald=130,
                            outfile='', reshID, enhetsUtvalg=1, MCEType=99, Hastegrad='', AlvorlighetKompl='',
                            hentData=0, preprosess=0)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra) #, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  ###----------- Figurparametre ------------------------------
  retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
  cexgr <- 1	#Kan endres for enkeltvariable
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  subtxt <- ''	#Benevning
  flerevar <- 0
  antDes <- 1
  '%i%' <- intersect


  ###############
  ### Variable
  ###############

  ### Kategoriske variable:
  grtxt <- ''
  koder <- NULL


  if (valgtVar=='HysGjforingsGrad') {
    #Gjennomføringsgrad av hysteroskopi
    #Kode •	1-Fullstendig, 2-Ufullstendig, 3-Mislykket
    RegData <- RegData[which(RegData$OpMetode == 2), ]
    grtxt <- c('Fullstendig', 'Ufullstendig', 'Mislykket', 'Ukjent')
    Tittel <- 'Gjennomføringsgrad av hysteroskopi'
    koder <- 1:3
  }
  if (valgtVar == 'LapTilgangsMetode') {
    # 0: Åpent, 1: Veress-nål, 2: Annet
    #Bare laparoskopi og begge
    RegData <- RegData[which(RegData$OpMetode %in% c(1,3)), ]
    Tittel <- 'Teknikk for laparaskopisk tilgang'
    grtxt <- c('Åpent', 'Veress-nål', 'Annet', 'Ukjent') #Ny kategori: Palmers point, neste prod.setting, etterreg. fra 1.1.2016(?)
    koder <- 0:2
  }
  if (valgtVar == 'Norsktalende') {
    # 0:Nei, 1:Ja, 2:Delvis, 9:Ukjent
    Tittel <- 'Patient forstår og gjør seg forstått på norsk'
    grtxt <- c('Nei', 'Ja', 'Delvis', 'Ukjent')
    koder <- 0:2
  }
  if (valgtVar == 'OpAnestesi') {
    # 1-Ingen, 2-Lokal, 3-Generell, 4-Spinal, 5-Annet
    Tittel <- 'Anestesitype ved endoskopiske inngrep'
    grtxt <- c('Ingen', 'Lokal', 'Generell', 'Spinal', 'Annet', 'Ukjent')
    koder <- 1:5
    retn <- 'H'
  }
  if (valgtVar == 'OpASA') {
    koder <- 1:5
    grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende', 'Ukjent')
    subtxt <- 'Sykdomsgrad'
    Tittel <-  'ASA-gruppe'
    retn <- 'H'
  }
  if (valgtVar == 'OpDagkirurgi') {
    #0: Nei, 1: Ja Manglende:Ukjent
    Tittel <- 'Dagkirurgiske Inngrep'
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    koder <- 0:1
  }
  if (valgtVar=='Opf0AlvorlighetsGrad') {
    #Postoperative komplikasjoner
    #Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
    #		RegData <- RegData[which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner == 1), ]
    RegData <- RegData[(which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner %in% 0:1)), ]
    grtxt <- c('Lite alvorlig', 'Middels alvorlig', 'Alvorlig', 'Dødelig', 'Ingen kompl.')
    Tittel <- 'Alvorlighetsgrad av komplikasjoner'
    koder <- 1:4
    retn <- 'H'
  }
  if (valgtVar == 'OpIVaktTid') {
    #0: Nei, 1: Ja Manglende:Ukjent
    Hastegrad <- as.character(2:3)
    Tittel <- 'Operasjon i vakttid'
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    koder <- 0:1
  }
  if (valgtVar == 'OpKategori') {
    # 1:Elektiv, 2:Akutt, 3:Øyeblikkelig hjelp
    Tittel <- 'Operasjonskategori'
    grtxt <- c('Elektiv', 'Akutt', 'Ø-hjelp', 'Ukjent')
    koder <- 1:3
  }
  if (valgtVar == 'OpMetode') {
    #1:Laparoskopi, 2:Hysteroskopi, 3:Begge
    Tittel <- 'Operasjonsmetode'
    koder <- 1:3
    grtxt <- c('Laparoskopi', 'Hysteroskopi', 'Begge', 'Ukjent')
    retn <- 'H'
  }
  if (valgtVar %in% c('OpTidlVagInngrep', 'OpTidlLapsko', 'OpTidlLaparotomi')) {
    # 0: Nei, 1: Ja, 9: Vet ikke
    Tittel <- sprintf('Tidligere %s', switch(as.character(valgtVar),
                                             'OpTidlVagInngrep' = 'vaginale inngrep',
                                             'OpTidlLapsko' = 'laparoskopiske inngrep',
                                             'OpTidlLaparotomi' = 'laparatomi'))
    grtxt <- c('Nei', 'Ja', 'Vet ikke/Ukjent')
    koder <- 0:1
  }
  if (valgtVar == 'OpType') {
    # 1:Primærinngrep, 2:Reoperasjon
    Tittel <- 'Operasjonstype'
    grtxt <- c('Primærinngrep', 'Reoperasjon', 'Ukjent')
    koder <- 1:2
  }
  if (valgtVar == 'Sivilstatus') {
    # 1:Enslig, 2:Særboer, 3:Samboer, 4:Gift, 5:Skilt, 6:Enke, 9:Ukjent
    Tittel <- 'Sivilstatus'
    grtxt <- c('Enslig', 'Særboer', 'Samboer', 'Gift', 'Skilt', 'Enke', 'Ukjent')
    koder <- 1:6
    retn <- 'H'
  }
  if (valgtVar == 'Utdanning') {
    # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
    Tittel <- 'Utdanningsnivå'
    grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år', 'Ukjent')
    koder <- 1:5
    retn <- 'H'
  }


  #Likt for alle kategoriske variable
  if (length(koder)>0) {
    RegData$Variabel <- 99
    indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
    RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
  }
  ### Numeriske variable:

  if (valgtVar == 'Alder') {
    Tittel <- 'Aldersfordeling'
    gr <- c(0, seq(15, 80, 5), 120)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = FALSE)
    grtxt <- c('<15', levels(RegData$VariabelGr)[2:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
    retn <- 'H'
  }
  if (valgtVar == 'OpBMI') {
    # 1:Alvorlig undervekt,2:moderat undervekt, 3:mild undervekt, 4:normal vekt, 5:overvekt,
    # 6:fedme kl.I, 7:fedme kl.II, 8:fedme kl.III
    Tittel <- 'BMI-kategorier' #, Slå sammen undervekt, fedme 2 og 3.
    #grtxtAlle <- c('Undervekt','Undervekt','Undervekt','Normal vekt', 'Overvekt', 'Fedme kl.I',
    #	'Fedme kl.II&III', 'Fedme kl.II&III' 'Ukjent')
    #mapvalues(RegData$OpBMIKategori, from = 1:8, to = grtxtAlle)
    #       RegData$OpBMIKategori <- plyr::revalue(as.character(RegData$OpBMIKategori), c('1'='1', '2'='1', '3'='1', '4'='2', '5'='3', '6'='4', '7'='5', '8'='5'))

    gr <- c(-1, 0, 18.5, 25, 30, 35, 1000)
    ind <- which(RegData$OpBMI>0)
    #	RegData$VariabelGr[ind] <- cut(RegData[ind ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    RegData$Variabel <- -1
    RegData$Variabel[ind] <- RegData$OpBMI[ind]
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('Ukjent', 'Undervekt','Normalvekt', 'Overvekt', 'Fedme kl.I', 'Fedme kl.II&III')
    #       koder <- as.character(1:5)
    retn <- 'H'
  }
  if (valgtVar == 'LapNumHjelpeinnstikk') {
    # Velge antall fra 0 til 6
    #IKKE gjort noen utvalg. (StatusLap==1?, LapHjelpeinnstikk==1?)
    Tittel <- 'Antall hjelpeinnstikk, laparaskopi'
    grtxt <- 0:6 #Kategoriser: 0,1,2,3,4+
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = grtxt)
  }


  ###Gjør utvalg (NGERUtvalg)
  ###Kjører denne etter variabeldefinisjon for at utvalgTxt skal bli riktig
  NGERUtvalg <- NGERUtvalg(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra,
                           datoTil = datoTil, MCEType = MCEType, AlvorlighetKompl=AlvorlighetKompl,
                           Hastegrad=Hastegrad)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt



  shtxt <- switch(as.character(enhetsUtvalg),
                  '0' = 'Hele landet',
                  '1' = as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)]),
                  '2' = as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)]))

  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}


  #Generere hovedgruppe og sammenlikningsgruppe
  #Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
  if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$ReshId)])
  } else {shtxt <- 'Hele landet'}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'Landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }



  #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
  Andeler <- list(Hoved = 0, Rest =0)
  NRest <- 0
  AntRest <- 0

  #     if (flerevar == 0 ) { #pt. alltid 0 når kommer hit...
  AntHoved <- table(RegData$VariabelGr[indHoved])
  NHoved <- sum(AntHoved)
  Andeler$Hoved <- 100*AntHoved/NHoved
  if (medSml==1) {
    AntRest <- table(RegData$VariabelGr[indRest])
    NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
    Andeler$Rest <- 100*AntRest/NRest
  }
  #    }




  #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
  if (valgtVar %in% c('Diagnoser', 'KomplPost', 'HysKomplikasjoner', 'LapKomplikasjoner',
                      'KomplPostUtd', 'KomplReopUtd', 'LapEkstrautstyr',
                      'LapIntraabdominell', 'Prosedyrer')){
    #flerevar <-  1
    retn <- 'H'
    utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
    RegDataLand <- RegData
    NHoved <-length(indHoved)
    NRest <- length(indRest)

    for (teller in 1:(medSml+1)) {
      #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
      RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]

      if (valgtVar=='Diagnoser') {
        DiagLap <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3')
        DiagHys <- c('HysDiagnose1', 'HysDiagnose2', 'HysDiagnose3')
        AlleDiag <- sort(table(toupper(as.vector(as.matrix(RegData[ ,c(DiagHys, DiagLap)])))), decreasing = TRUE)
        ant <- 20
        grtxt <- names(AlleDiag)[1:ant+1]	#Evt. 2:11..
        Tittel <- 'Hyppigst forekommende diagnoser'
        AntVar <- AlleDiag[1:ant+1] #Må fjerne tomme. Starter derfor på 2. Hva om vi ikke har tomme...?
        NVar <- dim(RegData)[1]
		N <- NVar
      }


      if (valgtVar=='HysKomplikasjoner') {
        #Hysteroskopi intrapoerative komplikasjoner:
        Var <- c('HysTilgang',
                 'HysPerforasjon',
                 'HysTeknisk',
                 'HysFluidOverload',
                 'HysBlodning')
        grtxt <- c('Ved tilgang', 'Perforasjon', 'Teknisk/utstyr',
                   'Fluid overload', 'Blødning')
        Tittel <- 'Intraoperative komplikasjoner ved hysteroskopi'
        indMed <- which(RegData$HysKomplikasjoner %in% 0:1)	#Velger ikke ut på OpMetode=2 siden ønsker også de som har begge
        AntVar <- colSums(RegData[indMed ,Var], na.rm=T)
        NVar <- length(indMed)
        N <- NVar
      }
      if (valgtVar=='KomplPost') {
        #Postoperative komplikasjoner. Bare registreringer hvor Opf0Komplikasjoner er 0 el. 1
        Var <- c('Opf0KomplBlodning', 'Opf0KomplUtstyr', 'Opf0KomplInfeksjon', 'Opf0KomplOrgan')
        grtxt <- c('Blødning', 'Med utstyr', 'Infeksjon', 'Organskade')
        Tittel <- 'Postoperative komplikasjoner'
        indMed <- intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1))
        AntVar <- colSums(RegData[indMed ,Var], na.rm=T)
        NVar <- length(indMed)
        N <- NVar
      }

      if (valgtVar == 'KomplPostUtd') {		#Evt. ReopUtd
        #Postoperative komplikasjoner for ulike utdanningsgrupper
        #Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper.
        ####!!!Usikker på hvilke variable som skal inngå, dvs. om tomme burde være med i N.
        # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 9:Ukjent
        Tittel <- 'Postop. komplikasjon i utdanningsgrupper'
        grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år')
        #RegData <- RegData[which(RegData$Utdanning %in% 1:5), ] #Antar at tomme Opf0Reoperasjon er nei. & which(RegData$Opf0Reoperasjon %in% 0:1)
        RegData <- RegData[which(RegData$Opf0Status == 1) %i% which(RegData$Utdanning %in% 1:5)
                           %i% which(RegData$Opf0Komplikasjoner %in% 0:1), ] #Antar at tomme Opf0Reoperasjon er nei. & which(RegData$Opf0Reoperasjon %in% 0:1)
        RegData$Utdanning <- factor(RegData$Utdanning, levels=1:5)
        AntVar <- table(RegData$Utdanning[which(RegData$Opf0Komplikasjoner ==1)])
        NVar <- table(RegData$Utdanning)
        N <- sum(NVar, na.rm = T)
      }
      if (valgtVar == 'KomplReopUtd') {		#Evt. ReopUtd
        #Andel reoperasjoner som følge av komplikasjon for ulike utdanningsgrupper.
        ####!!!Usikker på hvilke variable som skal inngå. Eks Opf0Reoperasjon=1, OpType=2, tomme?
        # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
        Tittel <- 'Reoperasjon (grunnet komplikasjon) i utdanningsgrupper'
        grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år')
        RegData <- RegData[which(RegData$Utdanning %in% 1:5) %i% which(RegData$Opf0Komplikasjoner %in% 0:1), ] #Antar at tomme Opf0Reoperasjon er nei.
        RegData$Utdanning <- factor(RegData$Utdanning, levels=1:5)
        AntVar <- table(RegData$Utdanning[which(RegData$Opf0Reoperasjon ==1)])
        NVar <- table(RegData$Utdanning)
        N <- sum(NVar, na.rm = T)
        retn <- 'H'
      }
      if (valgtVar=='LapEkstrautstyr') {
        #Laparaskopisk ekstrautstyr
        #OpMetode=1 el 3 (Laparoskopi eller begge)
        #29.08.2016: Thunderbeat -> To nye variable: Kob.bipolar og ultralys. Legg til «Bipolar koagulasjon og klipping»
        #LapThunderbeat -> LapIntKombo

        retn <- 'H'
        Var <- c('LapAdherProfylakse',
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
                   'Morc. u/pose [01.03.2016]', 'Morc. m/pose [01.03.2016]',
                   'Nett', 'Preparatpose', 'Uterusmanipulator', 'Robotkirurgi', 'Singel port',
                   'Stapler/endoGIA', 'Sutur', 'Bipolar og ultralyd', 'Bipolar koag. og klipping',
                   'Unipolar Diatermi')
        Tittel <- 'Laparaskopisk ekstrautstyr'
        indMed <- which(RegData$OpMetode %in% c(1,3))
        AntVar <- colSums(RegData[indMed ,Var], na.rm=T)
        indinnfDato <- which(as.Date(RegData$HovedDato) >= as.Date('2016-03-01'))
        varBytt <- c('LapMorcellatorUtenPose', 'LapMorcellatorMedPose')
        varByttind <- which(Var %in% varBytt)
        AntVar[varBytt] <- colSums(RegData[indinnfDato,varBytt], na.rm=T)
        N <- length(indMed)
		NVar <- rep(N, length(Var))
        NVar[varByttind] <- length(indinnfDato)
              }

      if (valgtVar=='LapKomplikasjoner') {
        #Laparoskopiske intrapoerative komplikasjoner:
		#Andel komplikasjoner ved bruk av de ulike utstyrstypene?
		#!!!!!!!!!!! AVVENTER KLARGJØRING AV HVA FIGUREN SKAL VISE.
        Var <- c('LapUterusmanipulator', #0,1
                 'LapTilgang',	#1,2,NA
                 'LapHjelpeinnstikk', #0,1
                 'LapIntraabdominell',  #0,1
                 'LapTekniskUtstyr', #0,1
                 'LapPostoperativ', #0,1
                 'LapKonvertert') #0,1
        grtxt <- c('Uterusmanipulator', 'Tilgangsmetode', 'Hjelpeinnstikk',
                   'Intraabdominal', 'Utstyr', 'Postoperativ', 'Konvertert')
        Tittel <- 'Intraoperative komplikasjoner ved laparoskopi'
        indMed <- which(RegData$LapKomplikasjoner %in% 0:1)	#
		#Tror vi skal summere komplikasjoner for hver variabel
        #AntVar <- colSums(RegData[indMed ,Var], na.rm=T) FEIL
        NVar <- length(indMed)
        N <- NVar
      }
      if (valgtVar=='LapIntraabdominell') {
        #Laparoskopiske intraabdominale komplikasjoner:
        Var <- c('LapNerv',
                 'LapUreter',
                 'LapTarm',
                 'LapBlare',
                 'LapKarBlodning')
        grtxt <- c('Nerve', 'Ureter', 'Tarm', 'Blære', 'Kar')
        Tittel <- 'Intraabdominelle komplikasjoner ved laparoskopi'
        indMed <- which(RegData$LapIntraabdominell %in% 0:1)	#
        AntVar <- colSums(RegData[indMed ,Var], na.rm=T)
        NVar <- length(indMed)
        N <- NVar
      }
      if (valgtVar=='Prosedyrer') {
        #Hyppigst forekommende prosedyrer
        #RegData$Opf0Status == 1 OK
        ProsHys <- c('HysProsedyre1', 'HysProsedyre2', 'HysProsedyre3')
        ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
        AllePros <- sort(table(toupper(as.vector(as.matrix(RegData[ ,c(ProsHys, ProsLap)])))), decreasing = TRUE)
        ant <- 20
        grtxt <- names(AllePros)[1:ant+1]
        Tittel <- 'Hyppigst forekommende prosedyrer'
        AntVar <- AllePros[1:ant+1] #Må fjerne tomme. Starter derfor på 2. Hva om vi ikke har tomme...?
        NVar <- dim(RegData)[1]
        N <- NVar
      }



      #Generell beregning for alle figurer med sammensatte variable:
      if (teller == 1) {
        AntHoved <- AntVar
        NHoved <- N #sum(NVar, na.rm=T)	#feil: max(NVar, na.rm=T)
        Andeler$Hoved <- 100*AntVar/NVar
      }
      if (teller == 2) {
        AntRest <- AntVar
        NRest <- N #sum(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
        Andeler$Rest <- 100*AntVar/NVar
      }
    } #end medSml (med sammenligning)
  }	#end begrensning til valgtVar som inneholder flere variable




  ###-----------Figur---------------------------------------
  ##Hvis for få observasjoner..
  #    if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {
  #      FigTypUt <- figtype(outfile)
  #      farger <- FigTypUt$farger
  #      plot.new()
  #      title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
  #      legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
  #      text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre 10 totalt', cex=1.2)

  if ( NHoved %in% 1:5 | 	(medSml ==1 & NRest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=Tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 "egne" registreringer eller færre enn 10 totalt', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, Tittel, Andeler, utvalgTxt, retn, cexgr
    FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    antDesTxt <- paste('%.', antDes, 'f', sep='')
    if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst

    #Horisontale søyler
    if (retn == 'H') {
      xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=Tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

      if (medSml == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
                        paste(smltxt, ' (N=', NRest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }

    if (retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (medSml == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=2, cex=cexleg)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }


    title(Tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  #Beregninger som returneres fra funksjonen.
  AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
  rownames(AndelerUt) <- c('Hoved', 'Rest')
  AntallUt <- rbind(AntHoved, AntRest)
  rownames(AntallUt) <- c('Hoved', 'Rest')

  UtData <- list(paste(toString(Tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
  names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
  return(invisible(UtData))
}

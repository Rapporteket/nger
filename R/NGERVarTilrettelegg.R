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
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NGERVarTilrettelegg  <- function(RegData, valgtVar, grVar='', figurtype='andeler'){


      "%i%" <- intersect

      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      yAkseTxt <- ''
      subtxt <- ''
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaal <- NA
      tittel <- 'Mangler tittel'
      variable <- 'Ingen'
      #deltittel <- ''
      RegData$Variabel <- 0
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???


      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      # For hver valgtVar:
      # Definer og gjør utvalg for variabelen
      # tittel, xAkseTxt, sortAvtagende (standard: TRUE)

      tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen

      #-------------------------------------

      if (valgtVar=='Alder') {	#Andeler, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse'
            if (figurtype == 'andeler') {	#Fordelingsfigur
				tittel <- 'Aldersfordeling'
				gr <- c(0, seq(15, 80, 5), 120)
				RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
				grtxt <- c('<15', levels(RegData$VariabelGr)[2:(length(gr)-2)], '80+')
				RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
				subtxt <- 'Aldersgrupper (år)' }
			retn <- 'V'
			sortAvtagende <- FALSE
			cexgr <- 0.8
      }
     if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (figurtype == 'andeler') {	#Fordelingsfigur
                  gr <- c(seq(0, 100, 10),150)
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
                  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }

      if (valgtVar=='alder70Rygg') {	#AndelTid, AndelerGrVar
            RegData <- RegData[which(RegData$Alder >= 0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder >= 70)] <- 1
            varTxt <- 'over 70 år'
            tittel <- 'Pasienter over 70 år'
      }


#-----------Fra FigAndeler---------------

      if (valgtVar=='HysGjforingsGrad') {   #Andeler
		RegData$Variabel <- 99
        #Gjennomføringsgrad av hysteroskopi
        #Kode •	1-Fullstendig, 2-Ufullstendig, 3-Mislykket
        RegData <- RegData[which(RegData$OpMetode == 2), ]
        grtxt <- c('Fullstendig', 'Ufullstendig', 'Mislykket', 'Ukjent')
        tittel <- 'Gjennomføringsgrad av hysteroskopi'
        koder <- 1:3
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Norsktalende') {   #Andeler
		RegData$Variabel <- 99
        # 0:Nei, 1:Ja, 2:Delvis, 9:Ukjent
        tittel <- 'Patient forstår og gjør seg forstått på norsk'
        grtxt <- c('Nei', 'Ja', 'Delvis', 'Ukjent')
        koder <- 0:2
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'OpAnestesi') {   #Andeler
		RegData$Variabel <- 99
        # 1-Ingen, 2-Lokal, 3-Generell, 4-Spinal, 5-Annet
        tittel <- 'Anestesitype ved endoskopiske inngrep'
        grtxt <- c('Ingen', 'Lokal', 'Generell', 'Spinal', 'Annet', 'Ukjent')
        koder <- 1:5
        retn <- 'H'
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'OpASA') {   #Andeler
		RegData$Variabel <- 99
        koder <- 1:5
        grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende', 'Ukjent')
        subtxt <- 'Sykdomsgrad'
        tittel <-  'ASA-gruppe'
        retn <- 'H'
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'OpDagkirurgi') {   #Andeler
		RegData$Variabel <- 99
        #0: Nei, 1: Ja Manglende:Ukjent
        tittel <- 'Dagkirurgiske Inngrep'
        grtxt <- c('Nei', 'Ja', 'Ukjent')
        koder <- 0:1
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar=='Opf0AlvorlighetsGrad') {   #Andeler
		RegData$Variabel <- 99
        #Postoperative komplikasjoner
        #Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
        #		RegData <- RegData[which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner == 1), ]
        RegData <- RegData[(which(RegData$Opf0Status == 1) %i% which(RegData$Opf0Komplikasjoner %in% 0:1)), ]
        grtxt <- c('Lite alvorlig', 'Middels alvorlig', 'Alvorlig', 'Dødelig', 'Ingen kompl.')
        tittel <- 'Alvorlighetsgrad av komplikasjoner'
        koder <- 1:4
        retn <- 'H'
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'OpIVaktTid') {   #Andeler
		RegData$Variabel <- 99
        #0: Nei, 1: Ja Manglende:Ukjent
        Hastegrad <- as.character(2:3)
        tittel <- 'Operasjon i vakttid'
        grtxt <- c('Nei', 'Ja', 'Ukjent')
        koder <- 0:1
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
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
		RegData$Variabel <- 99
        #1:Laparoskopi, 2:Hysteroskopi, 3:Begge
        tittel <- 'Operasjonsmetode'
        koder <- 1:3
        grtxt <- c('Laparoskopi', 'Hysteroskopi', 'Begge', 'Ukjent')
        retn <- 'H'
        indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
        RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
        RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
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



      #Likt for alle kategoriske variable
      #  if (length(koder)>0) {
      #    RegData$Variabel <- 99
      #    indVar <- which(RegData[ ,valgtVar] %in% koder)	#Må definere koder eks <- 1:5 i variabeldef.
      #    RegData$Variabel[indVar] <- RegData[indVar, valgtVar]
      #    RegData <- RegData[indVar, ]
      #    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(koder,99), labels = grtxt) #levels=c(nivaa,9)
      #  }
      ### Numeriske variable:

      if (valgtVar == 'OpBMI') {   #Andeler
        # 1:Alvorlig undervekt,2:moderat undervekt, 3:mild undervekt, 4:normal vekt, 5:overvekt,
        # 6:fedme kl.I, 7:fedme kl.II, 8:fedme kl.III
        tittel <- 'BMI-kategorier' #, Slå sammen undervekt, fedme 2 og 3.
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
        retn <- 'V'
      }

      if (valgtVar == 'LapNumHjelpeinnstikk') {   #Andeler
        # Velge antall fra 0 til 6
        #IKKE gjort noen utvalg. (StatusLap==1?, LapHjelpeinnstikk==1?)
        tittel <- 'Antall hjelpeinnstikk, laparaskopi'
        grtxt <- 0:6 #Kategoriser: 0,1,2,3,4+
        RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = grtxt)
      }

      if (valgtVar == 'OpTid') {   #Andeler
        #0-20, 21-40, 41-60, 61-80, 81-100, 101-120, 121-140, 141-160, 161-180, 181-200, 201-220, 221-240, > 240
        tittel <- 'Operasjonstid'
        gr <- c(seq(0, 180, 20), 1000) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = FALSE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '180+')
        subtxt <- 'minutter'
        cexgr <- 0.9
        retn <- 'V'
      }


      if (valgtVar == 'R0ScorePhys') {  #Andeler, #GjsnGrVar
        #Verdier: 0:5:100
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData$Variabel > -1), ]
        RegData$Variabel <- RegData$R0ScorePhys
        tittel <- 'Fysisk funksjon'
        subtxt <- 'sumskår (høyest er best)'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }

      if (valgtVar == 'R0ScoreRoleLmtPhy') { #Andeler, #GjsnGrVar
        #Verdier: 0:25:100
		RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Rollebegrensning grunnet fysisk helse'
        gr <- c(0,25,50,75,100) #seq(0, 100, 25) #c(seq(0, 100, 25), 100) #c(seq(0, 180, 30), 1000) #
        grtxt <- gr #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
        subtxt <- 'sumskår (høyest er best)'
        RegData$VariabelGr <- factor(RegData$R0ScoreRoleLmtPhy, levels=grtxt) #cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
      }

      if (valgtVar == 'R0ScoreRoleLmtEmo') {  #Andeler, #GjsnGrVar
       #Verdier: 0:33.3:100
         RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Følelsesmessig rollebegrensning'
        gr <-c(0,33,66,100) #seq(0, 100, 33) #c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        grtxt <- gr #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
        subtxt <- 'sumskår (høyest er best)'
        RegData$VariabelGr <- as.factor(RegData$R0ScoreRoleLmtEmo) #, levels=grtxt) #cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
      }

      if (valgtVar == 'R0ScoreEnergy') { #GjsnGrVar, andeler
         #Verdier: 0:5:100
       RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Energinivå/fatigue'
        gr <- seq(0, 100, 20)
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- levels(RegData$VariabelGr)
        subtxt <- 'sumskår (høyest er best)'
      }
      if (valgtVar == 'R0ScoreEmo') { #Andeler#GjsnGrVar
        #Verdier: 0:4:100
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Følelsesmessig velvære'
        subtxt <- 'sumskår (høyest er best)'
        gr <- seq(0, 100, 20)
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }
      if (valgtVar == 'R0ScoreSosial') { #Andeler#GjsnGrVar
        #Verdier: 0:12.5:100
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Sosial tilpassethet'
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
      if (valgtVar == 'Tss2Mott') {   #Andeler
        tittel <- 'Hvordan ble du møtt på gynekologisk avdeling?'
        grtxt <- c('Mindre godt','Ingen mening','Ganske godt','Svært godt')
        koder <- 0:3
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Mott, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }

      if (valgtVar == 'Tss2Behandling') {   #Andeler
        tittel <- '	Hvordan passet behandlingens opplegg og innhold for deg?	'
        grtxt <- c('Paset ikke','Verken eller','Ganske bra','Svært bra')
        koder <- 0:3
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Behandling, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Tss2Lytte') {   #Andeler
        #		[]
        tittel <- 'Lyttet og forsto dine behandlere det du tok opp?'
        grtxt <- c("Nei","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
        koder <- 0:3
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Lytte, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Tss2Behandlere') {   #Andeler
        #	["Nei, ikke tilstrekkelig","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad"]
        tittel <- 'Hadde du tillit til dine behandlere på gynekologisk avd.?'
        grtxt <- c("Nei, ikke tilstrekkelig","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
        koder <- 0:3
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Behandlere, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Tss2Enighet') {   #Andeler
        #		["Nei","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad"]
        tittel <- 'Var du og dine behandlere enige om målsettingen for din behandling?'
        grtxt <- c("Nei","Ja, til en viss grad","Ja, i ganske stor grad","Ja, i svært stor grad")
        koder <- 0:3
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Enighet, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Tss2Generelt') {   #Andeler
        tittel <- '	Hvilken oppfatning har du om gynekologisk avdeling generelt?'
        grtxt <- c("Svært negativ","Negativ","Nøytral","Positiv","Svært positiv")
        koder <- 0:4
        retn <- 'H'
        RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Tss2Generelt, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }
      if (valgtVar == 'Utdanning') {   #Andeler
        # 1:Grunnskole, 2:VG, 3:Fagskole, 4:Universitet<4 år, 5:Universitet>4 år, 6:Ukjent
        tittel <- 'Utdanningsnivå'
        grtxt <- c('Grunnskole', 'Videregående', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år', 'Ukjent')
        koder <- c(1:5,9)
        retn <- 'H'
        RegData <- RegData[which(RegData$Utdanning %in% koder), ]
        RegData$VariabelGr <- factor(RegData$Utdanning, levels=koder, labels = grtxt) #levels=c(nivaa,9)
      }


      #-------------- SAMMENSATTE variable
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #(Alternativt kan vi gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen)

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
  if (valgtVar %in% c('Diagnoser', 'KomplPost', 'HysKomplikasjoner', 'LapKomplikasjoner',
                      'KomplPostUtd', 'KomplReopUtd', 'LapEkstrautstyr',
                      'LapIntraabdominell', 'LapTeknikk', 'Prosedyrer')){
    flerevar <- 1
    retn <- 'H'}

 #   for (teller in 1:(medSml+1)) {
      #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
      #RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]

      if (valgtVar=='Diagnoser') { #Tilfelle hvor man heller endrer format på variablene...?
        #PER NÅ FEIL. SAMME DIAGNOSE KAN VÆRE FØRT OPP FLERE GANGER FOR SAMME PASIENT.
        tittel <- 'Hyppigst forekommende diagnoser'
        diagnoser <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3','HysDiagnose1', 'HysDiagnose2', 'HysDiagnose3')
        ant <- 20
        cexgr <- 1-0.005*ant
        AlleDiag <- as.vector(as.matrix(RegData[ , diagnoser]))
        AlleDiagSort <- sort(table(AlleDiag[which(AlleDiag != '')]), decreasing = TRUE)
        grtxt <- names(AlleDiagSort)[1:ant]	#Alle diagnoser som skal være med. Kan benyttes til å lage indeks...
        variable <- grtxt
        nymatr <- as.data.frame(matrix(0,dim(RegData)[1],ant))
        names(nymatr) <- grtxt
        test <- nymatr
        for (k in grtxt) {
          #nymatr[,k] <- rowSums(RegData[ ,diagnoser]== k)
          nymatr[rowSums(RegData[ ,diagnoser]== k)>0, k] <- 1
          test[,k] <- rowSums(RegData[ ,diagnoser]== k)
        }
        RegData <- data.frame(RegData,nymatr)
        #AntVar <- AlleDiagSort[1:ant]
        #NVar <- dim(RegData)[1]
		    #N <- NVar
        }

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
        indMed <- which(RegData$HysKomplikasjoner %in% 0:1)	#Velger ikke ut på OpMetode=2 siden ønsker også de som har begge
        ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
        RegData[ ,variable] <- 0
        RegData[ ,variable][ind1] <- 1
        #AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
        #NVar <- length(indMed)
        #N <- NVar
      }
if (valgtVar == 'spesTiltak' ) {   # Andeler
	#INTENSIV!!!!!!!!!
	tittel <- 'Spesielle tiltak/intervensjoner'
	RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2016-01-01')), ]
	flerevar <- 1
	variable <- c('TerapetiskHypotermi', 'EcmoEcla', 'Iabp', 'Impella', 'Icp', 'Oscillator', 'No',
				  'Leverdialyse', 'Hyperbar', 'Eeg')
	grtxt <- c('Terapetisk hypotermi', 'ECMO/ECLA', 'IABP Aortaballongpumpe', 'Impella/VV-assist',
			   'ICP, intrakranielt trykk', 'Oscillator', 'NO-behandling',
			   'Leverdialyse', 'Hyperbar oksygenbeh.', 'Kontinuerlig EEG')
	#ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
	ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
	RegData[ ,variable] <- 0
	RegData[ ,variable][ind1] <- 1
	xAkseTxt <- 'Andel opphold (%)'
}
      if (valgtVar=='KomplPost') {
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
        indMed <- which(RegData$LapIntraabdominell %in% 0:1)	#
        AntVar <- colSums(RegData[indMed ,variable], na.rm=T)
        NVar <- length(indMed)
        N <- NVar
      }
	  if (valgtVar == 'LapTeknikk') { #Tidl: LapTilgangsMetode
	#		LapTilgangsMetode: LAPAROSCOPY_ACCESS_METHOD_TEXT: 1,2,NA
	#		LapTilgang: LAPAROSCOPY_ACCESS_TEXT
		#LapTilgangsMetode 0: Åpent, 1: Veress-nål, 2: Annet
		#LapTilgang, fra 1/3-16: 1-Venstre Palmers point
		#Bare laparoskopi og begge
		#Ny kategori, dvs. ny variabel: Palmers point, neste prod.setting, etterreg. fra 1.3.2016. Mangler noen og disse filtreres bort.
		RegData <- RegData[which(RegData$OpMetode %in% c(1,3)), ]
		tittel <- 'Laparoskopisk tilgang, teknikk og metode' #'Teknikk for laparaskopisk tilgang'
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
      if (valgtVar=='Prosedyrer') {
        #Hyppigst forekommende prosedyrer
        #RegData$Opf0Status == 1 OK
        ProsHys <- c('HysProsedyre1', 'HysProsedyre2', 'HysProsedyre3')
        ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
        #Må fjerne tomme. Tomme behandles som tomme lokalt, men NA på server.
        AllePros <- toupper(as.vector(as.matrix(RegData[ , c(ProsHys, ProsLap)])))
        AlleProsSort <- sort(table(AllePros[which(AllePros != '')]), decreasing = TRUE)
        ant <- 20
        grtxt <- names(AlleProsSort)[1:ant]
        cexgr <- 1-0.005*ant
        tittel <- 'Hyppigst forekommende prosedyrer'
        AntVar <- AlleProsSort[1:ant]
        NVar <- dim(RegData)[1]
        N <- NVar
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

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

NGERVarTilrettelegg  <- function(RegData, valgtVar, grVar='', ktr=0, figurtype='andeler'){


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

      if (valgtVar=='alderRygg') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (grVar == '') {	#Fordelingsfigur
                  gr <- c(seq(0, 100, 10),150)
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
                  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }

      if (valgtVar=='alder70Rygg') {	#AndelTid, AndelerGrVar
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder>=70)] <- 1
            varTxt <- 'over 70 år'
            tittel <- 'Pasienter over 70 år'
      }

      if (valgtVar == 'AlderRygg') { #AndelGrVar
            #Andel over 75 år
            RegData$Variabel[which(RegData[ ,valgtVar] >= 75)] <- 1
            tittel <- 'Pasienter over 75 år'
      }

      if (valgtVar == 'R0ScorePhys') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Fysisk funksjon'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }

      if (valgtVar == 'R0ScoreRoleLmtPhy') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Rollebegrensning grunnet fysisk helse'
        gr <- c(0,25,50,75,100) #seq(0, 100, 25) #c(seq(0, 100, 25), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- gr #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }

      if (valgtVar == 'R0ScoreRoleLmtEmo') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Følelsesmessig rollebegrensning'
        gr <-c(0,33,66,100) #seq(0, 100, 33) #c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- RegData[ ,valgtVar] #cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- gr #c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }

      if (valgtVar == 'R0ScoreEnergy') { #GjsnGrVar, andeler
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Energinivå/fatigue'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }
      if (valgtVar == 'R0ScoreEmo') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Følelsesmessig velvære'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }
      if (valgtVar == 'R0ScoreSosial') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Sosial tilpassethet'
        gr <- c(seq(0, 75, 25), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }
      if (valgtVar == 'R0ScorePain') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Smerte'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }
      if (valgtVar == 'R0ScoreGeneral') { #GjsnGrVar
        RegData <- RegData[which(RegData$R0Status==1) %i% which(RegData[,valgtVar] > -1), ]
        RegData$Variabel <- RegData[ ,valgtVar]
        tittel <- 'Generell helsetilstand'
        gr <- c(seq(0, 90, 10), 100) #c(seq(0, 180, 30), 1000) #
        RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-1)])
      }



      if (valgtVar=='OswEndr') {#gjsnGrVar
            RegData$Variabel <- switch(as.character(ktr),
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            tittel <- 'forbedring av Oswestry' #gjsnGrVar

      }

      if (valgtVar == 'nyreBeh' ) {   # Andeler, andelerGrVar
            tittel <- 'Andel av opphold med registrert nyreerstattende behandling'
            RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2015-01-01')), ]
            if (figurtype == 'andelGrVar') {
                  RegData <- RegData[RegData$KidneyReplacingTreatment %in% 1:2,]
                  RegData$Variabel[which(RegData$KidneyReplacingTreatment ==1)] <- 1
            }
            if (figurtype == 'andeler') {
                  RegData <- RegData[which(RegData$KidneyReplacingTreatment ==1), ]		#Bare de som har fått behandling
                  RegData$VariabelGr <- 9
                  RegData$VariabelGr[which(RegData$Kontinuerlig == TRUE)] <- 1
                  RegData$VariabelGr[which(RegData$Intermitterende == TRUE)] <- 2
                  RegData$VariabelGr[(RegData$Kontinuerlig & RegData$Intermitterende) == TRUE] <- 3 #Overskriver tidl 1 eller 2
                  RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:3,9))
            }
            grtxt <- c('Kontinuerlig \n(hemo-/dia-filtrasjon)', 'Intermitterende \n(hemodialyse)', 'Begge', 'Ukjent')
            retn <- 'H'
            #xAkseTxt <- 'Andel (%)'
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
      if (valgtVar == 'spesTiltak' ) {   # Andeler
            #SpecialMeasures
            tittel <- 'Spesielle tiltak/intervensjoner'
            RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2016-01-01')), ]
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('TerapetiskHypotermi', 'EcmoEcla', 'Iabp', 'Impella', 'Icp', 'Oscillator', 'No',
                          'Leverdialyse', 'Hyperbar', 'Eeg')
            #retn <- 'H'
            grtxt <- c('Terapetisk hypotermi', 'ECMO/ECLA', 'IABP Aortaballongpumpe', 'Impella/VV-assist',
                       'ICP, intrakranielt trykk', 'Oscillator', 'NO-behandling',
                       'Leverdialyse', 'Hyperbar oksygenbeh.', 'Kontinuerlig EEG')
            #ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }


      UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}

#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer

#' RegData må inneholde OpDato og Aar.
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil.
#' Antall opphold siste X (antMnd) mnd
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
# @inheritParams NGERFigAndeler
#' @return Div tabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0,
                            OpMetode=99, velgDiag=0){
      #RegData må inneholde ..
  gyldigResh <- reshID!=0 & !is.na(match(reshID, RegData$ReshId))
  if (gyldigResh) {RegData <- RegData[which(RegData$ReshId==reshID), ]}
      datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd, abbreviate = T), unit='month')
      aggVar <-  c('ShNavn', 'OpDato')
      Utvalg <- NGERUtvalgEnh(RegData=RegData, OpMetode = OpMetode, velgDiag=velgDiag)
      RegData <- Utvalg$RegData
      RegDataDum <- RegData[RegData$OpDato <= as.Date(datoTil, tz='UTC')
                              & RegData$OpDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$OpDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(lubridate::ymd(colnames(tabAvdMnd1)), label = T)
      if (reshID==0){
        tabAvdMnd1 <- addmargins((tabAvdMnd1))}
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1, digits=0)
      #return(tabAvdMnd1)
	return(list(tabAntAvd=tabAvdMnd1, utvalgTxt = Utvalg$utvalgTxt))
}
#tabAntOpphShMnd(RegData, datoTil=Sys.Date(), antMnd=3)


#' Antall opphold siste 5 år
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date(),
                             OpMetode=99, velgDiag=0){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      Utvalg <- NGERUtvalgEnh(RegData=RegData, OpMetode = OpMetode, velgDiag=velgDiag)
      RegData <- Utvalg$RegData
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(list(tabAntAvd=tabAvdAarN, utvalgTxt = Utvalg$utvalgTxt))
      #return(tabAvdAarN)
}


#'  Hvor mange skjema av hver type
#' @export
tabAntSkjemaGml <- function(skjemaoversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1){
  #tabAntSkjema(skjemaoversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1)
  #NB: Denne skal også kunne vise skjema i kladd!
  #Operasjon	Laparoskopi,	Hysteroskopi,	Oppfølging, RAND36, ,TSS2
  #Skjemastatus kan være -1, 0 og 1
  skjemaoversikt$SkjemaRekkeflg <- factor(skjemaoversikt$SkjemaRekkeflg, levels = c(1,3,5,7,9,11, 15))
  skjemanavn <- c('Operasjon','Laparoskopi','Hysteroskopi', 'Oppfølging', 'RAND36', 'TSS2', 'RAND36, 1år')


  indDato <- which(as.Date(skjemaoversikt$OpDato) >= datoFra & as.Date(skjemaoversikt$OpDato) <= datoTil)
  indSkjemastatus <- which(skjemaoversikt$SkjemaStatus==skjemastatus)
  skjemaoversikt <- skjemaoversikt[intersect(indDato, indSkjemastatus),]

  tab <-table(skjemaoversikt[,c('ShNavn', 'SkjemaRekkeflg')])
  colnames(tab) <- skjemanavn
  tab <- xtable::xtable(tab)

return(tab)
}

#'  Hvor mange skjema av hver type
#'  @param RegData allevarnum påkoblet RAND-data
#' @export
tabAntSkjema <- function(RegData, datoFra = '2019-01-01', datoTil=Sys.Date()){
  #Operasjon	Laparoskopi,	Hysteroskopi - bare besvarte skjema,	Oppfølging, RAND36, ,TSS2
  #  RAND-tabellen inneholder bare besvarte skjema, så her kan jeg telle ut fra «Metode» = 1,2 el 3.
#  TSS2 har ingen egen metode-variabel. Teller alle som har fått beregnet en Tss2Score.
#  For oppfølging en måned etter: Opf0metode = 1 | Opfmetode=2 | (Opf0metode=3 &  Opf0UtfViaEprom=1)

  indDato <- which(as.Date(RegData$OpDato) >= datoFra & as.Date(RegData$OpDato) <= datoTil)
  RegData <- RegData[indDato, ]
  RegData$ShNavn <- as.factor(RegData$ShNavn)

  indOpf0 <- with(RegData, which(Opf0metode == 1 | Opf0metode==2 | (Opf0metode==3 & Opf0UtfViaEprom == 1)))
  tab <- cbind(
    'Operasjon' = table(RegData$ShNavn),
    'Laparoskopi' = table(RegData$ShNavn[RegData$LapStatus==1]),
    'Hysteroskopi' = table(RegData$ShNavn[RegData$HysStatus==1]),
    'Oppfølging' = table(RegData$ShNavn[indOpf0]),
    'TSS2' = table(RegData$ShNavn[which(RegData$Tss2Score >=0)]),
    'RAND36' = table(RegData$ShNavn[which(RegData$R0Metode %in% 1:3)]),
    "RAND36-1år" = table(RegData$ShNavn[which(RegData$R1Metode %in% 1:3)]),
    "RAND36-3år" = table(RegData$ShNavn[which(RegData$R3Metode %in% 1:3)])
  )
  tab <- addmargins(tab, 1)
  tab <- xtable::xtable(tab)

  return(tab)
}


lagTabavFig <- function(UtDataFraFig, figurtype='andeler'){ #lagTabavFigAndeler

  attach(UtDataFraFig, warn.conflicts = F)
  #medSml==1

  if (figurtype %in% c('andeler','gjsnGrVar', 'andelTid')){
  tab <-cbind(UtDataFraFig$Ngr$Hoved,
              UtDataFraFig$AggVerdier$Hoved,
              if (medSml==1){cbind(
                UtDataFraFig$Ngr$Rest,
                UtDataFraFig$AggVerdier$Rest)})
  }

  if (figurtype %in% c('andeler', 'andelTid')) {
    colnames(tab) <- c(paste0(hovedgrTxt,', Antall'),
                     paste0(hovedgrTxt, ', Andel (%)'),
                     if (medSml==1) {
                       cbind(paste0(smltxt,', Antall'),
                             paste0(smltxt, ', Andel (%)'))}
                 )}

  if (figurtype == 'gjsnTid'){
    tab <- AggVerdier
    colnames(tab) <-  grtxt
    tab <- t(tab)
  }

    if(figurtype=='gjsnGrVar') {
    kolnavn <- c('Antall (N)', SentralmaalTxt)
    if (medSml==1) {
      colnames(tab) <-  c(kolnavn, paste0(smltxt, c(', Antall', ', Andel (%)')))}
    }

  rownames(tab) <- UtDataFraFig$grtxt
  return(tab)
}


#'  Vise figurdata som tabell, sentralmål per sykshus
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr,
              UtDataFraFig$AggVerdier$Hoved
  )
  colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
  return(tab)
}


#'  Generere tabell med nøkkeltall
#' @export
tabNGERpasientegenskaper <- function(RegData, datoFra='2022-01-01', datoTil=Sys.Date(), tidsenhet='Kvartal') {
  # make dummy column for all MCEs
  RegData <- NGERUtvalgEnh(RegData=RegData, datoFra = datoFra, datoTil = datoTil)$RegData
  RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
  RegData <- RegDataFunk$RegData
  tidtxt <- RegDataFunk$tidtxt

  n <- dim(RegData)[1]
  RegData$dummy <- rep("\\textbf{BMI, alle} ($kg/m^2$)", n)
  myTab <- xtabs(OpBMI ~ dummy + TidsEnhetSort,
                 aggregate(OpBMI~dummy+TidsEnhetSort,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ OpMetode + TidsEnhetSort,
                       aggregate(OpBMI~OpMetode+TidsEnhetSort,RegData,mean)))
  RegData$dummy <- "\\textbf{Fødsler, alle} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ dummy + TidsEnhetSort,
                       aggregate(OpPariteter~dummy+TidsEnhetSort,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ OpMetode + TidsEnhetSort,
                       aggregate(OpPariteter~OpMetode+TidsEnhetSort,RegData,mean)))
  RegData$dummy <- "\\textbf{Knivtider, alle} (\\textit{minutt})"
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ dummy + TidsEnhetSort,
                       aggregate(OpTid~dummy+TidsEnhetSort,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ OpMetode + TidsEnhetSort,
                       aggregate(OpTid~OpMetode+TidsEnhetSort,RegData,mean)))
colnames(myTab) <- tidtxt
  # move rownames to its own column do allow duplicate names
  # OpMetode 1=laparo, 2=hysteroskopi, 3=begge
  pe <- rownames(myTab)
  pe[which(pe==1)] <- "\\quad Laparoskopi"
  pe[which(pe==2)] <- "\\quad Hysteroskopi"
  pe[which(pe==3)] <- "\\quad Begge"

  mydf <- data.frame(Pasientegenskap=pe, myTab, check.names = FALSE)
  #  list(tabVI=mydf)
  return(invisible(mydf))
}



#'  instrumentbruk, Laparoskopi
#' @export
instrumentbruk <- function(RegData, datoFra='2019-01-01', datoTil=Sys.Date()){
  #Fra mars 2016 er morcellator med og uten pose.Velger å ikke ta høyde for dette siden det nå er gamle tall
  #LapSingelPort = portioadapter??
  #LapIntKombo = Thunderbeat
RegData <- NGERUtvalgEnh(RegData, datoFra = datoFra, datoTil = datoTil)$RegData
  Instr <- c('LapVevforsegl', 'LapMorcellatorUtenPose', # 'LapMorcellatorMedPose', # 'LapHarmonicS', 'LapIntKombo',
             'LapSingelPort',  'LapRobotKirurgi', 'LapUterusman', 'LapOptTro', 'LapPrepOppdel')
NavnInstr <- c('Intl.vevsforsegler', 'Morcellator', 'Portioad.', 'Robotkir.', 'Uterusmanip.',
               'Optisk trokar', 'Oppd. av preparat') # 'Ultralyd Scalp.', 'IntKombo', 'Morc. m/pose',

RegDataUtvalg <- RegData[which(RegData$OpMetode==1), c('ShNavn', Instr)]

InstrTabDum <- plyr::ddply(RegDataUtvalg, .variables='ShNavn', .drop=F, plyr::colwise(sum), na.rm=T)  #Dataramme m/7dim
#InstrTabDum <- ftable(RegDataUtvalg[,Instr])
Tot <- colSums(InstrTabDum[,2:(length(Instr)+1)])
ShNavn <- InstrTabDum[,1]

InstrTab <- rbind(InstrTabDum[,2:(length(Instr)+1)],
                  Sum = Tot)

colnames(InstrTab) <- NavnInstr
rownames(InstrTab) <- c(ShNavn, 'Hele landet')

# print(xtable(InstrTab, digits=0, align=c('l', rep('r', max(c(1,ncol(InstrTab)), na.rm=T))),
#              caption=paste('Antall ganger ulike instrumenter er benyttet.', tidsperiodeTxt),
#              label='tab:Instr'), include.rownames=TRUE, include.colnames=TRUE)
return(invisible(InstrTab))
}



#'  komplikasjoner, Laparoskopi
#' @export
tabKomplLap <- function(RegData, reshID=0, datoFra='2019-01-01', datoTil=Sys.Date()){

  #Blødning:
  BlodTxt <- c('Blødning', '...I abdominal vegg', '...Intraabdominal', '...Vaginal')
Blod <- c('Opf0KomplBlodning', 'Opf0BlodningAbdom', 'Opf0BlodningIntraabdominal', 'Opf0BlodningVaginal')

#Utstyr
#UtstyrTxt <- c('Problemer m/Instrumenter', '...Nett', '...Laparoskopisk sutur') #m/utstyr','...
#Utstyr <- c('Opf0UtstyrInstrumenter', 'Opf0UtstyrNett', 'Opf0UtstyrSutur') #'Opf0KomplUtstyr',
#"Opf0UtstyrInstrumenter", "Opf0UtstyrNett" og "Opf0UtstyrSutur"

#Infeksjon:
# Opf0InfEndometritt = Salpingitt JA, ok.
InfTxt <- c('Infeksjon', '...Urinveisinf.', '...I operasjonssår', '...Intraabdominal ', '...Salpingitt', '...Andre inf.')
Infeksjon <- c('Opf0KomplInfeksjon', 'Opf0InfUVI', 'Opf0InfOpSaar'  , 'Opf0InfIntraabdominal',
               'Opf0InfEndometritt', 'Opf0InfAnnen')

#Organskade
OrganTxt <- c('Organskade', '...Blære', '...Tarm', '...Ureter', '...Kar', '...Andre')
Organ <- c('Opf0KomplOrgan', 'Opf0OrganBlare', 'Opf0OrganTarm', 'Opf0OrganUreter', 'Opf0OrganKar', 'Opf0OrganAnnen')

#Reoperasjon
ReopTxt <- c('Reoperasjon', '...til laparoskopi', '...laparotomi')
Reop <- c("Opf0Reoperasjon", "Opf0ReopLaparoskopi", "Opf0ReopLaparotomi")

RegData <- NGERUtvalgEnh(RegData, datoFra = datoFra, datoTil = datoTil)$RegData
indMed <- intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$OpType %in% c(1,3)))

LapKomplVar <- c(Blod, Infeksjon, Organ, Reop) #c(Blod, Utstyr, Infeksjon, Organ, Reop)
LapKomplTxt <- c(BlodTxt, InfTxt, OrganTxt, ReopTxt) #c(BlodTxt, UtstyrTxt, InfTxt, OrganTxt, ReopTxt)
RegDataLapKompl <- RegData[indMed, c(LapKomplVar, "Opf0Komplikasjoner")]

AntLap <- dim(RegDataLapKompl)[1]
AndelLapKompl <- colSums(RegDataLapKompl, na.rm=T)/AntLap*100
AndelLapKomplTab <- as.table(AndelLapKompl)

if (reshID != 0) {
  indEgenLap <- which(RegData$ReshId == reshID)
  AndelLapKomplEget <- colSums(RegDataLapKompl[indEgenLap,], na.rm=T)/length(indEgenLap)*100

  AndelLapKomplTab <- cbind(
    'Eget' = AndelLapKomplEget,
    'Hele landet' = AndelLapKompl)
}
row.names(AndelLapKomplTab) <- c(LapKomplTxt, 'Totalt')
# print(xtable(AndelLapKomplTab, digits=c(0,1,1), align=c('l', 'l', rep('r', max(c(1,ncol(AndelLapKomplTab)-1), na.rm=T))),
#              caption=paste0('Hyppighet av laparoskopiske komplikasjoner. ', tidsperiodeTxt,
#                             ' Totalt ble det utført ', AntLap, 'laparoskopier i tidsperioden.'),
#              label='tab:LapKompl'), include.rownames=TRUE, include.colnames=TRUE)
UtData <- list(AndelLapKomplTab=AndelLapKomplTab, AntLap=AntLap)
return(UtData)
}



#'  Konvertert laparoskopi til laparotomi
#'
#' @param RegData dataramme
#' @param reshID reshID
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @export
tabKonvertertLap <- function(RegData, reshID=0, datoFra='2016-01-01', datoTil=Sys.Date()){
  RegData <- NGERUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil)$RegData
  RegDataLap <- RegData[which(RegData$OpMetode %in% c(1,3)), c('LapKonvertert','Aar','ReshId')]
  RegDataLap$Aar <- as.factor(RegDataLap$Aar)
  indKonv <- which(RegDataLap$LapKonvertert == 1)
  Konv <- table(RegDataLap[indKonv,'Aar'])/table(RegDataLap$Aar)*100
  KonvTab <- Konv
if (reshID != 0){
  indEgenLap <- which(RegDataLap$ReshId == reshID)
  KonvEget <- table(RegDataLap[intersect(indEgenLap, indKonv) , 'Aar'])/table(RegDataLap$Aar[indEgenLap])*100
  KonvTab <- rbind(
    'Konvertert' = Konv,
    'Konvertert, Eget' = KonvEget)}
  #AntKol <- ncol(KonvTab)
  # print(xtable(KonvTab, digits=c(0,rep(1, AntKol)), align=c('l', rep('r', AntKol, na.rm=T)),
  #              caption='Andel laparoskopiske inngrep som konverteres til laparotomi.',
  #              label='tab:LapKonv'), include.rownames=TRUE, include.colnames=TRUE)
  return(invisible(KonvTab))
}


#'  Vise vanligste prosedyrer eller diagnoser
#'
#' @param RegData dataramme
#' @param ant antall prosedyrer/diagnoser
#' @param prosdiag 'pros'-prosedyrer, 'diag'-diagnoser
#'
#' @export
visVanligsteProcDiag <- function(RegData, ant=20, prosdiag='pros'){

ant <- 20
  ProsHys <- c('HysProsedyre1', 'HysProsedyre2', 'HysProsedyre3')
  ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
  DiagLap <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3')
  DiagHys <- c('HysDiagnose1', 'HysDiagnose2', 'HysDiagnose3')

  variable <- switch(prosdiag,
                     diag = c(DiagHys, DiagLap),
                     pros = c(ProsHys, ProsLap))

AlleProsDiag <- as.vector(as.matrix(RegData[ , variable]))
AllePDsort <- sort(table(AlleProsDiag[which(AlleProsDiag != '')]), decreasing = TRUE)


tab <- cbind( #Må fjerne tomme
  Andel = (AllePDsort[1:ant])/dim(RegData)[1]*100 ,
  Antall = AllePDsort[1:ant] )

type <- switch(prosdiag, pros='prosedyr', diag='diagnos')
tittel <- paste0('Vanligste ', type,'er. Andel angir prosent av utførte
                 operasjoner hvor ', type, 'en er benyttet.')

tabUt <- xtable(tab, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(tab)), na.rm=T))),
       caption=tittel,
       linclude.rownames=TRUE, include.colnames=TRUE)

# xtable(Proc, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(Proc)), na.rm=T))),
#        caption='Vanligste prosedyrer. Andel angir andel av antall utførte
#        operasjoner hvor prosedyra er benyttet.',
#        label='tab:Proc', include.rownames=TRUE, include.colnames=TRUE)
#

return(tabUt)
}


#Nøkkeltall
#HYSTEROSKOPI:

#' Definerer rader i en nøkkeltallstabell
#'
#' @param RegData - dataramme, NGER-data
#' @param var - hvilken variabel en ønsker resultat for
#' @param stat - 'median' eller 'pst' (prosent)
#' @param verdi - hvilken verdi/kode som skal telles når en beregner prosent
#' @param met - 'hys' (hysteroskopi) eller 'lap' (laparoskopi)
#'
#' @return Leverer beregnede tall til rad i nøkkeltallstabell.
#' NB, NB: Rad 2 og 3 gjelder kun hysteroskopi, rad 4 kun laparoskopi
#' @export
#'
rader <- function(RegData, var, stat = 'median', verdi=1){ #, met='hys'

  indPostKomp <- which(RegData$Opf0Komplikasjoner==1)
  indTss2Gen <- which(RegData$Tss2Generelt %in% 3:4)

#  Kun hysteroskopi
    indUfull <- which(RegData$HysGjforingsGrad==2)
    indPerKompHys <- which(RegData$HysKomplikasjoner==1)
#  Nhys <- c(dim(RegData)[1], length(indUfull), length(indPerKompHys), length(indPostKomp), length(indTss2Gen))

    # Kun lap
    indPerKompLap <- which(RegData$LapKomplikasjoner==1)

    N_hys_lap <- c(dim(RegData)[1], length(indUfull), length(indPerKompHys),
                   length(indPerKompLap), length(indPostKomp), length(indTss2Gen))


  pst <- function(var, verdi) {100*sum(var==verdi)/length(var)}

  if (stat=='median') {
    rad <- c(median(var, na.rm = T),
             median(var[indUfull], na.rm = T), #Ufullstendige, hys
             median(var[indPerKompHys], na.rm = T), # Perop./Intraop kompl, hys
             median(var[indPerKompLap], na.rm = T), # Postop kompl, lap
             median(var[indPostKomp], na.rm = T), # Postop kompl
             median(var[indTss2Gen], na.rm = T) # Fornøyd + svært fornøyd
    )
    rad <- sprintf("%.1f", rad)
  }
  if (stat=='gjsn') {
    rad <- c(mean(var, na.rm = T),
             mean(var[indUfull], na.rm = T), #Ufullstendige, hys
             mean(var[indPerKompHys], na.rm = T), # Perop./Intraop kompl, hys
             mean(var[indPerKompLap], na.rm = T), # Postop kompl, lap
             mean(var[indPostKomp], na.rm = T), # Postop kompl
             mean(var[indTss2Gen], na.rm = T) # Fornøyd + svært fornøyd
    )
    rad <- sprintf("%.2f", rad)
  }

  if (stat == 'pst'){
    var <- var[!is.na(var)]
    rad <-c(pst(var, verdi = verdi),
            pst(var[indUfull], verdi = verdi), #Ufullstendige
            pst(var[indPerKompHys], verdi = verdi), # Perop./Intraop kompl, hys
            pst(var[indPerKompLap], verdi = verdi), # Perop./Intraop kompl, lap
            pst(var[indPostKomp], verdi = verdi), # Postop kompl
            pst(var[indTss2Gen], verdi = verdi) # Fornøyd + svært fornøyd
    )
    rad <- paste0(sprintf("%.1f", rad),'%')
  }

  names(rad) <- c('Alle', 'Ufullst. inngrep', 'Perop. kompl', 'Perop. kompl',
                  'Postop. kompl', 'Fornøyde pasienter')

  UtData <- list(Rad = rad, N = N_hys_lap)
   return(invisible(UtData))
}



#' Nøkkeltallstabell for hysteroskopi
#'
#' @param RegData NGER-data, dataramme
#'
#' @return Leverer formattert tabell for nøkkeltall
#' @export
#'

tabNokkelHys <- function(RegData= RegData, datoFra=Sys.Date()-365, datoTil = Sys.Date(),
                         reshID = 0, velgAvd=0, enhetsUtvalg = 0) {
  # Andel ufulstendige - HysGjforingsGrad, peroperative komplikasjoner, postoperative komplikasjoner
  # og pasienttilfredshet (generell oppfatning, fornøyd+svært fornøyd) ut i fra:

  RegData <- NGERUtvalgEnh(RegData,
                           datoFra = datoFra,
                           datoTil = datoTil,
                           reshID = reshID,
                           velgAvd = velgAvd,
                           enhetsUtvalg = enhetsUtvalg,
                           OpMetode=2)$RegData
  ald = rader(RegData=RegData, var=RegData$Alder, stat = 'median')

  tab <- rbind(
    'Antall forløp (N)' = ald$N,
    'Alder (median)' = ald$Rad,
    'BMI (median)' = rader(RegData=RegData, var=RegData$OpBMI, stat = 'median')$Rad,
    'Operasjonstid (median)'  = rader(RegData=RegData, var=RegData$OpTid, stat = 'median')$Rad,
    'Blodfortynnende (%)' = rader(RegData=RegData, var=RegData$OpBlodfortynnende, stat = 'pst', verdi=1)$Rad,
    'Poliklinkk (%)' = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 1)$Rad,
    'Dagkirurgi (%)' = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 2)$Rad,
    'Innlagt (%)'  = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 3)$Rad,
    'Konvertert (%)' =  rader(RegData=RegData, var = RegData$HysKonvertert, stat = 'pst', verdi = 1)$Rad,
    'Perop. kompl. (%)' = rader(RegData=RegData, var = RegData$HysKomplikasjoner, stat = 'pst', verdi = 1)$Rad
  )

  tabHys <- tab[ ,-4]
}

#' Nøkkeltallstabell for laparoskopi
#'
#' @param RegData NGER-data, dataramme
#'
#' @return Leverer formatert tabell for nøkkeltall
#' @export
#'

tabNokkelLap <- function(RegData= RegData, datoFra=Sys.Date()-365, datoTil = Sys.Date(),
                         reshID = 0, velgAvd=0, enhetsUtvalg = 0) {
  # Andel ufulstendige - HysGjforingsGrad, peroperative komplikasjoner, postoperative komplikasjoner
  # og pasienttilfredshet (generell oppfatning, fornøyd+svært fornøyd) ut i fra:

  RegData <- NGERUtvalgEnh(RegData,
                           datoFra = datoFra,
                           datoTil = datoTil,
                           reshID = reshID,
                           velgAvd = velgAvd,
                           enhetsUtvalg = enhetsUtvalg,
                           OpMetode=1)$RegData
  ald = rader(RegData=RegData, var=RegData$Alder, stat = 'median')

  tab <- rbind(
    'Antall forløp (N)' = ald$N,
    'Alder (median)' = ald$Rad,
    'BMI (median)' = rader(RegData=RegData, var=RegData$OpBMI, stat = 'median')$Rad,
    'Operasjonstid (median)'  = rader(RegData=RegData, var=RegData$OpTid, stat = 'median')$Rad,
    'Hjelpeinnstikk (gj.sn.)'  =  rader(RegData=RegData, var = RegData$LapOptTro, stat = 'gjsn')$Rad, # (gj.sn)
    #    'Blodfortynnende (%)' = rader(RegData=RegData, var=RegData$OpBlodfortynnende, stat = 'pst', verdi=1)$Rad,
    'Poliklinkk (%)' = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 1)$Rad,
    'Dagkirurgi (%)' = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 2)$Rad,
    'Innlagt (%)'  = rader(RegData=RegData, var = RegData$OpBehNivaa, stat = 'pst', verdi = 3)$Rad,
    'Antibiotikaprofylakse (%)' =  rader(RegData=RegData, var = RegData$OpAntibProfylakse, stat = 'pst', verdi = 1)$Rad,
    'Robotkirurgi (%)' =  rader(RegData=RegData, var = RegData$LapRobotKirurgi, stat = 'pst', verdi = 1)$Rad,
    'Tidligere laparotomi (%)' =  rader(RegData=RegData, var = RegData$OpTidlLaparotomi, stat = 'pst', verdi = 1)$Rad,
    'Tidligere laparoskopi (%)' =  rader(RegData=RegData, var = RegData$OpTidlLapsko, stat = 'pst', verdi = 1)$Rad,
    'Metode: Åpen (%)' = rader(RegData=RegData, var = RegData$LapTilgangsMetode, stat = 'pst', verdi = 0)$Rad,
    'Metode: Veress-nål (%)' = rader(RegData=RegData, var = RegData$LapTilgangsMetode, stat = 'pst', verdi = 1)$Rad,
    'Metode: Direkte (%)' = rader(RegData=RegData, var = RegData$LapTilgangsMetode, stat = 'pst', verdi = 2)$Rad,
    'Optisk trokar (%)' =  rader(RegData=RegData, var = RegData$LapOptTro, stat = 'pst', verdi = 1)$Rad,
    'Perop. kompl. (%)' = rader(RegData=RegData, var = RegData$LapKomplikasjoner, stat = 'pst', verdi = 1)$Rad
  )
  tabLap <- tab[ ,c(-2,-3)]
}


#Laparoskopi
# Ønsker tabeller på andel peroperative komplikasjoner, postoperative komplikasjoner og
# pasienttilfredshet (andel fornøyd/svært fornøyd variabel) ut ifra
#ok Alder (median)
#ok BMI (median)
#ok Operasjonstid (median)
#ok Behandlingsnivå (tre)
#ok Konvertert (andel)
#ok AB profylakse
#ok Robotkirurgi (andel)
# Tidligere laparotomi (andel)
# Tidligere laparoskopi (andel)
# Metode (åpen, veress-nål eller Direkte)
# Optisk trokar (andel)
# Hjelpeinnstikk (gj.sn)
#ok Peroperative komplikasjoner (andel)

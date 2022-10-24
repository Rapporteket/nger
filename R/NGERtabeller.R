#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer

#' RegData må inneholde InnDato og Aar.
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
      aggVar <-  c('ShNavn', 'InnDato')
      Utvalg <- NGERUtvalgEnh(RegData=RegData, OpMetode = OpMetode, velgDiag=velgDiag)
      RegData <- Utvalg$RegData
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(lubridate::ymd(colnames(tabAvdMnd1)), label = T)
      if (reshID==0){
        tabAvdMnd1 <- addmargins((tabAvdMnd1))}
      return(tabAvdMnd1)
	#return(list(tabAvdMnd1=tabAvdMnd1, utvalgTxt <- Utvalg$utvalgTxt))
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
      #return(list(tabAvdAarN=tabAvdAarN, utvalgTxt <- Utvalg$utvalgTxt))
      return(tabAvdAarN)
}


#'  Hvor mange skjema av hver type
#' @export
tabAntSkjema <- function(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1){
  #tabAntSkjema(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1)
  #NB: Denne skal også kunne vise skjema i kladd!
  #Operasjon	Laparoskopi,	Hysteroskopi,	Oppfølging, 6u, RAND36, ,TSS2
  #Skjemastatus kan være -1, 0 og 1
  SkjemaOversikt$SkjemaRekkeflg <- factor(SkjemaOversikt$SkjemaRekkeflg, levels = c(1,3,5,7,9,11, 15))
  skjemanavn <- c('Operasjon','Laparoskopi','Hysteroskopi', 'Oppfølging', 'RAND36', 'TSS2', 'RAND36, 1år')

  indDato <- which(as.Date(SkjemaOversikt$InnDato) >= datoFra & as.Date(SkjemaOversikt$InnDato) <= datoTil)
  indSkjemastatus <- which(SkjemaOversikt$SkjemaStatus==skjemastatus)
  SkjemaOversikt <- SkjemaOversikt[intersect(indDato, indSkjemastatus),]

  tab <-table(SkjemaOversikt[,c('ShNavn', 'SkjemaRekkeflg')])
  colnames(tab) <- skjemanavn
  tab <- xtable::xtable(tab)

return(tab)
}


#'  Vise figurdata som tabell
#' @export
# lagTabavFig <- function(UtDataFraFig){
#       tab <-cbind(UtDataFraFig$Ngr$Hoved,
#                   UtDataFraFig$AggVerdier$Hoved,
#                   UtDataFraFig$Ngr$Rest,
#                   UtDataFraFig$AggVerdier$Rest)
#       grtxt <- UtDataFraFig$grtxt
#       if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
#             grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
#       rownames(tab) <- grtxt
#       kolnavn <- c('Antall' , 'Andel (%)')
#       colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
#
# return(tab)
# }

lagTabavFig <- function(UtDataFraFig, figurtype='andeler'){ #lagTabavFigAndeler

  attach(UtDataFraFig, warn.conflicts = F)
  #medSml==1

  if (figurtype %in% c('andeler','gjsnGrVar', 'andelTid')){
  tab <-cbind(Ngr$Hoved,
              AggVerdier$Hoved,
              if (medSml==1){cbind(
                Ngr$Rest,
                AggVerdier$Rest)})
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



#'  instrumentbruk, Laparaskopi
#' @export
instrumentbruk <- function(RegData, datoFra='2019-01-01', datoTil=Sys.Date()){
  #Fra mars 2016 er morcellator med og uten pose.Velger å ikke ta høyde for dette siden det nå er gamle tall
  #LapSingelPort = portioadapter??
  #LapIntKombo = Thunderbeat
RegData <- NGERUtvalgEnh(RegData, datoFra = datoFra, datoTil = datoTil)$RegData
  Instr <- c('LapMorcellatorUtenPose', 'LapMorcellatorMedPose', 'LapHarmonicS',
             'LapSingelPort', 'LapIntKombo', 'LapRobotKirurgi', 'LapUterusmanipulator')
NavnInstr <- c('Morc.u/pose', 'Morc. m/pose', 'Ultralyd Scalp.', 'Portioad.', 'IntKombo', 'Robotkir.', 'Uterusmanip.') #}

RegDataUtvalg <- RegData[which(RegData$OpMetode==1), c('ShNavn', Instr)]


#InstrTabDum <- plyr::ddply(RegDataUtvalg[ ,Instr], .(RegDataUtvalg$ShNavn), .drop=F, colwise(sum), na.rm=T)  #Dataramme m/7dim
InstrTabDum <- plyr::ddply(RegDataUtvalg, .variables='ShNavn', .drop=F, colwise(sum), na.rm=T)  #Dataramme m/7dim
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



#'  komplikasjoner, Laparaskopi
#' @export
tabKomplLap <- function(RegData, reshID=0, datoFra='2019-01-01', datoTil=Sys.Date()){

  #Blødning:
  BlodTxt <- c('Blødning', '...I abdominal vegg', '...Intraabdominal', '...Vaginal')
Blod <- c('Opf0KomplBlodning', 'Opf0BlodningAbdom', 'Opf0BlodningIntraabdominal', 'Opf0BlodningVaginal')

#Utstyr
#UtstyrTxt <- c('Problemer m/Instrumenter', '...Nett', '...Laparaskopisk sutur') #m/utstyr','...
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
#                             ' Totalt ble det utført ', AntLap, 'laparaskopier i tidsperioden.'),
#              label='tab:LapKompl'), include.rownames=TRUE, include.colnames=TRUE)
UtData <- list(AndelLapKomplTab=AndelLapKomplTab, AntLap=AntLap)
return(UtData)
}



#'  Konvertert laparoskopi til laparatomi
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

#AlleProsEget <- as.vector(as.matrix(RegData[indEget, c(ProsHys, ProsLap)]))
#AlleProsEgetSort <- sort(table(AlleProsEget[which(AlleProsEget != '')]), decreasing = TRUE)

tab <- cbind( #Må fjerne tomme
  Andel = (AllePDsort[1:ant])/dim(RegData)[1]*100 ,
  Antall = AllePDsort[1:ant] )

# ProcEget <- cbind( #Må fjerne tomme
#   Andel = (AlleProsEgetSort[1:ant])/Neget*100 ,
#   Antall = AlleProsEgetSort[1:ant] )

# AlleDiag <- as.vector(as.matrix(RegData[ , c(DiagHys,DiagLap)]))
# AlleDiagSort <- sort(table(AlleDiag[which(AlleDiag != '')]), decreasing = TRUE)
# AlleDiagEget <- as.vector(as.matrix(RegData[indEget , c(DiagHys,DiagLap)]))
# AlleDiagEgetSort <- sort(table(AlleDiagEget[which(AlleDiagEget != '')]), decreasing = TRUE)


# Diag <- cbind( #Må fjerne tomme
#   Andel = (AlleDiagSort[1:ant])/N*100 ,
#   Antall = AlleDiagSort[1:ant] )

# DiagEget <- cbind( #Må fjerne tomme
#   Andel = (AlleDiagEgetSort[1:ant])/Neget*100 ,
#   Antall = AlleDiagEgetSort[1:ant] )

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
# xtable(ProcEget, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(ProcEget)), na.rm=T))),
#        caption='Vanligste prosedyrer, eget sykehus. Andel angir andel av antall utførte
#        operasjoner hvor prosedyra er benyttet.',
#        label='tab:ProcEget', include.rownames=TRUE, include.colnames=TRUE)
#
# xtable(Diag, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(Diag)), na.rm=T))),
#        caption='Vanligste diagnoser. Andel angir andel av antall utførte
#        operasjoner hvor diagnosen er benyttet.',
#        label='tab:Diag', include.rownames=TRUE, include.colnames=TRUE)
#
# xtable(DiagEget, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(DiagEget)), na.rm=T))),
#        caption='Vanligste diagnoser, eget sykehus. Andel angir andel av antall utførte
#        operasjoner hvor diagnosen er benyttet.',
#        label='tab:DiagEget', include.rownames=TRUE, include.colnames=TRUE)

return(tabUt)
}

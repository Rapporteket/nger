#' Preprosesser data fra NGER
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERUtvalgEnh
#'
#' @return RegData En data.frame med det preprosesserte datasettet
#'
#' @export
#'
NGERPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer:
  #OK
  if ('BasisRegStatus' %in% (names(RegData))) { RegData <- RegData[RegData$BasisRegStatus==1, ]}#Leveres fortsatt registreringer m/BasisRegStatus=0
  #Opf0Status=1 for alle registreringer i FollowupsNum
  #OppflgRegStatus:
    # NULL	Oppfølginger finnes ikke for denne typen forløp.
    # -2	Ingen oppføringer er opprettet.
    # -1	En eller flere oppfølgninger er opprettet, og ingen er lagret eller ferdigstilt.
    # 0	En eller flere er i kladd, men ingen ferdigstilt
    # 1	En eller flere er ferdigstilt (noen kan være i kladd eller ikke opprettet)
    # 2	Alle oppfølgningene er ferdigstilt.
  #OppflgStatus
   #Tekstfelt som angir om oppfølging er mulig, evt, hvorfor ikke. Hvis oppfølging var mulig
    #og utført er teksten: Oppfølging utført.
  #ErOppflg
    #Hvis oppføringen er en oppfølgning skal denne settes til 1.
    #Det er kun godkjent med 0 på de som ikke er oppfølginger.
    #Hvis ForløpsID'en inneholder både hovedforløpet og oppfølgingen skal dette feltet settes lik 0.


  #Riktig format på datovariable:
  #RegData$FodselsDato <- as.Date(RegData$FodselsDato, format="%Y-%m-%d")
  #RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d")
  #!!! HovedDato tilsvarer OpDato. Benytter HovedDato siden OpDato ikke finnes i alle skjema
  RegData$InnDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")
  #RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #
  RegData$MndNum <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")$mon +1
  RegData$Kvartal <- ceiling(RegData$MndNum/3)
  RegData$Halvaar <- ceiling(RegData$MndNum/6)
  RegData$Aar <- 1900 + as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
  RegData$MndAar <- format(RegData$InnDato, '%b%y')


  #Riktig navn på resh-variabel:
  names(RegData)[which(names(RegData)=='AvdRESH')] <- 'ReshId' #Change var name
  names(RegData)[which(names(RegData)=='PasientAlder')] <- 'Alder' #Change var name
  RegData$SykehusNavn <- trimws(as.character(RegData$SykehusNavn)) #Fjerner mellomrom (før) og etter navn
  names(RegData)[which(names(RegData)=='SykehusNavn')] <- 'ShNavn' #Change var name

  #Endrer til bare store bokstaver
  if ('LapDiagnose1' %in% (names(RegData))) {
  DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3', 'HysDiagnose1','HysDiagnose2', 'HysDiagnose3')
  ProsVar <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3', 'HysProsedyre1','HysProsedyre2', 'HysProsedyre3')

  for (var in c(DiagVar,ProsVar)) {
    RegData[ ,var] <- toupper(RegData[ ,var])
  }
  }

  return(invisible(RegData))
}

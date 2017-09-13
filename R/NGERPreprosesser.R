#' Preprosesser data fra NGER
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams NGERFigAndeler
#'
#' @return RegData En data.frame med det preprosesserte datasettet
#'
#' @export
#'
NGERPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer:
  #OK
  RegData <- RegData[RegData$BasisRegStatus==1, ]#Leveres fortsatt registreringer m/BasisRegStatus=0
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
  RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d")
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")
  RegData$SykehusNavn <- trimws(as.character(RegData$SykehusNavn)) #Fjerner mellomrom (før) og etter navn

  #Riktig navn på resh-variabel:
  names(RegData)[which(names(RegData)=='AvdRESH')] <- 'ReshId' #Change var name
  names(RegData)[which(names(RegData)=='PasientAlder')] <- 'Alder' #Change var name
  names(RegData)[which(names(RegData)=='SykehusNavn')] <- 'ShNavn' #Change var name


  return(invisible(RegData))
}

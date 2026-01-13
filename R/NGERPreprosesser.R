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
  #Opf0Status=1 for alle registreringer i followupsnum
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
  RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d")
  RegData$MndNum <- as.POSIXlt(RegData$OpDato, format="%Y-%m-%d")$mon +1
  RegData$Kvartal <- ceiling(RegData$MndNum/3)
  RegData$Halvaar <- ceiling(RegData$MndNum/6)
  RegData$Aar <- 1900 + as.POSIXlt(RegData$OpDato, format="%Y-%m-%d")$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
  RegData$MndAar <- format(RegData$InnDato, '%b%y')


  #Vask og nye:
  #names(RegData)[which(names(RegData)=='PasientAlder')] <- 'Alder'
  RegData$Alder <- (as.Date(RegData$OpDato) - as.Date(RegData$FodselsDato))/365.25
  names(RegData)[which(names(RegData)=='AvdRESH')] <- 'ReshId' #Change var name
  RegData$ShNavn <- trimws(as.character(RegData$SykehusNavn)) #Fjerner mellomrom (før) og etter navn
  # names(RegData)[which(names(RegData)=='SykehusNavn')] <- 'ShNavn'

  #108698 (Kongsvinger Innland) endres til Kongsvinger 4215373
#ahs  ind <- which(RegData$ReshId == 108698)
#ahs  RegData$ShNavn[ind] <- RegData$ShNavn[match(4215373, RegData$ReshId)] #which(RegData$ReshId==4215373)][1]
#ahs  RegData$ReshId[ind] <- 4215373

  #Tomme sykehusnavn får resh som navn:
  indTom <- which(is.na(RegData$ShNavn) | RegData$ShNavn == '')
  RegData$ShNavn[indTom] <- RegData$ReshId[indTom]

  #Sjekker om alle resh har egne enhetsnavn
  dta <- unique(RegData[ ,c('ReshId', 'ShNavn')])
  duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
  duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn)>1)])

  if (length(c(duplSh, duplResh)) > 0) {
    ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
    RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind],' (', RegData$ReshId[ind], ')')
  }

  #Endrer til bare store bokstaver
  if ('LapDiagnose1' %in% (names(RegData))) {
    DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3', 'LapDiagnose4',
                 'HysDiagnose1','HysDiagnose2', 'HysDiagnose3',
                 'OpIndikasjon1', 'OpIndikasjon2', 'OpIndikasjon3')
    ProsVar <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3', 'LapProsedyre4',
                 'HysProsedyre1','HysProsedyre2', 'HysProsedyre3')

    for (var in c(DiagVar,ProsVar)) {
      RegData[ ,var] <- toupper(RegData[ ,var])
    }

    #Innhold i diagnose- og prosedyrevariabler er endret fra femsifret kode til kode + forklaring
    #LapDiag og HysDiag: 3-4 tegn
    #LapPros og HysPros: 5 tegn

    for (innh in ProsVar) {
      ind <- which(!is.na(RegData[ ,innh]))
      RegData[ind ,innh] <- substr(RegData[ind ,innh],1,5)
    }

    for (innh in DiagVar) {
      ind <- which(!is.na(RegData[ ,innh]))
      RegData[ind ,innh] <- stringr::str_trim(substr(RegData[ind ,innh],1,4))
    }

  }

  return(invisible(RegData))
}

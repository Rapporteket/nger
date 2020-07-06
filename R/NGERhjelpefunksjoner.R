# Hjelpefunksjoner for NGER
#---------------------------------------------


#' Tilrettelegge tidsenhetvariabel:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#'
#' @param RegData dataramme
#' @param tidsenhet tidsenhet: 'Aar', 'Mnd', 'Kvartal', 'Halvaar'
#' @param tab Hmmm
#'
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar', tab=0) {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                          +(RegData$Aar-min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      # format.Date(seq(from=as.Date('2018-01-01'),
      #                 to=as.Date('2018-09-01'), by='month'), format = '%b%y')

      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #           sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       #Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       # Mnd = format.Date(seq(from=min(as.Date(RegData$InnDato), na.rm = T),
                       #                       to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%b%y'),
                       #Henter fullt månedsnavn og forkorter etterpå.
                       Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'),
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))

      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}
      #RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      #RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      #a <- factor(c(1:10,3,2,4,3,7,9,4), levels=1:11, labels = letters[1:11])
#table(a)

      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}


#' Lage tulledata (simulerte data)
#'
#' @param RegData dataramme
#' @param varBort variable som skal fjernes. Variabelnavn angis som vektor av tekststrenger
#' @param antSh antall sykehus
#' @param antObs antall observasjoner (rader)
#'
#' @export
lageTulleData <- function(RegData, varBort=NA, antSh=26, antObs=20000) {
  #Må også legge på resh som svarer til sykehusnavn.
      library(synthpop)
      library(dplyr)
      #ForlopsID <- RegData$ForlopsID
  if (!is.na(varBort[1])) {
      RegData <- RegData[,-which(names(RegData) %in% varBort)]}
      #RegData <- RegData[sample(1:dim(RegData)[1], antObs, replace = T),]
      sykehus <- cbind(ShNavn=paste('Sykehus', LETTERS[1:antSh]),
                       ReshId=1:antSh)
      fordelingPasienter <- sample(1:10,antSh, replace = TRUE)
      indSample <-  sample(1:antSh, prob=fordelingPasienter/sum(fordelingPasienter),
                           replace = TRUE, size=antObs)

      RegDataSyn <- synthpop::syn(RegData, method = "sample", k=antObs, seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn)
      RegData[c('SykehusNavn','ReshId')] <- sykehus[indSample,]

	  return(RegData)
}

#' Generere samlerapporter i app
#'
#' @param filnavn benyttes i downloadhandler
#' @param rnwFil Fila som skal kompileres. Eks. 'Eksempel.rnw'
#' @param reshID reshID
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @export
henteSamlerapporter <- function(filnavn, rnwFil, reshID=0,
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package='nger'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  owd <- setwd(tempdir())
  #on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
  # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
}

#' Kjøre samlerapporter for abonnement i NGER
#'
#' @param rnwFil NoWeb-fil med filending: eksempel.rnw
#' @param brukernavn innlogget bruker
#' @param reshID brukerens reshID
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @return gir filsti til pdf-samledokument
#' @export
#'
abonnementNGER <- function(rnwFil, brukernavn='ngerBrukernavn', reshID=0,
                           datoFra=Sys.Date()-180, datoTil=Sys.Date()) {

    raplog::subLogger(author = brukernavn, reshId = reshID[[1]],
                      registryName = 'NGER',
                      msg = paste("Abonnement, ", rnwFil))

  #rnwFil  <- rnwFil[[1]]
  #brukernavn <- brukernavn[[1]]
  reshID <- reshID[[1]]
  datoFra <- datoFra[[1]]
  datoTil <- datoTil[[1]]

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='nger'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile) #, output = paste0(filbase, digest::digest(brukernavn),'.tex'))
  #utfil <-  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'))
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  raplog::subLogger(author = brukernavn, reshId = reshID[[1]],
                    registryName = 'NGER',
                    msg = paste("Sender: ", utfil))
  return(utfil)
}



#' Generere data til Resultatportalen
#'
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtKI - KomplIntra, KomplPostop, Opf0AlvorlighetsGrad,
#' HysKonvertert, LapKonvertert, Tss2Generelt, Tss2Sumskaar
#' @inheritParams NGERUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilResPort <- function(RegData = RegData, valgtKI, datoFra = '2016-01-01',
                           aar=2016:2019, OpMetode=0, filUt='dummy'){

    #  For hver kvalitetsindikator
    #  Fil, KIdata: År	EnhetsID	Teller	Nevner	Kvalitetsindikator

    #Ngrense <- 10
    #RegData <- NGERPreprosess(RegData)
    RegData <- NGERUtvalgEnh(RegData, OpMetode = OpMetode,
                             datoFra = paste0(aar[1],'-01-01'), datoTil = paste0(rev(aar)[1], '-12-31'))$RegData
    RegData$Variabel <- 0
    RegData$ShNavn <- as.factor(RegData$ShNavn)
    RegData$ReshId <- as.factor(RegData$ReshId)
    figurtype <- ifelse(valgtKI=='Tss2Sumskaar', 'gjsnGrVar', 'andelGrVar') #MÅ ENDRES FOR SUMSKÅR!!
    RegData <- NGERVarTilrettelegg(RegData = RegData, valgtVar = valgtKI, figurtype=figurtype)$RegData

    dataDum <- aggregate(data=RegData[ ,c("ReshId", 'Aar', 'Variabel' )], Variabel~ReshId+Aar,
                         #x=RegData$Variabel, by=RegData[]
                         FUN=function(x) {c(sum(x, na.rm=T), sum(!is.na(x)))})

    opMetTxt <- c('','Lap','Hys')[OpMetode+1]
    dataKI <- data.frame(dataDum[,1:2], Teller=dataDum$Variabel[,1], Nevner=dataDum$Variabel[,2],
                         KvalInd=paste0(valgtKI, opMetTxt))
    #write.table(dataKI, file = paste0('NGERkvalInd_', valgtKI, opMetTxt,'.csv'), row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')



  #   filUt <- paste0('RyggTilOff', ifelse(filUt=='dummy',  valgtVar, filUt), '.csv')
  # RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  # RyggUtvalg <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, aar=aar, hastegrad = hastegrad, tidlOp=tidlOp) #datoFra = datoFra) #, hovedkat=hovedkat) # #, datoTil=datoTil)
  # RegData <- RyggUtvalg$RegData
  # RyggTilOffvalgtVar <- RegData[,c('Aar', "ShNavn", "ReshId", "Variabel")]
  # info <- c(RyggVarSpes$tittel, RyggUtvalg$utvalgTxt)
  # RyggTilOffvalgtVar$info <- c(info, rep(NA, dim(RyggTilOffvalgtVar)[1]-length(info)))
  # #write.table(RyggTilOffvalgtVar, file = paste0('A:/Resultatportalen/', filUt), sep = ';', row.names = F) #, fileEncoding = 'UTF-8')
  return(invisible(dataKI))
}



#' ARE: Provide registration delay for NGER
#'
#' Provide registration delay in median number of days grouped by years
#'
#' @param years integer vector with years for results and grouping
#' @return data frame with registry name and values for each year
#' @export

NGERregDealy <- function(years) {

    query <- paste0(
      'select
  year(HovedDato) as year,
  DATEDIFF(SistLagretDato, HovedDato) as daysDiff
from
  SkjemaOversikt
where
  SkjemaStatus=1 and SkjemaNavn="Operasjon";'
    )

    NGERdelayData <- rapbase::LoadRegData(registryName="nger", query, dbType="mysql")

  # make data frame
  medianDelay <- data.frame(regName = "NGER", stringsAsFactors = FALSE)
  for (i in years) {
    ind <- which(NGERdelayData$year == i)
    medianDelay[[as.character(i)]] = median(NGERdelayData$daysDiff[ind])
    medianDelay[[paste0("N", as.character(i))]] = length(ind)

  }

  return(medianDelay)
}


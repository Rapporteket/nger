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
lageTulleData <- function(RegData=0, varBort=NA, datoFra= '2017-01-01', antSh=26, antObs=20000) {
  #Må også legge på resh som svarer til sykehusnavn.
      #library(synthpop)
      #library(dplyr)
  if (RegData == 0){
      RegData <- NGERPreprosess(NGERRegDataSQL(datoFra = datoFra))}
  if (!is.na(varBort[1])) {
      RegData <- RegData[,-which(names(RegData) %in% varBort)]}
      sykehus <- cbind('ShNavn'=paste('Sykehus', LETTERS[1:antSh]),
                       'ReshId'=1:antSh)
      fordelingPasienter <- sample(1:10, antSh, replace = TRUE)
      indSample <-  sample(1:antSh, prob=fordelingPasienter/sum(fordelingPasienter),
                           replace = TRUE, size=antObs)

      RegDataSyn <- synthpop::syn(RegData, method = "sample", k=antObs, seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn)
      RegData[ ,c('ShNavn','ReshId')] <- sykehus[indSample,]

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

    # raplog::subLogger(author = brukernavn, reshId = reshID,
    #                   registryName = 'NGER',
    #                   msg = paste("Abonnement, ", rnwFil))

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='nger'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  # raplog::subLogger(author = brukernavn, reshId = reshID,
  #                   registryName = 'NGER',
  #                   msg = paste("1 Klar til strikking, ", rnwFil))
  knitr::knit2pdf(input=tmpFile) #, output = paste0(filbase, digest::digest(brukernavn),'.tex'))
  # raplog::subLogger(author = brukernavn, reshId = reshID,
  #                   registryName = 'NGER',
  #                   msg = paste("2 Ferdig med strikking, ", rnwFil))
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  # raplog::subLogger(author = brukernavn, reshId = reshID,
  #                   registryName = 'NGER',
  #                   msg = paste("Sender: ", utfil))
  return(utfil)
}



#' Generere data til SKDEs interaktive nettsider
#'
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtVar - parameter for hvilken variabel det skal lages resultat for
#' @param indID indikator-id, eks. 'ind1', 'ind2', osv.
#' @inheritParams NGERUtvalgEnh
#' @return Datafil til Resultatportalen. Utfila må inneholde:
#' 'year', 'orgnr', 'var', 'denominator', 'caregiver' og 'ind_id'
#' @export

dataTilOffVisning <- function(RegData = RegData, valgtVar, aggData=0,
                              datoFra = '2014-01-01', datoTil = Sys.Date(), OpMetode=0,
                              indID = 'indDummy', lastNedFil=0, filUt='dummy'){


  filUt <- paste0('NGER', ifelse(filUt=='dummy',  valgtVar, filUt),'.csv')
  figurtype <- ifelse(valgtVar=='Tss2Sumskaar', 'gjsnGrVar', 'andelGrVar')
  NGERVarSpes <- NGERVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = figurtype)
  RegData <- NGERUtvalgEnh(RegData=NGERVarSpes$RegData, OpMetode = OpMetode,
                           datoFra = datoFra, datoTil = datoTil,
                           )$RegData

  if (aggData == 0) {
    RegDataUt <- RegData[,c('Aar', "ReshId", "Variabel", "ShNavn")]
    RegDataUt <- dplyr::rename(RegDataUt,
                               year = Aar,
                               var = Variabel)
    RegDataUt$denominator <- 1
  }

  if (aggData == 1) {
    if (figurtype == 'gjsnGrVar'){
      #aggData <- tapply(RegData$Variabel, INDEX = RegData[,c('Aar', 'ReshId')], FUN = mean)
      #aggData <- aggregate(RegData$Variabel, by = list(RegData$Aar, RegData$ReshId), FUN = mean)
      RegDataUt <- RegData %>%
        dplyr::group_by(ReshId, Aar) %>%
        dplyr::summarise(
          denominator = sum(!is.na(Variabel)),
          var = mean(Variabel, na.rm=T))

      RegDataUtLand <- RegData %>%
        dplyr::group_by(Aar) %>%
        dplyr::summarise(
          denominator = sum(!is.na(Variabel)),
          var = mean(Variabel, na.rm=T),
          ReshId = '1')

      RegDataUt <- rbind(RegDataUt, RegDataUtLand)

      RegDataUt <- RegDataUt[-which(RegDataUt$denominator == 0), ] #Fjerner tomme
      RegDataUt <- dplyr::rename(RegDataUt, year = Aar)

    } }

    # nytt navn = gammelt navn


    #Legge på orgID ("Sykehusviser")
    #ReshId	orgnr	RapporteketNavn	SKDEnavn
      nyID <- c('108172' = '974706490',		#Ahus	AHUS NORDBYHAGEN SOMATIKK	Ahus
                '107511' = '943545634',		#Aleris Frogner
                '103719' = '974631091',		#Arendal	SØRLANDET SYKEHUS HF SOMATIKK ARENDAL	Arendal
                '106843' = '922748144',		#Betanien	BETANIEN SYKEHUS AS	Betanien
                '706220' = '974795361',		#Bodø	NORDLANDSSYKEHUSET HF SOMATIKK - BODØ	Bodø
                '104736' = '974705788',		#Bærum	VESTRE VIKEN HF BÆRUM SYKEHUS - SOMATIKK	Bærum
                '700404' = '974707152',		#DNR	OSLO UNIVERSITETSSYKEHUS HF RADIUMHOSPITALET - SOMATIKK	Radiumhospitalet
                '103298' = 	'974631326',		#Drammen	VESTRE VIKEN HF DRAMMEN SYKEHUS - SOMATIKK	Drammen
                '108768' = 	'974631768',		#Elverum	SYKEHUSET INNLANDET HF ELVERUM - SOMATIKK	Elverum
                '108383' = 	'974595214',		#Flekkefjord	SØRLANDET SYKEHUS HF SOMATIKK FLEKKEFJORD	Flekkefjord
                '105226' = 	'974744570',		#Førde	HELSE FØRDE HF FØRDE SENTRALSJUKEHUS	Førde
                '108833' = 	'974632535',		#Gjøvik	SYKEHUSET INNLANDET HF GJØVIK - SOMATIKK	Gjøvik
                '706221' = 	'993573159',		#Gravdal	NORDLANDSSYKEHUSET HF HABILITERING/REHAB - GRAVDAL	Lofoten
                '101854' = 	'974795833',		#Hammerfest	FINNMARKSSYKEHUSET HF HAMMERFEST SYKEHUS	Hammerfest
                '706129' = 	'974795639',		#Harstad	UNIVERSITETSSYKEHUSET NORD-NORGE HF HARSTAD - SOMATIKK	Harstad
                '701437' = 	'974724774',		#Haugesund	HELSE FONNA HF HAUGESUND SJUKEHUS	Haugesund
                '102954' = 	'974557746',		#Haukeland	HELSE BERGEN HF HAUKELAND UNIVERSITETSSJUKEHUS	Haukeland
                '101828' = 	'974795930',		#Kirkenes	FINNMARKSSYKEHUSET HF KIRKENES SYKEHUS	Kirkenes
                '104174' = 	'974631385',		#Kongsberg	VESTRE VIKEN HF KONGSBERG SYKEHUS - SOMATIKK	Kongsberg
                '4215373' = 	'974631776',		#Kongsvinger	AHUS KONGSVINGER SOMATIKK	Kongsvinger
                '100412' = 	'974733013',		#Kristiansand	SØRLANDET SYKEHUS HF SOMATIKK KRISTIANSAND	Kristiansand
        #        '103189' = 	'974746948',		#Kristiansund	HELSE MØRE OG ROMSDAL HF KRISTIANSUND SJUKEHUS - SOMATIKK	Kristiansund. Ingen reg...
                '105863' = 	'974754118',		#Levanger	HELSE NORD-TRØNDELAG HF SOMATIKK - LEVANGER	Levanger
                '111180' = 	'874632562',		#Lillehammer	SYKEHUSET INNLANDET HF LILLEHAMMER - SOMATIK
                '700789' = 	'974795515',		#Mo i Rana	HELGELANDSSYKEHUSET HF MO I RANA - SOMATIKK	Mo i Rana
                '103188' = 	'974745569',		#Molde	HELSE MØRE OG ROMSDAL HF MOLDE SJUKEHUS - SOMATIKK	Molde
                '108048' = 	'974633752',		#Kalnes Østfold	SYKEHUSET ØSTFOLD
                '105874' = 	'974753898',		#Namsos	HELSE NORD-TRØNDELAG HF SOMATIKK - NAMSOS	Namsos
                '706130' = 	'974795396',		#Narvik	UNIVERSITETSSYKEHUSET NORD-NORGE HF NARVIK - SOMATIKK	Narvik
                '103575' = 	'974631407',		#Ringerike	VESTRE VIKEN HF RINGERIKE SYKESHUS - SOMATIKK	Ringerike
                '103162' = 	'974795477',		#Sandessjøen	HELGELANDSSYKEHUSET HF SANDNESSJØEN - SOMATIKK	Sandnessjøen
                '4205296' = 	'974633191',		#Skien	SYKEHUSET TELEMARK HF SKIEN - SOMATIKK	Skien
                '105460' = 	'974703300',		#Stavanger	STAVANGER UNIVERSITETSSJUKEHUS SOMATIKK VÅLAND	Stavanger
                '100460' = 	'974795574',		#Stokmarknes	NORDLANDSSYKEHUSET HF SOMATIKK - STOKMARKNES	Vesterålen
                '701482' = 	'974742985',		#Stord	HELSE FONNA HF STORD SJUKEHUS	Stord
                '601263' = 	'974795787',		#Tromsø	UNIVERSITETSSYKEHUSET NORD-NORGE HF TROMSØ - SOMATIKK	Tromsø gyn. kreft
                '601227' = 	'974795787',		#Tromsø	UNIVERSITETSSYKEHUSET NORD-NORGE HF TROMSØ - SOMATIKK	Tromsø
                '107644' = 	'974749025',		#Trondheim	ST OLAVS HOSPITAL HF UNIVERSITETSSYKEHUSET I TRONDHEIM	St. Olavs
                '110734' = 	'974633574',		#Tønsberg	SYKEHUSET I VESTFOLD HF, SOMATIKK, TØNSBERG	Tønsberg
                '700399' = 	'974589095',		#Ullevål	OSLO UNIVERSITETSSYKEHUS HF ULLEVÅL - SOMATIKK	Ullevål
                '102583' = 	'974747545',		#Volda	HELSE MØRE OG ROMSDAL HF VOLDA SJUKEHUS - SOMATIKK	Volda
                '4215139' = '973129856',    #Volvat Majorstuen
                '106026' = 	'974743272',		#Voss	HELSE BERGEN HF VOSS SJUKEHUS	Voss
                '102582' = 	'974747138',		#Ålesund	HELSE MØRE OG ROMSDAL HF ÅLESUND SJUKEHUS - SOMATIKK	Ålesund
                    '1' = '1'              #Hele landet
      )

    RegDataUt$orgnr <- as.character(nyID[as.character(RegDataUt$ReshId)])
    #unique(RegDataUt[ ,c('ShNavn', "ReshId", "orgnr")])
    RegDataUt <- RegDataUt[ ,c('year', 'orgnr', 'var', 'denominator')]

    #finnNyResh <- setdiff(sort(unique(RegDataUt$ReshId)), sort(names(nyID)))
    #RegDataUt[match(nyResh, RegDataUt$ReshId), 'ShNavn']
    RegDataUt$ind_id <- indID
    RegDataUt$context <- 'caregiver'


  if (lastNedFil==1) {
    write.table(RegDataUt, file = filUt, sep = ';', row.names = F)} #, fileEncoding = 'UTF-8')}
  return(invisible(RegDataUt))
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


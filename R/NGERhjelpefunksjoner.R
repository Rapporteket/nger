# Hjelpefunksjoner for NGER
#---------------------------------------------


#' Kjør Shiny Application
#'
#' @param browser App åpner i browser
#' @param logAsJson Logg i json-format
#'
#' @return Et objekt som representerer den NGERapp'en
#' @export

kjor_NGERapp <- function(browser = FALSE, logAsJson = FALSE) {

  if (logAsJson) {
    rapbase::loggerSetup()
  }
  if (browser) {
    options(shiny.launch.browser = TRUE)
  }
  app <- shiny::shinyApp(
    ui = nger::ui_nger,
    server = nger::server_nger
  )

  return(app)
}


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

      tidtxt <- switch(tidsenhet,
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
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
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
#' @param rmdFil Fila som skal kompileres. Eks. 'Eksempel.Rmd'
#' @param reshID reshID
#'
#' @export
henteSamlerapporter <- function(filnavn, rmdFil, reshID = 0) {

  owd <- getwd()
  setwd(tempdir())
  on.exit(setwd(owd))

  message(paste0("Genererer rapport for reshID=", reshID, " ..."))
  rmarkdown::render(
    input = system.file(rmdFil, package = "nger"),
    params = list(reshId = reshID),
    output_format = "pdf_document",
    clean = TRUE,
    output_file = filnavn
  )
  gc() #Opprydning gc-"garbage collection"
}

#' Kjøre samlerapporter for abonnement i NGER
#'
#' @param rnwFil NoWeb-fil med filending: eksempel.rnw
#' @param reshID brukerens reshID
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @return gir filsti til pdf-samledokument
#' @export
#'
abonnementNGER <- function(rnwFil, reshID=0, #brukernavn='ngerBrukernavn',
                           datoFra=Sys.Date()-180, datoTil=Sys.Date()) {

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(Sys.time()), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='nger'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(input=tmpFile)

  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')


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
  #Legge på orgID
  #ReshId	orgnr	RapporteketNavn	SKDEnavn
  nyID <- c('108172' = '974706490',		#Ahus	AHUS NORDBYHAGEN SOMATIKK	Ahus
            '107511' = '975787419',		#Aleris Frogner 975 787 419
            '4212372' = '981541499',  #Aleris Majorstuen / Aleris Colosseum Nobel
            '4211880' = '974518821', #Aleris Nesttun
            '4209817' = '974504863', #Aleris Trondheim (Solsiden)
            '4209824' = '983896383', #Aleris Sykehus Stavanger 983 896 383
            '103719' = '974631091',		#Arendal	SØRLANDET SYKEHUS HF SOMATIKK ARENDAL	Arendal
            '106843' = '922748144',		#Betanien	BETANIEN SYKEHUS AS	Betanien
            '706220' = '974795361',		#Bodø	NORDLANDSSYKEHUSET HF SOMATIKK - BODØ	Bodø
            '104736' = '974705788',		#Bærum	VESTRE VIKEN HF BÆRUM SYKEHUS - SOMATIKK	Bærum
            '4211285' = '812794922',  #C-Medical Colloseum - C-MEDICAL AS, tidl. resh 4211286
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
            '108048' = 	'974633752',		#Kalnes Østfold	SYKEHUSET ØSTFOLD
            '101828' = 	'974795930',		#Kirkenes	FINNMARKSSYKEHUSET HF KIRKENES SYKEHUS	Kirkenes
            '104174' = 	'974631385',		#Kongsberg	VESTRE VIKEN HF KONGSBERG SYKEHUS - SOMATIKK	Kongsberg
            '4215373' = 	'974631776',		#Kongsvinger	AHUS KONGSVINGER SOMATIKK	Kongsvinger
            '100412' = 	'974733013',		#Kristiansand	SØRLANDET SYKEHUS HF SOMATIKK KRISTIANSAND	Kristiansand
            #        '103189' = 	'974746948',		#Kristiansund	HELSE MØRE OG ROMSDAL HF KRISTIANSUND SJUKEHUS - SOMATIKK	Kristiansund. Ingen reg...
            '105863' = 	'974754118',		#Levanger	HELSE NORD-TRØNDELAG HF SOMATIKK - LEVANGER	Levanger
            '111180' = 	'874632562',		#Lillehammer	SYKEHUSET INNLANDET HF LILLEHAMMER - SOMATIK
            '700789' = 	'974795515',		#Mo i Rana	HELGELANDSSYKEHUSET HF MO I RANA - SOMATIKK	Mo i Rana
            '105874' = 	'974753898',		#Namsos	HELSE NORD-TRØNDELAG HF SOMATIKK - NAMSOS	Namsos
            '706130' = 	'974795396',		#Narvik	UNIVERSITETSSYKEHUSET NORD-NORGE HF NARVIK - SOMATIKK	Narvik
            '103188' = 	'974745569',		#Molde	HELSE MØRE OG ROMSDAL HF MOLDE SJUKEHUS - SOMATIKK	Molde
            '4212080' = '974745569',    #Nordmøre og Romsdal
            '103575' = 	'974631407',		#Ringerike	VESTRE VIKEN HF RINGERIKE SYKESHUS - SOMATIKK	Ringerike
            '103162' = 	'974795477',		#Sandessjøen	HELGELANDSSYKEHUSET HF SANDNESSJØEN - SOMATIKK	Sandnessjøen
            '4205296' = '974633191',		#Skien	SYKEHUSET TELEMARK HF SKIEN - SOMATIKK	Skien
            '105460' = 	'974703300',		#Stavanger	STAVANGER UNIVERSITETSSJUKEHUS SOMATIKK VÅLAND	Stavanger
            '100460' = 	'974795574',		#Stokmarknes	NORDLANDSSYKEHUSET HF SOMATIKK - STOKMARKNES	Vesterålen
            '701482' = 	'974742985',		#Stord	HELSE FONNA HF STORD SJUKEHUS	Stord
            '601263' = 	'974795787',		#Tromsø	UNIVERSITETSSYKEHUSET NORD-NORGE HF TROMSØ - SOMATIKK	Tromsø gyn. kreft
            '601227' = 	'974795787',		#Tromsø	UNIVERSITETSSYKEHUSET NORD-NORGE HF TROMSØ - SOMATIKK	Tromsø
            '107644' = 	'974749025',		#Trondheim	ST OLAVS HOSPITAL HF UNIVERSITETSSYKEHUSET I TRONDHEIM	St. Olavs
            '110734' = 	'974633574',		#Tønsberg	SYKEHUSET I VESTFOLD HF, SOMATIKK, TØNSBERG	Tønsberg
            '700399' = 	'974589095',		#Ullevål	OSLO UNIVERSITETSSYKEHUS HF ULLEVÅL - SOMATIKK	Ullevål
            '102583' = 	'974747545',		#Volda	HELSE MØRE OG ROMSDAL HF VOLDA SJUKEHUS - SOMATIKK	Volda
            '4205477' = '991811869', #Volvat Forus
            '4215139' = '973129856',    #Volvat Majorstuen -> Flyttet til Storo?
            '4219487' = '928089428',     #Volvat Storo
            '4218851' = '990240116',     #Volvat Ulriksdal
            '106026' = 	'974743272',		#Voss	HELSE BERGEN HF VOSS SJUKEHUS	Voss
            '102582' = 	'974747138',		#Ålesund	HELSE MØRE OG ROMSDAL HF ÅLESUND SJUKEHUS - SOMATIKK	Ålesund
            '1' = '1'              #Hele landet
  )

  RegData$orgnr <- as.character(nyID[as.character(RegData$ReshId)])
  #unique(RegDataUt[ ,c('ShNavn', "ReshId", "orgnr")])


  #finnNyResh <- setdiff(sort(unique(RegDataUt$ReshId)), sort(names(nyID))) ,
  #RegDataUt[match(finnNyResh, RegDataUt$ReshId), 'ShNavn']

  RegDataUt <- RegData[,c('Aar', "orgnr", "Variabel")]
  RegDataUt <- dplyr::rename(RegDataUt,
                             year = Aar,
                             var = Variabel)

      if (aggData == 0) {
        RegDataUt$denominator <- 1
        }

      if (aggData == 1) {
        if (figurtype == 'gjsnGrVar'){
          #aggData <- tapply(RegData$Variabel, INDEX = RegData[,c('year', 'orgnr')], FUN = mean)
          #aggData <- aggregate(RegData$Variabel, by = list(RegData$year, RegData$orgnr), FUN = mean)
          RegDataUt <- RegDataUt %>%
            dplyr::group_by(orgnr, year) %>%
            dplyr::summarise(
              denominator = sum(!is.na(var)),
              var = mean(var, na.rm=T))

          RegDataUtLand <- RegDataUt %>%
            dplyr::group_by(year) %>%
            dplyr::summarise(
              denominator = sum(!is.na(var)),
              var = mean(var, na.rm=T),
              orgnr = '1')

          RegDataUt <- rbind(RegDataUt, RegDataUtLand)

          RegDataUt <- RegDataUt[-which(RegDataUt$denominator == 0), ] #Fjerner tomme
          RegDataUt <- dplyr::rename(RegDataUt, year = year)

        } }


    RegDataUt$ind_id <- indID
    RegDataUt$context <- 'caregiver'

  if (lastNedFil==1) {
    write.table(RegDataUt, file = filUt, sep = ';', row.names = F)} #, fileEncoding = 'UTF-8')}
  return(invisible(RegDataUt))
}




#' Funksjoner for å lage tabeller Group of functions page title
#'
#' Fil som beregner div tabeller.Group of functions Description section
#'
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer
#' RegData må inneholde InnDato og Aar.
#' Aktuelle tabeller:
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil.
#'
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
# @inheritParams NIRFigAndeler
#' @return Div tabeller
#' @name NGERtabeller
NULL
#' @rdname NGERtabeller
#' @export

#' @section Antall opphold siste X (antMnd) mnd
#' @rdname NGERtabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive
      datoFra <- lubridate::floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month') #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(ymd(colnames(tabAvdMnd1)), '%b %y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd1 <- addmargins((tabAvdMnd1))
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1, digits=0)
	return(tabAvdMnd1)
}
tabAntOpphShMnd(RegData, datoTil=Sys.Date(), antMnd=3)

#' @section Antall opphold siste 5 år
#' @rdname NGERtabeller
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date()){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('SykehusNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)

      RegData$Sh
      tabAntOpphSh5Aar(RegData=RegData, datoTil=Sys.Date())
}




#' @section Vise figurdata som tabell
#' @rdname NGERtabeller
#' @export
lagTabavFig <- function(UtDataFraFig){
      tab <-cbind(UtDataFraFig$Ngr$Hoved,
                  UtDataFraFig$AggVerdier$Hoved,
                  UtDataFraFig$Ngr$Rest,
                  UtDataFraFig$AggVerdier$Rest)
      grtxt <- UtDataFraFig$grtxt
      if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
            grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
      rownames(tab) <- grtxt
      kolnavn <- c('Antall' , 'Andel (%)')
      colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
      # colnames(tab) <- c(paste0(UtDataFraFig$hovedgrTxt,', Antall'),
#                    paste0(UtDataFraFig$hovedgrTxt, ', Andel (%)'),
#                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt,', Antall')},
#                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt, ', Andel (%)')})

return(tab)
}

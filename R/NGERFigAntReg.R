#' Søylediagram som viser antall registreringer per måned
#'
#' Søylediagram som viser antall registreringer per måned, siste 12 hele måneder fra valgt sluttdato,
#'  og sammenligner med fjoråret.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NGERFigAndeler
#' @param enhetsUtvalg Gjør gruppeutvalg for
#'                 0: Hele landet
#'                 2: Egen enhet
#'
#' @return Søylediagram som viser antall registreringer per måned for valgte 12 måneder
#'
#' @export
NGERFigAntReg  <- function(RegData, datoTil=Sys.Date(), antMnd=12, #datoFra = '2014-01-01',
                           minald=0, maxald=130, erMann='', outfile='',
                           reshID=0, enhetsUtvalg=2, preprosess=1, hentData=0)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd*2+1, abbreviate = T), unit='month')
    RegData <- NGERRegDataSQL(datoFra = datoFra) #, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }


  #Gjør utvalg (siste 12 hele mnd)
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, enhetsUtvalg = enhetsUtvalg, reshID=reshID )
                              #datoFra = datoFra, datoTil = datoTil,
  # , OpMetode = OpMetode, minald = minald, maxald = maxald,

  RegData <- NGERUtvalg$RegData
  #hovedgrTxt <- NGERUtvalg$hovedgrTxt
  utvalgTxt <- NGERUtvalg$utvalgTxt

  tittel <- 'Registreringer, 12 månedersperiode,'

reshID <- 0#103575

gyldigResh <- reshID!=0 & !is.na(match(reshID, RegData$ReshId))
if (gyldigResh) {RegData <- RegData[which(RegData$ReshId==reshID), ]}
datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd, abbreviate = T), unit='month')
aggVar <-  c('ShNavn', 'InnDato')
# Utvalg <- NGERUtvalgEnh(RegData=RegData, OpMetode = OpMetode, velgDiag=velgDiag)
# RegData <- Utvalg$RegData
RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                      & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(lubridate::ymd(colnames(tabAvdMnd1)), label = T)

  # tabAntOpphEget12Mnd <- tabAntOpphShMnd(RegData=RegData, datoTil=Sys.Date(), antMnd=12, reshID=reshID,
  #                             OpMetode=99, velgDiag=0)
  # tabAntOpphEget12MndForrige <- tabAntOpphShMnd(RegData=RegData, datoTil=Sys.Date()-365, antMnd=12, reshID=reshID,
  #                                        OpMetode=99, velgDiag=0)
  # if (reshID == 0) {
  #   tabAntOpphEget12Mnd <- tabAntOpphEget12Mnd['Sum',-which(dimnames(tabAntOpphEget12Mnd)$Maaned1=='Sum')]
  #   tabAntOpphEget12MndForrige <-
  #     tabAntOpphEget12MndForrige['Sum', -which(dimnames(tabAntOpphEget12MndForrige)$Maaned1=='Sum')]
  #   }

  #-----------Figur---------------------------------------

  #Plottspesifikke parametre:
  NutvTxt <- length(utvalgTxt)
  ymax <- max(AntHoved,na.rm=T)*1.25

  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)	#res=96, width=555,height=555,
  farger <- FigTypUt$farger
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

  pos <- barplot(tabAntOpphEget12Mnd, beside=FALSE, las=1, ylab="Antall registreringer",
                 pos = ,
                # names.arg=rep(NA,12), #cex.names=0.9,
                 col=farger[1], border=NA, ylim=c(0, ymax))	#
  points(as.numeric(tabAntOpphEget12MndForrige), pos, col=farger[3],  cex=2, pch=18) #c("p","b","o"),
  # legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
  #                 paste0(smltxt, ' (N=', Nfig$Rest,')')),
  #        border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
  #        lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)

  #mtext(grtxt, at=pos, side=1, las=1, cex=1, adj=0.5, line=0.5)
  mtext('Siste 12 hele måneder fra valgt sluttdato', side=1, las=1, cex=1, adj=0.5, line=2.5)
  lines(x=c(pos[13-startmnd]+0.6, pos[13-startmnd]+0.6), y=c(0, max(AntHoved)), lty=2, col=farger[3])

  legend('topright', c('Valgte tidsperiode', 'Forrige tidsperiode'), bty='n',
         fill=farger[c(3,1)], border=NA, ncol=1, cex=0.9)
  title(tittel, font.main=1, cex=1.2)	#line=0.5,
  title(paste0(hovedgrTxt, ' (N=', dim(RegData)[1],')'), font.main=1, line=0.5)

  #Tekst som angir hvilket utvalg som er gjort
  avst <- 0.8
  utvpos <- 3	#Startlinje for teksten
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

  #}
}

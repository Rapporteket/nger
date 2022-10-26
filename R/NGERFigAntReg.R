#' Søylediagram som viser antall registreringer per måned
#'
#' Søylediagram som viser antall registreringer per måned, siste 12 hele måneder fra valgt sluttdato,
#'  og sammenligner med fjoråret.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NGERFigAndeler
#'
#' @return Søylediagram som viser antall registreringer per måned for valgte 12 måneder
#'
#' @export
NGERFigAntReg  <- function(RegData, datoTil=Sys.Date(), reshID=0,
                           minald=0, maxald=130, erMann='',
                           preprosess=0, hentData=0, outfile='')
{
  #reshID <- 103575

  datoFra <- lubridate::floor_date(as.Date(datoTil)- months(23, abbreviate = T), unit='month')
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil) #, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  gyldigResh <- reshID!=0 & !is.na(match(reshID, RegData$ReshId))
  reshID <- ifelse(gyldigResh, reshID, 0)
  if (gyldigResh) {RegData <- RegData[which(RegData$ReshId==reshID), ]}

#Gjør utvalg
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil)
  # , OpMetode = OpMetode, minald = minald, maxald = maxald,

  RegData <- NGERUtvalg$RegData
  ShTxt <- ifelse(reshID ==0, 'Hele landet', RegData$ShNavn[match(reshID, RegData$ReshId)])
  utvalgTxt <- NGERUtvalg$utvalgTxt[-1] #Ønsker ikke datoutvalg
  tittel <- 'Registrerte operasjoner'

maaneder <- factor(format(RegData$InnDato, '%b %y'),
                levels = format(seq(datoFra, as.Date(datoTil), by="month"), "%b %y"))

AntMnd <- table(maaneder)

  #-----------Figur---------------------------------------

  #Plottspesifikke parametre:
  NutvTxt <- length(utvalgTxt)
  ymax <- max(AntMnd,na.rm=T)*1.25

  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)	#res=96, width=555,height=555,
  farger <- FigTypUt$farger
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt)))

  pos <- barplot(AntMnd[13:24], beside=FALSE, las=1, ylab="Antall registreringer",
                # names.arg=rep(NA,12), #cex.names=0.9,
                 col=farger[1], border=NA, ylim=c(0, ymax))	#
  points(pos[1:12], as.numeric(AntMnd[1:12]), col=farger[3], cex=2, pch=18) #c("p","b","o"), #

  mtext('Siste 12 måneder', side=1, las=1, cex=1, adj=0.5, line=2.5)
  startmnd <- lubridate::month(datoFra)
  if (startmnd != 1) {
    lines(x=c(pos[13-startmnd]+0.6, pos[13-startmnd]+0.6), y=c(0, ymax), lty=2, col=farger[3])
  }

  legend('topright', c('Siste 12 mnd.', 'Forrige 12 mnd.'), bty='n',
          ncol=1, cex=0.9, pch=c(15,18), pt.cex=2, col=farger[c(1,3)], border=NA)
  title(tittel, font.main=1, cex=1.2)	#line=0.5,
  title(paste0(ShTxt, ' (N=', sum(AntMnd[13:24]),')'), font.main=1, line=0.5)

  #Tekst som angir hvilket utvalg som er gjort
  avst <- 0.8
  utvpos <- 3	#Startlinje for teksten
  if (NutvTxt>0) {
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
  }

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

  return(invisible(AntMnd))
}

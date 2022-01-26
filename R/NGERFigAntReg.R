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
#'				   7: Egen region
#'
#' @return Søylediagram som viser antall registreringer per måned for valgte 12 måneder
#'
#' @export
NGERFigAntReg  <- function(RegData, datoTil=Sys.Date(),
                           minald=0, maxald=130, erMann='', outfile='',
                           reshID=0, enhetsUtvalg=2, preprosess=1, hentData=0)
{


  # if ("session" %in% names(list(...))) {
  #   rapbase::repLogger(session = list(...)[["session"]], msg = paste0('NGERFigAntReg: ',valgtVar))
  # }

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra) #, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }


    #reshID <- as.numeric(reshID)
  # indEgen1 <- match(reshID, RegData$ReshId)
  # shtxt <- switch(as.character(enhetsUtvalg),
  #                 '0' = 'Hele landet',
  #                 '2' = as.character(RegData$Avdeling[indEgen1]),
  #                 '7' = as.character(RegData$Region[indEgen1]))
  # if (enhetsUtvalg %in% c(2,7)) {
  #   RegData <- switch(as.character(enhetsUtvalg),
  #                     '2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
  #                     '7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
  # }


  DagensDato <- as.POSIXlt(datoTil, tz='UTC', format='%Y-%m-%d')	#min(, as.POSIXlt(Sys.Date()))
  datoFra <- as.POSIXlt(paste(DagensDato$year-1+1900,'-',(DagensDato$mon+1),'-',1, sep=''))
  datoTil <- as.POSIXlt(paste(DagensDato$year+1900,'-',(DagensDato$mon+1),'-',1, sep='')) - 60*60*24

  #Gjør utvalg (siste 12 hele mnd)
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil)
  # , OpMetode = OpMetode, minald = minald, maxald = maxald,
  # enhetsUtvalg = enhetsUtvalg, reshID=reshID )
  RegData <- NGERUtvalg$RegData
  #utvalgTxt <- NGERUtvalg$utvalgTxt
  ind <- NGERUtvalg$ind
  hovedgrTxt <- NGERUtvalg$hovedgrTxt
  utvalgTxt <- NGERUtvalg$utvalgTxt

  tittel <- 'Registreringer, 12 månedersperiode,'


  #Standardparametre
  cexgr <- 0.9
  retn <- 'V'
  txtretn <- 1
  grtxt <- ''
  subtxt <- 'Siste 12 hele måneder fra valgt sluttdato'
  Nsvar <- dim(RegData)[1]

  mndtxt <- c('jan','feb','mar','apr','mai','jun','jul','aug','sep','okt','nov','des')
  startmnd <- DagensDato$mon+1
  mndRekkef <- c(startmnd:12, min(1,(startmnd-1)):(startmnd-1))	#Må ta høyde for startmnd=1
  grtxt <- mndtxt[mndRekkef]
  RegData$InnDato <- as.POSIXlt(RegData$InnDato)
  RegData$Mnd <- factor(RegData$InnDato$mon, levels= 0:11, labels = mndtxt)
  AntHoved <- table(RegData$Mnd)[mndRekkef]
  indOppf <- which(RegData$OppfolgUtf==1) 						#Levende med oppfølging
  AntOppf <- table(RegData$Mnd[indOppf])[mndRekkef]

  #Splitte opp i Lap og Hys? Sjekk om Lap+Hys+Begge=100
  indDod <- with(RegData, which(UtskrTil==10 | Dod98==1))	#Død under sykehusoppholdet eller innen 98 dager
  AntDod <- table(RegData$Mnd[indDod])[mndRekkef]

  AntHODmnd <- cbind(AntOppf, AntDod, AntHoved-(AntOppf+AntDod))


  #-----------Figur---------------------------------------

  #Plottspesifikke parametre:
  antGr <- 12
  Ngr <- matrix(c(AntHoved, AntOppf, AntDod), antGr, 3)
  NutvTxt <- length(utvalgTxt)
  ymax <- max(AntHoved,na.rm=T)*1.25

  FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)	#res=96, width=555,height=555,
  farger <- FigTypUt$farger
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

  pos <- barplot(t(AntHODmnd), beside=FALSE, las=txtretn, ylab="Antall registreringer",
                 names.arg=rep(NA,12), #cex.names=cexgr,
                 col=farger[1:3], border=NA, ylim=c(0, ymax))	#
  mtext(grtxt, at=pos, side=1, las=1, cex=1, adj=0.5, line=0.5)
  mtext(subtxt, side=1, las=1, cex=1, adj=0.5, line=2.5)
  lines(x=c(pos[13-startmnd]+0.6, pos[13-startmnd]+0.6), y=c(0, max(AntHoved)), lty=2, col=farger[3])

  legend('topright', c('Bare hovedskjema', 'Døde', 'Oppfølging, 3 mnd'), bty='n',
         fill=farger[3:1], border=NA, ncol=1, cex=0.9)
  title(tittel, font.main=1, cex=1.2)	#line=0.5,
  title(paste0(hovedgrTxt, ' (N=', Nsvar,')'), font.main=1, line=0.5)

  #Tekst som angir hvilket utvalg som er gjort
  avst <- 0.8
  utvpos <- 3	#Startlinje for teksten
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

  #}
}

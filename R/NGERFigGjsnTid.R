#' Utvikling over tid for gjennomsnitt/median av valgt variabel
#'
#' Figuren viser gjennomsnitt/median per år med konfidensintervall for valgt variabel.
#' I bakgrunn vises konfidensintervall for det man har valt å sammenlikne med.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Pasientens alders
#'    }
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERVarTilrettelegg
#' @param valgtMaal
#'        'gjsn': gir middelverdi (standard)
#'        'med': gir median
#' @param valgtVar Hvilken variabel som skal visualiseres
#'
#' @return Linjediagram som viser utvikling over tid for valgt variabel
#'
#' @export
NGERFigGjsnTid <- function(RegData, valgtVar='alder', datoFra='2011-01-01', datoTil='3000-12-31',
                           tidsenhet='Aar', minald=0, maxald=110, reshID=0,
                           OpMetode=99, velgDiag = 0,
                           behNivaa = 0, AlvorlighetKompl=0, #Hastegrad='',
                           outfile='',enhetsUtvalg=0, valgtMaal='Gjsn', preprosess=1, hentData=0,
                           lagFigur=1,...){

  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)	#, reshID=reshID)
  }


  #--------------- Definere variable ------------------------------

  NGERVarSpes <- NGERVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='gjsnTid')
  RegData <- NGERVarSpes$RegData


  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, #aar=aar,
                              minald=minald, maxald=maxald,
                              OpMetode=OpMetode, behNivaa = behNivaa, # Hastegrad=Hastegrad,
                              AlvorlighetKompl=AlvorlighetKompl, velgDiag = velgDiag,
                              reshID=reshID, enhetsUtvalg=enhetsUtvalg)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt
  medSml <- NGERUtvalg$medSml
  #------------------------Klargjøre tidsenhet--------------
  N <- list(Hoved = dim(RegData)[1], Rest=0)
  ind <- NGERUtvalg$ind
  Ngr <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))

    #Nok observasjoner til å lage figur?
  if (length(ind$Hoved)<10 | ((medSml == 1) & (length(ind$Rest) < 10))) {

    FigDataParam <- list(AggVerdier = rbind(Midt=NA, Konf=NA, MidtRest=NA, KonfRest=NA),
                         N=N,
                         Ngr=Ngr,
                         grtxt=levels(RegData$TidsEnhet),
                         tittel=NGERVarSpes$tittel,
                         utvalgTxt=utvalgTxt,
                         fargepalett=NGERUtvalg$fargepalett,
                         medSml=medSml,
                         hovedgrTxt=NGERUtvalg$hovedgrTxt,
                         smltxt=NGERUtvalg$smltxt)

    rapFigurer::figtype(outfile)
    plot.new()
    title(main=NGERVarSpes$tittel)
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    #	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
    if ( outfile != '') {dev.off()}

  } else {

    RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
    RegData <- RegDataFunk$RegData
    tidNum <- min(RegData$TidsEnhetSort, na.rm=T):max(RegData$TidsEnhetSort, na.rm = T) #as.numeric(levels(RegData$TidsEnhetSort))

    KIekstrem <- NULL


        #--------------- Gjøre beregninger ------------------------------

    #Resultat for hovedgruppe
    N <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], length)
    if (valgtMaal=='med') {
      MedIQR <- plot(RegData$TidsEnhet[ind$Hoved],RegData$Variabel[ind$Hoved],  notch=TRUE, plot=FALSE)
      Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
      Konf <- MedIQR$conf
      #Hvis vil bruke vanlige konf.int:
      #j <- ceiling(N/2 - 1.96*sqrt(N/4))
      #k <- ceiling(N/2 + 1.96*sqrt(N/4))
      #KIHele <- sort(RegData$Variabel)[c(j,k)]
      #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
      #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
      #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
      #roughly a 95% confidence interval for the difference in two medians.
    } else {	#Gjennomsnitt blir standard.
      Midt <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], mean, na.rm=T)
      SD <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], sd, na.rm=T)
      Konf <- rbind(Midt - 2*SD/sqrt(N), Midt + 2*SD/sqrt(N))
    }

    if (length(KIekstrem) == 0) {	#Hvis ikke KIekstrem definert i variabeldefinisjonen
      KIekstrem <- c(0,max(RegData$Variabel, na.rm=T))
    }
    Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
    Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

    #Resten (gruppa det sammenliknes mot)
    MidtRest <- NULL
    KonfRest <- NULL
    if (medSml ==  1) {
      Nrest <- tapply(RegData[ind$Rest ,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
      if (valgtMaal=='med') {
        MedIQRrest <- plot(RegData$TidsEnhet[ind$Rest],RegData$Variabel[ind$Rest],  notch=TRUE, plot=FALSE)
        MidtRest <- as.numeric(MedIQRrest$stats[3, ])
        KonfRest <- MedIQRrest$conf
      } else {
        MidtRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], mean, na.rm=T)	#ind$Rest
        SDRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], sd, na.rm=T)
        Nrest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
        KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(Nrest), MidtRest + 2*SDRest/sqrt(Nrest))
      }
    }

 # }
  t1 <- switch(valgtMaal,
               med = 'Median ',
               gjsn = 'Gjennomsnittlig ')
  tittel <- paste0(t1, NGERVarSpes$tittel)

  if (valgtMaal=='med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}

  ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
  rownames(ResData) <- c(maaltxt, 'KImin', 'KImaks',
                         paste0(maaltxt, 'Resten'), 'KImin, Resten', 'KImaks, Resten')[1:(3*(medSml+1))]

  FigDataParam <- list(AggVerdier=ResData,
                       N=N,
                       Ngr=Ngr,
                       grtxt=levels(RegData$TidsEnhet),
                       tittel=NGERVarSpes$tittel,
                       utvalgTxt=utvalgTxt,
                       fargepalett=NGERUtvalg$fargepalett,
                       medSml=medSml,
                       hovedgrTxt=NGERUtvalg$hovedgrTxt,
                       smltxt=NGERUtvalg$smltxt)


if (lagFigur==1) {
  #-----------Figur---------------------------------------


    xmin <- min(tidNum)-0.5
    xmax <- max(tidNum)+0.5
    cexgr <- 0.9	#Kan endres for enkeltvariable
    ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
    ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
    ytxt <- maaltxt #paste0(maaltxt, ytxt1, sep='')

    #Plottspesifikke parametre:
    FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

    farger <- FigTypUt$farger
    fargeHovedRes <- farger[1]
    fargeRestRes <- farger[4]
    #
    plot(tidNum,Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
         #cex=0.8, cex.lab=0.9, cex.axis=0.9,
         ylab=c(ytxt,'med 95% konfidensintervall'),
         xlab='Operasjonstidspunkt', xaxt='n',
         sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
    axis(side=1, at = tidNum, labels = levels(RegData$TidsEnhet))
    #Sammenlikning:
    if (medSml==1) {
      # polygon( c(tidtxt[1]-0.01,tidtxt, tidtxt[AntTidsenh]+0.012,
      #            tidtxt[AntTidsenh]+0.012, tidtxt[AntTidsenh:1], tidtxt[1]-0.01),
      #          c(KonfRest[1,c(1,1:AntTidsenh, AntTidsenh)], KonfRest[2,c(AntTidsenh,AntTidsenh:1,1)]),
      #          col=fargeRestRes, border=NA)
      AntTidsenh <- max(which(!is.na(KonfRest[1,])))
      polygon( c(tidNum[1]-0.01,tidNum[1:AntTidsenh], tidNum[AntTidsenh]+0.012,
                 tidNum[AntTidsenh]+0.012, tidNum[AntTidsenh:1], tidNum[1]-0.01),
               c(KonfRest[1,c(1,1:AntTidsenh, AntTidsenh)], KonfRest[2,c(AntTidsenh,AntTidsenh:1,1)]),
               col=fargeRestRes, border=NA)
      legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
             paste0('95% konfidensintervall for ', NGERUtvalg$smltxt, ', N=', sum(Nrest, na.rm=T)))
    }
    h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
    b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(tidNum)/30
    rect(tidNum-b, Midt-h, tidNum+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
    text(tidNum, Midt, N, col=fargeHovedRes, cex=cexgr)

    #Konfidensintervall:
    ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
    options('warn'=-1)
    arrows(x0=tidNum, y0=Midt-h, x1=tidNum, length=0.08, code=2, angle=90,
           y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
    arrows(x0=tidNum, y0=Midt+h, x1=tidNum, y1=replace(Konf[2, ], ind, Midt[ind]+h),
           length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

    title(main=c(tittel, NGERUtvalg$hovedgrTxt), font.main=1, line=1)
    #Tekst som angir hvilket utvalg som er gjort
    if (length(utvalgTxt)>0) {
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))}

    if ( outfile != '') {dev.off()}
} #lag figur
 }	#end if statement for få observations
  return(invisible(FigDataParam))

}	#end function

#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERFigAndelerGrVar
#' @inheritParams NGERUtvalgEnh
#' @param valgtMaal 'med' = median. Alt annet gir gjennomsnitt
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Liggetid
#'     \item RAND36 - alle dimensjoner
#'     \item Registreringsforsinkelse
#'     \item TSS2
#'    }
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NGERFigGjsnGrVar <- function(RegData, datoFra='2013-01-01', datoTil='3000-12-31',
                             valgtVar, minald=0, maxald=130, OpMetode=99, AlvorlighetKompl=99,
                             Hastegrad=99, valgtMaal='gjsn', hentData=0, preprosess=1,
                             grVar='ShNavn', velgAvd=0, velgDiag=0, Ngrense=10,medKI=1,
                             lagFig=1, outfile='',...) {     #aar=0,

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('Figur, sentralmål: ',valgtVar))
  }
  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøres dette i samledokumentet)
  if (preprosess == 1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  #------- Tilrettelegge variable
  NGERVarSpes <- NGERVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
  RegData <- NGERVarSpes$RegData
  KvalIndGrenser <- NGERVarSpes$KvalIndGrenser
  sortAvtagende <- NGERVarSpes$sortAvtagende

  #------- Gjøre utvalg
  NGERUtvalg <- NGERUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil,
                              minald = minald, maxald = maxald,
                              OpMetode = OpMetode, AlvorlighetKompl=AlvorlighetKompl,
                              Hastegrad=Hastegrad, velgAvd=velgAvd, velgDiag=velgDiag)
  smltxt <- NGERUtvalg$smltxt
  #medSml <- NGERUtvalg$medSml
  utvalgTxt <- NGERUtvalg$utvalgTxt
  #ind <- NGERUtvalg$ind
  hovedgrTxt <- NGERUtvalg$hovedgrTxt
  RegData <- NGERUtvalg$RegData

  '%i%' <- intersect

  RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
  #Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
  #grupper komme med uansett om de ikke har registreringer.

  if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
  sjekkNgr <- max(Ngr, na.rm = T) < Ngrense

  t1 <- switch(valgtMaal,
               med = 'Median ',
               gjsn = 'Gjennomsnittlig ')

  tittel <- paste0(t1, NGERVarSpes$tittel) #NGERVarSpes$tittel #

  Ngrtxt <- paste0(' (', as.character(Ngr),')')
  indGrUt <- which(Ngr < Ngrense)
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')
  N <- dim(RegData)[1]

  KIHele <- c(0,0)
  KIned <- c(0,0)
  KIhele <- c(0,0)


  dummy0 <- NA #-0.0001
  #Kommer ut ferdig sortert!
  if (valgtMaal=='med') {
    MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
    MedIQR$stats[ ,indGrUt] <- dummy0
    MedIQR$conf[ ,indGrUt] <- dummy0
    sortInd <- order( MedIQR$stats[3,], decreasing=NGERVarSpes$sortAvtagende, na.last = FALSE)
    Midt <- as.numeric(MedIQR$stats[3, sortInd])
    KIned <- MedIQR$conf[1, sortInd]
    KIopp <- MedIQR$conf[2, sortInd]
    MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
    MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
    KIHele <- MedIQRHele$conf
    xAkseTxt <- paste0('Median ',NGERVarSpes$xAkseTxt)
    #Hvis vil bruke vanlige konf.int:
    #j <- ceiling(N/2 - 1.96*sqrt(N/4))
    #k <- ceiling(N/2 + 1.96*sqrt(N/4))
    #KIHele <- sort(RegData$Variabel)[c(j,k)]
    #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
    #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
    #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
    #roughly a 95% confidence interval for the difference in two medians.
  }

  if (valgtMaal=='gjsn') {	#Gjennomsnitt er standard, men må velges.
    Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
    SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
    MidtHele <- mean(RegData$Variabel, na.rm=T)	#mean(RegData$Variabel)
    KIHele <- MidtHele + sd(RegData$Variabel, na.rm = T)/sqrt(N)*c(-2,2)
    Gjsn[indGrUt] <- dummy0
    SE[indGrUt] <- 0
    sortInd <- order(Gjsn, decreasing=NGERVarSpes$sortAvtagende, na.last = FALSE)
    Midt <- Gjsn[sortInd] #as.numeric(Gjsn[sortInd])
    KIned <- Gjsn[sortInd] - 2*SE[sortInd]
    KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
    xAkseTxt <- paste0('Gjennomsnittlig ', NGERVarSpes$xAkseTxt)
  }


  GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd])
  soyletxt <- sprintf(paste0('%.1f'), Midt)
  indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
  soyletxt[indUT] <- ''
  KIned[indUT] <- NA
  KIopp[indUT] <- NA

  AggVerdier <- list(Hoved=Midt, Rest=NULL, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
  Ngr <- list(Hoved=Ngr[sortInd], Rest=NULL)

  SentralmaalTxt <- switch(valgtMaal,
                           gjsn='Gjennomsnitt',
                           med='Median')
  #Se NGERFigSoyler for forklaring av innhold i lista GjsnGrVarData
  GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                        N=list(Hoved=N),
                        Ngr=Ngr,
                        grtxt2='',
                        medKI=medKI,
                        KImaal = NGERVarSpes$KImaal,
                        soyletxt=soyletxt,
                        grtxt=GrNavnSort,
                        valgtMaal=valgtMaal,
                        SentralmaalTxt=SentralmaalTxt,
                        tittel=tittel,    #NGERVarSpes$tittel,
                        #yAkseTxt=yAkseTxt,
                        retn='H',
                        xAkseTxt=NGERVarSpes$xAkseTxt,
                        grTypeTxt=NGERUtvalg$grTypeTxt,
                        utvalgTxt=NGERUtvalg$utvalgTxt,
                        fargepalett=NGERUtvalg$fargepalett,
                        medSml=NGERUtvalg$medSml,
                        smltxt=NGERUtvalg$smltxt)


  #FigDataParam skal inn som enkeltparametre i funksjonskallet
  #lagFig <- 1
  if (lagFig == 1) {
    cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
    AggTot <- MidtHele
    KImaal <- NGERVarSpes$KImaal

    #---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
    #Hvis for få observasjoner..

    if (dim(RegData)[1] < 10 | sjekkNgr)
      #|(grVar=='' & length(enhetsUtvalg %in% c(1,3)))
    {
      #-----------Figur---------------------------------------
      FigTypUt <-rapFigurer::figtype(outfile)  #FigTypUt <- rapFigurer::figtype(outfile)
      farger <- FigTypUt$farger
      plot.new()
      title(tittel)	#, line=-6)
      legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      tekst <- 'For få registreringer for hver enhet'
      text(0.5, 0.6, tekst, cex=1.2)
      if ( outfile != '') {dev.off()}

    } else {


      #Plottspesifikke parametre:
      #Høyde må avhenge av antall grupper
      hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
      FigTypUt <- rapFigurer::figtype(outfile, height=hoyde, fargepalett=NGERUtvalg$fargepalett)
      #Tilpasse marger for å kunne skrive utvalgsteksten
      NutvTxt <- length(utvalgTxt)
      vmarg <- switch('H', V=0.04, H=min(1,max(0, strwidth(GrNavnSort, units='figure', cex=cexgr)*0.75)))
      #NB: strwidth oppfører seg ulikt avh. av device...
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med


      farger <- FigTypUt$farger
      fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
      fargeRest <- farger[3]
      graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
      antGr <- length(GrNavnSort)
      lwdRest <- 3	#tykkelse på linja som repr. landet
      cexleg <- 0.9	#Størrelse på legendtekst


      #Definerer disse i beregningsfunksjonen?
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest), na.rm=T)*1.2
      print(xmax)
      if (valgtVar %in% c('R0ScorePhys',	'R0ScoreRoleLmtPhy',	'R0ScoreRoleLmtEmo',	'R0ScoreEnergy',
                         'R0ScoreEmo', 'R0ScoreSosial',	'R0ScorePain',	'R0ScoreGeneral')) {
        xmax <- min(xmax, 100)}
      ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
      ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved)

      #Må def. pos først for å få strek for hele gruppa bak søylene
      ### reverserer for å slippe å gjøre det på konf.int
      pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
                         xlab=xAkseTxt, border=NA, col=fargeHoved)) #xlab=NGERVarSpes$xAkseTxt
      indOK <- which(AggVerdier$Hoved>=0)
      posOK <- pos[indOK]
      posOver <- max(pos)+0.35*log(max(pos))
      posDiff <- 1.2*(pos[1]-pos[2])
      posOK <- pos[indOK]
      minpos <- min(posOK)-0.7
      maxpos <- max(posOK)+0.7
      AntGr <- length(which(AggVerdier$Hoved>0))
      if (max(AggVerdier$Hoved, na.rm=T) == 0 ){medKI <- 0}

      #Legge på målnivå
      if (!is.na(KvalIndGrenser[1])) {
        antMaalNivaa <- length(KvalIndGrenser)-1
        rekkef <- 1:antMaalNivaa
        if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
        fargerMaalNiva <-  c('#3baa34', '#fd9c00', '#e30713')[rekkef] #Grønn, gul, rød
        maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
        if (antMaalNivaa==3) {maalOppTxt[2] <- 'Moderat' }
        rect(xleft=KvalIndGrenser[1:antMaalNivaa], ybottom=0, xright=KvalIndGrenser[2:(antMaalNivaa+1)],
             ytop=max(pos)+0.4, col = fargerMaalNiva[1:antMaalNivaa], border = NA) #add = TRUE, #pos[AntGrNgr+1],
        legPos <- ifelse(AntGr < 31, ifelse(AntGr < 15, -1, -2.5), -3.5)
        legend(x=0, y=legPos, pch=c(NA,rep(15, antMaalNivaa)), col=c(NA, fargerMaalNiva[1:antMaalNivaa]),
               ncol=antMaalNivaa+1,
               xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
               legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
      }



      if (medKI == 1) {	#Legge på konf.int for hele populasjonen
        #options(warn=-1)	#Unngå melding om KI med lengde 0
        KIHele <- AggVerdier$KIHele
        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
                c(minpos, maxpos, maxpos, minpos))
      }

      #GrNavnSort <- rev(GrNavnSort)
      grTypeTxt <- smltxt
      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
      #Linje for hele landet/utvalget:
      lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55),
      #Linje for kvalitetsindikatormål:
      if (!is.na(KImaal[1])) {
        lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55),
        text(x=KImaal, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0))
      }
      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
      text(x=soyleXpos+xmax*0.01, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus


      if (medKI == 1) {	#Legge på konf.int for hver enkelt gruppe/sykehus
        suppressWarnings(arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIopp, y1=pos,
                                length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1]))
        suppressWarnings(arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIned, y1=pos,
                                length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1]))
      }
      #------Tegnforklaring (legend)--------
      if (medKI == 0) { #Hopper over hvis ikke valgtMaal er oppfylt
        TXT <- paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N)
        legend('top', TXT, fill=NA,  border=NA, lwd=2.5, xpd=TRUE, #xmax/4, posOver+posDiff, inset=c(-0.1,0),
               col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
      } else {
        TXT <- c(paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N),
                 paste0('95% konf.int., ', hovedgrTxt,  ' (', #grTypeTxt, 'sykehus..
                        sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')')) #xmax/4, posOver+2*posDiff
        legend('top', TXT, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
               col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n',
               yjust=0)
      }


      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=GrNavnSort, side=2, las=1, cex=cexgr, adj=1, line=0.25)

      title(tittel, line=1.5) #cex.main=1.3)

      #Tekst som angir hvilket utvalg som er gjort
      avst <- 0.8
      utvpos <- 3	#Startlinje for teksten
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}

    }  #Figur
  }

  return(invisible(GjsnGrVarData))

}




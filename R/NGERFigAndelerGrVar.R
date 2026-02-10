#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for grupperingsvariabelen sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#'  Variable funksjonen benytter: Alder (beregnes), Opf0Komplikasjoner, Opf0Reoperasjon, Opf0KomplBlodning, Opf0KomplUtstyr,
#'  Opf0KomplInfeksjon, Opf0KomplOrganUtdanning, KomplPostopAlvor
#'  HysKomplikasjoner, LapKomplikasjoner, OpMetode, OpAntibProfylakse, OpASA, OpBMI, Opf0Status.
#'  Det benyttes også andre variable til utvalg osv.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item Alder: Andel pasienter over 70 år.
#'		\item KomplIntra: Komplikasjoner under operasjon (intraoperativt)
#'		\item KomplPostop: Postoperative komplikasjoner
#'		\item OpAntibProfylakse: Fått antibiotikaprofylakse
#'		\item OpASA: ASA-grad > II
#'		\item OpBMI: Pasienter med fedme (BMI>30)
#'		\item KomplPostopAlvor: Alvorlige komplikasjoner (grad 3 og 4)
#'		\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
#'		\item Opf0KomplUtstyr: Postop. komplikasjon: Problemer med ustyr
#'		\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
#'		\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
#'		\item Opf0Reoperasjon: Reoperasjon som følge av komplikasjon
#'		\item Opf0Status: Fått postoperativ oppfølging
#'    \item RegForsinkelse: Mer enn XX (1mnd?) fra operasjon til ferdigstilt registrering
#'    \item Tss2Mott: Møtet med gynekologisk avdeling var mindre godt
#'    \item Tss2Behandling: Behandlingens opplegg og innhold passet ikke pasienten
#'    \item Tss2Lytte: Pasientens behandlere lyttet- og forsto ikke det som ble tatt opp
#'    \item Tss2Behandlere: Pasienten hadde ikke tillit til sine behandlere
#'    \item Tss2Enighet: Pasient og behandlere ikke enige om målsetn. for behandlinga
#'    \item Tss2Generelt: Negativ eller svært negativ oppfatning om gyn. avd.
#'		\item Utdanning: Pasienter med høyere utdanning
#'    }
#'
#' @inheritParams NGERFigAndeler
#' @inheritParams NGERUtvalgEnh
#' @export

NGERFigAndelerGrVar <- function(RegData=0, valgtVar='Alder',
                                datoFra='2013-01-01', datoTil='3000-12-31',
                                velgAvd=0, minald=0, maxald=130,
                                OpMetode=99, # Hastegrad='',
                                AlvorlighetKompl='', behNivaa = 0,
                                Ngrense=10, reshID=0, outfile='',
                                velgDiag=0, preprosess=1, hentData=0, ...
                                ) {
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  '%i%' <- intersect
  cexShNavn <- 0.85

  NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar, #grVar='',
                                     OpMetode=OpMetode , figurtype='andelGrVar')
  RegData <- NGERVarSpes$RegData
  #flerevar <- NGERVarSpes$flerevar
  #subtxt <- NGERVarSpes$subtxt
  grtxt <- NGERVarSpes$grtxt
  tittel <- NGERVarSpes$tittel
  KvalIndGrenser <- NGERVarSpes$KvalIndGrenser
  sortAvtagende <- NGERVarSpes$sortAvtagende
  grVar <- 'ShNavn'

  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, OpMetode=OpMetode
                              ,minald=minald, maxald=maxald,
                              AlvorlighetKompl=AlvorlighetKompl, behNivaa = behNivaa, # Hastegrad=Hastegrad,
                              velgAvd=velgAvd, velgDiag=velgDiag)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt



  dummy0 <- NA #-0.001
  N <- dim(RegData)[1]
  Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
  if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
  AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
  #Nvar[names(Nvar) == 'Trondheim'] <- 387-191
  AndelerGr <- round(100*Nvar/Ngr,2)

  indGrUt <- as.numeric(which(Ngr < Ngrense))
  if (length(indGrUt)==0) { indGrUt <- 0}
  AndelerGr[indGrUt] <- dummy0
  sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
  Ngrtxt <- as.character(Ngr)
  Ngrtxt[indGrUt] <- paste0('<', Ngrense)	#paste(' (<', Ngrense,')',sep='')	#
  Ngr <- Ngr

  AggVerdier <- list(Hoved = NULL, Tot =NULL)
  AggVerdier$Hoved <- AndelerGr[sortInd]
  AggVerdier$Tot <- round(100*sum(RegData$Variabel)/N, 2)
  GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')

  andeltxt <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') 	#round(as.numeric(AggVerdier$Hoved),1)
  if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=N, #Nfig,
                       Ngr=Ngr[sortInd],
                       KvalIndGrenser <- NGERVarSpes$KvalIndGrenser,
                       grtxt=grtxt,
                       tittel=tittel,
                       utvalgTxt=utvalgTxt,
                       fargepalett=NGERUtvalg$fargepalett,
                       hovedgrTxt=NGERUtvalg$hovedgrTxt)

  #-----------Figur---------------------------------------
  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (dim(RegData)[1]>0) {
      tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=tittel)
    text(0.5, 0.6, tekst, cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}

  } else {

    #--------------------------FIGUR---------------------------------------------------
    #Innparametre: ...


    FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)	#height=3*800,
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.75)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    xmax <- min(max(AggVerdier$Hoved, na.rm = T),100)*1.15
    pos <- barplot(as.numeric(AggVerdier$Hoved), horiz=T, border=NA, col=farger[3], #main=tittel,
                   xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)
    #Legge på målnivå
    if (!is.na(KvalIndGrenser[1])) {
      antMaalNivaa <- length(KvalIndGrenser)-1
      rekkef <- 1:antMaalNivaa
      if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
        fargerMaalNiva <- adjustcolor(c('#3baa34', '#fd9c00', '#e30713'), alpha.f = 0.2)[rekkef] #Grønn, gul, rød  c('#58A55C', '#FD9C00', '#D85140')
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

    barplot(as.numeric(AggVerdier$Hoved), horiz=T, border=NA, col=farger[3], add=T)

    ybunn <- 0.1
    ytopp <- pos[AntGr]+1	#-length(indGrUt)]
    lines(x=rep(AggVerdier$Tot, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
    legend('top', xjust=1, cex=1, lwd=2, col=farger[2],
           legend=paste0(NGERUtvalg$hovedgrTxt, ' (', sprintf('%.1f',AggVerdier$Tot), '%), ', 'N=', N),
           bty='o', bg='white', box.col='white')
    mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
    mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
    title(tittel, line=1, font.main=1, cex.main=1.2)

    text(x=AggVerdier$Hoved+xmax*0.01, y=pos+0.1, andeltxt,
         las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
  }
  return(invisible(FigDataParam))
}

NGERFigAndelerGrVar_no_tests <- function(RegData=0, valgtVar='Alder',
                                datoFra='2013-01-01', datoTil='3000-12-31',
                                velgAvd=0, minald=0, maxald=130,
                                OpMetode=99, # Hastegrad='',
                                AlvorlighetKompl='', behNivaa = 0,
                                Ngrense=10, reshID=0, outfile='',
                                velgDiag=0, preprosess=1, hentData=0, ...
                                ) {
  
  
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

  '%i%' <- intersect

  NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar,
                                     OpMetode=OpMetode , figurtype='andelGrVar')
  RegData <- NGERVarSpes$RegData

  grtxt <- NGERVarSpes$grtxt
  tittel <- NGERVarSpes$tittel
  
  KvalIndGrenser <- NGERVarSpes$KvalIndGrenser
  sortAvtagende <- NGERVarSpes$sortAvtagende
  
  grVar <- 'ShNavn'

  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, OpMetode=OpMetode
                              ,minald=minald, maxald=maxald,
                              AlvorlighetKompl=AlvorlighetKompl, behNivaa = behNivaa,
                              velgAvd=velgAvd, velgDiag=velgDiag)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt

  #--------------------------FIGUR---------------------------------------------------

  FigDataParam <- plotAndelerGrVar(RegData, Variabel = RegData$Variabel,
                                    hovedgrTxt = NGERUtvalg$hovedgrTxt,
                                    grVar = grVar,
                                    KvalIndGrenser = NGERVarSpes$KvalIndGrenser,
                                    tittel = tittel,
                                    utvalgTxt = utvalgTxt,
                                    Ngrense = Ngrense,
                                    fargepalett = NGERUtvalg$fargepalett,
                                    grtxt = grtxt,
                                    outfile = outfile)


  
  

  return(invisible(FigDataParam))

                                }


plotAndelerGrVar <- function(RegData, 
                            Variabel,
                            hovedgrTxt = '',
                            grVar ='ShNavn', 
                            KvalIndGrenser = NA, 
                            tittel = 'tittel', 
                            utvalgTxt = '',
                            Ngrense = 10,
                            titleSize = 20, 
                            subtitleSize = 15, 
                            legendSize = 12, 
                            axisTextSize = 10, 
                            bestKvalInd = 'lav', 
                            nTicks = 5,
                            fargepalett = 'BlaaOff',
                            grtxt = '',
                            outfile='') {
  library(ggplot2)
  
  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=fargepalett)	#height=3*800,
  farger <- FigTypUt$farger

  dummy0 <- NA  # -0.001

  N <- nrow(RegData)

  # Gruppestørrelser og summer per gruppe (robust ved N == 0)
  if (N > 0) {
    Ngr  <- table(RegData[, grVar])
    Nvar <- tapply(RegData$Variabel, RegData[, grVar], sum, na.rm = TRUE)
  } else {
    Ngr  <- table(factor(character(0)))
    Nvar <- numeric(0)
  }

  # Andeler per gruppe (i %), og hvilke grupper som er under grense
  AndelerGr <- round(100 * Nvar / Ngr, 2)

  indGrUt <- which(Ngr < Ngrense)
  AntGr   <- sum(Ngr >= Ngrense)

  # Sett under-grense til dummy0 (NA)
  if (length(indGrUt) > 0) {
    AndelerGr[indGrUt] <- dummy0
  }

  # Sorter synkende (NB: NA havner sist)
  sortInd <- order(AndelerGr, decreasing = TRUE, na.last = TRUE)

  # Tekst for N per gruppe (med <Ngrense for små grupper)
  Ngrtxt <- as.character(Ngr)
  if (length(indGrUt) > 0) {
    Ngrtxt[indGrUt] <- paste0("<", Ngrense)
  }

  # Aggregerte verdier
  AggVerdier <- list(Hoved = NULL, Tot = NULL)
  AggVerdier$Hoved <- AndelerGr[sortInd]
  AggVerdier$Tot   <- if (N > 0) round(100 * sum(RegData$Variabel, na.rm = TRUE) / N, 2) else NA_real_

  # Sorterte gruppenavn med N-tekst
  GrNavnSort <- paste0(names(Ngr)[sortInd], " (", Ngrtxt[sortInd], ")")

  # Andeltekst (blank for under-grense)
  andeltxt <- paste0(sprintf("%.1f", AggVerdier$Hoved), "%")
  if (length(indGrUt) > 0) {
    andeltxt[(AntGr + 1):(AntGr + length(indGrUt))] <- ""
  }

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=N, #Nfig,
                       Ngr=Ngr[sortInd],
                       KvalIndGrenser <- KvalIndGrenser,
                       grtxt=grtxt,
                       tittel=tittel,
                       utvalgTxt=utvalgTxt,
                       fargepalett=fargepalett,
                       hovedgrTxt=hovedgrTxt)
  
  if (all(is.na(Ngr))) {

    tekst <- "Ingen registrerte data for dette utvalget"

    p <- ggplot() +
      theme_void() +
      labs(title = tittel) +
      annotate("text", x = 0, y = 0, label = tekst, size = 5) +
      annotate("text", x = 0, y = -0.2, label = paste(utvalgTxt, collapse = "\n"),
              size = 3.5, color = farger[1])


  } else if (max(Ngr, na.rm = TRUE) < Ngrense) {

    tekst <- paste0("Færre enn ", Ngrense, " registreringer ved hvert av sykehusene")

    p <- ggplot() +
      theme_void() +
      labs(title = tittel) +
      annotate("text", x = 0, y = 0, label = tekst, size = 5) +
      annotate("text", x = 0, y = -0.2, label = paste(utvalgTxt, collapse = "\n"),
              size = 3.5)


  } else {
  # 1) AndelerPlot (NA → 0 kun for plotting, ggplot fjerner NA verdier)
  andeler <- as.numeric(AggVerdier$Hoved)
  andelerPlot <- replace(andeler, is.na(andeler), 0)

  # 2) Datasett til ggplot
  ggDataFrame <- data.frame(
    andelProsent = andeler,      # ekte verdi (kan være NA)
    andelerPlot  = andelerPlot,  # brukt til stolpehøyde
    gruppeNavn   = as.character(GrNavnSort),
    andelTekst   = as.character(andeltxt)
  )
  ggDataFrame <- rbind(
    ggDataFrame,
    data.frame(
      andelProsent = NA,
      andelerPlot  = 0,
      gruppeNavn   = "(N)",
      andelTekst   = ""
    )
  )

  # ---- Sorter alle "(N)" ----
  rest <- ggDataFrame[ggDataFrame$gruppeNavn != "(N)", ]
  rest <- rest[order(-ifelse(is.na(rest$andelProsent), -Inf, rest$andelProsent)), ]

  # Legg "(N)" sist i datasettet for å sikre at det plottes sist (øverst etter coord_flip)
  ggDataFrame <- rbind(rest, ggDataFrame[ggDataFrame$gruppeNavn == "(N)", ])

  # ---- Lås rekkefølge ----
  ggDataFrame$gruppeNavn <- factor(
    ggDataFrame$gruppeNavn,
    levels = ggDataFrame$gruppeNavn
  )
  nLevels <- length(levels(ggDataFrame$gruppeNavn))
  
  # 3) Gjennomsnittslinje
  gjennomsnittY <- AggVerdier$Tot[1]

  gjennomsnittEtikett <- paste0(
    hovedgrTxt[1], " (",
    sprintf("%.1f", gjennomsnittY), "%), N=", N
  )

  # 4) Dynamisk øvre grense på prosentaksen
  maksAndel <- max(ggDataFrame$andelProsent, na.rm = TRUE)*1.15
  prettyVals <- pretty(c(0, maksAndel), n = nTicks) #funksjon som finner "pent" fordelte verdier for aksen
  ovreGrense <- max(prettyVals)

  # 5) Kvalitetsindikator: Bakgrunnsbånd basert på kvalitetsgrenser
  visKvalIndGrenser <- any(KvalIndGrenser > 0, na.rm = TRUE)
  kvalIndFarger <- c("#3baa34", "#fd9c00", "#e30713") # Grønn, gul, rød
  if (bestKvalInd == 'høy') {
    kvalIndFarger <- rev(kvalIndFarger) # Rød, gul, grønn
  }

  if (visKvalIndGrenser) {

    # Sikre at grensene er numeriske, sorterte og gyldige
    kvalBreaks <- sort(as.numeric(KvalIndGrenser))
    stopifnot(length(kvalBreaks) == 4, all(diff(kvalBreaks) > 0))

    # Lag kvalitetsindikator-bånd på prosentaksen (bånd etter verdi-aksen; blir "vertikale" etter coord_flip)
    bakgrunnBånd <- data.frame(
      ymin = kvalBreaks[-length(kvalBreaks)],
      ymax = kvalBreaks[-1],
      fill = kvalIndFarger
    )

    # Hvis aksen ikke går til 100, klipp båndene til ovreGrense
    bakgrunnBånd$ymin <- pmax(bakgrunnBånd$ymin, 0)
    bakgrunnBånd$ymax <- pmin(bakgrunnBånd$ymax, ovreGrense)

    # fjern kvalitetsindikator-bånd for (N)-label
    bakgrunnBånd$xmin <- 0.5
    bakgrunnBånd$xmax <- (nLevels - 1) + 0.5

    # Fjern bånd som ender opp tomme
    bakgrunnBånd <- bakgrunnBånd[bakgrunnBånd$ymax > bakgrunnBånd$ymin, ]
  }

  # 6) Plot
  p <- ggplot(ggDataFrame, aes(x = gruppeNavn, y = andelerPlot))

  # Legg til bakgrunnsbånd først slik at de ligger bak stolpene
  if (visKvalIndGrenser) {
    p <- p +
      geom_rect(
        data = bakgrunnBånd,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
        inherit.aes = FALSE,
        alpha = 0.20
      ) +
      scale_fill_identity()
  }
  
  # Stolper, linjer og tekst
  p <- p +
    geom_col(fill = farger[3], width = 0.65) +

    geom_segment(
      data = data.frame(
        x = 0.5,
        xend = (nLevels - 1) + 0.5,   # stop before "(N)"
        y = gjennomsnittY,
        yend = gjennomsnittY,
        lab = gjennomsnittEtikett
      ),
      aes(x = x, xend = xend, y = y, yend = yend, linetype = lab),
      color = farger[2],
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    scale_linetype_manual(
      values = setNames("solid", gjennomsnittEtikett),
      name = NULL
    )+

    # Verdietiketter til høyre for stolpene
    geom_text(
      aes(label = andelTekst),
      color = farger[1],
      hjust = -0.2
    ) +

    coord_flip() +

    # Prosentakse
    scale_y_continuous(
      breaks = prettyVals,
      limits = c(0, ovreGrense),
      expand = expansion(mult = c(0, 0))
    ) +

    # Tittel og undertittel
    labs(
      title = tittel,
      subtitle = paste(utvalgTxt, collapse = "\n"),
      x = NULL,
      y = "Andel (%)"
    ) +

    # Layout
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks.x = element_line(color = "black"),
      axis.line.x  = element_line(color = "black"),
      legend.position = "top",
      legend.justification = "center",
      legend.text = element_text(size = legendSize),
      plot.subtitle = element_text(size = subtitleSize, color = farger[1]),
      plot.title = element_text(size = titleSize),
      axis.text.y = element_text(size = axisTextSize)
    )
  }
  print(p)
  if( outfile != '') {
    print(outfile)
    ggsave(outfile, plot = p)
  }
  return(FigDataParam)
}
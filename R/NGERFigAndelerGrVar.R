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

  FigDataParam <- PlotAndelerGrVar(RegData, Variabel = RegData$Variabel,
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


PlotAndelerGrVar <- function(RegData, 
                            Variabel,  
                            grVar ='ShNavn',
                            hovedgrTxt = '', 
                            KvalIndGrenser = NA, 
                            tittel = 'tittel', 
                            utvalgTxt = '',
                            Ngrense = 10,
                            titleSize = 20, 
                            subtitleSize = 15, 
                            legendSize = 12, 
                            axisTextSize = 10, 
                            bestKvalInd = 'lav', # 'høy' for omvendt rekkfølge på indikatorfarger
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

  # Sorter synkende ( NA havner sist)
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

  # Litt triksing for å sikre at "(N)" alltid kommer sist (øverst i plottet etter coord_flip)
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
  prettyVals <- pretty(c(0, maksAndel), n = nTicks) # Funksjon som finner "pent" fordelte verdier for aksen
  ovreGrense <- max(prettyVals, gjennomsnittY*1.15, na.rm = TRUE) # Sørg for at både maks andel og gjennomsnittslinje får plass

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
    
    # Plot stolper horisontalt
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
    dev.off()
  }
  return(FigDataParam)
}
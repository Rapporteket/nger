#' Henter global dataramme for NGER
#'
#' Henter NGER-data fra staging.
#'
#' @inheritParams NGERFigAndeler
#' @param medPROM: koble på RAND og TSS2-variabler
#'
#' @return RegData data frame
#'
#' @export


NGERRegDataSQL <- function(datoFra = '2013-01-01', datoTil = Sys.Date(), medPROM=1, ...) {

  registryName = "data"  #"nger"
  #Flyttet til PROM-tabell? R1BesvarteProm,    -- ny jan.-2022
  # Hvor har denne blitt av?? Tss2BesvarteProm -- ny jan.-2022

  #Faser ut forlopsoversikt
  #forlopsoversikt.BasisRegStatus - bare ei registrering i AlleVarNum har BasisRegStatus=0
  #forlopsoversikt.OppflgRegStatus - ikke i bruk
  #forlopsoversikt.OppflgStatus - ikke i bruk
  #forlopsoversikt.PasientAlder, - endrer til å beregne selv

  cat("\nBefore allevarnum\n")
  RegData <- AlleVarNum()
  cat("\nAfter allevarnum\n")

  cat("\nBefore oppfolging\n")
  Oppfolging <- followupsnum()
# setdiff(sort(Oppfolging$ForlopsID), RegData$ForlopsID)
  cat("\nAfter oppfolging\n")

  RegData <- dplyr::left_join(RegData, Oppfolging, by="ForlopsID")

  if (medPROM==1) {

    #Sjekk ved å sammenligne R0 og R1-variabler fra allevarnum OG rand36-TABELL
    #UTGÅR siden RAND-variabler nå er fjernet fra allevarnum
    # R0var <- grep(pattern='R0', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
    # R1var <- grep(pattern='R1', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
    # if (length(c(R0var, R1var)) >0) {
    #   RegDataUrand <- RegData[, -which(names(RegData) %in% c(R0var, R1var))]
    # }

    RAND36 <-  rand36report()

    Rvar <- grep(pattern='R', x=names(RAND36), value = TRUE, fixed = TRUE)
    #Navneendring; fjerne R..
    Rvar_uR <- substring(Rvar, 2)
    names(RAND36)[which(names(RAND36) %in% Rvar)] <- Rvar_uR

    RAND36w <- RAND36 %>%
      tidyr::pivot_wider(
        id_cols = 'ForlopsID',
        id_expand = FALSE,
        names_from ='Aar',  #c('Aar', Rvar),
        #names_prefix = "A",
        names_sep = "",
        names_glue = "{'R'}{Aar}{.value}",
        names_sort = FALSE,
        names_vary = "fastest",
        names_repair = "check_unique",
        values_from = all_of(c('Metode', Rvar_uR))
      )

    RegData <- dplyr::left_join(RegData, RAND36w, by="ForlopsID")
  }
  #Testing
  # Rvar <- grep(pattern='R', x=sort(names(RegDataR)), value = TRUE, fixed = TRUE)
  # RegDataR <- RegDataR[,c("ForlopsID", Rvar)]
  # summary(RegDataR)
# Flyttet fra allevarnum til rand36report i mai23
#
#   -- R0Metode,
#   -- R0ScorePhys,
#   -- R0ScoreRoleLmtPhy,
#   -- R0ScorePain,
#   -- R0ScoreGeneral,
#   -- R0ScoreEnergy,
#   -- R0ScoreSosial,
#   -- R0ScoreRoleLmtEmo,
#   -- R0ScoreEmo,
#   -- R0Spm2,
#   -- R0Status,
#   -- RY1metode,
#   -- R1ScorePhys,
#   -- R1ScoreRoleLmtPhy,
#   -- R1ScorePain,
#   -- R1ScoreGeneral,
#   -- R1ScoreEnergy,
#   -- R1ScoreSosial,
#   -- R1ScoreRoleLmtEmo,
#   -- R1ScoreEmo,
#   -- R1Spm2,
#   -- RY1Status,

  return(invisible(RegData))
}

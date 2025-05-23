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


  query <- paste0('SELECT
    allevarnum.PasientID,
    allevarnum.ForlopsID,
    allevarnum.AvdRESH,
    allevarnum.FodselsDato AS Fodselsdato,
    forlopsoversikt.SykehusNavn,
    allevarnum.SivilStatus,
    Utdanning,
    allevarnum.Norsktalende,
    allevarnum.Morsmaal,
    allevarnum.MorsmaalAnnet,
-- HysBlodning, erstattet nov23
    -- HysFluidOverload, erstattet nov23
    HysGjforingsGrad,
    -- HysPerforasjon, erstattet nov23
    HysDiagnose1,
    HysDiagnose2,
    HysDiagnose3,
    HysKomplikasjoner,
    HysKonvertert,
    HysProsedyre1,
    HysProsedyre2,
    HysProsedyre3,
    HysStatus,
    -- HysTeknisk,
    -- HysTilgang, fjernet nov23
    OpBehNivaa, #Ny nov 23
  HysUfullSmerte, #Ny nov 23
  HysUfullMisGass, #Ny nov 23
  HysUfullKompl, #Ny nov 23
  HysUfullHoyVaeske, #Ny nov 23
  HysKomplViaFalsa, #Ny nov 23
  HysKomplVaeske, #Ny nov 23
  HysKomplPerf, #Ny nov 23
  HysKomplGass, #Ny nov 23
  HysKomplBlodn, #Ny nov 23
  HysKomplAnnet, #Ny nov 23
  HysSkadeaarsakStenose, #Ny nov 23
  HysSkadeaarsakAd, #Ny nov 23
  HysSkadeaarsakTeknUtst, #Ny nov 23
  HysSkadeaarsakAnatomi, #Ny nov 23
  HysSkadeaarsakAnnet, #Ny nov 23
  HysKomplTiltakTamp, #Ny nov 23
  HysKomplTiltakAvbr, #Ny nov 23
  HysKomplTiltakAnnet, #Ny nov 23
  HysKomplTiltakIngen, #Ny nov 23
    Konverteringsstatus,
    LapAdherProfylakse,
    LapBipolarDiatermi,
    LapBlare,
    LapClips,
    -- LapHarmonicS, fjernet nov 23
    LapHjelpeinnstikk,
    -- LapIntKoagOgKlipp, fjernet nov 23
    -- LapIntKombo, fjernet nov 23
    -- LapIntraabdominell, fjernet nov23
    LapKarBlodning,
    LapKomplikasjoner,
    -- LapKompTilgang, iflg Toril har denne samme innhold som LapSkadeTilgang
    LapKonvertert,
    -- LapMorcellator ERSTATTET når?
    -- LapMorcellatorMedPose, fjeret nov 23
    LapMorcellatorUtenPose,
    LapNerv,
    LapNett,
    LapNumHjelpeinnstikk,
    LapDiagnose1,
    LapDiagnose2,
    LapDiagnose3,
    LapKomplKar, #Ny nov 23
  LapKomplTarm, #Ny nov 23
  LapKomplBlaere, #Ny nov 23
  LapKomplUreter, #Ny nov 23
  LapKomplAnnet, #Ny nov 23
  LapSkadeTilgang, #Ny nov 23
  LapSkadeUthent, #Ny nov 23
  LapSkadeDissek, #Ny nov 23
  LapSkadeForsegl, #Ny nov 23
  LapSkadeAnnet, #Ny nov 23
  LapSkadeaarsakTeknUtst, #Ny nov 23
  LapSkadeaarsakAdher, #Ny nov 23
  LapSkadeaarsakTidlKir, #Ny nov 23
  LapSkadeaarsakAnnet, #Ny nov 23
    -- LapPlasmajet, Fjernet 2022
    LapProsedyre1,
    LapProsedyre2,
    LapProsedyre3,
    -- LapPostoperativ, # Fjernet nov 23
    LapPreparatopose,
    LapRobotKirurgi,
    LapSingelPort,
    LapStaplerEndogia,
    LapStatus,
    LapSutur,
    LapTarm,
    LapTekniskUtstyr,
    LapTilgang,
    LapTilgangsMetode,
    LapUnipolarDiatermi,
    LapUreter,
    -- LapUterusmanipulator, # Fjernet nov 23
    LapVevforsegl,  # = LapHarmonicS+LapIntKombo+LapIntKoagOgKlipp, Ny nov23
    LapAndre, #Ny nov 23
    LapHemastase, #Ny nov 23
    LapOptTro, #Ny nov 23
    LapPrepOppdel, #Ny nov 23
    LapUterusman, #Ny nov 23
    Leveringsdato,
    -- OpAnestesi, fjernet nov 23
    OpAnestesiIngen, #ny nov23
    OpAnestesiLok, #ny nov23
    OpAnestesiGen, #ny nov23
    OpAnestesiSpinEDA, #ny nov23
    OpAnestesiSed, #ny nov23
    OpAntibProfylakse,
    OpASA,
    OpBMI,
    OpHoyde,
    OpVekt,
    OpBlodfortynnende, # endret navn fra Blodfortynnende nov 23
    -- OpBMIKategori,  ikke i bruk
    -- OpDagkirurgi,
    OpDato,
    OpForstLukket,
    OpIVaktTid,
    -- OpGraviditeter,
    OpKategori,
    OpMetode,
    OpPariteter,
    OpStatus,
    OpTid,
    OpTidlLaparotomi,
    OpTidlLapsko,
    OpTidlVagInngrep,
    OpType
    FROM allevarnum
    INNER JOIN forlopsoversikt
    ON allevarnum.ForlopsID = forlopsoversikt.ForlopsID
 WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\'')

  RegData <- rapbase::loadRegData(registryName = registryName, query=query, dbType = "mysql") #registryName = "nger"

  qOppfolging <- 'SELECT
  ForlopsID,
      Opf0BesvarteProm,   -- ny jan.-2022
    Opf0metode,
    Opf0AlvorlighetsGrad,
    Opf0KomplBlodning,
    Opf0BlodningAbdom,
    Opf0BlodningIntraabdominal,
    Opf0BlodningVaginal,
    Opf0Komplikasjoner,
    Opf0KomplInfeksjon,
    Opf0KomplOrgan,
    -- Opf0KomplUtstyr,
    -- Opf0UtstyrInstrumenter,
    -- Opf0UtstyrNett,
    Opf0InfUVI,
    Opf0InfOpSaar  ,
    Opf0InfIntraabdominal,
    Opf0InfEndometritt,
    Opf0InfAnnen,
    Opf0OrganBlare,
    Opf0OrganTarm,
    Opf0OrganUreter,
    Opf0OrganKar,
    Opf0OrganAnnen,
    Opf0Reoperasjon,
    Opf0ReopLaparoskopi,
    Opf0ReopLaparotomi,
    Opf0Status,
    -- Opf0UtstyrSutur,
    Tss2Behandling,
    Tss2Behandlere,
    -- Tss2BesvarteProm,  -- ny jan.-2022
    Tss2Enighet,
    Tss2Generelt,
    Tss2Lytte,
    Tss2Mott,
    Tss2Score,
    Tss2Status,
    Tss2Type
  FROM followupsnum'

  Oppfolging <- rapbase::loadRegData(registryName = registryName, query=qOppfolging)
# setdiff(sort(Oppfolging$ForlopsID), RegData$ForlopsID)

  RegData <- dplyr::left_join(RegData, Oppfolging, by="ForlopsID")

  if (medPROM==1) {

    #Sjekk ved å sammenligne R0 og R1-variabler fra allevarnum OG rand36-TABELL
    #UTGÅR siden RAND-variabler nå er fjernet fra allevarnum
    # R0var <- grep(pattern='R0', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
    # R1var <- grep(pattern='R1', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
    # if (length(c(R0var, R1var)) >0) {
    #   RegDataUrand <- RegData[, -which(names(RegData) %in% c(R0var, R1var))]
    # }

    queryRAND36 <- 'select * FROM rand36report'
    RAND36 <-  rapbase::loadRegData(registryName = registryName, queryRAND36, dbType = "mysql")

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

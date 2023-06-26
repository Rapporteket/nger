#' Henter global dataramme for NGER
#'
#' Henter NGER-data fra staging.
#' Klargjort (delvis) for å kunne  sende inn en vektor med hvilke variable det skal spørres etter.
#'
#' @inheritParams NGERFigAndeler
#' @param medPROM: koble på RAND og TSS2-variabler
#'
#' @return RegData data frame
#' @export
#'

NGERRegDataSQL <- function(datoFra = '2014-01-01', datoTil = Sys.Date(),...) {
#NGERRegDataSQL <- function(datoFra = '2014-01-01', datoTil = '2099-01-01', medPROM=1, ...) {

  medPROM <- 0

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('Hentet rådata'))
  }
#  query <- paste0('SELECT ',
#  paste0('AlleVarNum.',varUtvalg,suffix=', \n'),
  #"Opf0UtstyrInstrumenter", "Opf0UtstyrNett" og "Opf0UtstyrSutur"

query <- paste0('SELECT
  HysBlodning,
    HysFluidOverload,
    HysGjforingsGrad,
    HysKomplikasjoner,
    HysPerforasjon,
    HysDiagnose1,
    HysDiagnose2,
    HysDiagnose3,
    HysKomplikasjoner,
    HysKonvertert,
    HysProsedyre1,
    HysProsedyre2,
    HysProsedyre3,
    HysStatus,
    HysTeknisk,
    HysTilgang,
    Konverteringsstatus,
    LapAdherProfylakse,
    LapBipolarDiatermi,
    LapBlare,
    LapClips,
    LapHarmonicS,
    LapHjelpeinnstikk,
    LapIntKoagOgKlipp,
    LapIntKombo,
    LapIntraabdominell,
    LapKarBlodning,
    LapKomplikasjoner,
    LapKompTilgang,
    LapKonvertert,
    -- LapMorcellator ERSTATTET
    LapMorcellatorMedPose,
    LapMorcellatorUtenPose,
    LapNerv,
    LapNett,
    LapNumHjelpeinnstikk,
    LapDiagnose1,
    LapDiagnose2,
    LapDiagnose3,
    LapKonvertert,
    -- LapPlasmajet, Fjernet
    LapProsedyre1,
    LapProsedyre2,
    LapProsedyre3,
    LapPostoperativ,
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
    LapUterusmanipulator,
    Leveringsdato,
    Blodfortynnende,
    OpAnestesi,
    OpAntibProfylakse,
    OpASA,
    OpBMI,
    OpBMIKategori,
    OpDagkirurgi,
    OpDato,
    Opf0BesvarteProm,   -- ny jan.-2022
    Opf0metode,
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
    OpType,
    R0Metode,
    R0ScorePhys,
    R0ScoreRoleLmtPhy,
    R0ScorePain,
    R0ScoreGeneral,
    R0ScoreEnergy,
    R0ScoreSosial,
    R0ScoreRoleLmtEmo,
    R0ScoreEmo,
    R0Spm2,
    R0Status,
    RY1metode,
    R1BesvarteProm,    -- ny jan.-2022
    R1ScorePhys,
    R1ScoreRoleLmtPhy,
    R1ScorePain,
    R1ScoreGeneral,
    R1ScoreEnergy,
    R1ScoreSosial,
    R1ScoreRoleLmtEmo,
    R1ScoreEmo,
    -- R1Spm2,
    RY1Status,
    Tss2Behandling,
    Tss2Behandlere,
    Tss2BesvarteProm,  -- ny jan.-2022
    Tss2Enighet,
    Tss2Generelt,
    Tss2Lytte,
    Tss2Mott,
    Tss2Score,
    Tss2Status,
    Tss2Type,
    AlleVarNum.SivilStatus,
    Utdanning,
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
    AlleVarNum.AvdRESH,
    AlleVarNum.Norsktalende,
    AlleVarNum.PasientID,
    AlleVarNum.ForlopsID
    ,ForlopsOversikt.BasisRegStatus
    ,ForlopsOversikt.FodselsDato AS Fodselsdato
    ,ForlopsOversikt.HovedDato
    ,ForlopsOversikt.OppflgRegStatus
    ,ForlopsOversikt.OppflgStatus
    ,ForlopsOversikt.PasientAlder
    ,ForlopsOversikt.SykehusNavn
    FROM AlleVarNum
    INNER JOIN ForlopsOversikt
    ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
 WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

# -- NB  Opf0BesvarteProm, -- -- ny jan.-2022
#ForlopsOversikt.PasientAlder
#Tatt ut av alleVarNum: 	AVD_RESH,

#FROM alleVarNum INNER JOIN ForlopsOversikt ON alleVarNum.MCEID = ForlopsOversikt.ForlopsID
# query <- 'select * FROM AlleVarNum
#     INNER JOIN ForlopsOversikt
#     ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID'

#Data_AWN <- rapbase::loadRegData(registryName = "nger", query_AWN, dbType = "mysql")
#Data_Forl <- rapbase::loadRegData(registryName = "nger", query_Forl, dbType = "mysql")
RegData <- rapbase::loadRegData(registryName = "nger", query, dbType = "mysql")

if (medPROM==1) {
#Må gjøre ei ny vurdering av om nok variabler er med og om jeg kan ha filtrert bort for mye.
R0var <- grep(pattern='R0', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
R1var <- grep(pattern='R1', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
TSS2var <- grep(pattern='Tss2', x=sort(names(RegData)), value = TRUE, fixed = TRUE)
AlleVarNum <- RegData[, -which(names(RegData) %in% c(R0var, R1var, TSS2var))]


queryPROMtab <- 'select * FROM PromPrem'
PROM <-  rapbase::loadRegData(registryName = "nger", queryPROMtab, dbType = "mysql")

TSSvar <- c(TSS2var, "SendtDato", "Metode", 'ForlopsID')
PROM_TSS <- PROM[ ,TSSvar] %>%
  dplyr::filter(Tss2BesvarteProm %in% 0:1)

Rvar <- grep(pattern='R', x=sort(names(PROM)), value = TRUE, fixed = TRUE)
PROM_RAND <- PROM[ ,c(Rvar,'Aar', 'ForlopsID')] %>%
  dplyr::filter(!is.na(Aar))  #RBesvarteProm %in% 0:1 gir bare noen få
Rvar_uR <- substring(Rvar, 2) #gsub('R', '', Rvar)
names(PROM_RAND)[which(names(PROM_RAND) %in% Rvar)] <- Rvar_uR

PROM_RANDw <- PROM_RAND %>%
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
    values_from = all_of(Rvar_uR)
  )

RegDataR <- dplyr::left_join(AlleVarNum, PROM_RANDw, by="ForlopsID")
RegData <- dplyr::left_join(RegDataR, PROM_TSS, by="ForlopsID")
}

  return(RegData)
}




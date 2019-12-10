#' Henter global dataramme for NGER
#'
#' Henter NGER-data fra staging.
#' Klargjort (delvis) for å kunne  sende inn en vektor med hvilke variable det skal spørres etter.
#'
#' @inheritParams NGERFigAndeler
#' @param varUtvalg: Hvilke variable som skal hentes. (Oppf)
#'
#' @return RegData data frame
#' @export
#'
NGERRegDataSQL <- function(datoFra = '2014-01-01', datoTil = '2099-01-01',...) {

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = paste0('Hentet rådata'))
  }
#  query <- paste0('SELECT ',
#  paste0('AlleVarNum.',varUtvalg,suffix=', \n'),
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
    HysTeknisk,
    HysTilgang,
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
    LapMorcellatorMedPose,
    LapMorcellatorUtenPose,
    LapNerv,
    LapNett,
    LapNumHjelpeinnstikk,
    LapDiagnose1,
    LapDiagnose2,
    LapDiagnose3,
    LapKonvertert,
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
    OpAnestesi,
    OpAntibProfylakse,
    OpASA,
    OpBMI,
    OpBMIKategori,
    OpDagkirurgi,
    OpDato,
    Opf0metode,
    OpIVaktTid,
    OpKategori,
    OpMetode,
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
    Tss2Mott,
    Tss2Behandling,
    Tss2Lytte,
    Tss2Behandlere,
    Tss2Enighet,
    Tss2Generelt,
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
    Opf0KomplUtstyr,
    Opf0UtstyrInstrumenter,
    Opf0UtstyrNett,
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
    Opf0UtstyrSutur,
    AlleVarNum.AvdRESH,
    AlleVarNum.Norsktalende,
    AlleVarNum.PasientID
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


#  FROM AlleVarNum
#  INNER JOIN ForlopsOversikt
#  ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
#  LEFT JOIN FollowupsNum
#  ON ForlopsOversikt.ForlopsID = FollowupsNum.ForlopsID
# WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

  #LapMorcellator,  Erstattet
  #LapPlasmajet, Fjernet
  #?    OpOptimeCount,
  #?    OpParities,
  #?    OpPregnancies,
#ForlopsOversikt.PasientAlder
#Tatt ut av alleVarNum: 	AVD_RESH,

  #FROM alleVarNum INNER JOIN ForlopsOversikt ON alleVarNum.MCEID = ForlopsOversikt.ForlopsID


  RegData <- rapbase::LoadRegData(registryName = "nger", query, dbType = "mysql")


return(RegData)
}




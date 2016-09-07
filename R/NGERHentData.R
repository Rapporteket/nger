#' Provide global dataframe for NGER
#'
#' Provides NGER data from staging
#'
#' @inheritParams NGERFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0('SELECT
	FodselsDato,
	Opf0KomplBlodning,
	LapAdherProfylakse,
	Opf0Komplikasjoner,
	Opf0KomplInfeksjon,
	Opf0KomplOrgan,
	Opf0Reoperasjon,
	Utdanning,
	Opf0AlvorlighetsGrad,
	HysTilgang,
	HysBlodning,
	HysGjforingsGrad,
	HysKomplikasjoner,
	HysFluidOverload,
	HysPerforasjon,
	HysTeknisk,
	LapTilgangsMetode,
	LapAdherProfylakse,
	LapBipolarDiatermi,
	LapBlare,
	LapClips,
	LapKomplikasjoner,
	LapKonvertert,
	LapHarmonicS,
	LapHjelpeinnstikk,
	LapIntraabdominell,
	LapKarBlodning,
	LapMorcellatorUtenPose,
	LapMorcellatorMedPose,
	LapNerv,
	LapNett,
	LapNumHjelpeinnstikk,
	LapPostoperativ,
	LapPreparatopose,
	LapUterusmanipulator,
	LapRobotKirurgi,
	LapSingelPort,
	LapStaplerEndogia,
	LapSutur,
	LapTarm,
	LapTekniskUtstyr,
	LapIntKombo,
	LapTilgang,
	LapUnipolarDiatermi,
	LapUreter,
	Sivilstatus,
	OpMetode,
	OpAnestesi,
	OpASA,
	OpAntibProfylakse,
	OpBMI,
	OpBMIKategori,
	OpKategori,
	OpDato,
	OpDagkirurgi,
	OpTidlLapsko,
	OpTidlLaparotomi,
	OpTidlVagInngrep,
	OpIVaktTid,
	OpType,
	PasientID,
	Norsktalende,
	Opf0Status,
  ForlopsOversikt.AvdRESH,
	ForlopsOversikt.BasisRegStatus,
	ForlopsOversikt.HovedDato,
	ForlopsOversikt.OppflgRegStatus,
	ForlopsOversikt.OppflgStatus,
	ForlopsOversikt.SykehusNavn
FROM alleVarNum INNER JOIN ForlopsOversikt ON alleVarNum.MCEID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

RegData <- rapbase::LoadRegData(registryName, query, dbType)

#ForlopsOversikt.PasientAlder
#Tatt ut av alleVarNum: 	AVD_RESH,

return(RegData)
}




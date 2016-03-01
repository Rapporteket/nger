#' Provide global dataframe for NGER
#'
#' Provides NGER data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0('SELECT
	AVD_RESH,
	BirthDate,
	ComplAfterBleed, 
	ComplEquipment,
	ComplExist,
	ComplInfection,
	ComplOrgan
	ComplReop
	Education,
	FollowupSeriousness,
	HypAccess,
	HypBleeding,
	HypCompleteness,
	HypFluidOverload,
	HypPerforation,
	HypTechnical,
	KomplHyp,
	KomplIntra,
	KomplLap,
	KomplPost,
	KomplPostop,
	KomplPostUtd,
	KomplReopUtd,
	LapAccessMethod,
	LapAdheanseprofylakse,
	LapBipolarDiatermi,
	LapBlaere,
	LapClips,
	LapConverted
	LapEkstrautstyr,
	LapHarmonicS,
	LapHjelpeinnstikk,
	LapIntraAbdominal,
	LapKarBleed,
	LapMorcellator,
	LapNerv,
	LapNett,
	LapNumHjelpeinnstikk,
	LapPlasmajet,
	LapPostoperativ,
	LapPreparatopose,
	LapProtoadapter,
	LapRobotKirurgi,
	LapSingelPort,
	LapStaplerEndogia,
	LapSutur,
	LapTarm,
	LapTekniskUtstyr,
	LapThunderbeat,
	LapTilgang,
	LapUnipolarDiatermi,
	LapUreter,
	MaritalStatus,
	MCEType,
	MCETypeOpAnesthetic,
	OpAnesthetic,
	OpASA,
	OpAntibioticProphylaxis,
	OpBMI,
	OpBMICategory,
	Opcat,
	OpDate,
	OpDaySurgery,
	OpEarlierLaparatomy,
	OpEarlierLaparoscopy,
	OpEarlierPaparotomy,
	OpEarlierVaginal,
	OpOpcatOutsideDaytime,
	OpOutsideDaytime,
	OpType,
	PatientID,
	PatientNorwegian,
	Reop,
	StatusFollowup,
	ForlopsOversikt.BasisRegStatus,
	ForlopsOversikt.HovedDato,
	ForlopsOversikt.OppflgRegStatus, 
	ForlopsOversikt.OppflgStatus, 
	ForlopsOversikt.SykehusNavn
FROM AlleVarNum INNER JOIN ForlopsOversikt ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

RegData <- rapbase::LoadRegData(registryName, query, dbType)

#ForlopsOversikt.AvdRESH
#ForlopsOversikt.PasientAlder

return(RegData)
}




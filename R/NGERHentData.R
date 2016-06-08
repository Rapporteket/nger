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
	BirthDate,
	ComplAfterBleed,
	ComplEquipment,
	ComplExist,
	ComplInfection,
	ComplOrgan,
	ComplReop,
	Education,
	FollowupSeriousness,
	HypAccess,
	HypBleeding,
	HypCompleteness,
	HypComplications,
	HypFluidOverload,
	HypPerforation,
	HypTechnical,
	LapAccessMethod,
	LapAdheanseprofylakse,
	LapBipolarDiatermi,
	LapBlaere,
	LapClips,
	LapComplications,
	LapConverted,
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
	OpAnesthetic,
	OpASA,
	OpAntibioticProphylaxis,
	OpBMI,
	OpBMICategory,
	Opcat,
	OpDate,
	OpDaySurgery,
	OpEarlierLaparoscopy,
	OpEarlierPaparotomy,
	OpEarlierVaginal,
	OpOpcatOutsideDaytime,
	OpType,
	PatientID,
	PatientNorwegian,
	StatusFollowup,
  ForlopsOversikt.AvdRESH
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




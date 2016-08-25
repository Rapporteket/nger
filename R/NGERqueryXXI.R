#' Provide dataframe for tab xxi NGER
#'
#' Provides NGER data for tab xxi from staging
#'
#' @inheritParams NGERFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataXXI <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
'
SELECT
  v.OpDato,
  f.AvdRESH,
  v.FodselsDato,
  v.Opf0AlvorlighetsGrad,
  v.Utdanning,
  v.Sivilstatus,
  v.OpAnestesi,
  v.OpASA,
  v.HysGjforingsGrad,
  v.Norsktalende,
  v.OpBMIKategori,
  v.OpKategori,
  v.OpType,
  v.OpTidlVagInngrep,
  v.OpTidlLapsko,
  v.OpTidlLaparotomi,
  v.OpIVaktTid,
  v.OpDagkirurgi,
  v.OpMetode,
  f.BasisRegStatus,
  v.PasientID,
  f.HovedDato,
  f.SykehusNavn,
  v.Opf0Komplikasjoner,
  v.Opf0Reoperasjon,
  v.Opf0ReoperasjonLaparoscopy,
  v.Opf0ReoperasjonHysteroscopy,
  v.Opf0ReoperasjonLaparotomy,
  v.Opf0KomplInfeksjon,
  v.ComplInfSurg,
  v.ComplInfIntra,
  v.ComplInfEndoSalpin,
  v.ComplInfUVI,
  v.ComplInfOther,
  v.Opf0KomplBlodning,
  v.Opf0KomplBlodningAbdom,
  v.Opf0KomplBlodningVaginal,
  v.Opf0KomplBlodningIntra,
  v.Opf0KomplOrgan,
  v.Opf0KomplOrganIntestinal,
  v.Opf0KomplOrganBladder,
  v.Opf0KomplOrganUreter,
  v.Opf0KomplOrganKar,
  v.Opf0KomplOrganOther,
  v.LapAdherProfylakse,
  v.ComplEquipNet,
  v.ComplEquipInstruments,
  v.ComplEquipSuture
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON v.MCEID = f.ForlopsID
WHERE
  v.OpMetode = 1 AND
  f.HovedDato >= \'', datoFra, '\' AND
  f.HovedDato < \'', datoTil, '\'
'
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}

#' Lager followupsnum
#'
#' @return Returnerer followupsnum
#' @export
#'

followupsnum <- function(datoFra = '2013-01-01', datoTil = Sys.Date()) {
  query <- paste0('SELECT
  --  patient.ID as PasientID,
  mce.MCEID as ForlopsID,
  --  mce.CENTREID AS AvdRESH,
  followup.FOLLOWUP_TYPE AS Opf0metode,
  followup.PROM_ANSWERED AS Opf0BesvarteProm,
  followup.COMPLICATIONS_EXIST AS Opf0Komplikasjoner,
  followup.COMPLICATIONTYPE_REOP AS Opf0Reoperasjon,
  followup.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROSCOPY AS Opf0ReopLaparoskopi,
  followup.COMPLICATIONTYPE_REOP_SPECIFY_HYSTEROSCOPY AS Opf0ReopHysteroskopi,
  followup.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROTOMY AS Opf0ReopLaparotomi,
  followup.COMPLICATIONTYPE_REOP_SPECIFY_VAGINAL AS Opf0ReopVaginal,
  followup.COMPLICATIONTYPE_INFECTION AS Opf0KomplInfeksjon,
  followup.COMPLICATIONTYPE_INFECTION_SPECIFY_SURG_WOUND AS Opf0InfOpSaar,
  followup.COMPLICATIONTYPE_INFECTION_SPECIFY_INTRAABD AS Opf0InfIntraabdominal,
  followup.COMPLICATIONTYPE_INFECTION_SPECIFY_ENDO_SALPIN AS Opf0InfEndometritt,
  followup.COMPLICATIONTYPE_INFECTION_SPECIFY_UVI AS Opf0InfUVI,
  followup.COMPLICATIONTYPE_INFECTION_SPECIFY_OTHER AS Opf0InfAnnen,
  followup.COMPLICATIONTYPE_AFTERBLEED AS Opf0KomplBlodning,
  followup.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_ABDOMWALL AS Opf0BlodningAbdom,
  followup.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_VAGINAL AS Opf0BlodningVaginal,
  followup.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_INTRAABD AS Opf0BlodningIntraabdominal,
  followup.COMPLICATIONTYPE_ORGAN AS Opf0KomplOrgan,
  followup.COMPLICATIONTYPE_ORGAN_SPECIFY_INTESTINAL AS Opf0OrganTarm,
  followup.COMPLICATIONTYPE_ORGAN_SPECIFY_BLADDER AS Opf0OrganBlare,
  followup.COMPLICATIONTYPE_ORGAN_SPECIFY_URETER AS Opf0OrganUreter,
  followup.COMPLICATIONTYPE_ORGAN_SPECIFY_KAR AS Opf0OrganKar,
  followup.COMPLICATIONTYPE_ORGAN_SPECIFY_OTHER AS Opf0OrganAnnen,
  followup.SERIOUSNESS AS Opf0AlvorlighetsGrad,
  followup.FIRST_TIME_CLOSED as Opf0ForstLukket,
  followup.STATUS as Opf0Status,

  tss2.FOLLOWUP_TYPE as Tss2Type,
  tss2.Q1 AS Tss2Mott,
  tss2.Q2 as Tss2Behandling,
  tss2.Q3 as Tss2Lytte,
  tss2.Q4 as Tss2Behandlere,
  tss2.Q5 as Tss2Enighet,
  tss2.Q6 as Tss2Generelt,
  tss2.SCORE as Tss2Score,
  tss2.SCORE_AVG as Tss2ScoreAVG,
  tss2.FIRST_TIME_CLOSED as Tss2ForstLukket,
  tss2.STATUS as Tss2Status

FROM mce
INNER JOIN patient ON mce.PATIENT_ID = patient.ID
INNER JOIN operation on mce.MCEID = operation.MCEID
LEFT OUTER JOIN followup ON (mce.MCEID = followup.MCEID AND followup.STATUS = 1)
LEFT OUTER JOIN tss2 ON (mce.MCEID = tss2.MCEID AND tss2.STATUS = 1)
LEFT OUTER JOIN user u_followup ON followup.FIRST_TIME_CLOSED_BY = u_followup.ID -- ADDED
LEFT OUTER JOIN user u_tss2 ON tss2.FIRST_TIME_CLOSED_BY = u_tss2.ID -- ADDED
WHERE
  operation.STATUS = 1
   AND operation.OP_DATE >= \'', datoFra, '\' AND operation.OP_DATE <= \'', datoTil, '\'')


  followupsnum <- rapbase::loadRegData(registryName = 'data', query=query, dbType = "mysql")

  # Tatt ut:  CONCAT(u_followup.FIRSTNAME, ' ', u_followup.LASTNAME) as Opf0ForstLukketAv, -- MODIFIED
#  CONCAT(u_tss2.FIRSTNAME, ' ', u_tss2.LASTNAME) as Tss2ForstLukketAv, -- MODIFIED


#  Variabler som skal være med:
  # ForlopsID,
  # Opf0BesvarteProm,   -- ny jan.-2022
  # Opf0metode,
  # Opf0AlvorlighetsGrad,
  # Opf0KomplBlodning,
  # Opf0BlodningAbdom,
  # Opf0BlodningIntraabdominal,
  # Opf0BlodningVaginal,
  # Opf0Komplikasjoner,
  # Opf0KomplInfeksjon,
  # Opf0KomplOrgan,
  # Opf0InfUVI,
  # Opf0InfOpSaar  ,
  # Opf0InfIntraabdominal,
  # Opf0InfEndometritt,
  # Opf0InfAnnen,
  # Opf0OrganBlare,
  # Opf0OrganTarm,
  # Opf0OrganUreter,
  # Opf0OrganKar,
  # Opf0OrganAnnen,
  # Opf0Reoperasjon,
  # Opf0ReopLaparoskopi,
  # Opf0ReopLaparotomi,
  # Opf0Status,
  # Tss2Behandling,
  # Tss2Behandlere,
  # Tss2Enighet,
  # Tss2Generelt,
  # Tss2Lytte,
  # Tss2Mott,
  # Tss2Score,
  # Tss2Status,
  # Tss2Type
  # FROM followupsnum'


}

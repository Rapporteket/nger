#' Gamle tabeller fra Are
#' Provide dataframe for tab vi NGER
#' Provides NGER data for tab vi from staging
#' @inheritParams NGERFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataVI <- function(reportYear = 2099) {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
"
SELECT
  v.OpBMI,
  v.OpParities,
  v.OpPregnancies,
  v.OpOptimeCount,
  v.OpKategori,
  v.OpType,
  v.OpMetode,
  f.BasisRegStatus,
  v.PasientID,
  YEAR(f.HovedDato) AS year,
  f.ShNavn
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON v.MCEID = f.ForlopsID
WHERE
  YEAR(f.HovedDato) < ", reportYear + 1, " AND
  YEAR(f.HovedDato) >= ", reportYear - 2
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}


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
  f.ShNavn,
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


#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams NGERFigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  # make dummy column for all MCEs
  n <- dim(RegData)[1]
  RegData$dummy <- rep("\\textbf{Alle BMI} ($kg/m^2$)", n)
  myTab <- xtabs(OpBMI ~ dummy + Aar,
                 aggregate(OpBMI~dummy+Aar,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ OpMetode + Aar,
                       aggregate(OpBMI~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ dummy + Aar,
                       aggregate(OpPariteter~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ OpMetode + Aar,
                       aggregate(OpPariteter~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ dummy + Aar,
                       aggregate(OpGraviditeter~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ OpMetode + Aar,
                       aggregate(OpGraviditeter~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle knivtider} (\\textit{minutt})"
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ dummy + Aar,
                       aggregate(OpTid~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ OpMetode + Aar,
                       aggregate(OpTid~OpMetode+Aar,RegData,mean)))

  # move rownames to its own column do allow duplicate names
  # OpMetode 1=laparo, 2=hysteroskopi, 3=begge
  pe <- rownames(myTab)
  pe[which(pe==1)] <- "\\quad Laparoskopi"
  pe[which(pe==2)] <- "\\quad Hysteroskopi"
  pe[which(pe==3)] <- "\\quad Begge"

  mydf <- data.frame(Pasientegenskap=pe, myTab, check.names = FALSE)
  #  list(tabVI=mydf)
  return(invisible(mydf))
}


#' Generate tab XXI
#'
#' Yes, generate tab XXI
#'
#' @inheritParams NGERFigAndeler
#' @return list $tabXXI data frame of table data
#' @return list $personsWithMultipleCompl number of persons
#' @export

NGERtabXXI <- function(RegData) {

  N <- dim(RegData)[1]
  mydf <- RegData[ , c("Opf0KomplBlodning",
                       "Opf0KomplBlodningAbdom",
                       "Opf0KomplBlodningVaginal",
                       "Opf0KomplBlodningIntra",
                       "LapAdherProfylakse",
                       "ComplEquipNet",
                       "ComplEquipInstruments",
                       "ComplEquipSuture",
                       "Opf0KomplInfeksjon",
                       "ComplInfSurg",
                       "ComplInfIntra",
                       "ComplInfEndoSalpin",
                       "ComplInfUVI",
                       "ComplInfOther",
                       "Opf0KomplOrgan",
                       "Opf0KomplOrganIntestinal",
                       "Opf0KomplOrganBladder",
                       "Opf0KomplOrganUreter",
                       "Opf0KomplOrganKar",
                       "Opf0KomplOrganOther",
                       "Opf0Reoperasjon",
                       "Opf0ReoperasjonLaparoscopy",
                       "Opf0ReoperasjonHysteroscopy",
                       "Opf0ReoperasjonLaparotomy"
  )
  ]

  # how many persons have more than one compl?
  indCompl <- union(which(mydf$Opf0KomplBlodning==1),
                    which(mydf$LapAdherProfylakse==1))
  indCompl <- union(indCompl, which(mydf$Opf0KomplInfeksjon==1))
  indCompl <- union(indCompl, which(mydf$Opf0KomplOrgan==1))

  tab <- table(RegData$PasientID[indCompl])
  personsWithMultipleCompl <- length(tab[tab > 1])

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N*100)
  # recode colname
  colnames(tabXXI)[2] <- "Andel (\\%)"

  # recode rownames, also with latex (table) tagging
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodning"] <- "\\textit{Blødning}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningAbdom"] <- "\\quad I abdominal vegg"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningVaginal"] <- "\\quad Vaginal"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningIntra"] <- "\\quad Intraabdominal "
  row.names(tabXXI)[row.names(tabXXI) == "LapAdherProfylakse"] <- "\\textit{Problem med utstyr}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipNet"] <- "\\quad Med nett"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipInstruments"] <- "\\quad Med instumenter"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipSuture"] <- "\\quad Med sutur"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplInfeksjon"] <- "\\textit{Infeksjon}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfSurg"] <- "\\quad Infeksjon i operasjonssår"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfIntra"] <- "\\quad Intraabdominal"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfEndoSalpin"] <- "\\quad Salpingitt"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfUVI"] <- "\\quad Urinveisinfeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfOther"] <- "\\quad Annen infeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrgan"] <- "\\textit{Organskade}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganIntestinal"] <- "\\quad Tarmskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganBladder"] <- "\\quad Blæreskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganUreter"] <- "\\quad Ureterskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganKar"] <- "\\quad Karskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganOther"] <- "\\quad Annen organskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0Reoperasjon"] <- "\\textit{Reoperasjon}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonLaparoscopy"] <- "\\quad til laparoskopi"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonHysteroscopy"] <- "\\quad til Hysteroskopi?"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonLaparotomy"] <- "\\quad til laparotomi"


  list(tabXXI=tabXXI, personsWithMultipleCompl=personsWithMultipleCompl)

}


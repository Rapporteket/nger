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


NGERRegDataSQL <- function(datoFra = '2013-01-01', datoTil = Sys.Date(), medPROM=1, gml=1, ...) {


  if (gml==0) {
# Raskest å hente alle også filtrere på dato eller filtrere på dato til slutt?

  #user - QReg-brukere
  #centreattribute - enhetsnavn (ID=resh, ATTRIBUTEVALUE=enhetsnavn)

#mce
  qmce <- 'SELECT * from mce'
  mceSkjema <-  rapbase::loadRegData(registryName = 'data',
                                     query=qmce)

#Operasjon
  qOp <- 'SELECT * from operation'
  OpSkjema <-  rapbase::loadRegData(registryName = 'data',
                                     query=qOp)
OpSkjema <- OpSkjema[ ,-which(names(OpSkjema) %in% intersect(names(OpSkjema), names(mceSkjema)))]


#Laparskopi
  qLap <- 'SELECT * from laparoscopy'
  LaparskopiSkjema <-  rapbase::loadRegData(registryName = 'data',
                                            query=qLap)

#Hysteroskopi
  qHys <- 'SELECT * from hysteroscopy'
  HysteroskopiSkjema <-  rapbase::loadRegData(registryName = 'data',
                                            query=qHys)


#Pasientskjema:
qPas <- paste0('SELECT
ID AS PasientID,
BIRTH_DATE,
REGISTERED_DATE,
NATIVE_LANGUAGE,
NORWEGIAN,
COUNTY,
REGIONAL_HEALTH_AUTHORITY AS RHF, -- fjern?
TOWN,
MUNICIPALITY_NUMBER,
MUNICIPALITY_NAME,
EDUCATION,
DECEASED,
DECEASED_DATE,
REAPER_DATE,
MARITAL_STATUS,
OWNING_CENTRE,
TSUPDATED,
TSCREATED
FROM patient')

 # 'select * from patient'
  PasientSkjema <- rapbase::loadRegData(registryName = 'data',
                                      query=qPas)

#Oppfølgigsskjema:
  #Ikke filtrert på ferdigstilt
   qOppf0 <- paste0('select * FROM followup
                    INNER JOIN operation on followup.MCEID = operation.MCEID
                    WHERE operation.STATUS = 1 AND
                    operation.OP_DATE >= \'', datoFra, '\' AND operation.OP_DATE <= \'', datoTil, '\'')
   Oppf0skjema <- rapbase::loadRegData(registryName = 'data', query=qOppf0)

   qOppf6 <- paste0('select * FROM followup6
                    INNER JOIN operation on followup.MCEID = operation.MCEID
                    WHERE operation.STATUS = 1 AND
                    operation.OP_DATE >= \'', datoFra, '\' AND operation.OP_DATE <= \'', datoTil, '\'')
   Oppf6skjema <- rapbase::loadRegData(registryName = 'data', query=qOppf6)

# SAMMENSTILL ALLE SKJEMA
# RegData <- ...
  }

   if (gml==1){
    Oppf0skjema1 <- followupsnum(datoFra = datoFra, datoTil = datoTil)
    AlleVarNum <- AlleVarNum(datoFra = datoFra, datoTil = datoTil)
    RegData <- dplyr::left_join(AlleVarNum, Oppf0skjema, by="ForlopsID")
  }
  if (medPROM==1) {
    RAND36 <-  rand36report()
    Rvar <- grep(pattern='R', x=names(RAND36), value = TRUE, fixed = TRUE)


    #Navneendring; fjerne prefix R..
    Rvar_uR <- substring(Rvar, 2)
    names(RAND36)[which(names(RAND36) %in% Rvar)] <- Rvar_uR

    RAND36w <- RAND36 %>%
      tidyr::pivot_wider(
        id_cols = 'ForlopsID',
        id_expand = FALSE,
        names_from ='Aar',
        names_sep = "",
        names_glue = "{'R'}{Aar}{.value}",
        names_sort = FALSE,
        names_vary = "fastest",
        names_repair = "check_unique",
        values_from = all_of(c('Metode', Rvar_uR))
      )

    RegData <- dplyr::left_join(RegData, RAND36w, by="ForlopsID")
  }

  return(invisible(RegData))
}

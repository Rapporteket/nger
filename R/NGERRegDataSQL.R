#' Endre variabelnavn/kolonnenavn til selvvalgte navn
#' @param tabell datatabellnavn i databasen
#' @param tabType REGISTRATION_TYPE
#' @return tabell med selvvalgte variabelnavn spesifisert i friendlyvar. Intern funksjon
#'
#' @export

mappingEgneNavn <- function(tabell, tabType) {

  friendlyVarTab  <-
    rapbase::loadRegData( "data",
                          query = "SELECT FIELD_NAME, REGISTRATION_TYPE, USER_SUGGESTION #, USER_DATE
                           FROM friendly_vars")
  friendlyVarTab <- friendlyVarTab[-which(friendlyVarTab$USER_SUGGESTION == 'VERBOTEN'), ]

  indTabType <- which(friendlyVarTab$REGISTRATION_TYPE %in% tabType)
   navnFr <- friendlyVarTab$FIELD_NAME[indTabType]
   navn <- gsub(paste0(tabType, '_'), "", navnFr)
   names(navn) <- friendlyVarTab$USER_SUGGESTION[indTabType]
  tabellEgne <- dplyr::rename(tabell, dplyr::any_of(navn)) #all_of(navn
  return(tabellEgne)
  }


# LEGG INN FJERNING AV VARIABLER SOM GJENTAS I FLERE TABELLER. f.EKS. ReshId (CENTREID)
# Alle variabler, Bare utvalgte var, Bare selvvalgte navn

#' Hent datatabell fra ngers database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
#'                     centreattribute.ATTRIBUTEVALUE as SykehusNavn
# INNER JOIN patient ON mce.PATIENT_ID = patient.ID
# INNER JOIN operation ON mce.MCEID = operation.MCEID
# LEFT JOIN laparoscopy ON mce.MCEID = laparoscopy.MCEID
# LEFT JOIN hysteroscopy ON mce.MCEID = hysteroscopy.MCEID
# LEFT JOIN user u_op ON operation.FIRST_TIME_CLOSED_BY = u_op.ID -- ADDED
# LEFT JOIN user u_hys ON hysteroscopy.FIRST_TIME_CLOSED_BY = u_hys.ID -- ADDED
#'                   Kan ha følgende verdier:
#'                   'mce', 'patient', 'operation', 'laparoscopy', 'hysteroscopy'
#'                   user - nødvendig?
#'                   For å mappe på enhetsnavn: centreattribute on mce.CENTREID = centreattribute.ID
#'
#' @param egneVarNavn 0 - Qreg-navn benyttes.
#'                    1 - selvvalgte navn fra Friendlyvar benyttes
#'
#' @export

hentDataTabell <- function(tabellnavn = "operation",
                           qVar = '*',
                           egneVarNavn = 1) { #  status = 1

  query <- paste0("SELECT ", qVar, " FROM ", tabellnavn)
  tabell <- rapbase::loadRegData(registryName = "data",
                                 query = query)

  # if ("STATUS" %in% names(tabell)) {
  #   tabell <- tabell[tabell$STATUS == status, ]
  # }

  if (egneVarNavn == 1) {
    # gyldigeTab <- c('patient', 'operation', 'laparoscopy', 'hysteroscopy',
    #                 'followup', 'followup6')

    # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
    # proms, rand36, tss2

    # gyldige_tabType <- c('PATIENT', 'OPERATION', 'HYSTEROSCOPY', 'LAPAROSCOPY',
    #            #  'followup' = 'OPERATIONFOLLOWUP',
    #              'FOLLOWUP6')
    #            # 'RAND36_0', 'RAND36_1', 'RAND36_3', 'TSS2')
    tabType <- toupper(tabellnavn)

    tabellMegne <- mappingEgneNavn(tabell, tabType)

  }

  return(tabell)
}

#' Henter NGER-data
#'
#' @inheritParams NGERFigAndeler
#' @param medPROM: koble på RAND og TSS2-variabler
#' @param alleData 1- alle variabler med, 0 - utvalgte variabler med
#'
#' @return RegData data frame
#'
#' @export


NGERRegDataSQL <- function(datoFra = '2013-01-01', datoTil = Sys.Date(),
                           medPROM=1, gml=1, alleData=1, ...) {
# Få til å fungere med ny sammenkobling av alle data
  # legg på valg av variabler
  # legg på datofiltrering

  if (gml==0) {
    # Raskest å hente alle og så filtrere på dato eller filtrere på dato til slutt?

    #mce Trenger nok ganske få av disse variablene
    mceSkjema <- hentDataTabell(tabellnavn = "mce",
                               qVar = '*',
                               egneVarNavn = 0)
    #Operasjon
    OpSkjema <-  hentDataTabell(tabellnavn = "operation",
                                qVar = '*',
                                egneVarNavn = 1)
    OpSkjema <- OpSkjema[ ,-which(names(OpSkjema) %in%
                                    intersect(names(OpSkjema), names(mceSkjema)))]


    #Laparoskopi
    LapSkjema <-  hentDataTabell(tabellnavn = "laparoscopy",
                                        qVar = '*',
                                        egneVarNavn = 1)
    LapSkjema <- LapSkjema[ ,-which(names(LapSkjema) %in%
                                    intersect(names(LapSkjema), names(mceSkjema)))]

    #Hysteroskopi
    HysSkjema <-  hentDataTabell(tabellnavn = "hysteroscopy",
                                 qVar = '*',
                                 egneVarNavn = 1)
    HysSkjema <- HysSkjema[ ,-which(names(HysSkjema) %in%
                            intersect(names(HysSkjema), names(mceSkjema)))]

    #Pasientskjema:
    qPas <- paste0('ID AS PasientID,
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
TSCREATED')

   PasSkjema <- hentDataTabell(tabellnavn = "patient",
                                                 qVar = '*',
                                                 egneVarNavn = 1)
   PasSkjema <- PasSkjema[ ,-which(names(PasSkjema) %in%
                            intersect(names(PasSkjema), names(mceSkjema)))]

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

    # SAMMENSTILL ALLE SKJEMA:
    # RegData <- Eksempel:
      #  merge(mce, centre, by.x = "CENTREID", by.y = "ID",
      #                suffixes = c("", "Shus"), all.y = TRUE) |>
      # merge(patient, by.x = "PATIENT_ID", suffixes = c("", "_pasOppl"),
      #       by.y = "PasientID") |>
      # merge(surgeon_form,
      #       by = "MCEID", suffixes = c("", "_lege")) |>
      # merge(patient_form,
      #       by = "MCEID", suffixes = c("", "_pasient"), all.x = TRUE) |>
      # merge(patient_followup,
      #       suffixes = c("", "_pasOppf"), by = "MCEID", all.x = TRUE) |>
      # merge(surgeon_followup,
      #       suffixes = c("", "_legeOppf"), by = "MCEID", all.x = TRUE)
  }

  if (gml==1){
    Oppf0skjema <- followupsnum(datoFra = datoFra, datoTil = datoTil)
    AlleVarNum <- AlleVarNum(datoFra = datoFra, datoTil = datoTil)
    RegData <- dplyr::left_join(AlleVarNum, Oppf0skjema, by="ForlopsID")
  }

  if (medPROM==1) {
    RAND36 <-  rand36report() # -> fas ut og la følge samme mønster som andre tab
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

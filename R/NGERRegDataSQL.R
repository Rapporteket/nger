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
  rydd <- which(friendlyVarTab$USER_SUGGESTION == 'VERBOTEN')
  if (length(rydd)>0) {
    friendlyVarTab <- friendlyVarTab[-which(friendlyVarTab$USER_SUGGESTION == 'VERBOTEN'), ]}

  indTabType <- which(friendlyVarTab$REGISTRATION_TYPE %in% tabType)
   navnFr <- friendlyVarTab$FIELD_NAME[indTabType]
   kuttTabPrefiks <- if (tabType %in% c( 'RAND36_0', 'RAND36_1', 'RAND36_3')) {
     'RAND36_'} else {paste0(tabType, '_')}
   navn <- gsub(kuttTabPrefiks, "", navnFr)
   names(navn) <- friendlyVarTab$USER_SUGGESTION[indTabType]
  tabellEgne <- dplyr::rename(tabell, dplyr::any_of(navn)) #all_of(navn
  return(tabellEgne)
  }


# LEGG INN FJERNING AV VARIABLER SOM GJENTAS I FLERE TABELLER. f.EKS. ReshId (CENTREID)
# Alle variabler, Bare utvalgte var, Bare selvvalgte navn

#' Hent datatabell fra ngers database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
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
    tabType <- toupper(tabellnavn)
    tabell <- mappingEgneNavn(tabell, tabType)
  }

  if (tabellnavn == 'rand36') {
    RAND36_0 <- mappingEgneNavn(tabell[tabell$YEAR == 0, ], 'RAND36_0')
    RAND36_1 <- mappingEgneNavn(tabell[tabell$YEAR == 1, ], 'RAND36_1')
    RAND36_3 <- mappingEgneNavn(tabell[tabell$YEAR == 3, ], 'RAND36_3')
    tabell <- merge(RAND36_0, RAND36_1, by='ForlopsID', all.x = TRUE,
                    suffixes = c('', '1aar') ) |>
      merge(RAND36_3, by='ForlopsID', all.x = TRUE, suffixes = c('', '3aar'))
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
                           medPROM=1, gml=1, alleVar=1, ...) {
# Få til å fungere med ny sammenkobling av alle data
  # legg på valg av variabler
  # legg på datofiltrering

  if (gml==0) {
    # Raskest å hente alle og så filtrere på dato eller filtrere på dato til slutt?

    #mce Trenger nok ganske få av disse variablene
    # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid

   # qmce <-
    mceSkjema <- hentDataTabell(tabellnavn = "mce",
                               qVar = '*',
                               egneVarNavn = 0)
    #Operasjon
    OpSkjema <-  hentDataTabell(tabellnavn = "operation",
                                qVar = '*',
                                egneVarNavn = 1)
    # OpSkjema <- OpSkjema[ ,-which(names(OpSkjema) %in%
    #                                 intersect(names(OpSkjema), names(mceSkjema)))]


    #Laparoskopi
    LapSkjema <-  hentDataTabell(tabellnavn = "laparoscopy",
                                        qVar = '*',
                                        egneVarNavn = 1)
    # LapSkjema <- LapSkjema[ ,-which(names(LapSkjema) %in%
    #                                 intersect(names(LapSkjema), names(mceSkjema)))]

    #Hysteroskopi
    HysSkjema <-  hentDataTabell(tabellnavn = "hysteroscopy",
                                 qVar = '*',
                                 egneVarNavn = 1)
    # HysSkjema <- HysSkjema[ ,-which(names(HysSkjema) %in%
    #                         intersect(names(HysSkjema), names(mceSkjema)))]


    #Pasientskjema:
    qPas <- if (alleVar == 1) {'*'} else {
     paste0('ID, # AS PasientID,
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
   }

   PasSkjema <- hentDataTabell(tabellnavn = "patient",
                                                 qVar = qPas,
                                                 egneVarNavn = 1)
   # PasSkjema <- PasSkjema[ ,-which(names(PasSkjema) %in%
   #                          intersect(names(PasSkjema), names(mceSkjema)))]

   #Sykehusnavn
   EnhetsNavn <- hentDataTabell(tabellnavn = "centreattribute",
                                qVar = 'ID,
                                ATTRIBUTEVALUE as ShNavn',
                                             egneVarNavn = 0)


    #Oppfølgigsskjema:
    #Ikke filtrert på ferdigstilt
    # qOppf0 <- paste0('select * FROM followup
    #                 INNER JOIN operation on followup.MCEID = operation.MCEID')
                    # WHERE operation.STATUS = 1 AND
                    # operation.OP_DATE >= \'', datoFra, '\' AND operation.OP_DATE <= \'', datoTil, '\'')

    Oppf0Skjema <- hentDataTabell(tabellnavn = "followup",
                                              qVar = '*',
                                              egneVarNavn = 1)

    Oppf6Skjema <- hentDataTabell(tabellnavn = "followup6",
                                  qVar = '*',
                                  egneVarNavn = 1)
    #Trenger ikke denne? For å avgjøre om svart?
    PromSkjema <- hentDataTabell(tabellnavn = "proms",
                                  qVar = '*',
                                  egneVarNavn = 0)
    RANDskjema <- hentDataTabell(tabellnavn = "rand36",
                                  qVar = '*') #Henter alltid egne variabelnavn

    TSS2Skjema <- hentDataTabell(tabellnavn = "tss2",
                                 qVar = '*',
                                 egneVarNavn = 1)
#    prem - tom, proms, rand36, tss2
# type: RAND36_0     RAND36_1 RAND36_3         TSS2

        # SAMMENSTILL ALLE SKJEMA:
    RegData <-
      merge(mceSkjema, # centre, by.x = "CENTREID", by.y = "ID", suffixes = c("", "Shus"), all.y = TRUE) |>
            PasSkjema, by.x = "PATIENT_ID", by.y = "PasientID",
            suffixes = c("", "_pas")) |>
      merge(LapSkjema, by = "MCEID", all.x=TRUE, suffixes = c("", "_lap")) |>
      merge(HysSkjema,
            by = "MCEID", all.x = TRUE, suffixes = c("", "_hys")) |>
      merge(OpSkjema,
            suffixes = c("", "_op"), by = "MCEID", all.x = TRUE) |>
      merge(EnhetsNavn,
            by.x = "CENTREID", by.y = 'ID', all.x = TRUE) |>
      merge(Oppf0Skjema,
            by = "MCEID",all.x = TRUE, suffixes = c("", "_oppf0")) |>
      merge(Oppf6Skjema,
            by = "MCEID", all.x = TRUE, suffixes = c("", "_oppf6"))  |>
      merge(RANDskjema,
          by.x = "MCEID", by.y = 'ForlopsID', all.x = TRUE,
          suffixes = c("", "_rand"))  |>
      merge(TSS2Skjema,
            by = "MCEID", all.x = TRUE, suffixes = c("", "_tss2"))
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

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
                           datoFra = '2011-01-01',
                           datoTil = Sys.Date(),
                           egneVarNavn = 1) { #  status = 1

  query <- paste0("SELECT ", qVar, " FROM ", tabellnavn)

  if (tabellnavn == 'operation'){
    query <- paste0(query,
                    ' WHERE OP_DATE >= \'', datoFra,
                    '\' AND OP_DATE <= \'', datoTil, '\' ')
  }
  tabell <- rapbase::loadRegData(registryName = "data",
                                 query = query)

  if (egneVarNavn == 1) {
    tabType <- toupper(tabellnavn)
    tabell <- mappingEgneNavn(tabell, tabType)
  }

  if (tabellnavn == 'rand36') {
    #Har oppdatert navnene i variabelregisteret, så skal ikke trenge suffiks
    RAND36_0 <- mappingEgneNavn(tabell[tabell$YEAR == 0, ], 'RAND36_0')
    RAND36_1 <- mappingEgneNavn(tabell[tabell$YEAR == 1, ], 'RAND36_1')
    RAND36_3 <- mappingEgneNavn(tabell[tabell$YEAR == 3, ], 'RAND36_3')
    tabell <- merge(RAND36_0, RAND36_1, by='ForlopsID',
                    all.x = TRUE, suffixes = c('', '1aar') ) |>
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


NGERRegDataSQL <- function(datoFra = '2011-01-01', datoTil = Sys.Date(),
                           medPROM=1, gml=0, ...) {
# Få til å fungere med ny sammenkobling av alle data
  # legg på datofiltrering

  if (gml==0) {
    # Raskest å hente alle og så filtrere på dato eller filtrere på dato til slutt?

    # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
    qmce <- paste0('MCEID, CENTREID, PATIENT_ID')
    # MCE_TYPE, OPER_DATE, PARENT_MCE, , TSCREATED, TSUPDATED
    # LAPARO_STATUS, LAPARO_TSUPDATED, HYSTERO_STATUS, FOLLOWUP_STATUS, FOLLOWUP_TYPE,
    # TSS2_STATUS, TSS2_FOLLOWUP_TYPE, FOLLOWUP_TSS2_PROM_STATUS, FOLLOWUP_6MND_STATUS, RAND36_Y1_STATUS,

    mceSkjema <- hentDataTabell(tabellnavn = "mce",
                               qVar = qmce,
                               egneVarNavn = 0)
    #Operasjon
    qOp <- "MCEID, CENTREID AS ReshId,
    HEIGHT, WEIGHT,  MCETYPE,
       BMI, PARITIES, EARLIER_VAGINAL,
       VAG_REVISIO, VAG_HYSTEROSCOPY, VAG_CONISATION, VAG_DESCENS,
       VAG_TVT, VAG_HYSTERECTOMY, EARLIER_LAPAROSCOPY, LAPARASCOPY_COUNT,
       EARLIER_LAPAROTOMY, LAPAROTOMI_COUNT, SECTIO_COUNT, BLOOD_THINNERS,
       OP_DATE, OPTYPE, COMPLICATION, MAIN_OPERATION,
       COMPLICATION_TYPE, OPCAT, OPCAT_OUTSIDE_DAYTIME,
       CARE_LEVEL, OP_INDICATION1, OP_INDICATION2, OP_INDICATION3,
       ANESTHESIA_NONE, ANESTHESIA_LOCAL, ANESTHESIA_GENERAL, ANESTHESIA_SPINAL_EDA,
       ANESTHESIA_SEDATION, ASA, OPTIME_COUNT, ANTIBIOTIC_PROPHYLAXIS, SURVIVED,
       STATUS, FIRST_TIME_CLOSED"
    # TSCREATED, COMPLICATION_COMMENT, TSUPDATED,
#datoFra <- '2025-01-01'
#datoTil <- '2025-12-31'
    OpSkjema <-  hentDataTabell(tabellnavn = "operation",
                                datoFra = datoFra,
                                datoTil = datoTil,
                                qVar = qOp,
                                egneVarNavn = 1)

    #Fjern var med Ver, CENTREID, UPDATEDBY
    # ??_SPECIFY??, COMMENT,
    # txtFjern <-
#varFjernes <- c(names(grep('Ver'))
#                RegData <- RegData[ ,-c(grep('_MISS', names(RegData)), grep('_UTFYLT', names(RegData)))]

    #Laparoskopi
    LapSkjema <-  hentDataTabell(tabellnavn = "laparoscopy",
                                        qVar = '*',
                                        egneVarNavn = 1)
    # intersect(sort(names(LapSkjema)), sort(names(OpSkjema)))
    #Hysteroskopi
    HysSkjema <-  hentDataTabell(tabellnavn = "hysteroscopy",
                                 qVar = '*',
                                 egneVarNavn = 1)

    #Pasientskjema:
    qPas <- 'ID AS PasientID,
              BIRTH_DATE,
              NATIVE_LANGUAGE,
              NORWEGIAN,
              REGIONAL_HEALTH_AUTHORITY AS RHF,
              EDUCATION,
              DECEASED,
              DECEASED_DATE,
              MARITAL_STATUS'

   PasSkjema <- hentDataTabell(tabellnavn = "patient",
                                                 qVar = qPas,
                                                 egneVarNavn = 1)
   #Sykehusnavn
   EnhetsNavn <- hentDataTabell(tabellnavn = "centreattribute",
                                qVar = 'ID,
                                ATTRIBUTEVALUE as ShNavn',
                                             egneVarNavn = 1)
   RegData <-
     merge(mceSkjema,
           PasSkjema, by.x = "PATIENT_ID", by.y = "PasientID",
           suffixes = c("", "_pas")) |>
     merge(OpSkjema,
           suffixes = c("", "_op"), by = "MCEID", all.x = F) |>
     merge(LapSkjema, by = "MCEID", all.x=TRUE, suffixes = c("", "_lap")) |>
     merge(HysSkjema,
           by = "MCEID", all.x = TRUE, suffixes = c("", "_hys")) |>
     merge(EnhetsNavn,
           by.x = "CENTREID", by.y = 'ID', all.x = TRUE)

if (medPROM == 1) {
    #Oppfølgigsskjema:
    Oppf0Skjema <- hentDataTabell(tabellnavn = "followup",
                                              qVar = '*',
                                              egneVarNavn = 1)
   Oppf0Skjema <- Oppf0Skjema |> dplyr::rename(Opf0metode = FOLLOWUP_TYPE)

   # followup.PROM_ANSWERED AS Opf0BesvarteProm, -> Opf0Utf ->Opf0UtfViaEprom

   Oppf6Skjema <- hentDataTabell(tabellnavn = "followup6",
                                  qVar = '*',
                                  egneVarNavn = 1)

   #Trenger ikke denne? For å avgjøre om svart?
    qProm <- "CENTREID, DISTRIBUTION_RULE, EXPIRY_DATE,
    FORM_ORDER_STATUS_ERROR_CODE, MCEID, NOTIFICATION_CHANNEL, REGISTRATION_TYPE,
    REMINDER_DATE, STATUS, TSRECEIVED, TSSENDT, TSUPDATED"
    PromSkjema <- hentDataTabell(tabellnavn = "proms",
                                  qVar = qProm,
                                  egneVarNavn = 0)

    RANDskjema <- hentDataTabell(tabellnavn = "rand36",
                                  qVar = '*') #Benytter alltid egne variabelnavn

    TSS2Skjema <- hentDataTabell(tabellnavn = "tss2",
                                 qVar = '*',
                                 egneVarNavn = 1)

        # SAMMENSTILL SKJEMA:
    RegData <- RegData |>
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
  }

  if (gml==1){
    Oppf0skjema <- followupsnum(datoFra = datoFra, datoTil = datoTil)
    AlleVarNum <- AlleVarNum(datoFra = datoFra, datoTil = datoTil)
    RegData <- dplyr::left_join(AlleVarNum, Oppf0skjema, by="ForlopsID")

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
  }

  #Fjern var med Ver, CENTREID, UPDATEDBY
  # ??_SPECIFY??, COMMENT,
 RegData <- RegData[ ,-c(grep('Ver', names(RegData)),
                         grep('CENTREID', names(RegData)),
                         grep('COMMENT', names(RegData)),
                         grep('CREATED', names(RegData)),
                         grep('CLOSED', names(RegData)),
                         grep('UPDATED', names(RegData)),
                         grep('_SPECIFY', names(RegData)))]


  return(invisible(RegData))
}

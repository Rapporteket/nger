-- Kode brukt i roskva for å lage tabeller som brukes av nger.
-- 22. mai 2025

-- forlopsoversikt
SELECT
    m.CENTREID AS AvdRESH,
    -- Inlined getFriendlyName(m.CENTREID)
    (
        SELECT IFNULL(
            (SELECT ca_fn.ATTRIBUTEVALUE FROM centreattribute ca_fn WHERE ca_fn.ID = c_fn.ID AND ca_fn.ATTRIBUTENAME = 'FRIENDLYNAME'),
            c_fn.CENTRENAME
        )
        FROM centre c_fn
        WHERE c_fn.ID = m.CENTREID
    ) AS SykehusNavn,
    CAST(p.ID AS CHAR(10)) AS PasientID,
    CAST(NULL AS CHAR(4)) AS PostNr,
    p.TOWN AS PostSted,
    p.MUNICIPALITY_NAME AS Kommune,
    p.MUNICIPALITY_NUMBER AS Kommunenr,
    p.COUNTY AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    p.SSN as KryptertFnr,
    'Kvinne' AS PasientKjonn, -- As per original view
    '0' AS ErMann, -- As per original view
    -- Inlined getAgeAtMainDate(m.MCEID)
    IFNULL(FLOOR(DATEDIFF(o.OP_DATE, p.BIRTH_DATE) / 365.25), 0) AS PasientAlder,
    p.BIRTH_DATE AS Fodselsdato,
    -- Inlined getListText('PATIENT_NORWEGIAN', p.NORWEGIAN)
    (
        SELECT t_pn.text
        FROM listboxtextrow lt_pn JOIN text t_pn ON t_pn.ID = lt_pn.TEXTID
        WHERE lt_pn.TEXTVALUE = CONCAT('', p.NORWEGIAN)
          AND lt_pn.ID = 'PATIENT_NORWEGIAN_L'
          AND t_pn.LANGUAGEID = 'no'
    ) as Norsktalende,
    -- Inlined getListText('PATIENT_MARITAL_STATUS', p.MARITAL_STATUS)
    (
        SELECT t_pms.text
        FROM listboxtextrow lt_pms JOIN text t_pms ON t_pms.ID = lt_pms.TEXTID
        WHERE lt_pms.TEXTVALUE = CONCAT('', p.MARITAL_STATUS)
          AND lt_pms.ID = 'PATIENT_MARITAL_STATUS_L'
          AND t_pms.LANGUAGEID = 'no'
    ) as SivilStatus,
    CASE p.EDUCATION
        WHEN 1 THEN '01 Grunnskolenivå'
        WHEN 2 THEN '02a Videregående skolenivå'
        WHEN 3 THEN '02a Videregående skolenivå'
        WHEN 4 THEN '03a Universitets- og høgskolenivå kort'
        WHEN 5 THEN '04a Universitets- og høgskolenivå lang'
        WHEN 9 THEN '09a Uoppgitt eller ingen fullført utdanning'
        ELSE '09a Uoppgitt eller ingen fullført utdanning'
    END AS UtdanningSSB,
    -- Inlined getListText('PATIENT_DECEASED', p.DECEASED)
    (
        SELECT t_pd.text
        FROM listboxtextrow lt_pd JOIN text t_pd ON t_pd.ID = lt_pd.TEXTID
        WHERE lt_pd.TEXTVALUE = CONCAT('', p.DECEASED)
          AND lt_pd.ID = 'PATIENT_DECEASED_L'
          AND t_pd.LANGUAGEID = 'no'
    ) AS Avdod,
    p.DECEASED_DATE as AvDodDato,
    CAST(m.MCEID AS CHAR(10)) AS ForlopsID,
    CAST(m.PARENT_MCE AS CHAR(10)) AS KobletForlopsID,
    CAST(
        CASE o.MCETYPE
          WHEN 1 THEN GREATEST(LEAST(IFNULL(o.STATUS, 0), IFNULL(l.STATUS,0)), 0)
          WHEN 2 THEN GREATEST(LEAST(IFNULL(o.STATUS, 0), IFNULL(h.STATUS,0)), 0)
          WHEN 3 THEN GREATEST(LEAST(LEAST(IFNULL(o.STATUS, 0), IFNULL(l.STATUS,0)), IFNULL(h.STATUS,0)), 0)
          ELSE 0
        END
    AS CHAR(2)) AS BasisRegStatus,
    CAST(
        IF (o.MCETYPE IS NULL,
            'Ikke satt',
            -- Inlined getListText('OPERATION_MCETYPE', o.MCETYPE)
            (
                SELECT t_omcet.text
                FROM listboxtextrow lt_omcet JOIN text t_omcet ON t_omcet.ID = lt_omcet.TEXTID
                WHERE lt_omcet.TEXTVALUE = CONCAT('', o.MCETYPE)
                  AND lt_omcet.ID = 'OPERATION_MCETYPE_L'
                  AND t_omcet.LANGUAGEID = 'no'
            )
        )
    AS CHAR(50)) AS ForlopsType1,
    o.MCETYPE as ForlopsType1Num,
    CAST(
        -- Inlined getListText('OPERATION_OPCAT', o.OPCAT)
        (
            SELECT t_oopc.text
            FROM listboxtextrow lt_oopc JOIN text t_oopc ON t_oopc.ID = lt_oopc.TEXTID
            WHERE lt_oopc.TEXTVALUE = CONCAT('', o.OPCAT)
              AND lt_oopc.ID = 'OPERATION_OPCAT_L'
              AND t_oopc.LANGUAGEID = 'no'
        )
    AS CHAR(50)) AS ForlopsType2,
    o.OPCAT AS ForlopsType2Num,
    o.OP_DATE AS HovedDato,
    CAST(
        CASE m.REVISION
          WHEN 1 THEN
            CASE (IFNULL(f0.STATUS, -2))
              WHEN -1 THEN -1
              WHEN 0 THEN 0
              WHEN 1 THEN 2
              ELSE -2
            END
          WHEN 2 THEN
            CASE
              WHEN
                ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- All compmpleted
                THEN 2
              WHEN
                ((GREATEST(IFNULL(f0.STATUS, -2), IFNULL(r0.STATUS, -2),IFNULL(tss2.STATUS, -2),IFNULL(r1.STATUS, -2))) = -2) -- Non exist
                THEN -2
              WHEN
                ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1))) = 1)  -- Year 0 part completed
                THEN 1
              WHEN
                ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- Some completed
                THEN 1
              WHEN
                ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 0) -- Non completed, some in progress
                THEN 0
              WHEN
                ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = -1)  -- All created
                THEN -1
              ELSE NULL -- unknown
            END
          ELSE 2
        END
    AS CHAR(2)) AS OppflgRegStatus,
    "0" AS ErOppflg, -- As per original view
    CASE m.REVISION
      WHEN 1 THEN
        CASE (IFNULL(f0.STATUS, -2))
          WHEN -1 THEN "Opprettet"
          WHEN 0 THEN "Under utfylling"
          WHEN 1 THEN "Oppfølging komplett"
          ELSE "Ingen opprettet"
        END
      WHEN 2 THEN
        CASE
          WHEN
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- All compmpleted
            THEN "Oppfølging komplett"
          WHEN
            ((GREATEST(IFNULL(f0.STATUS, -2), IFNULL(r0.STATUS, -2),IFNULL(tss2.STATUS, -2),IFNULL(r1.STATUS, -2))) = -2) -- Non exist
            THEN "Ingen opprettet"
          WHEN
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1))) = 1)  -- Year 0 part completed
            THEN "År 0 komplett"
          WHEN
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- Some completed
            THEN "Delvis komplett"
          WHEN
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = 0) -- Non completed, some in progress
            THEN "Under utfylling"
          WHEN
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1),IFNULL(r1.STATUS, -1))) = -1)  -- All created
            THEN "Opprettet"
          ELSE "Ingen opprettet"-- unknown
        END
      ELSE "Ingen opprettet"
    END AS OppflgStatus,
    CAST(NULL AS CHAR(6)) AS OppflgSekNr
 FROM
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
    INNER JOIN operation o on m.MCEID = o.MCEID
    LEFT OUTER JOIN user u ON m.CREATEDBY = u.ID
    LEFT OUTER JOIN laparoscopy l ON m.MCEID = l.MCEID
    LEFT OUTER JOIN hysteroscopy h ON m.MCEID = h.MCEID
    LEFT OUTER JOIN followup f0 ON (m.MCEID = f0.MCEID)
    LEFT OUTER JOIN rand36 r0 ON (m.MCEID = r0.MCEID and r0.`YEAR` = 0)
    LEFT OUTER JOIN tss2 ON m.MCEID = tss2.MCEID
    LEFT OUTER JOIN rand36 r1 ON (m.MCEID = r1.MCEID  and r1.`YEAR` = 1)
  WHERE o.STATUS = 1 AND (IFNULL(l.STATUS, 1) = 1 OR IFNULL(h.STATUS, 1) = 1);
  
-- skjemaoversikt
SELECT
  'Operasjon' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  skjema.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  1 AS SkjemaRekkeflg
FROM
  operation skjema
  LEFT OUTER JOIN centre c ON skjema.CENTREID = c.ID
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'Laparoskopi' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  3 AS SkjemaRekkeflg
FROM
  laparoscopy skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'Hysteroskopi' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  5 AS SkjemaRekkeflg
FROM
  hysteroscopy skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'Oppfølging år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  7 AS SkjemaRekkeflg
FROM
  followup skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'Rand-36 år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  9 AS SkjemaRekkeflg
FROM
  rand36 skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID AND skjema.`YEAR` = 0
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'TSS2 år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  11 AS SkjemaRekkeflg
FROM
  tss2 skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'
UNION ALL -- Changed from UNION
SELECT
  'Rand-36 år 1' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(20)) AS ForlopsID,
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  o.OP_DATE AS HovedDato,
  IFNULL(ca.ATTRIBUTEVALUE, c.CENTRENAME) AS SykehusNavn,
  c.ID AS AvdRESH,
  15 AS SkjemaRekkeflg
FROM
  rand36 skjema
  INNER JOIN operation o ON skjema.MCEID = o.MCEID AND skjema.`YEAR` = 1
  LEFT OUTER JOIN centre c ON o.CENTREID = c.ID -- Assuming SykehusNavn should come from operation's centre
  LEFT OUTER JOIN centreattribute ca ON c.ID = ca.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME';

-- allevarnum
SELECT
  patient.ID as PasientID,
  patient.REGISTERED_DATE as PasRegDato,
  patient.SSN as PersonNr,
  patient.SSN_TYPE as PersonNrType,
  patient.BIRTH_DATE as FodselsDato,
  patient.NATIVE_LANGUAGE as Morsmaal,
  patient.NATIVE_LANGUAGE_OTHER as MorsmaalAnnet,
  patient.NORWEGIAN as Norsktalende,
  patient.EDUCATION as Utdanning,
  patient.DECEASED as Avdod,
  patient.DECEASED_DATE as AvdodDato,
  patient.MARITAL_STATUS as SivilStatus,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
  mce.CALLS_MADE as RingtGanger,
  mce.DELIVERY_DATE as Leveringsdato,
  mce.REVISION as RegisterRevisjon,
  operation.HEIGHT AS OpHoyde,
  operation.WEIGHT AS OpVekt,
  operation.BMI AS OpBMI,
  CASE operation.BMI_CATEGORY
    WHEN 'Alvorlig undervekt' THEN 1
    WHEN 'Moderat undervekt' THEN 2
    WHEN 'Mild undervekt' THEN 3
    WHEN 'Normal' THEN 4
    WHEN 'Overvekt' THEN 5
    WHEN 'Moderat fedme, klasse I' THEN 6
    WHEN 'Fedme, klasse II' THEN 7
    WHEN 'Fedme, klasse III' THEN 8
    ELSE 9
    END AS OpBMIKategori,
  operation.PARITIES AS OpPariteter,
  operation.ADDR_TYPE AS OpAdrType,
  operation.ADDRESS AS OpAdr,
  operation.ZIPCODE AS OpPostnr,
  operation.TOWN AS OpPoststed,
  operation.MUNICIPALITY_NUMBER AS OpKomNr,
  operation.MUNICIPALITY_NAME AS OpKomNavn,
  operation.COUNTY AS OpFylke,
  operation.FREEFORM_ADDR AS OpAdresse,
  operation.EARLIER_VAGINAL AS OpTidlVagInngrep,
  operation.EARLIER_LAPAROSCOPY AS OpTidlLapsko,
  operation.LAPARASCOPY_COUNT AS OpTidlLapskoNum,
  operation.EARLIER_LAPAROTOMY AS OpTidlLaparotomi,
  operation.LAPAROTOMI_COUNT AS OpTidlLaparotomiNum,
  operation.SECTIO_COUNT AS OpSectioNum,
  operation.BLOOD_THINNERS AS OpBlodfortynnende,
  operation.OP_DATE AS OpDato,
  operation.OPTYPE AS OpType,
  operation.COMPLICATION AS OpArsakReopKomp,
  operation.MAIN_OPERATION AS OpHovedOperasjon,
  operation.COMPLICATION_TYPE AS OpKomplType,
  operation.OPCAT AS OpKategori,
  operation.OPCAT_OUTSIDE_DAYTIME AS OpIVaktTid,
  operation.MCETYPE AS OpMetode,
  operation.CARE_LEVEL AS OpBehNivaa,
  operation.OP_INDICATION1 AS OpIndikasjon1,
  operation.OP_INDICATION1_VERSION AS OpIndikasjon1Ver,
  operation.OP_INDICATION2 AS OpIndikasjon2,
  operation.OP_INDICATION2_VERSION AS OpIndikasjon2Ver,
  operation.OP_INDICATION3 AS OpIndikasjon3,
  operation.OP_INDICATION3_VERSION AS OpIndikasjon3Ver,
  operation.ANESTHESIA_NONE AS OpAnestesiIngen,
  operation.ANESTHESIA_LOCAL AS OpAnestesiLok,
  operation.ANESTHESIA_GENERAL AS OpAnestesiGen,
  operation.ANESTHESIA_SPINAL_EDA AS OpAnestesiSpinEDA,
  operation.ANESTHESIA_SEDATION AS OpAnestesiSed,
  operation.ASA AS OpASA,
  operation.OPTIME_COUNT AS OpTid,
  operation.ANTIBIOTIC_PROPHYLAXIS AS OpAntibProfylakse,
  operation.SURVIVED AS OpOverlevd,
  operation.FIRST_TIME_CLOSED as OpForstLukket,
  CONCAT(u_op.FIRSTNAME, ' ', u_op.LASTNAME) as OpForstLukketAv, -- MODIFIED
  operation.STATUS as OpStatus,
  laparoscopy.DIAGNOSIS1 AS LapDiagnose1,
  laparoscopy.DIAGNOSIS1_VERSION AS LapDiagnose1Ver,
  laparoscopy.DIAGNOSIS2 AS LapDiagnose2,
  laparoscopy.DIAGNOSIS2_VERSION AS LapDiagnose2Ver,
  laparoscopy.DIAGNOSIS3 AS LapDiagnose3,
  laparoscopy.DIAGNOSIS3_VERSION AS LapDiagnose3Ver,
  laparoscopy.DIAGNOSIS4 AS LapDiagnose4,
  laparoscopy.DIAGNOSIS4_VERSION AS LapDiagnose4Ver,
  laparoscopy.PROCEDURE1 AS LapProsedyre1,
  laparoscopy.PROCEDURE1_VERSION AS LapProsedyre1Ver,
  laparoscopy.PROCEDURE2 AS LapProsedyre2,
  laparoscopy.PROCEDURE2_VERSION AS LapProsedyre2Ver,
  laparoscopy.PROCEDURE3 AS LapProsedyre3,
  laparoscopy.PROCEDURE3_VERSION AS LapProsedyre3Ver,
  laparoscopy.PROCEDURE4 AS LapProsedyre4,
  laparoscopy.PROCEDURE4_VERSION AS LapProsedyre4Ver,
  laparoscopy.ACCESS_METHOD AS LapTilgangsMetode,
  laparoscopy.OPTICALTROCAR AS LapOptTro,
  laparoscopy.ACCESS AS LapTilgang,
  laparoscopy.NUM_HJELPEINNSTIKK AS LapNumHjelpeinnstikk,
  laparoscopy.SINGELPORT AS LapSingelPort,
  laparoscopy.BIPOLAR_DIATERMI AS LapBipolarDiatermi,
  laparoscopy.UNIPOLAR_DIATERMI AS LapUnipolarDiatermi,
  laparoscopy.ADVANCED_TISSUE_SEALER AS LapVevforsegl,
  laparoscopy.MORCELLATOR AS LapMorcellatorUtenPose,
  laparoscopy.SUTUR AS LapSutur,
  laparoscopy.CLIPS AS LapClips,
  laparoscopy.NETT AS LapNett,
  laparoscopy.ADHERANSEPROFYLAXE AS LapAdherProfylakse,
  laparoscopy.HAEMOSTASIS_AGENT AS LapHemastase,
  laparoscopy.STAPLER_ENDOGIA AS LapStaplerEndogia,
  laparoscopy.PREPARATOPOSE AS LapPreparatopose,
  laparoscopy.UTERUSMANIPULATOR AS LapUterusman,
  laparoscopy.ROBOT_KIRURGI AS LapRobotKirurgi,
  laparoscopy.OTHER AS LapAndre,
  laparoscopy.MEDICATIONWITHDRAWAL AS LapPrepUttak,
  laparoscopy.PREPARATION_SPLIT_AT_WITHDRAWAL AS LapPrepOppdel,
  laparoscopy.COMPLICATIONS AS LapKomplikasjoner,
  laparoscopy.COMPLICATIONS_VASCULAR AS LapKomplKar,
  laparoscopy.COMPLICATIONS_INTESTINE AS LapKomplTarm,
  laparoscopy.COMPLICATIONS_BLADDER AS LapKomplBlaere,
  laparoscopy.COMPLICATIONS_URETHRA AS LapKomplUreter,
  laparoscopy.COMPLICATIONS_OTHER AS LapKomplAnnet,
  laparoscopy.COMPLICATIONS_AROSE_ACCESS AS LapSkadeTilgang,
  laparoscopy.COMPLICATIONS_AROSE_EXTRACTION AS LapSkadeUthent,
  laparoscopy.COMPLICATIONS_AROSE_DISSECTION AS LapSkadeDissek,
  laparoscopy.COMPLICATIONS_AROSE_VESSEL_SEALING AS LapSkadeForsegl,
  laparoscopy.COMPLICATIONS_AROSE_OTHER AS LapSkadeAnnet,
  laparoscopy.COMPLICATIONS_CONTRIBUTING_TECHNICAL AS LapSkadeaarsakTeknUtst,
  laparoscopy.COMPLICATIONS_CONTRIBUTING_ADHESIONS AS LapSkadeaarsakAdher,
  laparoscopy.COMPLICATIONS_CONTRIBUTING_PREVIOUS_SURGERY AS LapSkadeaarsakTidlKir,
  laparoscopy.COMPLICATIONS_CONTRIBUTING_OTHER AS LapSkadeaarsakAnnet,
  laparoscopy.PORTIOADAPTER AS LapUterusmanipulatorFJERNET,
  laparoscopy.TILGANG AS LapKompTilgang,
  laparoscopy.HJELPEINSTIKK AS LapHjelpeinnstikk,
  laparoscopy.INTRAABDOMINAL AS LapIntraabdominell,
  laparoscopy.TEKNISK_UTSTYR AS LapTekniskUtstyr,
  laparoscopy.POSTOPERATIV AS LapPostoperativ,
  laparoscopy.KAR_BLEED AS LapKarBlodning,
  laparoscopy.TARM AS LapTarm,
  laparoscopy.BLAERE AS LapBlare,
  laparoscopy.URETER AS LapUreter,
  laparoscopy.NERV AS LapNerv,
  laparoscopy.CONVERTED AS LapKonvertert,
  laparoscopy.CONVERTED_STATUS AS Konverteringsstatus,
  laparoscopy.STATUS AS LapStatus,
  hysteroscopy.DIAGNOSIS1 AS HysDiagnose1,
  hysteroscopy.DIAGNOSIS1_VERSION AS HysDiagnose1Ver,
  hysteroscopy.DIAGNOSIS2 AS HysDiagnose2,
  hysteroscopy.DIAGNOSIS2_VERSION AS HysDiagnose2Ver,
  hysteroscopy.DIAGNOSIS3 AS HysDiagnose3,
  hysteroscopy.DIAGNOSIS3_VERSION AS HysDiagnose3Ver,
  hysteroscopy.PROCEDURE1 AS HysProsedyre1,
  hysteroscopy.PROCEDURE1_VERSION AS HysProsedyre1Ver,
  hysteroscopy.PROCEDURE2 AS HysProsedyre2,
  hysteroscopy.PROCEDURE2_VERSION AS HysProsedyre2Ver,
  hysteroscopy.PROCEDURE3 AS HysProsedyre3,
  hysteroscopy.PROCEDURE3_VERSION AS HysProsedyre3Ver,
  hysteroscopy.BIPOLAR_DIATHERMY AS HysBipolarDiatermi,
  hysteroscopy.UNIPLOAR_DIATHERMY AS HysUnipolarDiatermi,
  hysteroscopy.MORCELLATOR AS HysMorcellator,
  hysteroscopy.ADHERENCE_PROPHYLAXIS AS HysAdProf,
  hysteroscopy.COPPER_SPIRAL AS HysKobberspir,
  hysteroscopy.HORMONAL_IUD AS HysHormonspir,
  hysteroscopy.PRETREATMENT_COMBO AS HysMedForbehCervix,
  hysteroscopy.COMPLETENESS AS HysGjforingsGrad,
  hysteroscopy.PAINS AS HysUfullSmerte,
  hysteroscopy.GASEMBOLI AS HysUfullMisGass,
  hysteroscopy.CMPLTCOMPLICATIONS AS HysUfullKompl,
  hysteroscopy.HIGH_FLUID_DEFICIT AS HysUfullHoyVaeske,
  hysteroscopy.ANNET_ALTERNATIV AS HysUfullAnnet,
  hysteroscopy.ANNEN_ARSAK AS HysUfullAnnetArsak,
  hysteroscopy.COMPLICATIONS AS HysKomplikasjoner,
  hysteroscopy.ACCESS AS HysTilgang,
  hysteroscopy.TECHNICAL AS HysTeknisk,
  hysteroscopy.PERFORATION AS HysPerforasjon,
  hysteroscopy.BLEEDING AS HysBlodning,
  hysteroscopy.FLUID_OVERLOAD AS HysFluidOverload,
  hysteroscopy.COMPLICATIONS_CERVIX_STENOSIS AS HysSkadeaarsakStenose,
  hysteroscopy.COMPLICATIONS_INTRAUTERINE_ADHESIONS AS HysSkadeaarsakAd,
  hysteroscopy.COMPLICATIONS_TECHNICAL AS HysSkadeaarsakTeknUtst,
  hysteroscopy.COMPLICATIONS_ANATOMICAL_CONDITIONS AS HysSkadeaarsakAnatomi,
  hysteroscopy.COMPLICATIONS_OTHER AS HysSkadeaarsakAnnet,
  hysteroscopy.COMPLICATIONS_TYPE_VIAFALSA AS HysKomplViaFalsa,
  hysteroscopy.COMPLICATIONS_TYPE_FLUID_DEFICIT AS HysKomplVaeske,
  hysteroscopy.COMPLICATIONS_TYPE_PERFORATION AS HysKomplPerf,
  hysteroscopy.COMPLICATIONS_TYPE_GASEMBOLI AS HysKomplGass,
  hysteroscopy.COMPLICATIONS_TYPE_BLEEDING AS HysKomplBlodn,
  hysteroscopy.COMPLICATIONS_TYPE_OTHER AS HysKomplAnnet,
  hysteroscopy.COMPLICATIONS_MEASURES_TAMPONADE AS HysKomplTiltakTamp,
  hysteroscopy.COMPLICATIONS_MEASURES_ABORTED AS HysKomplTiltakAvbr,
  hysteroscopy.COMPLICATIONS_MEASURES_OTHER AS HysKomplTiltakAnnet,
  hysteroscopy.COMPLICATIONS_MEASURES_NONE AS HysKomplTiltakIngen,
  hysteroscopy.CONVERTED AS HysKonvertert,
  hysteroscopy.CONVERTED_LAPAROSCOPY AS HysKonvLaparoskopi,
  hysteroscopy.CONVERTED_LAPROTOMY AS HysKonvLaparotomi,
  hysteroscopy.FIRST_TIME_CLOSED as HysForstLukket,
  CONCAT(u_hys.FIRSTNAME, ' ', u_hys.LASTNAME) as HysForstLukketAv, -- MODIFIED
  hysteroscopy.STATUS as HysStatus
FROM mce
INNER JOIN patient ON mce.PATIENT_ID = patient.ID
INNER JOIN operation ON mce.MCEID = operation.MCEID
LEFT OUTER JOIN laparoscopy ON mce.MCEID = laparoscopy.MCEID
LEFT OUTER JOIN hysteroscopy ON mce.MCEID = hysteroscopy.MCEID
LEFT OUTER JOIN user u_op ON operation.FIRST_TIME_CLOSED_BY = u_op.ID -- ADDED
LEFT OUTER JOIN user u_hys ON hysteroscopy.FIRST_TIME_CLOSED_BY = u_hys.ID -- ADDED
WHERE
  operation.STATUS = 1
  AND (laparoscopy.STATUS = 1 OR hysteroscopy.STATUS = 1); -- SIMPLIFIED/MODIFIED

-- followupsnum
SELECT
  patient.ID as PasientID,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
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
  CONCAT(u_followup.FIRSTNAME, ' ', u_followup.LASTNAME) as Opf0ForstLukketAv, -- MODIFIED
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
  CONCAT(u_tss2.FIRSTNAME, ' ', u_tss2.LASTNAME) as Tss2ForstLukketAv, -- MODIFIED
  tss2.STATUS as Tss2Status

FROM mce
INNER JOIN patient ON mce.PATIENT_ID = patient.ID
INNER JOIN operation on mce.MCEID = operation.MCEID
LEFT OUTER JOIN followup ON (mce.MCEID = followup.MCEID AND followup.STATUS = 1)
LEFT OUTER JOIN tss2 ON (mce.MCEID = tss2.MCEID AND tss2.STATUS = 1)
LEFT OUTER JOIN user u_followup ON followup.FIRST_TIME_CLOSED_BY = u_followup.ID -- ADDED
LEFT OUTER JOIN user u_tss2 ON tss2.FIRST_TIME_CLOSED_BY = u_tss2.ID -- ADDED
WHERE
  operation.STATUS = 1;

-- rand36report
SELECT
    r.MCEID AS ForlopsID,
    r.CENTREID AS SykehusID,
    r.FOLLOWUP_TYPE AS Metode,
    r.PROM_ANSWERED AS RSkjemaBesvart,
    r.FORM_COMPLETED_VIA_PROMS AS RUtfyltViaEProm,
    r.YEAR AS Aar,
    r.Q1  AS RSpm1,
    r.Q2  AS RSpm2,
    r.Q3  AS RSpm3a,
    r.Q4  AS RSpm3b,
    r.Q5  AS RSpm3c,
    r.Q6  AS RSpm3d,
    r.Q7  AS RSpm3e,
    r.Q8  AS RSpm3f,
    r.Q9  AS RSpm3g,
    r.Q10 AS RSpm3h,
    r.Q11 AS RSpm3i,
    r.Q12 AS RSpm3j,
    r.Q13 AS RSpm4a,
    r.Q14 AS RSpm4b,
    r.Q15 AS RSpm4c,
    r.Q16 AS RSpm4d,
    r.Q17 AS RSpm5a,
    r.Q18 AS RSpm5b,
    r.Q19 AS RSpm5c,
    r.Q20 AS RSpm6,
    r.Q21 AS RSpm7,
    r.Q22 AS RSpm8,
    r.Q23 AS RSpm9a,
    r.Q24 AS RSpm9b,
    r.Q25 AS RSpm9c,
    r.Q26 AS RSpm9d,
    r.Q27 AS RSpm9e,
    r.Q28 AS RSpm9f,
    r.Q29 AS RSpm9g,
    r.Q30 AS RSpm9h,
    r.Q31 AS RSpm9i,
    r.Q32 AS RSpm10,
    r.Q33 AS RSpm11a,
    r.Q34 AS RSpm11b,
    r.Q35 AS RSpm11c,
    r.Q36 AS RSpm11d,
    r.SCORE_PHYS_FUNC AS RScorePhys,
    r.SCORE_ROLE_LIMIT_PHYS AS RScoreRoleLmtPhy,
    r.SCORE_ROLE_LIMIT_EMO AS RScoreRoleLmtEmo,
    r.SCORE_ENERGY_FATIGUE AS RScoreEnergy,
    r.SCORE_EMO_WELL_BEING AS RScoreEmo,
    r.SCORE_SOCIAL_FUNC AS RScoreSosial,
    r.SCORE_PAIN AS RScorePain,
    r.SCORE_GENERAL_HEALTH AS RScoreGeneral,
    r.USERCOMMENT AS RKommentar,
    r.FIRST_TIME_CLOSED AS RForstLukket,
    r.FIRST_TIME_CLOSED_BY AS RForstLukketAv, -- This selects the ID. If you need the name, a join to a user table would be required.
    r.STATUS AS RStatus,
    o.OP_DATE AS ROperasjonsDato,
    p.TSSENDT AS REpromUtsendtDato
FROM operation o
INNER JOIN rand36 r ON o.MCEID = r.MCEID -- Changed from JOIN to INNER JOIN for clarity, behavior is the same
LEFT JOIN proms p ON r.MCEID = p.MCEID AND ((p.REGISTRATION_TYPE ='RAND36Y3' AND r.YEAR=3) OR (p.REGISTRATION_TYPE = 'RAND36' AND r.YEAR=1))
WHERE r.FOLLOWUP_TYPE IN (1,2,3) AND r.STATUS=1
  AND r.Q1  IS NOT NULL  AND  r.Q2  IS NOT NULL AND r.Q3  IS NOT NULL AND r.Q4  IS NOT NULL AND r.Q5  IS NOT NULL AND r.Q6  IS NOT NULL AND r.Q7  IS NOT NULL AND r.Q8  IS NOT NULL
  AND r.Q9  IS NOT NULL  AND r.Q10 IS NOT NULL AND r.Q11 IS NOT NULL AND r.Q12 IS NOT NULL AND r.Q13 IS NOT NULL AND r.Q14 IS NOT NULL AND r.Q15 IS NOT NULL AND r.Q16 IS NOT NULL
  AND r.Q17 IS NOT NULL AND r.Q18 IS NOT NULL AND r.Q19 IS NOT NULL AND r.Q20 IS NOT NULL AND r.Q21 IS NOT NULL AND r.Q22 IS NOT NULL AND r.Q23 IS NOT NULL AND r.Q24 IS NOT NULL
  AND r.Q25 IS NOT NULL AND r.Q26 IS NOT NULL AND r.Q27 IS NOT NULL AND r.Q28 IS NOT NULL AND r.Q29 IS NOT NULL AND r.Q30 IS NOT NULL AND r.Q31 IS NOT NULL AND r.Q32 IS NOT NULL
  AND r.Q33 IS NOT NULL AND r.Q34 IS NOT NULL AND r.Q35 IS NOT NULL AND r.Q36 IS NOT NULL
ORDER BY ForlopsID;

-- This file contains views for fairly complex to complex queries or queries that return interesting stuff (more than one report)

-- This view is prepared for v1.1.2.
  
drop view if exists forlopsoversikt;
drop view if exists Dekningsgradsanalyse;

create view forlopsoversikt AS  
  select    
    m.CENTREID AS AvdRESH,
    getFriendlyName(m.CENTREID) AS SykehusNavn,  
	CAST(p.ID AS CHAR(10)) AS PasientID, 
	-- NEXT 6 left empty for now 
	CAST(NULL AS CHAR(4)) AS PostNr, 
	CAST(NULL AS CHAR(50)) AS PostSted, 
	CAST(NULL AS CHAR(50)) AS Kommune, 
	CAST(NULL AS CHAR(4)) AS Kommunenr,
	CAST(NULL AS CHAR(50)) AS Fylke, 
	CAST(NULL AS CHAR(2)) AS Fylkenr,
    p.SSN as KryptertFnr,
    'Kvinne' AS PasientKjonn,    
    '0' AS ErMann,    
    getAgeAtMainDate(m.MCEID) AS PasientAlder,
    p.BIRTH_DATE AS Fodselsdato,
    p.NORWEGIAN as Norsktalende,
    p.MARITAL_STATUS as Sivilstatus,
    p.EDUCATION as Utdanning,
    getListText('PATIENT_DECEASED',p.DECEASED) AS Avdod, 
    p.DECEASED_DATE as AvDodDato,
	CAST(m.MCEID AS CHAR(10)) AS ForlopsID, 
	CAST(m.PARENT_MCE AS CHAR(10)) AS KobletForlopsID, 
    CAST((SELECT 
    CASE o.MCETYPE
      WHEN 1 THEN GREATEST(LEAST(IFNULL(o.STATUS, 0), IFNULL(l.STATUS,0)), 0)
      WHEN 2 THEN GREATEST(LEAST(IFNULL(o.STATUS, 0), IFNULL(h.STATUS,0)), 0)
      WHEN 3 THEN GREATEST(LEAST(LEAST(IFNULL(o.STATUS, 0), IFNULL(l.STATUS,0)), IFNULL(h.STATUS,0)), 0)
      ELSE 0
    END 
    )  AS CHAR(2)) AS BasisRegStatus,  	    
    CAST(if (o.MCETYPE is null, 'Ikke satt', getListText('OPERATION_MCETYPE', o.MCETYPE)) AS CHAR(2)) AS ForlopsType1,
    o.MCETYPE as ForlopsType1Num,       
    CAST(getListText('OPERATION_OPCAT', o.OPCAT) AS CHAR(2)) AS ForlopsType2,       
    CAST(o.OPCAT AS CHAR(2)) AS ForlopsType2Num,  
	o.OP_DATE AS HovedDato,    
    CAST(
    CASE m.REVISION
      WHEN 1 THEN              
        CASE (IFNULL(f0.STATUS, -1))
          WHEN -1 THEN -2 
          WHEN 0 THEN 0
          WHEN 1 THEN 2
          ELSE -2
        END
      WHEN 2 THEN              
        CASE 
          WHEN 
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- All compmpleted
            THEN 2   
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -2), IFNULL(r0.STATUS, -2),IFNULL(tss2.STATUS, -2), IFNULL(f1.STATUS, -2),IFNULL(r1.STATUS, -2))) = -2) -- Non exist 
            THEN -2               
          WHEN 
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1))) = 1)  -- Year 0 part completed
            THEN 1                           
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- Some completed
            THEN 1   
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 0) -- Non completed, some in progress
            THEN 0    
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = -1)  -- All created
            THEN -1              
          ELSE NULL -- unknown    
        END         
      ELSE 2
    END 
    AS CHAR(2)) AS OppflgRegStatus,    
    "0" AS ErOppflg,        
    CASE m.REVISION
      WHEN 1 THEN              
        CASE (IFNULL(f0.STATUS, -1))
          WHEN -1 THEN -2 
          WHEN 0 THEN 0
          WHEN 1 THEN 2
          ELSE -2
        END
      WHEN 2 THEN              
        CASE 
          WHEN 
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- All compmpleted
            THEN "Oppfølging komplett"   
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -2), IFNULL(r0.STATUS, -2),IFNULL(tss2.STATUS, -2), IFNULL(f1.STATUS, -2),IFNULL(r1.STATUS, -2))) = -2) -- Non exist 
            THEN "Ingen opprettet"               
          WHEN 
            ((LEAST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1))) = 1)  -- Year 0 part completed
            THEN "År 0 komplett"                           
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 1) -- Some completed
            THEN "Delvis komplett"   
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = 0) -- Non completed, some in progress
            THEN "Under utfylling"   
          WHEN 
            ((GREATEST(IFNULL(f0.STATUS, -1), IFNULL(r0.STATUS, -1),IFNULL(tss2.STATUS, -1), IFNULL(f1.STATUS, -1),IFNULL(r1.STATUS, -1))) = -1)  -- All created
            THEN "Opprettet"
          ELSE "Ingen opprettet"-- unknown    
        END         
      ELSE "Ingen opprettet"
    END AS OppflgStatus,                       
	CAST(NULL AS CHAR(6)) AS OppflgSekNr 
 from
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID 
    INNER JOIN operation o on m.MCEID = o.MCEID
    LEFT OUTER JOIN user u ON m.CREATEDBY = u.ID
    LEFT OUTER JOIN laparoscopy l ON m.MCEID = l.MCEID
    LEFT OUTER JOIN hysteroscopy h ON m.MCEID = h.MCEID
    LEFT OUTER JOIN followup f0 ON (m.MCEID = f0.MCEID and f0.`YEAR` = 0)
    LEFT OUTER JOIN rand36 r0 ON (m.MCEID = r0.MCEID and r0.`YEAR` = 0)    
    LEFT OUTER JOIN tss2 ON m.MCEID = tss2.MCEID
    LEFT OUTER JOIN followup f1 ON (m.MCEID = f1.MCEID and f1.`YEAR` = 1)
    LEFT OUTER JOIN rand36 r1 ON (m.MCEID = r1.MCEID  and r1.`YEAR` = 1)
  where o.STATUS = 1 AND (ifnull(l.STATUS, 1) = 1 OR ifnull(h.STATUS, 1) = 1); 
  
  
drop view if exists skjemaoversikt;
create view skjemaoversikt AS
select 
  'Operasjon' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS SykehusNavn,
  c.ID AS AvdRESH,
  1 as SkjemaRekkeflg
from
  operation skjema,
  centre c
WHERE skjema.CENTREID = c.ID  
UNION
select 
  'Laparoskopi' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH, 
  3 as SkjemaRekkeflg
from
  laparoscopy skjema,
  centre c
WHERE skjema.CENTREID = c.ID  
UNION
select 
  'Hysteroskopi' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  5 as SkjemaRekkeflg
from
  hysteroscopy skjema,
  centre c
WHERE skjema.CENTREID = c.ID
UNION
select 
  'Oppfølging år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  7 as SkjemaRekkeflg
from
  followup skjema,
  centre c
WHERE skjema.CENTREID = c.ID and skjema.`YEAR` = 0
UNION
select 
  'Rand-36 år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  9 as SkjemaRekkeflg
from
  rand36 skjema,
  centre c
WHERE skjema.CENTREID = c.ID and skjema.`YEAR` = 0
UNION
select 
  'TSS2 år 0' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  11 as SkjemaRekkeflg
from
  followup skjema,
  centre c
WHERE skjema.CENTREID = c.ID
UNION
select 
  'Oppfølging år 1' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  13 as SkjemaRekkeflg
from
  followup skjema,
  centre c
WHERE skjema.CENTREID = c.ID and skjema.`YEAR` = 1
UNION
select 
  'Rand-36 år 1' AS Skjemanavn,
  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus, 
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  15 as SkjemaRekkeflg
from
  rand36 skjema,
  centre c
WHERE skjema.CENTREID = c.ID and skjema.`YEAR` = 1;  
  
    


drop view if exists AlleVar;
CREATE VIEW AlleVar AS
SELECT 
  patient.ID as PasientID,
  patient.REGISTERED_DATE as PasRegDato,
  patient.SSN as PersonNr,
  getListText('START_SSN_TYPE', patient.SSN_TYPE) as PersonNrType,
  patient.BIRTH_DATE as FodselsDato,
  getListText('PATIENT_NATIVE_LANGUAGE', patient.NATIVE_LANGUAGE) as Morsmaal,
  patient.NATIVE_LANGUAGE_OTHER as MorsmaalAnnet,
  getListText('PATIENT_NORWEGIAN', patient.NORWEGIAN) as Norsktalende,
  null AS Postnr,       
  null as Poststed,
  null as Kommune,
  null as Fylke,    
  getListText('PATIENT_EDUCATION', patient.EDUCATION) as Utdanning,
  getListText('PATIENT_DECEASED', patient.DECEASED) as Avdod,
  patient.DECEASED_DATE as AvdodDato, 
  getListText('PATIENT_MARITAL_STATUS', patient.MARITAL_STATUS) as SivilStatus,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
  mce.CALLS_MADE as RingtGanger,
  mce.DELIVERY_DATE as Leveringsdato,
  mce.REVISION as RegisterRevisjon,
  operation.HEIGHT as OpHoyde,
  operation.WEIGHT as OpVekt,
  operation.BMI as OpBMI,
  operation.BMI_CATEGORY as OpBMIKategori,
  operation.PREGNANCIES as OpGraviditeter,
  operation.PARITIES as OpPariteter,
  getListText('OPERATION_EARLIER_VAGINAL', operation.EARLIER_VAGINAL) as OpTidlVagInngrep,
  operation.VAG_REVISIO as OpVagRevisio,
  operation.VAG_HYSTEROSCOPY as OpVagHysteroskopi,
  operation.VAG_CONISATION as OpVagKonisering,
  operation.VAG_DESCENS as OpVagDescens,
  getListText('OPERATION_EARLIER_LAPAROSCOPY',operation.EARLIER_LAPAROSCOPY) as OpTidlLapsko,
  operation.LAPARASCOPY_COUNT as OpTidlLapskoNum, 
  getListText('OPERATION_EARLIER_LAPAROTOMY',operation.EARLIER_LAPAROTOMY) as OpTidlLaparotomi,
  operation.GYNECOLOGICAL_SURGERY_COUNT as OpGynKirurgiNum,
  operation.SECTIO_COUNT as OpSectioNum,
  operation.OTHER_COUNT as OpAndreNum,
  getCheckText(operation.COMORBIDITY_HEART) as OpKomHjerte,
  getCheckText(operation.COMORBIDITY_DIABETES) as OpKomDiabetes,
  getCheckText(operation.COMORBIDITY_HYPERTENSION) as OpKomHypertensjon,
  getCheckText(operation.COMORBIDITY_CANCER) as OpKomKreft,
  getCheckText(operation.COMORBIDITY_CANCER_CERVIX_UTERI) as OpKomKreftCervixUteri,
  getCheckText(operation.COMORBIDITY_CANCER_CAVUM_UTERI) as OpKomKreftCavumUteri,
  getCheckText(operation.COMORBIDITY_CANCER_OVAR) as OpKomKreftOvar,
  getCheckText(operation.COMORBIDITY_CANCER_INTESTINAL) as OpKomKreftTarm,
  getCheckText(operation.COMORBIDITY_CANCER_BLADDER) as OpKomKreftBlare,
  getCheckText(operation.COMORBIDITY_CANCER_MAMMA) as OpKomKreftMamma,
  getCheckText(operation.COMORBIDITY_CANCER_OTHER) as OpKomKreftAnnen,
  getCheckText(operation.COMORBIDITY_OTHER) as OpKomKreftAnnenSpes,
  operation.OP_DATE as OpDato,
  getListText('OPERATION_OPTYPE',operation.OPTYPE) as OpType,  
  operation.MAIN_OPERATION as OpHovedOperasjon,
  getListText('OPERATION_COMPLICATION',operation.COMPLICATION) as OpKomplikasjon,  
  getListText('OPERATION_COMPLICATION_TYPE',operation.COMPLICATION_TYPE) as OpKomplType,  
  getListText('OPERATION_OPCAT',operation.OPCAT) as OpKategori,  
  getCheckText(operation.OPCAT_OUTSIDE_DAYTIME) as OpIVaktTid,
  getListText('OPERATION_MCETYPE',operation.MCETYPE) as OpMetode,  
  getListText('OPERATION_DAY_SURGERY',operation.DAY_SURGERY) as OpDagkirurgi,  
  operation.OP_INDICATION1 as OpIndikasjon1,
  operation.OP_INDICATION2 as OpIndikasjon2,
  operation.OP_INDICATION3 as OpIndikasjon3,
  getListText('OPERATION_ANAESTHETIC',operation.ANAESTHETIC) as OpAnestesi,  
  getListText('OPERATION_ASA',operation.ASA) as OpASA,    
  operation.OPTIME_COUNT as OpTid,
  getListText('OPERATION_ANTIBIOTIC_PROPHYLAXIS',operation.ANTIBIOTIC_PROPHYLAXIS) as OpAntibProfylakse,      
  getListText('OPERATION_SURVIVED',operation.SURVIVED) as OpOverlevd,      
  getStatusText(operation.STATUS) as OpStatus,
  laparoscopy.DIAGNOSIS1 as LapDiagnose1, 
  laparoscopy.DIAGNOSIS1_VERSION as LapDiagnose1Ver, 
  laparoscopy.DIAGNOSIS2 as LapDiagnose2,
  laparoscopy.DIAGNOSIS2_VERSION as LapDiagnose2Ver, 
  laparoscopy.DIAGNOSIS3 as LapDiagnose3,
  laparoscopy.DIAGNOSIS3_VERSION as LapDiagnose3Ver, 
  laparoscopy.PROCEDURE1 as LapProsedyre1,
  laparoscopy.PROCEDURE2 as LapProsedyre2,
  laparoscopy.PROCEDURE3 as LapProsedyre3,
  getListText('LAPAROSCOPY_NON_ENDO_COMBO',laparoscopy.NON_ENDO_COMBO) as LapIkkeEndoKombo,
  getListText('LAPAROSCOPY_ACCESS_METHOD',laparoscopy.ACCESS_METHOD) as LapTilgangsMetode,
  getListText('LAPAROSCOPY_ACCESS',laparoscopy.ACCESS) as LapTilgang,
  getCheckText(laparoscopy.NUM_HJELPEINNSTIKK) as LapNumHjelpeinnstikk,
  getCheckText(laparoscopy.SINGELPORT) as LapSingelPort,
  getCheckText(laparoscopy.BIPOLAR_DIATERMI) as LapBipolarDiatermi,
  getCheckText(laparoscopy.UNIPOLAR_DIATERMI) as LapUnipolarDiatermi,
  getCheckText(laparoscopy.HARMONIC_SCALPELL) as LapHarmonicS,
  getCheckText(laparoscopy.THUNDERBEAT) as LapIntKombo,
  getCheckText(laparoscopy.INTELLIGENTCOAG) as LapIntKoagOgKlipp,
  getCheckText(laparoscopy.MORCELLATOR) as LapMorcellatorUtenPose,
  getCheckText(laparoscopy.MORCELLATOR_WITH_BAG) as LapMorcellatorMedPose,
  getCheckText(laparoscopy.SUTUR) as LapSutur,
  getCheckText(laparoscopy.CLIPS) as LapClips,
  getCheckText(laparoscopy.NETT) as LapNett,
  getCheckText(laparoscopy.ADHERANSEPROFYLAXE) as LapAdherProfylakse,
  getCheckText(laparoscopy.STAPLER_ENDOGIA) as LapStaplerEndogia,
  getCheckText(laparoscopy.PREPARATOPOSE) as LapPreparatopose,
  getCheckText(laparoscopy.ROBOT_KIRURGI) as LapRobotKirurgi,
  getCheckText(laparoscopy.OTHER) as LapAndre,
  getListText('LAPAROSCOPY_MEDICATIONWITHDRAWAL', laparoscopy.MEDICATIONWITHDRAWAL) as LapPrepUttak,
  getListText('LAPAROSCOPY_COMPLICATIONS',laparoscopy.COMPLICATIONS) as LapKomplikasjoner,
  getCheckText(laparoscopy.PORTIOADAPTER) as LapUterusmanipulator,  
  getCheckText(laparoscopy.TILGANG) as LapKompTilgang,
  getCheckText(laparoscopy.HJELPEINSTIKK) as LapHjelpeinnstikk,
  getCheckText(laparoscopy.INTRAABDOMINAL) as LapIntraabdominell,  
  getCheckText(laparoscopy.TEKNISK_UTSTYR) as LapTekniskUtstyr,
  getCheckText(laparoscopy.POSTOPERATIV) as LapPostoperativ,
  getCheckText(laparoscopy.KAR_BLEED) as LapKarBlodning,
  getCheckText(laparoscopy.TARM) as LapTarm,
  getCheckText(laparoscopy.BLAERE) as LapBlare,
  getCheckText(laparoscopy.URETER) as LapUreter,
  getCheckText(laparoscopy.NERV) as LapNerv,
  getListText('LAPAROSCOPY_CONVERTED',laparoscopy.CONVERTED) as LapKonvertert,
  getStatusText(laparoscopy.STATUS) as LapStatus,
  hysteroscopy.DIAGNOSIS1 as HysDiagnose1,
  hysteroscopy.DIAGNOSIS1_VERSION as HysDiagnose1Ver, 
  hysteroscopy.DIAGNOSIS2 as HysDiagnose2,
  hysteroscopy.DIAGNOSIS2_VERSION as HysDiagnose2Ver, 
  hysteroscopy.DIAGNOSIS3 as HysDiagnose3,
  hysteroscopy.DIAGNOSIS3_VERSION as HysDiagnose3Ver, 
  hysteroscopy.PROCEDURE1 as HysProsedyre1,
  hysteroscopy.PROCEDURE2 as HysProsedyre2,
  hysteroscopy.PROCEDURE3 as HysProsedyre3,
  getListText('HYSTEROSCOPY_NON_ENDO_COMBO',hysteroscopy.NON_ENDO_COMBO) as HysIkkeEndoKombo,   
  getListText('HYSTEROSCOPY_DISTENTION_COMBO',hysteroscopy.DISTENTION_COMBO) as HysDistentsjon,   
  getCheckText(hysteroscopy.BIPOLAR_DIATHERMY) as HysBipolarDiatermi,
  getCheckText(hysteroscopy.UNIPLOAR_DIATHERMY) as HysUnipolarDiatermi,
  getCheckText(hysteroscopy.MORCELLATOR) as HysMorcellator,
  getListText('HYSTEROSCOPY_PRETREATMENT_COMBO',hysteroscopy.PRETREATMENT_COMBO) as HysMedForbehCervix,   
  getCheckText(hysteroscopy.FLEXIBLE_LESS_OR_EQUAL_4MM) as HysFleksibelML4mm,
  getCheckText(hysteroscopy.FLEXIBLE_GREATER_4MM) as HysFleksibelS4mm,
  getCheckText(hysteroscopy.RIGID_LESS_OR_EQUAL_4MM) as HysRigidML4mm,
  getCheckText(hysteroscopy.RIGID_GREATER_4MM) as HysRigidS4mm, 
  getListText('HYSTEROSCOPY_COMPLETENESS',hysteroscopy.COMPLETENESS) as HysGjforingsGrad,
  getListText('HYSTEROSCOPY_COMPLICATIONS',hysteroscopy.COMPLICATIONS) as HysKomplikasjoner,  
  getCheckText(hysteroscopy.ACCESS) as HysTilgang,
  getCheckText(hysteroscopy.TECHNICAL) as HysTeknisk,
  getCheckText(hysteroscopy.PERFORATION) as HysPerforasjon,
  getCheckText(hysteroscopy.BLEEDING) as HysBlodning,
  getCheckText(hysteroscopy.FLUID_OVERLOAD) as HysFluidOverload,    
  getListText('HYSTEROSCOPY_CONVERTED',hysteroscopy.CONVERTED) as HysKonvertert,    
  getListText('HYSTEROSCOPY_CONVERTED_LAPAROSCOPY',hysteroscopy.CONVERTED_LAPAROSCOPY) as HysKonvLaparoskopi,    
  getListText('HYSTEROSCOPY_CONVERTED_LAPROTOMY',hysteroscopy.CONVERTED_LAPROTOMY) as HysKonvLaparotomi,    
  getStatusText(hysteroscopy.STATUS) as HysStatus
FROM mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID 
    INNER JOIN operation on mce.MCEID = operation.MCEID
    LEFT OUTER JOIN laparoscopy ON mce.MCEID = laparoscopy.MCEID
    LEFT OUTER JOIN hysteroscopy ON mce.MCEID = hysteroscopy.MCEID   
where 
	operation.STATUS = 1 
	AND (ifnull(laparoscopy.STATUS, 1) = 1 
		OR ifnull(hysteroscopy.STATUS, 1) = 1) 
ORDER BY patient.ID;




drop view if exists allevarnum;
CREATE VIEW allevarnum AS
SELECT 
  patient.ID as PasientID,
  patient.REGISTERED_DATE as PasRegDato,
  patient.SSN as PersonNr,
  patient.SSN_TYPE as PersonNrType,
  patient.BIRTH_DATE as FodselsDato,
  patient.NATIVE_LANGUAGE as Morsmaal,
  patient.NATIVE_LANGUAGE_OTHER as MorsmaalAnnet,
  patient.NORWEGIAN as Norsktalende,
  null AS Postnr,       
  null as PostSted,
  null as Kommune,
  null as Fylke,    
  patient.EDUCATION as Utdanning,
  patient.DECEASED as Avdod,
  patient.DECEASED_DATE as AvdodDato, 
  patient.MARITAL_STATUS as SivilStatus,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
  mce.CALLS_MADE as RingtGanger,
  mce.DELIVERY_DATE as Leveringsdato,
  mce.REVISION as RegisterRevisjon,
  operation.HEIGHT as OpHoyde,
  operation.WEIGHT as OpVekt,
  operation.BMI as OpBMI,
  operation.BMI_CATEGORY as OpBMIKategori,
  operation.PREGNANCIES as OpGraviditeter,
  operation.PARITIES as OpPariteter,
  operation.EARLIER_VAGINAL as OpTidlVagInngrep,
  operation.VAG_REVISIO as OpVagRevisio,
  operation.VAG_HYSTEROSCOPY as OpVagHysteroskopi,
  operation.VAG_CONISATION as OpVagKonisering,
  operation.VAG_DESCENS as OpVagDescens,
  operation.EARLIER_LAPAROSCOPY as OpTidlLapsko,
  operation.LAPARASCOPY_COUNT as OpTidlLapskoNum, 
  operation.EARLIER_LAPAROTOMY as OpTidlLaparotomi,
  operation.GYNECOLOGICAL_SURGERY_COUNT as OpGynKirurgiNum,
  operation.SECTIO_COUNT as OpSectioNum,
  operation.OTHER_COUNT as OpAndreNum,
  operation.COMORBIDITY_HEART as OpKomHjerte,
  operation.COMORBIDITY_DIABETES as OpKomDiabetes,
  operation.COMORBIDITY_HYPERTENSION as OpKomHypertensjon,
  operation.COMORBIDITY_CANCER as OpKomKreft,
  operation.COMORBIDITY_CANCER_CERVIX_UTERI as OpKomKreftCervixUteri,
  operation.COMORBIDITY_CANCER_CAVUM_UTERI as OpKomKreftCavumUteri,
  operation.COMORBIDITY_CANCER_OVAR as OpKomKreftOvar,
  operation.COMORBIDITY_CANCER_INTESTINAL as OpKomKreftTarm,
  operation.COMORBIDITY_CANCER_BLADDER as OpKomKreftBlare,
  operation.COMORBIDITY_CANCER_MAMMA as OpKomKreftMamma,
  operation.COMORBIDITY_CANCER_OTHER as OpKomKreftAnnen,
  operation.COMORBIDITY_OTHER as OpKomKreftAnnenSpes,
  operation.OP_DATE as OpDato,
  operation.OPTYPE as OpType,  
  operation.MAIN_OPERATION as OpHovedOperasjon,
  operation.COMPLICATION as OpKomplikasjon,  
  operation.COMPLICATION_TYPE as OpKomplType,  
  operation.OPCAT as OpKategori,  
  operation.OPCAT_OUTSIDE_DAYTIME as OpIVaktTid,
  operation.MCETYPE as OpMetode,  
  operation.DAY_SURGERY as OpDagkirurgi,  
  operation.OP_INDICATION1 as OpIndikasjon1,
  operation.OP_INDICATION2 as OpIndikasjon2,
  operation.OP_INDICATION3 as OpIndikasjon3,
  operation.ANAESTHETIC as OpAnestesi,  
  operation.ASA as OpASA,    
  operation.OPTIME_COUNT as OpTid,
  operation.ANTIBIOTIC_PROPHYLAXIS as OpAntibProfylakse,      
  operation.SURVIVED as OpOverlevd,      
  operation.STATUS as OpStatus,
  laparoscopy.DIAGNOSIS1 as LapDiagnose1, 
  laparoscopy.DIAGNOSIS1_VERSION as LapDiagnose1Ver, 
  laparoscopy.DIAGNOSIS2 as LapDiagnose2,
  laparoscopy.DIAGNOSIS2_VERSION as LapDiagnose2Ver, 
  laparoscopy.DIAGNOSIS3 as LapDiagnose3,
  laparoscopy.DIAGNOSIS3_VERSION as LapDiagnose3Ver, 
  laparoscopy.PROCEDURE1 as LapProsedyre1,
  laparoscopy.PROCEDURE2 as LapProsedyre2,
  laparoscopy.PROCEDURE3 as LapProsedyre3,
  laparoscopy.NON_ENDO_COMBO as LapIkkeEndoKombo,
  laparoscopy.ACCESS_METHOD as LapTilgangsMetode,
  laparoscopy.ACCESS as LapTilgang,
  laparoscopy.NUM_HJELPEINNSTIKK as LapNumHjelpeinnstikk,
  laparoscopy.SINGELPORT as LapSingelPort,
  laparoscopy.BIPOLAR_DIATERMI as LapBipolarDiatermi,
  laparoscopy.UNIPOLAR_DIATERMI as LapUnipolarDiatermi,
  laparoscopy.HARMONIC_SCALPELL as LapHarmonicS,
  laparoscopy.THUNDERBEAT as LapIntKombo,
  laparoscopy.INTELLIGENTCOAG as LapIntKoagOgKlipp,
  laparoscopy.MORCELLATOR as LapMorcellatorUtenPose,
  laparoscopy.MORCELLATOR_WITH_BAG as LapMorcellatorMedPose,
  laparoscopy.SUTUR as LapSutur,
  laparoscopy.CLIPS as LapClips,
  laparoscopy.NETT as LapNett,
  laparoscopy.ADHERANSEPROFYLAXE as LapAdherProfylakse,
  laparoscopy.STAPLER_ENDOGIA as LapStaplerEndogia,
  laparoscopy.PREPARATOPOSE as LapPreparatopose,
  laparoscopy.ROBOT_KIRURGI as LapRobotKirurgi,
  laparoscopy.OTHER as LapAndre,
  laparoscopy.MEDICATIONWITHDRAWAL as LapPrepUttak,
  laparoscopy.COMPLICATIONS as LapKomplikasjoner,
  laparoscopy.PORTIOADAPTER as LapUterusmanipulator,  
  laparoscopy.TILGANG as LapKompTilgang,
  laparoscopy.HJELPEINSTIKK as LapHjelpeinnstikk,
  laparoscopy.INTRAABDOMINAL as LapIntraabdominell,  
  laparoscopy.TEKNISK_UTSTYR as LapTekniskUtstyr,
  laparoscopy.POSTOPERATIV as LapPostoperativ,
  laparoscopy.KAR_BLEED as LapKarBlodning,
  laparoscopy.TARM as LapTarm,
  laparoscopy.BLAERE as LapBlare,
  laparoscopy.URETER as LapUreter,
  laparoscopy.NERV as LapNerv,
  laparoscopy.CONVERTED as LapKonvertert,   
  laparoscopy.STATUS as LapStatus,
  hysteroscopy.DIAGNOSIS1 as HysDiagnose1,
  hysteroscopy.DIAGNOSIS1_VERSION as HysDiagnose1Ver, 
  hysteroscopy.DIAGNOSIS2 as HysDiagnose2,
  hysteroscopy.DIAGNOSIS2_VERSION as HysDiagnose2Ver, 
  hysteroscopy.DIAGNOSIS3 as HysDiagnose3,
  hysteroscopy.DIAGNOSIS3_VERSION as HysDiagnose3Ver, 
  hysteroscopy.PROCEDURE1 as HysProsedyre1,
  hysteroscopy.PROCEDURE2 as HysProsedyre2,
  hysteroscopy.PROCEDURE3 as HysProsedyre3,
  hysteroscopy.NON_ENDO_COMBO as HysIkkeEndoKombo,   
  hysteroscopy.DISTENTION_COMBO as HysDistentsjon,   
  hysteroscopy.BIPOLAR_DIATHERMY as HysBipolarDiatermi,
  hysteroscopy.UNIPLOAR_DIATHERMY as HysUnipolarDiatermi,
  hysteroscopy.MORCELLATOR as HysMorcellator,
  hysteroscopy.PRETREATMENT_COMBO as HysMedForbehCervix,   
  hysteroscopy.FLEXIBLE_LESS_OR_EQUAL_4MM as HysFleksibelML4mm,
  hysteroscopy.FLEXIBLE_GREATER_4MM as HysFleksibelS4mm,
  hysteroscopy.RIGID_LESS_OR_EQUAL_4MM as HysRigidML4mm,
  hysteroscopy.RIGID_GREATER_4MM as HysRigidS4mm, 
  hysteroscopy.COMPLETENESS as HysGjforingsGrad,
  hysteroscopy.COMPLICATIONS as HysKomplikasjoner,  
  hysteroscopy.ACCESS as HysTilgang,
  hysteroscopy.TECHNICAL as HysTeknisk,
  hysteroscopy.PERFORATION as HysPerforasjon,
  hysteroscopy.BLEEDING as HysBlodning,
  hysteroscopy.FLUID_OVERLOAD as HysFluidOverload,    
  hysteroscopy.CONVERTED as HysKonvertert,    
  hysteroscopy.CONVERTED_LAPAROSCOPY as HysKonvLaparoskopi,    
  hysteroscopy.CONVERTED_LAPROTOMY as HysKonvLaparotomi,    
  hysteroscopy.STATUS as HysStatus
FROM mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID 
    INNER JOIN operation on mce.MCEID = operation.MCEID
    LEFT OUTER JOIN laparoscopy ON mce.MCEID = laparoscopy.MCEID
    LEFT OUTER JOIN hysteroscopy ON mce.MCEID = hysteroscopy.MCEID  
where 
	operation.STATUS = 1 
	AND (ifnull(laparoscopy.STATUS, 1) = 1 
		OR ifnull(hysteroscopy.STATUS, 1) = 1)    
ORDER BY patient.ID;



create view Dekningsgradsanalyse as 
select 
  p.ID AS PasientID,
  m.MCEID AS ForlopsID,
  getFriendlyName(m.CENTREID) AS SykehusNavn,
  c.ID AS AvdRESH,
  o.OP_DATE AS OperasjonsDato,
  o.OP_INDICATION1 AS OpIndikasjon1,
  o.OP_INDICATION2 AS OpIndikasjon2,
  o.OP_INDICATION3 AS OpIndikasjon3,
  l.DIAGNOSIS1 AS LapDiag1,
  l.DIAGNOSIS2 AS LapDiag2,
  l.DIAGNOSIS3 AS LapDiag3,
  l.PROCEDURE1 AS LapPros1,
  l.PROCEDURE2 AS LapPros2,
  l.PROCEDURE3 AS LapPros3,
  h.DIAGNOSIS1 AS HysDiag1,
  h.DIAGNOSIS2 AS HysDiag2,
  h.DIAGNOSIS3 AS HysDiag3,
  h.PROCEDURE1 AS HysPros1,
  h.PROCEDURE2 AS HysPros2,
  h.PROCEDURE3 AS HysPros3
from 
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID 
    INNER JOIN operation o on m.MCEID = o.MCEID
    INNER JOIN centre c ON c.ID = m.CENTREID
    LEFT OUTER JOIN laparoscopy l ON m.MCEID = l.MCEID
    LEFT OUTER JOIN hysteroscopy h ON m.MCEID = h.MCEID;   


drop view if exists followupsnum;
create view followupsnum as 
SELECT 
  patient.ID as PasientID,
  patient.REGISTERED_DATE as PasRegDato,
  patient.SSN as PersonNr,
  patient.SSN_TYPE as PersonNrType,
  patient.BIRTH_DATE as FodselsDato,
  patient.NATIVE_LANGUAGE as Morsmal,
  patient.NATIVE_LANGUAGE_OTHER as MorsmalAnnet,
  patient.NORWEGIAN as Norsktalende,
  null AS Postnr,       
  null as PostSted,
  null as Kommune,
  null as Fylke,    
  patient.EDUCATION as Utdanning,
  patient.DECEASED as Avdod,
  patient.DECEASED_DATE as AvdodDato, 
  patient.MARITAL_STATUS as SivilStatus,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
  mce.CALLS_MADE as RingtGanger,
  mce.DELIVERY_DATE as Leveringsdato,
  mce.REVISION as RegisterRevisjon,
  followupY0.FOLLOWUP_TYPE as Opf0metode,  
  followupY0.COMPLICATIONS_EXIST as Opf0Komplikasjoner,  
  followupY0.COMPLICATIONTYPE_REOP as Opf0Reoperasjon,  
  followupY0.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROSCOPY as Opf0ReopLaparoskopi,
  followupY0.COMPLICATIONTYPE_REOP_SPECIFY_HYSTEROSCOPY as Opf0ReopHysteroskopi,
  followupY0.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROTOMY as Opf0ReopLaparotomi,
  followupY0.COMPLICATIONTYPE_INFECTION as Opf0KomplInfeksjon, 
  followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_SURG_WOUND as Opf0InfOpSaar,
  followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_INTRAABD as Opf0InfIntraabdominal,
  followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_ENDO_SALPIN as Opf0InfEndometritt,
  followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_UVI as Opf0InfUVI,
  followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_OTHER as Opf0InfAnnen,
  followupY0.COMPLICATIONTYPE_AFTERBLEED as Opf0KomplBlodning, 
  followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_ABDOMWALL as Opf0BlodningAbdom,
  followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_VAGINAL as Opf0BlodningVaginal,
  followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_INTRAABD as Opf0BlodningIntraabdominal,
  followupY0.COMPLICATIONTYPE_ORGAN as Opf0KomplOrgan, 
  followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_INTESTINAL as Opf0OrganTarm,
  followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_BLADDER as Opf0OrganBlaere,
  followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_URETER as Opf0OrganUreter,
  followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_KAR as Opf0OrganKar,
  followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_OTHER as Opf0OrganAnnen,
  followupY0.COMPLICATIONTYPE_EQUIPMENT as Opf0KomplUtstyr,   
  followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_NET as Opf0UtstyrNett,
  followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_INSTRUMENTS as Opf0UtstyrInstrumenter,
  followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_SUTURE as Opf0UtstyrSutur,
  followupY0.SERIOUSNESS as Opf0AlvorlighetsGrad,   
  followupY0.STATUS as Opf0Status,
  
  rand36Y0.FOLLOWUP_TYPE as RY0metode,
  rand36Y0.YEAR as RY0Aar,
  rand36Y0.Q1 as RY0Spm1,
  rand36Y0.Q2 as RY0Spm2,
  rand36Y0.Q3 as RY0Spm3a,
  rand36Y0.Q4 as RY0Spm3b,
  rand36Y0.Q5 as RY0Spm3c,
  rand36Y0.Q6 as RY0Spm3d,
  rand36Y0.Q7 as RY0Spm3e,
  rand36Y0.Q8 as RY0Spm3f,
  rand36Y0.Q9 as RY0Spm3g,
  rand36Y0.Q10 as RY0Spm3h,
  rand36Y0.Q11 as RY0Spm3i,
  rand36Y0.Q12 as RY0Spm3j,
  rand36Y0.Q13 as RY0Spm4a,
  rand36Y0.Q14 as RY0Spm4b,
  rand36Y0.Q15 as RY0Spm4c,
  rand36Y0.Q16 as RY0Spm4d,
  rand36Y0.Q17 as RY0Spm5a,
  rand36Y0.Q18 as RY0Spm5b,
  rand36Y0.Q19 as RY0Spm5c,
  rand36Y0.Q20 as RY0Spm6,
  rand36Y0.Q21 as RY0Spm7,
  rand36Y0.Q22 as RY0Spm8,
  rand36Y0.Q23 as RY0Spm9a,
  rand36Y0.Q23 as RY0Spm9b,
  rand36Y0.Q25 as RY0Spm9c,
  rand36Y0.Q26 as RY0Spm9d,
  rand36Y0.Q27 as RY0Spm9e,
  rand36Y0.Q28 as RY0Spm9f,
  rand36Y0.Q29 as RY0Spm9g,
  rand36Y0.Q30 as RY0Spm9h,
  rand36Y0.Q31 as RY0Spm9i,
  rand36Y0.Q32 as RY0Spm10,
  rand36Y0.Q33 as RY0Spm11a,
  rand36Y0.Q34 as RY0Spm11b,
  rand36Y0.Q35 as RY0Spm11c,
  rand36Y0.Q36 as RY0Spm11d,  
  rand36Y0.STATUS as RY0Status,
  
  tss2.FOLLOWUP_TYPE as tss2Type,
  tss2.Q1 AS tss2Moett,
  tss2.Q2 as tss2Behandling,
  tss2.Q3 as tss2Lytte,
  tss2.Q4 as tss2Behandlere,
  tss2.Q5 as tss2Enighet,
  tss2.Q6 as tss2Generelt,  
  tss2.STATUS as tss2Status,
  
  followupY1.FOLLOWUP_TYPE as Opf1metode,  
  followupY1.COMPLICATIONS_EXIST as Opf1Komplikasjoner,  
  followupY1.COMPLICATIONTYPE_REOP as Opf1Reoperasjon,  
  followupY1.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROSCOPY as Opf1ReopLaparoskopi,
  followupY1.COMPLICATIONTYPE_REOP_SPECIFY_HYSTEROSCOPY as Opf1ReopHysteroskopi,
  followupY1.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROTOMY as Opf1ReopLaparotomi,
  followupY1.COMPLICATIONTYPE_INFECTION as Opf1KomplInfeksjon, 
  followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_SURG_WOUND as Opf1InfOpSaar,
  followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_INTRAABD as Opf1InfIntraabdominal,
  followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_ENDO_SALPIN as Opf1InfEndometritt,
  followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_UVI as Opf1InfUVI,
  followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_OTHER as Opf1InfAnnen,
  followupY1.COMPLICATIONTYPE_AFTERBLEED as Opf1KomplBlodning, 
  followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_ABDOMWALL as Opf1BlodningAbdom,
  followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_VAGINAL as Opf1BlodningVaginal,
  followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_INTRAABD as Opf1BlodningIntraabdominal,
  followupY1.COMPLICATIONTYPE_ORGAN as Opf1KomplOrgan, 
  followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_INTESTINAL as Opf1OrganTarm,
  followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_BLADDER as Opf1OrganBlaere,
  followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_URETER as Opf1OrganUreter,
  followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_KAR as Opf1OrganKar,
  followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_OTHER as Opf1OrganAnnen,
  followupY1.COMPLICATIONTYPE_EQUIPMENT as Opf1KomplUtstyr,   
  followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_NET as Opf1UtstyrNett,
  followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_INSTRUMENTS as Opf1UtstyrInstrumenter,
  followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_SUTURE as Opf1UtstyrSutur,
  followupY1.SERIOUSNESS as Opf1AlvorlighetsGrad,   
  followupY1.STATUS as Opf1Status, 
  
  rand36Y1.FOLLOWUP_TYPE as RY1metode,
  rand36Y1.YEAR as RY1Aar,
  rand36Y1.Q1 as RY1Spm1,
  rand36Y1.Q2 as RY1Spm2,
  rand36Y1.Q3 as RY1Spm3a,
  rand36Y1.Q4 as RY1Spm3b,
  rand36Y1.Q5 as RY1Spm3c,
  rand36Y1.Q6 as RY1Spm3d,
  rand36Y1.Q7 as RY1Spm3e,
  rand36Y1.Q8 as RY1Spm3f,
  rand36Y1.Q9 as RY1Spm3g,
  rand36Y1.Q10 as RY1Spm3h,
  rand36Y1.Q11 as RY1Spm3i,
  rand36Y1.Q12 as RY1Spm3j,
  rand36Y1.Q13 as RY1Spm4a,
  rand36Y1.Q14 as RY1Spm4b,
  rand36Y1.Q15 as RY1Spm4c,
  rand36Y1.Q16 as RY1Spm4d,
  rand36Y1.Q17 as RY1Spm5a,
  rand36Y1.Q18 as RY1Spm5b,
  rand36Y1.Q19 as RY1Spm5c,
  rand36Y1.Q20 as RY1Spm6,
  rand36Y1.Q21 as RY1Spm7,
  rand36Y1.Q22 as RY1Spm8,
  rand36Y1.Q23 as RY1Spm9a,
  rand36Y1.Q23 as RY1Spm9b,
  rand36Y1.Q25 as RY1Spm9c,
  rand36Y1.Q26 as RY1Spm9d,
  rand36Y1.Q27 as RY1Spm9e,
  rand36Y1.Q28 as RY1Spm9f,
  rand36Y1.Q29 as RY1Spm9g,
  rand36Y1.Q30 as RY1Spm9h,
  rand36Y1.Q31 as RY1Spm9i,
  rand36Y1.Q32 as RY1Spm10,
  rand36Y1.Q33 as RY1Spm11a,
  rand36Y1.Q34 as RY1Spm11b,
  rand36Y1.Q35 as RY1Spm11c,
  rand36Y1.Q36 as RY1Spm11d,
  rand36Y1.STATUS as RY1Status
FROM mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID 
    LEFT OUTER JOIN followup followupY0 ON (mce.MCEID = followupY0.MCEID and followupY0.`YEAR` = 0)
    LEFT OUTER JOIN tss2 ON mce.MCEID = tss2.MCEID
    LEFT OUTER JOIN rand36 rand36Y0 ON (mce.MCEID = rand36Y0.MCEID and rand36Y0.`YEAR` = 0)
    LEFT OUTER JOIN followup followupY1 ON (mce.MCEID = followupY1.MCEID and followupY1.`YEAR` = 1)
    LEFT OUTER JOIN rand36 rand36Y1 ON (mce.MCEID = rand36Y1.MCEID and rand36Y1.`YEAR` = 1)
where
	followupY0.STATUS = 1 
	AND ifnull(rand36Y0.STATUS, 1) = 1 
	AND ifnull(tss2.STATUS, 1) = 1 
	AND ifnull(followupY1.STATUS, 1) = 1 
	AND ifnull(rand36Y1.STATUS, 1) = 1   
ORDER BY patient.ID;

drop view if exists Followups;
create view Followups as
SELECT 
  patient.ID as PasientID,
  patient.REGISTERED_DATE as PasRegDato,
  patient.SSN as PersonNr,
  getListText('START_SSN_TYPE', patient.SSN_TYPE) PersonNrType,
  patient.BIRTH_DATE as FodselsDato,
  getListText('PATIENT_NATIVE_LANGUAGE', patient.NATIVE_LANGUAGE) as Morsmal,
  patient.NATIVE_LANGUAGE_OTHER as MorsmalAnnet,
  patient.NORWEGIAN as Norsktalende,
  null AS Postnr,       
  null as PostSted,
  null as Kommune,
  null as Fylke,    
  getListText('PATIENT_EDUCATION', patient.EDUCATION) as Utdanning,
  getListText('PATIENT_DECEASED', patient.DECEASED),
  patient.DECEASED_DATE as AvdodDato, 
  getListText('PATIENT_MARITAL_STATUS', patient.MARITAL_STATUS) as SivilStatus,
  mce.MCEID as ForlopsID,
  mce.CENTREID AS AvdRESH,
  mce.CALLS_MADE as RingtGanger,
  mce.DELIVERY_DATE as Leveringsdato,
  mce.REVISION as RegisterRevisjon,
  getListText('FOLLOWUP_FOLLOWUP_TYPE',followupY0.FOLLOWUP_TYPE) as Opf0metode,  
  getListText('FOLLOWUP_COMPLICATIONS_EXIST',followupY0.COMPLICATIONS_EXIST) as Opf0Komplikasjoner,  
  getListText('FOLLOWUP_COMPLICATIONTYPE_REOP',followupY0.COMPLICATIONTYPE_REOP) as Opf0Reoperasjon,  
  getCheckText(followupY0.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROSCOPY) as Opf0ReopLaparoskopi,
  getCheckText(followupY0.COMPLICATIONTYPE_REOP_SPECIFY_HYSTEROSCOPY) as Opf0ReopHysteroskopi,
  getCheckText(followupY0.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROTOMY) as Opf0ReopLaparotomi,
  getListText('FOLLOWUP_COMPLICATIONTYPE_INFECTION',followupY0.COMPLICATIONTYPE_INFECTION) as Opf0KomplInfeksjon, 
  getCheckText(followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_SURG_WOUND) as Opf0InfOpSaar,
  getCheckText(followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_INTRAABD) as Opf0InfIntraabdominal,
  getCheckText(followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_ENDO_SALPIN) as Opf0InfEndometritt,
  getCheckText(followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_UVI) as Opf0InfUVI,
  getCheckText(followupY0.COMPLICATIONTYPE_INFECTION_SPECIFY_OTHER) as Opf0InfAnnen,
  getListText('FOLLOWUP_COMPLICATIONTYPE_AFTERBLEED', followupY0.COMPLICATIONTYPE_AFTERBLEED) as Opf0KomplBlodning, 
  getCheckText(followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_ABDOMWALL) as Opf0BlodningAbdom,
  getCheckText(followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_VAGINAL) as Opf0BlodningVaginal,
  getCheckText(followupY0.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_INTRAABD) as Opf0BlodningIntraabdominal,
  getListText('FOLLOWUP_COMPLICATIONTYPE_ORGAN',followupY0.COMPLICATIONTYPE_ORGAN) as Opf0KomplOrgan, 
  getCheckText(followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_INTESTINAL) as Opf0OrganTarm,
  getCheckText(followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_BLADDER) as Opf0OrganBlaere,
  getCheckText(followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_URETER) as Opf0OrganUreter,
  getCheckText(followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_KAR) as Opf0OrganKar,
  getCheckText(followupY0.COMPLICATIONTYPE_ORGAN_SPECIFY_OTHER) as Opf0OrganAnnen,
  getListText('FOLLOWUP_COMPLICATIONTYPE_EQUIPMENT',followupY0.COMPLICATIONTYPE_EQUIPMENT) as Opf0KomplUtstyr,   
  getCheckText(followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_NET) as Opf0UtstyrNett,
  getCheckText(followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_INSTRUMENTS) as Opf0UtstyrInstrumenter,
  getCheckText(followupY0.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_SUTURE) as Opf0UtstyrSutur,
  getListText('FOLLOWUP_SERIOUSNESS',followupY0.SERIOUSNESS) as Opf0Alvorligh5etsGrad,   
  getStatusText(followupY0.STATUS) as Opf0Status,
  
  getListText('RAND36_FOLLOWUP_TYPE',rand36Y0.FOLLOWUP_TYPE) as RY0metode,
  rand36Y0.YEAR as RY0Aar,
  getListText('RAND36_Q1',rand36Y0.Q1) as RY0Spm1,  
  getListText('RAND36_Q2',rand36Y0.Q2) as RY0Spm2,
  getListText('RAND36_Q3',rand36Y0.Q3) as RY0Spm3a,
  getListText('RAND36_Q4',rand36Y0.Q4) as RY0Spm3b,
  getListText('RAND36_Q5',rand36Y0.Q5) as RY0Spm3c,
  getListText('RAND36_Q6',rand36Y0.Q6) as RY0Spm3d,
  getListText('RAND36_Q7',rand36Y0.Q7) as RY0Spm3e,
  getListText('RAND36_Q8',rand36Y0.Q8) as RY0Spm3f,
  getListText('RAND36_Q9',rand36Y0.Q9) as RY0Spm3g,
  getListText('RAND36_Q10',rand36Y0.Q10) as RY0Spm3h,
  getListText('RAND36_Q11',rand36Y0.Q11) as RY0Spm3i,
  getListText('RAND36_Q12',rand36Y0.Q12) as RY0Spm3j,
  getListText('RAND36_Q13',rand36Y0.Q13) as RY0Spm4a,
  getListText('RAND36_Q14',rand36Y0.Q14) as RY0Spm4b,
  getListText('RAND36_Q15',rand36Y0.Q15) as RY0Spm4c,
  getListText('RAND36_Q16',rand36Y0.Q16) as RY0Spm4d,
  getListText('RAND36_Q17',rand36Y0.Q17) as RY0Spm5a,
  getListText('RAND36_Q18',rand36Y0.Q18) as RY0Spm5b,
  getListText('RAND36_Q19',rand36Y0.Q19) as RY0Spm5c,
  getListText('RAND36_Q20',rand36Y0.Q20) as RY0Spm6,
  getListText('RAND36_Q21',rand36Y0.Q21) as RY0Spm7,
  getListText('RAND36_Q22',rand36Y0.Q22) as RY0Spm8,
  getListText('RAND36_Q23',rand36Y0.Q23) as RY0Spm9a,
  getListText('RAND36_Q24',rand36Y0.Q24) as RY0Spm9b,
  getListText('RAND36_Q25',rand36Y0.Q25) as RY0Spm9c,
  getListText('RAND36_Q26',rand36Y0.Q26) as RY0Spm9d,
  getListText('RAND36_Q27',rand36Y0.Q27) as RY0Spm9e,
  getListText('RAND36_Q28',rand36Y0.Q28) as RY0Spm9f,
  getListText('RAND36_Q29',rand36Y0.Q29) as RY0Spm9g,
  getListText('RAND36_Q30',rand36Y0.Q30) as RY0Spm9h,
  getListText('RAND36_Q31',rand36Y0.Q31) as RY0Spm9i,
  getListText('RAND36_Q32',rand36Y0.Q32) as RY0Spm10,
  getListText('RAND36_Q33',rand36Y0.Q33) as RY0Spm11a,
  getListText('RAND36_Q34',rand36Y0.Q34) as RY0Spm11b,
  getListText('RAND36_Q35',rand36Y0.Q35) as RY0Spm11c,
  getListText('RAND36_Q36',rand36Y0.Q36) as RY0Spm11d,  
  getStatusText(rand36Y0.STATUS) as RY0Status,
 
  getListText('TSS2_FOLLOWUP_TYPE',tss2.FOLLOWUP_TYPE) as tss2Type,
  getListText('TSS2_Q1',tss2.Q1) as tss2Moett,
  getListText('TSS2_Q2',tss2.Q2) as tss2Behandling,
  getListText('TSS2_Q3',tss2.Q3) as tss2Lytte,
  getListText('TSS2_Q4',tss2.Q4) as tss2Behandlere,
  getListText('TSS2_Q5',tss2.Q5) as tss2Enighet,
  getListText('TSS2_Q6',tss2.Q6) as tss2Generelt,
  getStatusText(tss2.STATUS) as tss2Status,
  
  getListText('FOLLOWUP_FOLLOWUP_TYPE',followupY1.FOLLOWUP_TYPE) as Opf1metode,  
  getListText('FOLLOWUP_COMPLICATIONS_EXIST',followupY1.COMPLICATIONS_EXIST) as Opf1Komplikasjoner,  
  getListText('FOLLOWUP_COMPLICATIONTYPE_REOP',followupY1.COMPLICATIONTYPE_REOP) as Opf1Reoperasjon,  
  getCheckText(followupY1.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROSCOPY) as Opf1ReopLaparoskopi,
  getCheckText(followupY1.COMPLICATIONTYPE_REOP_SPECIFY_HYSTEROSCOPY) as Opf1ReopHysteroskopi,
  getCheckText(followupY1.COMPLICATIONTYPE_REOP_SPECIFY_LAPAROTOMY) as Opf1ReopLaparotomi,
  getListText('FOLLOWUP_COMPLICATIONTYPE_INFECTION',followupY1.COMPLICATIONTYPE_INFECTION) as Opf1KomplInfeksjon, 
  getCheckText(followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_SURG_WOUND) as Opf1InfOpSaar,
  getCheckText(followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_INTRAABD) as Opf1InfIntraabdominal,
  getCheckText(followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_ENDO_SALPIN) as Opf1InfEndometritt,
  getCheckText(followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_UVI) as Opf1InfUVI,
  getCheckText(followupY1.COMPLICATIONTYPE_INFECTION_SPECIFY_OTHER) as Opf1InfAnnen,
  getListText('FOLLOWUP_COMPLICATIONTYPE_AFTERBLEED',followupY1.COMPLICATIONTYPE_AFTERBLEED) as Opf1KomplBlodning, 
  getCheckText(followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_ABDOMWALL) as Opf1BlodningAbdom,
  getCheckText(followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_VAGINAL) as Opf1BlodningVaginal,
  getCheckText(followupY1.COMPLICATIONTYPE_AFTERBLEED_SPECIFY_INTRAABD) as Opf1BlodningIntraabdominal,
  getListText('FOLLOWUP_COMPLICATIONTYPE_ORGAN',followupY1.COMPLICATIONTYPE_ORGAN) as Opf1KomplOrgan, 
  getCheckText(followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_INTESTINAL) as Opf1OrganTarm,
  getCheckText(followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_BLADDER) as Opf1OrganBlaere,
  getCheckText(followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_URETER) as Opf1OrganUreter,
  getCheckText(followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_KAR) as Opf1OrganKar,
  getCheckText(followupY1.COMPLICATIONTYPE_ORGAN_SPECIFY_OTHER) as Opf1OrganAnnen,
  getListText('FOLLOWUP_COMPLICATIONTYPE_EQUIPMENT',followupY1.COMPLICATIONTYPE_EQUIPMENT) as Opf1KomplUtstyr,   
  getCheckText(followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_NET) as Opf1UtstyrNett,
  getCheckText(followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_INSTRUMENTS) as Opf1UtstyrInstrumenter,
  getCheckText(followupY1.COMPLICATIONTYPE_EQUIPMENT_SPECIFY_SUTURE) as Opf1UtstyrSutur,
  getListText('FOLLOWUP_SERIOUSNESS',followupY1.SERIOUSNESS) as Opf1AlvorlighetsGrad,   
  getStatusText(followupY1.STATUS) as Opf1Status,
  
  getListText('RAND36_FOLLOWUP_TYPE',rand36Y1.FOLLOWUP_TYPE) as RY1metode,
  rand36Y1.YEAR as RY1Aar,
  getListText('RAND36_Q1',rand36Y1.Q1) as RY1Spm1,  
  getListText('RAND36_Q2',rand36Y1.Q2) as RY1Spm2,
  getListText('RAND36_Q3',rand36Y1.Q3) as RY1Spm3a,
  getListText('RAND36_Q4',rand36Y1.Q4) as RY1Spm3b,
  getListText('RAND36_Q5',rand36Y1.Q5) as RY1Spm3c,
  getListText('RAND36_Q6',rand36Y1.Q6) as RY1Spm3d,
  getListText('RAND36_Q7',rand36Y1.Q7) as RY1Spm3e,
  getListText('RAND36_Q8',rand36Y1.Q8) as RY1Spm3f,
  getListText('RAND36_Q9',rand36Y1.Q9) as RY1Spm3g,
  getListText('RAND36_Q10',rand36Y1.Q10) as RY1Spm3h,
  getListText('RAND36_Q11',rand36Y1.Q11) as RY1Spm3i,
  getListText('RAND36_Q12',rand36Y1.Q12) as RY1Spm3j,
  getListText('RAND36_Q13',rand36Y1.Q13) as RY1Spm4a,
  getListText('RAND36_Q14',rand36Y1.Q14) as RY1Spm4b,
  getListText('RAND36_Q15',rand36Y1.Q15) as RY1Spm4c,
  getListText('RAND36_Q16',rand36Y1.Q16) as RY1Spm4d,
  getListText('RAND36_Q17',rand36Y1.Q17) as RY1Spm5a,
  getListText('RAND36_Q18',rand36Y1.Q18) as RY1Spm5b,
  getListText('RAND36_Q19',rand36Y1.Q19) as RY1Spm5c,
  getListText('RAND36_Q20',rand36Y1.Q20) as RY1Spm6,
  getListText('RAND36_Q21',rand36Y1.Q21) as RY1Spm7,
  getListText('RAND36_Q22',rand36Y1.Q22) as RY1Spm8,
  getListText('RAND36_Q23',rand36Y1.Q23) as RY1Spm9a,
  getListText('RAND36_Q24',rand36Y1.Q24) as RY1Spm9b,
  getListText('RAND36_Q25',rand36Y1.Q25) as RY1Spm9c,
  getListText('RAND36_Q26',rand36Y1.Q26) as RY1Spm9d,
  getListText('RAND36_Q27',rand36Y1.Q27) as RY1Spm9e,
  getListText('RAND36_Q28',rand36Y1.Q28) as RY1Spm9f,
  getListText('RAND36_Q29',rand36Y1.Q29) as RY1Spm9g,
  getListText('RAND36_Q30',rand36Y1.Q30) as RY1Spm9h,
  getListText('RAND36_Q31',rand36Y1.Q31) as RY1Spm9i,
  getListText('RAND36_Q32',rand36Y1.Q32) as RY1Spm10,
  getListText('RAND36_Q33',rand36Y1.Q33) as RY1Spm11a,
  getListText('RAND36_Q34',rand36Y1.Q34) as RY1Spm11b,
  getListText('RAND36_Q35',rand36Y1.Q35) as RY1Spm11c,
  getListText('RAND36_Q36',rand36Y1.Q36) as RY1Spm11d,  
  getStatusText(rand36Y1.STATUS) as RY1Status
  
FROM mce INNER JOIN patient ON mce.PATIENT_ID = patient.ID
    LEFT OUTER JOIN followup followupY0 ON (mce.MCEID = followupY0.MCEID and followupY0.`YEAR` = 0)
    LEFT OUTER JOIN tss2 ON mce.MCEID = tss2.MCEID
    LEFT OUTER JOIN rand36 rand36Y0 ON (mce.MCEID = rand36Y0.MCEID and rand36Y0.`YEAR` = 0)
    LEFT OUTER JOIN followup followupY1 ON (mce.MCEID = followupY1.MCEID and followupY1.`YEAR` = 1)
    LEFT OUTER JOIN rand36 rand36Y1 ON (mce.MCEID = rand36Y1.MCEID and rand36Y1.`YEAR` = 1)
where
	followupY0.STATUS = 1 
	AND ifnull(rand36Y0.STATUS, 1) = 1 
	AND ifnull(tss2.STATUS, 1) = 1 
	AND ifnull(followupY1.STATUS, 1) = 1 
	AND ifnull(rand36Y1.STATUS, 1) = 1     
ORDER BY patient.ID;


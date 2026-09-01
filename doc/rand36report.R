#' Lager rand36report
#'
#' @return Returnerer rand36report
#' @export
#'

rand36report <- function() {
  query <- "
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
"

  rand36report <- rapbase::loadRegData(registryName = 'data', query=query, dbType = "mysql")

}

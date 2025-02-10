allevarnum:
  TSSvar
[1] "Tss2Behandlere"    "Tss2Behandling"    "Tss2BesvarteProm"  "Tss2Enighet"       "Tss2ForstLukket"   "Tss2ForstLukketAv"
[7] "Tss2Generelt"      "Tss2Lytte"         "Tss2Mott"          "Tss2Score"         "Tss2ScoreAVG"      "Tss2Status"
[13] "Tss2Type"

R1var
[1] "R1BesvarteProm"    "R1ForstLukket"     "R1ForstLukketAv"   "R1ScoreEmo"        "R1ScoreEnergy"     "R1ScoreGeneral"
[7] "R1ScorePain"       "R1ScorePhys"       "R1ScoreRoleLmtEmo" "R1ScoreRoleLmtPhy" "R1ScoreSosial"

R0var
[1] "R0ForstLukket"     "R0ForstLukketAv"   "R0Metode"          "R0ScoreEmo"        "R0ScoreEnergy"     "R0ScoreGeneral"
[7] "R0ScorePain"       "R0ScorePhys"       "R0ScoreRoleLmtEmo" "R0ScoreRoleLmtPhy" "R0ScoreSosial"     "R0Spm1"
[13] "R0Spm10"           "R0Spm11a"          "R0Spm11b"          "R0Spm11c"          "R0Spm11d"          "R0Spm2"
[19] "R0Spm3a"           "R0Spm3b"           "R0Spm3c"           "R0Spm3d"           "R0Spm3e"           "R0Spm3f"
[25] "R0Spm3g"           "R0Spm3h"           "R0Spm3i"           "R0Spm3j"           "R0Spm4a"           "R0Spm4b"
[31] "R0Spm4c"           "R0Spm4d"           "R0Spm5a"           "R0Spm5b"           "R0Spm5c"           "R0Spm6"
[37] "R0Spm7"            "R0Spm8"            "R0Spm9a"           "R0Spm9b"           "R0Spm9c"           "R0Spm9d"
[43] "R0Spm9e"           "R0Spm9f"           "R0Spm9g"           "R0Spm9h"           "R0Spm9i"           "R0Status"
[49] "R0Aar"
Emil
Sara
Tobis

sort(names(PROM))
"RBesvarteProm"     "RForstLukket"      "RForstLukketAv"    "RScoreEmo"
[7] "RScoreEnergy"      "RScoreGeneral"     "RScorePain"        "RScorePhys"        "RScoreRoleLmtEmo"  "RScoreRoleLmtPhy"
[13] "RScoreSosial"      "RSpm1"             "RSpm10"            "RSpm11a"           "RSpm11b"           "RSpm11c"
[19] "RSpm11d"           "RSpm2"             "RSpm3a"            "RSpm3b"            "RSpm3c"            "RSpm3d"
[25] "RSpm3e"            "RSpm3f"            "RSpm3g"            "RSpm3h"            "RSpm3i"            "RSpm3j"
[31] "RSpm4a"            "RSpm4b"            "RSpm4c"            "RSpm4d"            "RSpm5a"            "RSpm5b"
[37] "RSpm5c"            "RSpm6"             "RSpm7"             "RSpm8"             "RSpm9a"            "RSpm9b"
[43] "RSpm9c"            "RSpm9d"            "RSpm9e"            "RSpm9f"            "RSpm9g"            "RSpm9h"
[49] "RSpm9i"            "RStatus"
[1] "ForlopsID"         "Metode"            "SendtDato"         "Tss2Behandlere"    "Tss2Behandling"    "Tss2BesvarteProm"
[55] "Tss2Enighet"       "Tss2ForstLukket"   "Tss2ForstLukketAv" "Tss2Generelt"      "Tss2Lytte"         "Tss2Mott"
[61] "Tss2Score"         "Tss2ScoreAVG"      "Tss2Status"        "Tss2Type"          "Aar"


query <- 'select * FROM allevarnum'
allevarnum <- rapbase::loadRegData(registryName = "nger", query, dbType = "mysql")
R0var <- grep(pattern='R0', x=sort(names(allevarnum)), value = TRUE, fixed = TRUE)
R1var <- grep(pattern='R1', x=sort(names(allevarnum)), value = TRUE, fixed = TRUE)
TSS2var <- grep(pattern='Tss2', x=sort(names(allevarnum)), value = TRUE, fixed = TRUE)
allevarnum <- allevarnum[, -which(names(allevarnum) %in% c(R0var, R1var, TSS2var))]


queryPROMtab <- 'select * FROM promprem'
PROM <-  rapbase::loadRegData(registryName = "nger", queryPROMtab, dbType = "mysql")

TSSvar <- c(TSS2var, "SendtDato", "Metode", 'ForlopsID')
PROM_TSS <- PROM[ ,TSSvar] %>%
  dplyr::filter(Tss2BesvarteProm %in% 0:1)

Rvar <- grep(pattern='R', x=sort(names(PROM)), value = TRUE, fixed = TRUE)
PROM_RAND <- PROM[ ,c(Rvar,'Aar', 'ForlopsID')] %>%
  dplyr::filter(!is.na(Aar))  #RBesvarteProm %in% 0:1 gir bare noen få
Rvar_uR <- gsub('R', '', Rvar)
names(PROM_RAND)[which(names(PROM_RAND) %in% Rvar)] <- Rvar_uR

PROM_RANDw <- PROM_RAND %>%
  tidyr::pivot_wider(
  id_cols = 'ForlopsID',
  id_expand = FALSE,
  names_from ='Aar',  #c('Aar', Rvar),
  #names_prefix = "A",
  names_sep = "",
  names_glue = "{'R'}{Aar}{.value}",
  names_sort = FALSE,
  names_vary = "fastest",
  names_repair = "check_unique",
  values_from = all_of(Rvar_uR)
)

RegDataR <- dplyr::left_join(allevarnum, PROM_RANDw, by="ForlopsID")
RegDataPROM <- dplyr::left_join(RegDataR, PROM_TSS, by="ForlopsID")


forl_0 <- PROM$ForlopsID[PROM$Aar==0 & !is.na(PROM$RScoreGeneral)]
forl_1 <- PROM$ForlopsID[PROM$Aar==1 & !is.na(PROM$RScoreGeneral)]
forl_3 <- PROM$ForlopsID[PROM$Aar==3 & !is.na(PROM$RScoreGeneral)]

test13 <- intersect(forl_3, forl_1)
test1 <- intersect(forl_0, forl_1)
test013 <- intersect(test1, forl_3)
test <- PROM[PROM$ForlopsID %in% test013, ]



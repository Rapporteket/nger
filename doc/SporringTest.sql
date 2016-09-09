SELECT 
    OpTidlLapsko,
    OpTidlVagInngrep,
    OpType
  ,Opf0AlvorlighetsGrad
  ,ForlopsOversikt.AvdRESH
	,ForlopsOversikt.BasisRegStatus
  ,ForlopsOversikt.Fodselsdato
	,ForlopsOversikt.HovedDato
  ,ForlopsOversikt.Norsktalende
	,ForlopsOversikt.OppflgRegStatus
	,ForlopsOversikt.OppflgStatus
  ,ForlopsOversikt.PasientID
  ,ForlopsOversikt.Sivilstatus
	,ForlopsOversikt.SykehusNavn
  ,ForlopsOversikt.Utdanning
FROM AlleVarNum 
INNER JOIN ForlopsOversikt
  ON AlleVarNum.ForlopsID = FollowupsNum.ForlopsID
LEFT JOIN FollowupsNum
  ON ForlopsOversikt.ForlopsID = AlleVarNum.ForlopsID
WHERE HovedDato >= "2015-01-01"  AND HovedDato <= "2016-08-01"

#LapMorcellator,  Erstattet 
#LapPlasmajet, Fjernet
#?    OpOptimeCount,
#?    OpParities,
#?    OpPregnancies,
    
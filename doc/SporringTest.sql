SELECT 
    OpTidlLapsko,
    OpTidlVagInngrep,
    OpType
  ,Opf0AlvorlighetsGrad
  ,forlopsoversikt.AvdRESH
	,forlopsoversikt.BasisRegStatus
  ,forlopsoversikt.Fodselsdato
	,forlopsoversikt.HovedDato
  ,forlopsoversikt.Norsktalende
	,forlopsoversikt.OppflgRegStatus
	,forlopsoversikt.OppflgStatus
  ,forlopsoversikt.PasientID
  ,forlopsoversikt.Sivilstatus
	,forlopsoversikt.SykehusNavn
  ,forlopsoversikt.Utdanning
FROM allevarnum 
INNER JOIN forlopsoversikt
  ON allevarnum.ForlopsID = followupsnum.ForlopsID
LEFT JOIN followupsnum
  ON forlopsoversikt.ForlopsID = allevarnum.ForlopsID
WHERE HovedDato >= "2015-01-01"  AND HovedDato <= "2016-08-01"

#LapMorcellator,  Erstattet 
#LapPlasmajet, Fjernet
#?    OpOptimeCount,
#?    OpParities,
#?    OpPregnancies,
    

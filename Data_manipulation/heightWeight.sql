SELECT Height.PATIENTID, Height.ADMID, HEIGHT, WEIGHT FROM 
--This view collects the observations about height and weight and presents them respective to an individual admission
(SELECT PATIENTID, ADMID, 0.0254 * REC as "HEIGHT"
FROM Obs
WHERE OBS_TYPE = 'HEIGHT') as Height
--Height has been changed to m as unit
INNER JOIN 
(SELECT PATIENTID, ADMID, REC as "WEIGHT"
FROM Obs
WHERE OBS_TYPE = 'WEIGHT') as Weight 
ON Height.PATIENTID = Weight.PATIENTID AND Height.ADMID = Weight.ADMID

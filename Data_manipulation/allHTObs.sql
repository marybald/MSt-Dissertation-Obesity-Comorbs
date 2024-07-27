SELECT 
SBP.ADMID,
SBP.PATIENTID,
SBP.SYSBP,
DBP.DIABP,
CASE
when SYSBP between 140 and 159.99 or DIABP between 90 and 99.99 then 'Stage 1'
when SYSBP between 160 and 179.99 or DIABP between 100 and 119.99 then 'Stage 2'
when SYSBP >180 or DIABP >120 then 'Stage 3'
else 'Normal'
END as HT_STAGE

-- The purpose of this view is to put systolic and diastolic blood pressure measurements into the same row, so they can be connected to individual admissions for each patient. These values are averaged across the admission to moderate the influence of anomalous readings for that patient. 
FROM 
(SELECT ADMID, PATIENTID, ROUND(AVG(REC), 2) SYSBP
FROM Obs
WHERE OBS_TYPE = 'SBP' AND REC IS NOT NULL
GROUP BY ADMID) AS SBP
LEFT JOIN
(SELECT ADMID, PATIENTID, ROUND(AVG(REC), 2) DIABP
FROM Obs
WHERE OBS_TYPE = 'DBP' AND REC IS NOT NULL
GROUP BY ADMID) AS DBP
ON SBP.ADMID = DBP.ADMID

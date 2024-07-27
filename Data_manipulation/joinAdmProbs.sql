SELECT Probs.ADMID, 
Probs.PATIENTID, 
Adm.ADM_DATE, 
Probs.NOTED_DATE, 
Probs.RESOLVED_DATE, 
Probs.PROBLEM, 
Probs.PROB_NAME, 
AllHTObs.HT_STAGE,
Probs.AGE_GRP_AT_NOTED_DATE as AGE_GRP, 
Probs.OSAFLAG
--The purpose of this view is to include diagnoses for each admission. Only problems noted within admissions in the period of study are included here, not previous diagnoses. The table also adds a category of hypertension to each admission, regardless of whether this was recorded as a 'problem'.
FROM Probs
INNER JOIN Adm ON Adm.ADMID = Probs.ADMID
INNER JOIN AllHTObs ON Probs.ADMID = AllHTObs.ADMID
-- Where statement here so that previous problems aren't included in the analysis. Only want problems that get diagnosed in an admission that's in this data set.
WHERE Adm.ADM_DATE <= Probs.NOTED_DATE AND AGE_GRP !=''

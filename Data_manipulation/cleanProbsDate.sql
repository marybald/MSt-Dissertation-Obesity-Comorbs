DELETE FROM Probs
WHERE NOTED_DATE NOT IN
(
SELECT MIN(NOTED_DATE) AS MinRecordID
FROM Probs
GROUP BY PATIENTID, ADMID, PROBLEM, PROB_NAME
);

UPDATE Probs
SET OSAFLAG = 1
WHERE EXISTS (
SELECT 1
FROM OSAPatients 
WHERE Probs.PATIENTID = OSAPatients.PATIENTID);

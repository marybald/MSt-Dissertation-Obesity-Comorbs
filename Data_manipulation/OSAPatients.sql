SELECT DISTINCT PATIENTID
--This view filters out all patients with sleep-disordered breathing. In this dataset, that equated to patients with an OSAHS diagnosis
from Probs
where PROBLEM IN (78275009, 73430006, 429487005)
ORDER BY PATIENTID

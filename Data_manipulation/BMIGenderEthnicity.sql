SELECT PATIENTID, 
ADMID, 
ADM_DATE, 
HEIGHT, 
AVG(WEIGHT) AS AVG_WEIGHT,
AVG(WEIGHT) / (HEIGHT * HEIGHT) AS BMI, 
CASE 
when GENDER like 1 then 'Male'
when Gender like 2 then 'Female'
else 'Other'
END as GENDER,
CASE
when ETHNICITY like 'A' then 'White'
when ETHNICITY like 'B' then 'White' 
when ETHNICITY like 'C' then 'White'
when ETHNICITY like 'D' then 'Non-white'
when ETHNICITY like 'E' then 'Non-white'
when ETHNICITY like 'F' then 'Non-white'
when ETHNICITY like 'G' then 'Non-white'
when ETHNICITY like 'H' then 'Non-white'
when ETHNICITY like 'J' then 'Non-white'
when ETHNICITY like 'K' then 'Non-white'
when ETHNICITY like 'L' then 'Non-white'
when ETHNICITY like 'M' then 'Non-white'
when ETHNICITY like 'N' then 'Non-white'
when ETHNICITY like 'P' then 'Non-white'
when ETHNICITY like 'R' then 'Non-white'
when ETHNICITY like 'S' then 'Non-white' 
when ETHNICITY like 'Z' then 'Unstated'
when ETHNICITY like '99' then 'Unstated'
else 'Unstated'
END AS ETHNICITY
--The purpose of this view connect the height and weight recordings from the Obs table to information about Admissions, as well as to calculate BMI 
-- Adding in admission date as a precursor for finding out which problems were identified within an active admission in this data set, in order to filter out pre-existing IHD.
-- Adding Gender and Ethnicity info in for use in later analysis
FROM 
(SELECT * 
FROM HeightWeight
INNER JOIN Adm ON Adm.ADMID = HeightWeight.ADMID
INNER JOIN Demog ON Demog.PATIENTID = HeightWeight.PATIENTID)
GROUP BY PATIENTID, ADMID
ORDER BY PATIENTID, ADM_DATE

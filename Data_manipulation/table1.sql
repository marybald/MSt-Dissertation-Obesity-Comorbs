SELECT PATIENTID, 
PROBLEM, 
PROB_NAME,
IHD_CAT, 
CASE
when AGE_GRP in ('20 - 24','25 - 29') then 'Under 30'
when AGE_GRP in ('30 - 34','35 - 39') then '30-39'
when AGE_GRP in ('40 - 44','45 - 49') then '40-49'
when AGE_GRP in ('50 - 54','55 - 59') then '50-59'
when AGE_GRP in ('60 - 64','65 - 69') then '60-69'
when AGE_GRP in ('70 - 74','75 - 79') then '70-79'
when AGE_GRP in ('80 - 84','85 - 89','90 - 94','95 - 99', '100 - 104') then 'Over 80'
else 'Missing'
END AS AGE_GRP, 
BMI, 
CASE
when BMI <18.5 then 'Underweight'
when BMI between 18.50 and 24.999 and ETHNICITY like '%White%' then 'Healthy'
when BMI between 25 and 29.999 and ETHNICITY like '%White%' then 'Overweight'
when BMI between 30 and 34.999 and ETHNICITY like '%White%' then 'Class 1'
when BMI between 35 and 39.999 and ETHNICITY like '%White%' then 'Class 2'
when BMI >40 and ETHNICITY like '%White%' then 'Class 3'
when BMI between 18.5 and 22.999 and ETHNICITY like '%Non-white%' then 'Healthy'
when BMI between 23 and 27.499 and ETHNICITY like '%Non-white%' then 'Overweight'
when BMI between 27.5 and 32.499 and ETHNICITY like '%Non-white%' then 'Class 1'
when BMI between 32.5 and 37.499 and ETHNICITY like '%Non-white%' then 'Class 2'
when BMI >37.5 and ETHNICITY like '%Non-white%' then 'Class 3'
when BMI between 18.5 and 24.999 and ETHNICITY like '%Unstated%' then 'Healthy'
when BMI between 25 and 29.999 and ETHNICITY like '%Unstated%' then 'Overweight'
when BMI between 30 and 34.999 and ETHNICITY like '%Unstated%' then 'Class 1'
when BMI between 35 and 39.999 and ETHNICITY like '%Unstated%' then 'Class 2'
when BMI >40 and ETHNICITY like '%Unstated%' then 'Class 3'
else 'Other'
END AS BMI_CAT, 
GENDER, 
OSAFLAG,
HT_STAGE
FROM AllIHDAdm
WHERE AGE_GRP !=''
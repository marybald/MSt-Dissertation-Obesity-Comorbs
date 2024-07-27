SELECT ProbsByAdmView.PATIENTID, 
ProbsByAdmView.ADMID, 
ProbsByAdmView.PROBLEM, 
ProbsByAdmView.PROB_NAME,
CASE 
when ProbsByAdmView.PROBLEM in (53741008, 414545008) then 'Coronary Artery Disease'
when ProbsByAdmView.PROBLEM in (194828000, 4557003, 394659003) then 'Angina Pectoris'
when ProbsByAdmView.PROBLEM in (22298006, 76388001) then 'Acute Myocardial Infarction'
else 'Not IHD'
END as IHD_CAT, 
HeightWeightSequence.BMI, 
HeightWeightSequence.GENDER, 
HeightWeightSequence.ETHNICITY, 
ProbsByAdmView.AGE_GRP, 
ProbsByAdmView.OSAFLAG,
ProbsByAdmView.HT_STAGE
--The purpose of this view is to restrict diagnoses to IHD endpoint, removing any other problems the patient may have
--Categorising the IHD diagnoses 
--Dropping Height and Average Weight for the admission, dropping the date the problem was noted
FROM ProbsByAdmView
INNER JOIN HeightWeightSequence
ON ProbsByAdmView.ADMID = HeightWeightSequence.ADMID
WHERE PROBLEM IN (414545008, 194828000, 4557003, 22298006, 76388001, 394659003, 53741008)

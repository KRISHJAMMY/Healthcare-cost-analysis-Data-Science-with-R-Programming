getwd()
HospitalCosts<-read.csv("hospitalcosts.csv", header=TRUE)
head(HospitalCosts)
names(HospitalCosts)

# 1. Record patient statistics:
summary(HospitalCosts)
# Get number of hospital visits based on age
summary(as.factor(HospitalCosts$AGE))
hist(HospitalCosts$AGE, main="Histogram of Age Group and their hospital visits",
     xlab="Age group", border="black", col=c("light green", "dark green"), xlim=c(0,20), ylim=c(0,350))
#Summarize expenditure based on age group
ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data=HospitalCosts)
# Get the maximum expense and its age group
which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))
barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))

# 2. Diagnosis-related group that has maximum hospitalization and expenditure
summary(as.factor(HospitalCosts$APRDRG))
DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = HospitalCosts)
DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]

# 3. Race vs Hospitalization costs
summary(as.factor(HospitalCosts$RACE))
HospitalCosts = na.omit(HospitalCosts)
summary(as.factor(HospitalCosts$RACE))
raceInfluence=lm(TOTCHG~ RACE, data=HospitalCosts)
summary(raceInfluence)
# Anaysis using ANOVA
raceInfluenceAOV <- aov(TOTCHG ~ RACE, data=HospitalCosts)
raceInfluenceAOV
summary(raceInfluenceAOV)

# 4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources
summary(as.factor(HospitalCosts$FEMALE))
ageGenderInflModel=lm(LOS ~ AGE + FEMALE + RACE, data = HospitalCosts)
summary(ageGenderInflModel)

# 5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
ageGenderInflModel=lm(LOS ~ AGE + FEMALE + RACE, data = HospitalCosts)
summary(ageGenderInflModel)

# 6. Complete analysis
hospitalCostModel=lm(TOTCHG ~ ., data = HospitalCosts)
summary(hospitalCostModel)

hcm1=lm(TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = HospitalCosts)
summary(hcm1)

hcm2=lm(TOTCHG ~ AGE + LOS + APRDRG, data = HospitalCosts)
summary(hcm2)

hcm3=lm(TOTCHG ~ AGE + LOS, data = HospitalCosts)
summary(hcm3)


## Analysis Conclusion:
  As is evident in the multiple models above, health care costs is dependent on age, length of stay and the diagnosis type.
Healthcare cost is the most for patients in the 0-1 yrs age group category
Maximum expenditure for 0-1 yr is 678118
Length of Stay increases the hospital cost

All Patient Refined Diagnosis Related Groups also affects healthcare costs

640 diagnosis related group had a max cost of 437978
Race or gender doesn’t have that much impact on hospital cost

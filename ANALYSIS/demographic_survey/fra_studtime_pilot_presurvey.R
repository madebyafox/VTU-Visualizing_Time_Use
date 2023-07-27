# fra_studtime_pilot_presurvey
# Researchers: Alain Fernex, PhD. Laurent Lima, PhD. Amy Rae Fox BSc.
# Data Analysis: Amy Fox 
# Date: 04.01.2014
# GOAL 
# Descriptive statistical analysis of the preliminary survey of the pilot student time allocation
# project.

#library(psych)
#library(sm)

## LOAD DATA
# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("[insert path here]")
# Read data into a dataframe called survey
survey <- read.csv("transcribed_surveys.csv")

##  DATA CLEANSING
# Get the dimensions of the dataframe
dim(survey) # should be 26,12
names(survey) # should display column names
str(survey) #view the structure of the imported data

#change data types certain columns to factors 
survey$student <- factor(survey$student) 


##FREQUENCY TABLES

#1. DATE OF BIRTH // ANNÉE DE NAISSANCE
t_birth_year = table(survey$birth_year) # simple table
#x_birth_year= xtable(t_birth_year,caption="Birth Year") #LaTEX table
#print(x_birth_year,type="html") #html table
summary(survey$birth_year) # view summary statistics for birth year
median(survey$birth_year)
barplot(t_birth_year,xlab="Year of Birth",ylab="Number of Students",main="Frequency of Birth Years")

#2. GENDER // VOTRE SEXE
t_gender = table(survey$gender)
pie(t_gender, main="Student Gender")

#3. DISCPLINE OF STUDY //QUELLE DISCIPLINE ETUDIEREZ-VOUS?
t_discipline = table(survey$major_1)

#4. EDUCATIONAL GOAL // FIN D'ÉTUDES
#add factor values to columns where no response was recorded
edu_goal.f = factor(survey$edu_goal, levels = c("a","b","c","d","e","f","g","h","i")) #define all possible values
t_edu_goal = table(edu_goal.f) #display summary of frequencies for this column
# edu_goal_labels = c("license","license professionnelle","master 1eme année","master professionnel 2eme année","master recherche 2eme année","concours d'accès a l'enseignement","doctorat","je ne sais pas","autre")
barplot (t_edu_goal,xlab="Educational Goal",ylab="Number of Students",main="Frequency of Student Educational Goals")

#5a. REPEATED GRADES HS // REDOUBLÉ AVANT SUPERIEUR
t_hs_repeat = table(survey$hs_repeat)
barplot(t_hs_repeat,xlab="Number of Years Repeated", ylab="Number of Students", main="Frequency of Years Repeated in Secondary School")
summary(survey$hs_repeat)

#5b. REPEATED GRADES // REDOUBLÉ DURANT SUPERIEUR
t_uni_repeat = table(survey$uni_repeat)
barplot(t_uni_repeat,xlab="Number of Years Repeated", ylab="Number of Students", main="Frequency of Years Repeated in University")
summary(survey$uni_repeat)

#6a. YEAR OF HIGH SCHOOL GRADUATION // L'ANNÉE BACCALAUREAT 
t_hs_grad = table(survey$hs_grad)
barplot(t_hs_grad, xlab="Year of High School Graduation", ylab="Number of Students", main="Frequency of High School Graduation Year")
summary(survey$hs_grad)

#6b. HIGH SCHOOL SPECIALITY // BACCALAUREAT SERIES
f_hs_dip = survey$hs_dip # create a copy of the hs_dip factor
levels(f_hs_dip) #examine the levels of the factor 
levels(f_hs_dip) = c("Other","Other","ES","L","S","Other") #rename the levels we want to group
t_hs_dip = table(f_hs_dip)
barplot(t_hs_dip, xlab="Diploma", ylab="Number of Students", main="Frequency of High School Diploma Specialities")
x_hs_dip= xtable(t_hs_dip,caption="Diploma Speciality") #LaTEX table
print(x_hs_dip,type="html") #html table

#6c. HIGH SCHOOL MENTION // BACCALAUREAT MENTION
f_hs_mention = survey$hs_mention 
levels(f_hs_mention) = c("None","AB","B","P","TB") #add "None" to levels
f_hs_mention <- factor(f_hs_mention, levels = c("None","P","B","AB","TB"))#reorder levels
t_hs_mention = table(f_hs_mention)
barplot(t_hs_mention, xlab="Honors", ylab="Number of Students", main="Frequency of High School Honors")
x_hs_mention= xtable(t_hs_dip,caption="Diploma Speciality") #LaTEX table
print(x_hs_dip,type="html") #html table

#7. Intention to Pursue Higher Education // D'entreprendre D’etudes Universitaires
t_edu_affect = table(survey$edu_affect)
barplot(t_edu_affect, xlab="Intention", ylab="Number of Students", main="Frequency of Higher Education Intentions")

#EXPLORE CONCURRENCE OF CATEGORICAL VARIABLES
repeat_concurrence = table(survey$hs_repeat,survey$uni_repeat)
hs_concurrence = table(f_hs_dip,f_hs_mention)
gender_concurrence = table (survey$gender,survey$edu_affect)





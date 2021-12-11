survey_data = read.csv('stack-overflow-developer-survey-2018/survey_results_public.csv')


# 1.check NA, if a respondent left more than 50% questions unanswered, we delete that record
num_na = apply(survey_data, 1, function(x) {sum(is.na(x))})
survey_data = survey_data[which(num_na < 129 * 0.5), ]


# 2.convert Country to Continent
library(countrycode)
survey_data$Continent = countrycode(sourcevar = survey_data[, "Country"],
                                    origin = "country.name",
                                    destination = "continent")


# 3.convert categorical variables in blue into dummy variables
blue_names = c("Hobby", "OpenSource", "Continent", "Student", "Employment", "FormalEducation", "UndergradMajor", "HopeFiveYears", 
               "JobSearchStatus", "AssessJob1", "AssessBenefits1", "JobContactPriorities1", "JobEmailPriorities1", "AgreeDisagree1", 
               "AgreeDisagree2", "AgreeDisagree3", "OperatingSystem", "AIDangerous", "AIInteresting", "AIResponsible", "AIFuture",
               "EthicsChoice", "EthicsReport", "EthicsResponsible", "EthicalImplications", "StackOverflowVisit", "WakeTime",  
               "EducationParents", "Dependents")
blue = survey_data[names(survey_data) %in% blue_names]
NA_count = lapply(blue, function(x){sum(is.na(x))}) 
full = fastDummies::dummy_cols(blue, ignore_na = TRUE) ## Create dummy variable for all 
full = full[!names(full) %in% blue_names]
dummies = fastDummies::dummy_cols(blue, remove_first_dummy = TRUE, ignore_na = TRUE) ## Create dummy for all except first one 
dummies = dummies[!names(dummies) %in% blue_names]
Num_col = lapply(blue, is.numeric) 
# Creat dummy variable for numeric columns
AssessJob1_1 = 1 * (blue$AssessJob1 == "1")
AssessJob1_2 = 1 * (blue$AssessJob1 == "2")
AssessJob1_3 = 1 * (blue$AssessJob1 == "3")
AssessJob1_4 = 1 * (blue$AssessJob1 == "4")
AssessJob1_5 = 1 * (blue$AssessJob1 == "5")
AssessJob1_6 = 1 * (blue$AssessJob1 == "6")
AssessJob1_7 = 1 * (blue$AssessJob1 == "7")
AssessJob1_8 = 1 * (blue$AssessJob1 == "8")
AssessJob1_9 = 1 * (blue$AssessJob1 == "9")
AssessJob1_10 = 1 * (blue$AssessJob1 == "10")
AssessBenefits1_1 = 1 * (blue$AssessBenefits1 == "1")
AssessBenefits1_2 = 1 * (blue$AssessBenefits1 == "2")
AssessBenefits1_3 = 1 * (blue$AssessBenefits1 == "3")
AssessBenefits1_4 = 1 * (blue$AssessBenefits1 == "4")
AssessBenefits1_5 = 1 * (blue$AssessBenefits1 == "5")
AssessBenefits1_6 = 1 * (blue$AssessBenefits1 == "6")
AssessBenefits1_7 = 1 * (blue$AssessBenefits1 == "7")
AssessBenefits1_8 = 1 * (blue$AssessBenefits1 == "8")
AssessBenefits1_9 = 1 * (blue$AssessBenefits1 == "9")
AssessBenefits1_10 = 1 * (blue$AssessBenefits1 == "10")
AssessBenefits1_11 = 1 * (blue$AssessBenefits1 == "11")
JobContactPriorities1_1 = 1 * (blue$JobContactPriorities1 == "1")
JobContactPriorities1_2 = 1 * (blue$JobContactPriorities1 == "2")
JobContactPriorities1_3 = 1 * (blue$JobContactPriorities1 == "3")
JobContactPriorities1_4 = 1 * (blue$JobContactPriorities1 == "4")
JobContactPriorities1_5 = 1 * (blue$JobContactPriorities1 == "5")
JobEmailPriorities1_1 = 1 * (blue$JobEmailPriorities1 == "1")
JobEmailPriorities1_2 = 1 * (blue$JobEmailPriorities1 == "2")
JobEmailPriorities1_3 = 1 * (blue$JobEmailPriorities1 == "3")
JobEmailPriorities1_4 = 1 * (blue$JobEmailPriorities1 == "4")
JobEmailPriorities1_5 = 1 * (blue$JobEmailPriorities1 == "5")
JobEmailPriorities1_6 = 1 * (blue$JobEmailPriorities1 == "6")
JobEmailPriorities1_7 = 1 * (blue$JobEmailPriorities1 == "7")

num_dummy = data.frame(AssessJob1_2, AssessJob1_3, AssessJob1_4, AssessJob1_5, AssessJob1_6, AssessJob1_7, AssessJob1_8, AssessJob1_9, 
                       AssessJob1_10, AssessBenefits1_2, AssessBenefits1_3, AssessBenefits1_4, AssessBenefits1_5, AssessBenefits1_6, 
                       AssessBenefits1_7, AssessBenefits1_8, AssessBenefits1_9, AssessBenefits1_10, AssessBenefits1_11, 
                       JobContactPriorities1_2, JobContactPriorities1_3, JobContactPriorities1_4, JobContactPriorities1_5, 
                       JobEmailPriorities1_2, JobEmailPriorities1_3, JobEmailPriorities1_4, JobEmailPriorities1_5, 
                       JobEmailPriorities1_6, JobEmailPriorities1_7)
blue_dummy_df = cbind(dummies, num_dummy)
blue_dummy_matrix = as.matrix(blue_dummy_df)


# 4.convert categorical variables in green into dummy varibles
green_names = c("CompanySize","YearsCoding", "YearsCodingProf", "JobSatisfaction", "CareerSatisfaction", 
                "LastNewJob", "TimeFullyProductive","CheckInCode", "HoursComputer", "HoursOutside", "SkipMeals", 
                "Exercise", "Age")
green = survey_data[names(survey_data) %in% green_names]

# reference: "Fewer than 10 employees" 
CompanySize_2 = 1 * (green$CompanySize == "10 to 19 employees")
CompanySize_3 = 1 * (green$CompanySize == "20 to 99 employees")
CompanySize_4 = 1 * (green$CompanySize == "100 to 499 employees")
CompanySize_5 = 1 * (green$CompanySize == "500 to 999 employees")
CompanySize_6 = 1 * (green$CompanySize == "1,000 to 4,999 employees")
CompanySize_7 = 1 * (green$CompanySize == "5,000 to 9,999 employees")
CompanySize_8 = 1 * (green$CompanySize == "10,000 or more employees")

# reference: "0-2 years"
YearsCoding_2 = 1 * (green$YearsCoding == "3-5 years")
YearsCoding_3 = 1 * (green$YearsCoding == "6-8 years")
YearsCoding_4 = 1 * (green$YearsCoding == "9-11 years")
YearsCoding_5 = 1 * (green$YearsCoding == "12-14 years")
YearsCoding_6 = 1 * (green$YearsCoding == "15-17 years")
YearsCoding_7 = 1 * (green$YearsCoding == "18-20 years")
YearsCoding_8 = 1 * (green$YearsCoding == "21-23 years")
YearsCoding_9 = 1 * (green$YearsCoding == "24-26 years")
YearsCoding_10 = 1 * (green$YearsCoding == "27-29 years")
YearsCoding_11 = 1 * (green$YearsCoding == "30 or more years")

# reference: "0-2 years"
YearsCodingProf_2 = 1 * (green$YearsCodingProf == "3-5 years")
YearsCodingProf_3 = 1 * (green$YearsCodingProf == "6-8 years")
YearsCodingProf_4 = 1 * (green$YearsCodingProf == "9-11 years")
YearsCodingProf_5 = 1 * (green$YearsCodingProf == "12-14 years")
YearsCodingProf_6 = 1 * (green$YearsCodingProf == "15-17 years")
YearsCodingProf_7 = 1 * (green$YearsCodingProf == "18-20 years")
YearsCodingProf_8 = 1 * (green$YearsCodingProf == "21-23 years")
YearsCodingProf_9 = 1 * (green$YearsCodingProf == "24-26 years")
YearsCodingProf_10 = 1 * (green$YearsCodingProf == "27-29 years")
YearsCodingProf_11 = 1 * (green$YearsCodingProf == "30 or more years")

green$JobSatisfaction[which(green$JobSatisfaction=="Extremely dissatisfied")] <- 1
green$JobSatisfaction[which(green$JobSatisfaction=="Slightly dissatisfied")] <- 2
green$JobSatisfaction[which(green$JobSatisfaction=="Moderately dissatisfied")] <- 3
green$JobSatisfaction[which(green$JobSatisfaction=="Neither satisfied nor dissatisfied")] <- 4
green$JobSatisfaction[which(green$JobSatisfaction=="Slightly satisfied")] <- 5
green$JobSatisfaction[which(green$JobSatisfaction=="Moderately satisfied")] <- 6
green$JobSatisfaction[which(green$JobSatisfaction=="Extremely satisfied")] <- 7

green$CareerSatisfaction[which(green$CareerSatisfaction=="Extremely dissatisfied")] <- 1
green$CareerSatisfaction[which(green$CareerSatisfaction=="Slightly dissatisfied")] <- 2
green$CareerSatisfaction[which(green$CareerSatisfaction=="Moderately dissatisfied")] <- 3
green$CareerSatisfaction[which(green$CareerSatisfaction=="Neither satisfied nor dissatisfied")] <- 4
green$CareerSatisfaction[which(green$CareerSatisfaction=="Slightly satisfied")] <- 5
green$CareerSatisfaction[which(green$CareerSatisfaction=="Moderately satisfied")] <- 6
green$CareerSatisfaction[which(green$CareerSatisfaction=="Extremely satisfied")] <- 7

# reference: "I've never had a job"
LastNewJob_2 = 1 * (green$LastNewJob == "Less than a year ago")
LastNewJob_3 = 1 * (green$LastNewJob == "Between 1 and 2 years ago")
LastNewJob_4 = 1 * (green$LastNewJob == "Between 2 and 4 years ago")
LastNewJob_5 = 1 * (green$LastNewJob == "More than 4 years ago")

# reference: "Less than a month"
TimeFullyProductive_2 = 1 * (green$TimeFullyProductive == "One to three months")
TimeFullyProductive_3 = 1 * (green$TimeFullyProductive == "Three to six months")
TimeFullyProductive_4 = 1 * (green$TimeFullyProductive == "Six to nine months")
TimeFullyProductive_5 = 1 * (green$TimeFullyProductive == "Nine months to a year")
TimeFullyProductive_6 = 1 * (green$TimeFullyProductive == "More than a year")

# reference: "Never"
CheckInCode_2 = 1 * (green$CheckInCode == "Less than once per month")
CheckInCode_3 = 1 * (green$CheckInCode == "A few times per week")
CheckInCode_4 = 1 * (green$CheckInCode == "Weekly or a few times per month")
CheckInCode_5 = 1 * (green$CheckInCode == "Once a day")
CheckInCode_6 = 1 * (green$CheckInCode == "Multiple times per day")

# reference: "Less than 1 hour"
HoursComputer_2 = 1 * (green$HoursComputer == "1 - 4 hours")
HoursComputer_3 = 1 * (green$HoursComputer == "5 - 8 hours")
HoursComputer_4 = 1 * (green$HoursComputer == "9 - 12 hours")
HoursComputer_5 = 1 * (green$HoursComputer == "Over 12 hours")

# reference: "Less than 30 minutes"
HoursOutside_2 = 1 * (green$HoursOutside == "30 - 59 minutes")
HoursOutside_3 = 1 * (green$HoursOutside == "1 - 2 hours")
HoursOutside_4 = 1 * (green$HoursOutside == "3 - 4 hours")
HoursOutside_5 = 1 * (green$HoursOutside == "Over 4 hours")

# reference: "Never"
SkipMeals_2 = 1 * (green$SkipMeals == "1 - 2 times per week")
SkipMeals_3 = 1 * (green$SkipMeals == "3 - 4 times per week")
SkipMeals_4 = 1 * (green$SkipMeals == "Daily or almost every day")

# reference: "I don't typically exercise"
Exercise_2 = 1 * (green$Exercise == "1 - 2 times per week")
Exercise_3 = 1 * (green$Exercise == "3 - 4 times per week")
Exercise_4 = 1 * (green$Exercise == "Daily or almost every day")

# reference: "Under 18 years old"
Age_2 = 1 * (green$Age == "18 - 24 years old")
Age_3 = 1 * (green$Age == "25 - 34 years old")
Age_4 = 1 * (green$Age == "35 - 44 years old")
Age_5 = 1 * (green$Age == "45 - 54 years old")
Age_6 = 1 * (green$Age == "55 - 64 years old")
Age_7 = 1 * (green$Age == "65 years or older")

green_dummy <- cbind(CompanySize_2, CompanySize_3, CompanySize_4, CompanySize_5, CompanySize_6, CompanySize_7, CompanySize_8, 
                     YearsCoding_2, YearsCoding_3, YearsCoding_4, YearsCoding_5, YearsCoding_6, YearsCoding_7, YearsCoding_8, 
                     YearsCoding_9, YearsCoding_10, YearsCoding_11, YearsCodingProf_2, YearsCodingProf_3, YearsCodingProf_4, 
                     YearsCodingProf_5, YearsCodingProf_6, YearsCodingProf_7, YearsCodingProf_8, YearsCodingProf_9, 
                     YearsCodingProf_10, YearsCodingProf_11, green$JobSatisfaction, green$CareerSatisfaction, 
                     LastNewJob_2, LastNewJob_3, LastNewJob_4, LastNewJob_5, TimeFullyProductive_2, TimeFullyProductive_3, 
                     TimeFullyProductive_4, TimeFullyProductive_5, TimeFullyProductive_6, HoursComputer_2, HoursComputer_3, 
                     HoursComputer_4, HoursComputer_5, SkipMeals_2, SkipMeals_3, SkipMeals_4, Exercise_2, Exercise_3, 
                     Exercise_4, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7)
green_dummy = apply(green_dummy, 2, as.numeric)

# 5.binarily code questions with 'check all that apply' (varibles in yellow)
multiple_options_convertor = function(response, question_name) {
  n = length(response)
  option_list = c()
  for (i in 1:n) {
    response_i = response[i]
    if (is.na(response_i) == FALSE) {
      option_list = c(option_list, strsplit(response_i, split = ';')[[1]])
      option_list = unique(option_list)
    }
  }
  p = length(option_list)
  out_mat = matrix(0, nrow = n, ncol = p)
  for (i in 1:n) {
    response_i = response[i]
    if (is.na(response_i)) {
      out_mat[i, ] = NA
    } else {
      response_i = strsplit(response_i, split = ';')[[1]]
      split_up = option_list %in% response_i
      out_mat[i, ] = split_up
    }
  }
  colnames(out_mat) = paste(question_name, ': ', option_list, sep = '')
  return(out_mat)
}
question_names = c('DevType', 'EducationTypes', 'LanguageWorkedWith', 'DatabaseWorkedWith', 'PlatformWorkedWith',
                   'FrameworkWorkedWith', 'IDE', 'Methodology', 'VersionControl', 'ErgonomicDevices', 'Gender',
                   'SexualOrientation', 'RaceEthnicity')
converted_matrix = list()
combined_matrix = c()
for (question_name in question_names) {
  converted_matrix[[question_name]] = multiple_options_convertor(survey_data[, question_name], question_name)
  combined_matrix = cbind(combined_matrix, converted_matrix[[question_name]])
}


# number of monitors
num_monitor = survey_data$NumberMonitors
num_monitor[num_monitor == 'More than 4'] = 4
num_monitor = as.numeric(num_monitor)


# column bind
data = cbind(blue_dummy_df, green_dummy, combined_matrix, num_monitor, survey_data$ConvertedSalary)
dim(data)
table(rowSums(is.na(data)))


# missing data imputation by knn
descale = function(x, imputed_x) {
  scaled_x = scale(x)
  descaled_x = t(t(imputed_x) * attr(scaled_x, 'scaled:scale') + attr(scaled_x, 'scaled:center'))
  return(descaled_x)
}
library(caret)
knn_imputor = preProcess(data, method = 'knnImpute', k = 1)  # caution: preProcess scales the data automatically
imputed_data = predict(knn_imputor, data)
descaled_imputed_data = descale(data, imputed_data)  # descale data
descaled_imputed_data[, 1:367] = round(descaled_imputed_data[, 1:367])  # deal with numerical errors from descaling 
save(descaled_imputed_data, file = 'imputed_data.RData')


# outcome variables
dummy_to_cate = function(dummy_matrix) {
  options_statement = colnames(dummy_matrix)
  n = nrow(dummy_matrix)
  category = rep(0, n)
  for (i in 1:n) {
    loc = which(dummy_matrix[i, ] == 1)
    if (length(loc) > 0) {
      category[i] = loc
    }
  }
  category = category + 1
  map = c('reference', options_statement)
  return(list(category = category, map = map))
}
AI_Dangerous = descaled_imputed_data[, grep('AIDangerous', colnames(descaled_imputed_data))]
AI_Dangerous_ = dummy_to_cate(AI_Dangerous)
AI_Dangerous = AI_Dangerous_$category
AI_Dangerous_map = AI_Dangerous_$map

AI_Interesting = descaled_imputed_data[, grep('AIInteresting', colnames(descaled_imputed_data))]
AI_Interesting_ = dummy_to_cate(AI_Interesting)
AI_Interesting = AI_Interesting_$category
AI_Interesting_map = AI_Interesting_$map

AI_Responsible = descaled_imputed_data[, grep('AIResponsible', colnames(descaled_imputed_data))]
AI_Responsible_ = dummy_to_cate(AI_Responsible)
AI_Responsible = AI_Responsible_$category
AI_Responsible_map = AI_Responsible_$map

AI_Future = descaled_imputed_data[, grep('AIFuture', colnames(descaled_imputed_data))]
AI_Future_ = dummy_to_cate(AI_Future)
AI_Future = AI_Future_$category
AI_Future_map = AI_Future_$map

descaled_imputed_data = descaled_imputed_data[, -grep('AI', colnames(descaled_imputed_data))]
cleaned_data = cbind(descaled_imputed_data, AI_Dangerous, AI_Interesting, AI_Responsible, AI_Future)
save(AI_Dangerous_map, AI_Interesting_map, AI_Responsible_map, AI_Future_map, cleaned_data, file = 'cleaned_data.RData')

write.csv(cleaned_data, file = 'AI_data.csv')
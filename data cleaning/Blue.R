survey_data = read.csv('stack-overflow-developer-survey-2018/survey_results_public.csv')

# check NA, if a respondent left more than 70% questions unanswered, we delete that record
num_na = apply(survey_data, 1, function(x) {sum(is.na(x))})
survey_data = survey_data[which(num_na < 129 * 0.7), ]

# convert Country to Continent
library(countrycode)
survey_data$Continent = countrycode(sourcevar = survey_data[, "Country"],
                                    origin = "country.name",
                                    destination = "continent")

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

reference_group = cbind(full[which(!names(full) %in% names(dummies))], AssessJob1_1, 
                        AssessBenefits1_1, JobContactPriorities1_1, JobEmailPriorities1_1) ## Reference Group data
name_ref = names(reference_group) ## To view reference group options

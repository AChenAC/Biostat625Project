survey_data = read.csv('stack-overflow-developer-survey-2018/survey_results_public.csv')

# check NA, if a respondent left more than 70% questions unanswered, we delete that record
num_na = apply(survey_data, 1, function(x) {sum(is.na(x))})
survey_data = survey_data[which(num_na < 129 * 0.7), ]

# convert Country to Continent
library(countrycode)
survey_data$Continent = countrycode(sourcevar = survey_data[, "Country"],
                                    origin = "country.name",
                                    destination = "continent")

# binarily code questions with 'check all that apply'
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
      if (option_list[p] == TRUE) {
        out_mat[i, ] = 0
      } else {
        out_mat[i, 1:(p-1)] = split_up[-p]
      }
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
library(Matrix)
sparce_combined_matrix = Matrix(combined_matrix)

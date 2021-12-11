setwd("D:/625 final project")
data <- load('cleaned_data.RData')
data <- data.frame(cleaned_data)




resultpublic <- read.csv("survey_results_public.csv")
resultname <- colnames(resultpublic)

name <- colnames(data)
name <- name[1:357]
name[145:146] <- c("JobSatisfaction", "CareerSatisfaction")

question <- c()
for (i in 1:357) {
  if(grepl("_", name[i], fixed = TRUE)){
     question <- cbind(question, strsplit(name[i], "[_]")[[1]][1])
  }else{
     question <- cbind(question, strsplit(name[i], "[..]")[[1]][1])
  }
}

question[356] <- "NumberMonitors"
question[357]  <- "ConvertedSalary"

indexofquestion <- c()
for (i in 1:357) {
  for (j in 1:129) {
    if(question[i] == resultname[j]){
      indexofquestion <- cbind(indexofquestion, rbind(j,name[i]))
      break    
    }
    # if(j==129){
    #   cat(i,"\n")
    # }
  }
}


continent <- cbind(rbind("Continent","Continent"),rbind("Continent","Continent"),rbind("Continent","Continent"),rbind("Continent","Continent"))

indexofquestion <- cbind(indexofquestion[,1:84], continent, indexofquestion[,85:353])
indexofquestion <- rbind(indexofquestion,question)

indexofquestion[2:3, 145] <- "JobSatisfaction"
indexofquestion[2:3, 146] <- "CareerSatisfaction"
indexofquestion[1, 85:88] <- 4

save(indexofquestion, file = 'question_map.Rdata')

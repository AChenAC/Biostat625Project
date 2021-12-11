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


indexofquestion <- c()
for (i in 1:357) {
  for (j in 1:129) {
    if(question[i] == resultname[j]){
      indexofquestion <- cbind(indexofquestion, rbind(j,question[i]))
      break    
    }
    if(j==129){
      cat(i,"\n")
    }
  }
}


continent <- cbind(rbind(85,"Continent"),rbind(86,"Continent"),rbind(87,"Continent"),rbind(88,"Continent"))

ncol(indexofquestion)
indexofquestion <- cbind(indexofquestion[,1:84], continent,indexofquestion[,85:351])
indexofquestion <- cbind(indexofquestion,rbind(356,question[356]))
indexofquestion <- cbind(indexofquestion,rbind(357,question[357]))


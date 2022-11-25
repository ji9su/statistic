library(flextable)

data = read.csv("test/骨髓1110625R1.csv", fileEncoding = "CP950", header = TRUE, sep = ",")

description <- function(data, Continuous_variables = NA, Categorical_variable = NA) {
  
  description_table <- data.frame(Total = NA)
  if(!is.na(Continuous_variables)) {
    for(i in 1:length(Continuous_variables)) {
      mean <- mean(data[,Continuous_variables[i]], na.rm = TRUE)
      sd <- sd(data[,Continuous_variables[i]], na.rm = TRUE)
      mean <- formatC(mean, 2, format = "f")
      sd <- formatC(sd, 2, format = "f")
      description_table[i,"Total"] <- paste0(mean, " ± ", sd)
      rownames(description_table)[i] <- Continuous_variables[i]
    }
  }
  
  if(!is.na(Categorical_variable)) {
    for(i in 1:length(Categorical_variable)) {
      Categorical_number <- table(data[,Categorical_variable[i]])
      Categorical_percentage <- Categorical_number/sum(Categorical_number)*100
      Categorical_percentage <- formatC(Categorical_percentage, 1, format = "f")
      last_number <- dim(description_table)[1]
      description_table[last_number + 1, "Total"] <- c("")
      rownames(description_table)[last_number + 1] <- Categorical_variable[i]
      Categorical_numes <- names(Categorical_number)
      for(j in 1:length(Categorical_number)) {
        description_table[last_number + 1 + j, "Total"] <- paste0(Categorical_number[j], " (",Categorical_percentage[j], "%)")
        rownames(description_table)[last_number + 1 + j] <- paste0(Categorical_variable[i], Categorical_numes[j])
      }
    }
  }
  
  Variable <- rownames(description_table)
  description_table <- cbind(Variable, description_table)
  
  return(description_table)
}


colnames(data)

Continuous_variables <- c("X.attitude_before", "X.attitude_after")
Categorical_variable <- c(NA)

description_table <- description(data, Continuous_variables, Categorical_variable)


write.csv(description_table, "test/description_table.csv", fileEncoding = "CP950", row.names=FALSE)

# create a flextable object
ft <- flextable(data = description_table)
ft <- autofit(ft)
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/description_table.docx")

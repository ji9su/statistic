data = read.csv("test/歸人急診清單.csv", fileEncoding = "CP950", header = TRUE, sep = ",")


posisson_regression <- function(data, Continuous_variables, Categorical_variable, adjustment_variable = NA) {
  
  variables <- c(Continuous_variables, Categorical_variable)
  poisson = data.frame(B = NA,CI_2.5 = NA, CI_97.5 = NA, p_value = NA)
  
  for(j in 1:length(Categorical_variable)){
    data[,Categorical_variable[j]] = as.factor(data[,Categorical_variable[j]])
  }
  
  for(i in 1:length(variables)) {
    if(is.na(adjustment_variable)){
      Formula <-  paste0(dependent, "~", variables[i])
    } else {
      Formula <-  paste0(dependent, "~", variables[i], "+", adjustment_variable)
    }
    
    model <- glm(Formula, family="poisson", data = data)
    result <- summary(model)
    Coefficients <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Estimate"]
    Std <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Std. Error"]
    
    #ci.up = Coefficients + qt(0.975, df = result$df[2]) * Std
    #ci.low = Coefficients - qt(0.975, df = result$df[2]) * Std
    
    CI <- confint(model)
    CI <- CI[grep(variables[i], rownames(result$coefficients)),]
    p_value <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Pr(>|z|)"]
    if(i == 1) {
      poisson[1, ] <- c(Coefficients, CI, p_value)
      row.names(poisson) <- row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))]
    } else {
      poisson[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(Coefficients, CI, p_value)
    }
  }
  
  poisson[,"B"] <- formatC(poisson[,"B"], 2, format = "f")
  poisson[,"CI_2.5"] <- formatC(poisson[,"CI_2.5"], 2, format = "f")
  poisson[,"CI_97.5"] <- formatC(poisson[,"CI_97.5"], 2, format = "f")
  poisson[,"p_value"] <- formatC(poisson[,"p_value"], 3, format = "f")
  
  return(poisson)
}

colnames(data)

dependent = "final"
Continuous_variables <- c("age", "number")
Categorical_variable <- c("gender", "diagnosis", "level", "to_hospital", "to_Emergency", "with_person")

poisson_table <- posisson_regression(data = data, Continuous_variables =Continuous_variables,
                                     Categorical_variable = Categorical_variable, adjustment_variable = NA)


adjustment_variable <- "age + number + diagnosis + level + to_hospital + to_Emergency + with_person"

adjust.poisson_table <- posisson_regression(data = data, Continuous_variables = Continuous_variables,
                                            Categorical_variable = Categorical_variable,
                                            adjustment_variable = adjustment_variable)

poisson <- cbind(poisson_table, adjust.poisson_table)
write.csv(poisson, "test/poisson.csv", fileEncoding = "UTF-8")



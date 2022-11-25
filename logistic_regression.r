library(flextable)

data = read.csv("test/Example data.csv", fileEncoding = "CP950", header = TRUE, sep = ",")

logistic_regression <- function(data, dependent, Continuous_variables,
                                Categorical_variable, adjustment_variable = NA) {
  
  variables <- c(Continuous_variables, Categorical_variable)
  logistic = data.frame(OR = NA, p_value = NA)
  
  for(j in 1:length(Categorical_variable)){
    data[,Categorical_variable[j]] = as.factor(data[,Categorical_variable[j]])
  }
  
  for(i in 1:length(variables)) {
    if(is.na(adjustment_variable)){
      Formula <-  paste0(dependent, "~", variables[i])
    } else {
      Formula <-  paste0(dependent, "~", variables[i], "+", adjustment_variable)
    }
    
    model <- glm(Formula, family = "binomial", data = data)
    result <- summary(model)
    
    Coefficients <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Estimate"]
    
    #Std <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Std. Error"]
    #ci.low = Coefficients - qnorm(0.975) * Std
    #ci.up = Coefficients + qnorm(0.975) * Std
    #CI <- c(ci.low, ci.up)
    
    CI <- confint(model)
    CI <- CI[grep(variables[i], rownames(result$coefficients)),]
    p_value <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Pr(>|z|)"]
    
    Coefficients <- exp(Coefficients)
    CI <- exp(CI)
    
    Coefficients <- formatC(Coefficients, 2, format = "f")
    CI <- formatC(CI, 2, format = "f")
    p_value <- formatC(p_value, 3, format = "f")
    
    if(i == 1) {
      logistic[1, ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
      row.names(logistic) <- row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))]
    } else {
      if(is.null(dim(CI))){
        if(length(unique(Categorical_variable %in% variables[i])) > 1)  {
          logistic[variables[i], ] <- c("", "")
          logistic[paste0(variables[i], 0), ] <- c("Reference", "")
          logistic[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
        } else {
          logistic[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
        }
      } else {
        if(length(unique(Categorical_variable %in% variables[i])) > 1) {
          logistic[variables[i], ] <- c("", "")
          logistic[paste0(variables[i], 0), ] <- c("Reference", "")
          logistic[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[,1], ", ", CI[,2], ")"), p_value)
        } else {
          logistic[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[,1], ", ", CI[,2], ")"), p_value)
        }
      }
    }
  }
  
  return(logistic)
}

colnames(data)

dependent = "Disease"   # only Categorical variables and must be 0 & 1 value
Continuous_variables <- c("eGFR")
Categorical_variable <- c("Death", "Income")

logistic_table <- logistic_regression(data = data, dependent = dependent,
                                      Continuous_variables = Continuous_variables,
                                      Categorical_variable = Categorical_variable,
                                      adjustment_variable = NA)

adjustment_variable <- "eGFR + Death + Income"

adjust.logistic_table <- logistic_regression(data = data, dependent = dependent,
                                             Continuous_variables = Continuous_variables,
                                             Categorical_variable = Categorical_variable,
                                             adjustment_variable = adjustment_variable)

colnames(logistic_table) <- c("Crude−OR (95% CI)", "p-value")
colnames(adjust.logistic_table) <- c("Adj−OR (95%CI)", " p-value")
logistic <- cbind(logistic_table, adjust.logistic_table)
Variable <- rownames(logistic)
logistic <- cbind(Variable, logistic)


write.csv(logistic, "test/logistic.csv", fileEncoding = "UTF-8", row.names=FALSE)

# create a flextable object
ft <- flextable(data = logistic)
ft <- autofit(ft)
ft <- add_footer_lines(ft, paste0("#All result of Adj-OR were adjusted by ",
                                  gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/logistic.docx")


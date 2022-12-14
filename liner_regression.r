library(flextable)

data = read.csv("test/coding book-20220321.csv", fileEncoding = "CP950", header = TRUE, sep = ",")

liner_regression <- function(data, dependent, Continuous_variables,
                             Categorical_variable, adjustment_variable = NA) {
  
  variables <- c(Continuous_variables, Categorical_variable)
  liner = data.frame(Beta = NA, p_value = NA)
  
  for(j in 1:length(Categorical_variable)){
    data[,Categorical_variable[j]] = as.factor(data[,Categorical_variable[j]])
  }
  
  for(i in 1:length(variables)) {
    if(is.na(adjustment_variable)){
      Formula <-  paste0(dependent, "~", variables[i])
    } else {
      Formula <-  paste0(dependent, "~", variables[i], "+", adjustment_variable)
    }
    
    model <- glm(Formula, family = "gaussian", data = data)
    result <- summary(model)
    
    Coefficients <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Estimate"]
    CI <- confint(model)
    CI <- CI[grep(variables[i], rownames(result$coefficients)),]
    p_value <- result$coefficients[grep(variables[i], rownames(result$coefficients)), "Pr(>|t|)"]
    
    Coefficients <- formatC(Coefficients, 2, format = "f")
    CI <- formatC(CI, 2, format = "f")
    p_value <- formatC(p_value, 3, format = "f")
    
    if(i == 1) {
      liner[1, ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
      row.names(liner) <- row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))]
    } else {
      if(is.null(dim(CI))){
        if(length(unique(Categorical_variable %in% variables[i])) > 1)  {
          liner[variables[i], ] <- c("", "")
          liner[paste0(variables[i], 0), ] <- c("Reference", "")
          liner[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
        } else {
          liner[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[1], ", ", CI[2], ")"), p_value)
        }
      } else {
        if(length(unique(Categorical_variable %in% variables[i])) > 1) {
          liner[variables[i], ] <- c("", "")
          liner[paste0(variables[i], 0), ] <- c("Reference", "")
          liner[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[,1], ", ", CI[,2], ")"), p_value)
        } else {
          liner[row.names(result$coefficients)[grep(variables[i], rownames(result$coefficients))], ] <- c(paste0(Coefficients, " (", CI[,1], ", ", CI[,2], ")"), p_value)
        }
      }
    }
  }
  return(liner)
}

colnames(data)

dependent = "beteeen.HADS"   # only Continuous variables
Continuous_variables <- c("??????", "????????????", "????????????", "????????????", "????????????", "????????????", "P?????????", "QOC??????", "QOC??????")
Categorical_variable <- c("F??????", "?????????", "F????????????", "F??????", '????????????', "F??????", "F??????", "F?????????",
                          "???.Tr.", "???.??????", "???.Tr.", "???.??????", "???.Tr.", "???.??????", "P.Tr.", "P.??????", "?????????ICU",
                          "P??????", "P??????", "P??????", "P??????", "P??????.???", "????????????", "P???ICU")

liner_table <- liner_regression(data = data, dependent = dependent, 
                                Continuous_variables = Continuous_variables,
                                Categorical_variable = Categorical_variable,
                                adjustment_variable = NA)

adjustment_variable <- "???????????? + P??????"

adjust.liner_table <- liner_regression(data = data, dependent = dependent, 
                                       Continuous_variables = Continuous_variables,
                                       Categorical_variable = Categorical_variable,
                                       adjustment_variable = adjustment_variable)

colnames(liner_table) <- c("Crude???Beta (95% CI)", "p-value")
colnames(adjust.liner_table) <- c("Adj???Beta (95%CI)", " p-value")
liner <- cbind(liner_table, adjust.liner_table)
Variable <- rownames(liner)
liner <- cbind(Variable, liner)


write.csv(liner, "test/liner.csv", fileEncoding = "CP950", row.names=FALSE)

# create a flextable object
ft <- flextable(data = liner)
ft <- autofit(ft)
ft <- add_footer_lines(ft, paste0("#All result of Adj-Beta were adjusted by ",
                                  gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/liner.docx")


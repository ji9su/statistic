library(lm.beta)
library(flextable)


data <- read.csv('data/護理人員照顧末期病人自覺準備度與照護需求_分析資料_合併檔1100819分析檔.csv', header = T, fileEncoding = 'CP950')

path.analysis_fun <- function(data = data, dependent, Continuous_variables, Categorical_variable, adjustment) {
  
  path_table <- data.frame(Coefficients = NA, p_value = NA)
  
  for(i in 1:length(Continuous_variables)) {
    if(is.null(adjustment)){
      model <- summary(lm.beta(lm(data[, dependent] ~ data[, Continuous_variables[i]])))
    } else if( length(adjustment) > 1){
      model <- summary(lm.beta(lm(data[, dependent] ~ data[, adjustment[1]] + data[, adjustment[2]] + data[, Continuous_variables[i]])))
    } else {
      model <- summary(lm.beta(lm(data[, dependent] ~ data[, adjustment] + data[, Continuous_variables[i]])))
    }
    Coefficients <- prod(model$coefficients[2:(2+length(adjustment)), 2])
    df <- model$df[2]
    T_value = abs(Coefficients*sqrt((df)/1-Coefficients^2))
    p_value = pt(T_value, df = df, lower.tail = FALSE) * 2
    path_table[Continuous_variables[i],] <- formatC(c(Coefficients, p_value), 3, format = "f")
  }
  
  if(!is.null(Categorical_variable)){
    for(j in 1:length(Categorical_variable)) {
      no_dummy <- length(unique(data[, Categorical_variable[j]]))
      dummy_matrix <- matrix(0, nrow = length(data[,Categorical_variable[j]]), ncol = no_dummy -1)
      for(i in 2:no_dummy){
        dummy_matrix[data[,Categorical_variable[j]] %in% levels(factor(data[,Categorical_variable[j]]))[i], i-1] <- 1
        if(is.null(adjustment)){
          model <- summary(lm.beta(lm(data[, dependent] ~ dummy_matrix[,i-1]))) 
        } else if(length(adjustment) > 1) {
          model <- summary(lm.beta(lm(data[, dependent] ~ data[, adjustment[1]] + data[, adjustment[2]]  + dummy_matrix[,i-1]))) 
        } else {
          model <- summary(lm.beta(lm(data[, dependent] ~ data[, adjustment] + dummy_matrix[,i-1])))
        }
        Coefficients <- prod(model$coefficients[2:(2+length(adjustment)), 2])
        df <- model$df[2]
        T_value = abs(Coefficients*sqrt((df)/1-Coefficients^2))
        p_value = pt(T_value, df = df, lower.tail = FALSE) * 2
        path_table[paste0(Categorical_variable[j], ".", i),] <- formatC(c(Coefficients, p_value), 3, format = "f")
      }
    }
  }
  
  path_table <- path_table[-1,]
  path_table[path_table[,1] ==  "-0.000", 1] = "0.000"
  path_table[path_table[,2] ==  "0.000", 2] = "<0.001"
  
  return(path_table)
}

print(colnames(data))
dependent <- "care"
Continuous_variables <- c("GSES")
Categorical_variable <- NULL
adjustment <- c("GSES")

path_table <- path.analysis_fun(data = data, dependent, Continuous_variables, Categorical_variable, adjustment)
adjustment <- "btw_知識總分"
path_table_1 <- path.analysis_fun(data = data[data["方式"] == 1,], dependent, Continuous_variables, Categorical_variable, adjustment)
adjustment <- NULL
path_table_2 <- path.analysis_fun(data = data, dependent, Continuous_variables, Categorical_variable, adjustment)

Continuous_variables <- NULL
adjustment <- NULL
path.analysis_fun(data = data, dependent, Continuous_variables, Categorical_variable = NULL, adjustment)


# create a flextable object
Variable <- rownames(path_table_2)
path_table <- cbind(Variable, path_table_2)


ft <- flextable(data = path_table)
ft <- autofit(ft)
# ft <- add_footer_lines(ft, paste0("#All result of Adj-Beta were adjusted by ",
#                                   gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/result/path_table.docx")


cbind(path_table_1, path_table)
merge(path_table, path_table_1, all.x = TRUE)




dependent <- "btw_行為總分"
adjustment <- "btw_態度總分"
variable <- "btw_知識總分"
model <- summary(lm.beta(lm(data[, dependent] ~  data[, variable] + data[, adjustment]))) # b, c'
# model <- summary(lm.beta(lm(data[, dependent] ~  data[, variable] + data[, adjustment])))
Coefficients <- prod(model$coefficients[2:(2+length(adjustment)), 2])
Coefficients <- 0.05
df <- model$df[2]
T_value = abs(Coefficients*sqrt((df)/1-Coefficients^2))
Coefficients/T_value
p_value = pt(T_value, df = df, lower.tail = FALSE) * 2
Coefficients + qt(0.975, df = df) * Coefficients
Coefficients - qt(0.975, df = df) * Coefficients


dependent <- "btw_行為總分"
variable <- "GSES"
model <- summary(lm.beta(lm(data[, dependent] ~  data[, variable])))  # c
# model <- summary(lm.beta(lm(data[, dependent] ~  data[, variable])))  # c

Coefficients <- prod(model$coefficients[2:2, 2])
Coefficients <- 0.0147
df <- model$df[2]
T_value = abs(Coefficients*sqrt((df)/1-Coefficients^2))
p_value = pt(T_value, df = df, lower.tail = FALSE) * 2
Coefficients + qt(0.975, df = df) * 0.09182
Coefficients - qt(0.975, df = df) * 0.09182


dependent <- "btw_態度總分"
variable <- "btw_知識總分"
model <- summary(lm.beta(lm(data[data["方式"] == 3, dependent] ~  data[data["方式"] == 3, variable])))  # c
# model <- summary(lm.beta(lm(data[, dependent] ~  data[, variable])))  # c
Coefficients <- prod(model$coefficients[2:2, 2])
Coefficients <- 9e-04
df <- model$df[2]
T_value = abs(Coefficients*sqrt((df)/1-Coefficients^2))
p_value = pt(T_value, df = df, lower.tail = FALSE) * 2
Coefficients + qt(0.975, df = df) * Coefficients
Coefficients - qt(0.975, df = df) * Coefficients









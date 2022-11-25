library(geepack)
library(flextable)

data <- read.csv("data/均衡飲食之態度知能與自我認知問卷2018.csv", header = TRUE, sep = ",", fileEncoding = "UTF-8")


gee_fun <- function(dependent = dependent, Continuous_variables = NA,
                    Categorical_variable = NA, adjustment_variable = NA,
                    times_variable = times_variable, ID = ID, data = data, family = "gaussian") {
  
  if(family == "gaussian") {
    gee_table = data.frame(Beta = NA, p_value = NA)
  } else {
    gee_table = data.frame(OR = NA, p_value = NA)
  }
  
  data = data[order(data[,ID]),]
  
  for(j in 1:length(Categorical_variable)){
    data[,Categorical_variable[j]] = as.factor(data[,Categorical_variable[j]])
  }
  
  variables <- c(Continuous_variables, Categorical_variable)
  variables <- variables[!is.na(variables)]
 
  for(i in 1:length(variables)) {
    if(all(is.na(adjustment_variable))) {
      if(is.na(times_variable)) {
        Formula <-  paste0(dependent, "~", variables[i])
      } else {
        data[,times_variable] = as.factor(data[,times_variable])
        Formula <-  paste0(dependent, "~", variables[i], "+", times_variable)
      }
    } else {
      adjust_variable = paste(adjustment_variable[adjustment_variable != variables[i]], collapse = "+")
      if(is.na(times_variable)) {
        Formula <-  paste0(dependent, "~", variables[i], "+", adjust_variable)
      } else {
        data[,times_variable] = as.factor(data[,times_variable])
        Formula <-  paste0(dependent, "~", variables[i], "+", times_variable, "+", adjust_variable)
      }
    }
    
    check_variable = c()
    list_items = strsplit(strsplit(Formula, "+", fixed = TRUE)[[1]], "~", fixed = TRUE)
    for(l in 1:length(list_items)){
      check_variable = c(check_variable, list_items[[l]])
    }
    
    
    gee <- tidy(geeglm(eval(parse(text=Formula)), id = ID, data = data[!rowSums(is.na(data[check_variable])),], family = family, corstr = "ar1"), conf.int = TRUE)
    
    if(is.null(levels(data[,variables[i]]))) {
      
      Beta <- gee[2, "estimate"]
      CI <- gee[2, c("conf.low", "conf.high")]
      P <- gee[2, "p.value"]
      if(family == "binomial") {
        Beta <- exp(Beta)
        CI <- exp(CI)
      }
      Beta <- formatC(unlist(Beta), 2, format = "f")
      CI <- formatC(unlist(CI), 2, format = "f")
      P <- formatC(unlist(P), 3, format = "f")

      gee_table[variables[i], ] <- c(paste0(Beta," (", CI[1], ", ", CI[2], ")"),  P)
      
    } else {
      
      cat_row <- levels(data[,variables[i]])
      
      Beta <- gee[2:length(cat_row), "estimate"]
      CI <- gee[2:length(cat_row), c("conf.low", "conf.high")]
      P <- gee[2:length(cat_row), "p.value"]
      if(family == "binomial") {
        Beta <- exp(Beta)
        CI <- exp(CI)
      }
      Beta <- as.data.frame(formatC(unlist(Beta), 2, format = "f"))
      CI <- as.data.frame(formatC(unlist(CI), 2, format = "f"))
      P <- as.data.frame(formatC(unlist(P), 3, format = "f"))


      gee_table[paste0(variables[i], ":"), ] <- c("", "")
      
      for(k in 1:length(cat_row)) {
        
        if(k == 1) {
          gee_table[paste0(variables[i], ":", cat_row[k]), ] <- c("Reference", "")
        } else {
         gee_table[paste0(variables[i], ":", cat_row[k]), ] <- c(paste0(Beta[k-1, 1], " (", CI[k-1, 1], ", ", CI[dim(CI)[1]/2 +k-1 , 1], ")"),  as.character(P[k-1, 1]))
        }
      }
    }
  }
  gee_table <- gee_table[-1,]
  return(gee_table)
}


colnames(data)

ID <- "ID"

dependent = "cognition"  # only Continuous variables
Continuous_variables <- c("self.efficacy")
Categorical_variable <- c("gender")

times_variable <- "time" # "time"

adjustment_variable <- c(NA)
family = "gaussian"  # binomial:logistic, gaussian:linear

gee_table <- gee_fun(dependent = dependent, Continuous_variables = Continuous_variables,
                     Categorical_variable = Categorical_variable, adjustment_variable = adjustment_variable,
                     times_variable = times_variable, ID = ID, data = data, family = family)

gee_table <- cbind(gee_table, gee_table1, gee_table2)
Variable <- rownames(gee_table)
gee_table <- cbind(Variable, gee_table)

write.csv(gee_table, "test/gee_table.csv", fileEncoding = "CP950", row.names=FALSE)



# create a flextable object
ft <- flextable(data = gee_table)
ft <- autofit(ft)
#ft <- add_footer_lines(ft, paste0("#All result of Adj-Beta were adjusted by ",
#                                  gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/result/gee_table.docx")



### test

summary(geeglm(final ~ factor(times), id = data[,'ID'], family="gaussian", corstr = "ar1", data = data))

ID <- "編號"
dependent = "經濟負荷"  # only Continuous variables
Continuous_variables <- c("年紀")

Categorical_variable <- c("病人性別")

times_variable <- c("時間")


fit1 <- geeglm(經濟負荷 ~ factor(時間) + factor(組別) + PPI總分, id = data[,'編號'],
               family="gaussian", corstr = "ar1", data = data)

summary(fit1)


gee <- summary(geeglm(經濟負荷 ~ factor(時間) + factor(病人宗教信仰) + factor(病人經濟來源) + PPI總分 , id = data[,'編號'],
               family="gaussian", corstr = "ar1", data = data))



sub("factor", "",rownames(fit1$coefficients)[-1])
fit1$coefficients[-1,c("Estimate", "Pr(>|W|)")]

2 * pnorm(abs(coef(summary(fit1))[,5]), lower.tail = FALSE)

summary(geeglm(final ~ factor(times)*factor(group) + age_family + factor(live.together) + factor(frequency) + close.level + age + factor(edu) + factor(religion) + factor(PPS) + factor(edema) + factor(Hospice.Care) + self, 
       id = data[,'ID'], family="gaussian", corstr = "ar1", data = data))


###交互作用

colnames(data)
#factor(times)*factor(group) + age_family + factor(live.together) + factor(frequency) + close.level + age + factor(edu) + factor(religion) + factor(PPS) + factor(edema) + factor(Hospice.Care) + self


gee <- tidy(geeglm(depression ~ factor(times)*factor(group) + factor(relationship) + factor(live.together) + factor(frequency) + close.level + age + factor(PPS) + factor(oral) + factor(end_of_life) + factor(Hospice.Care) + self, 
                      id = data[,'ID'], family="gaussian", corstr = "ar1", data = data), conf.int = TRUE)


Beta <- formatC(unlist(gee[, "estimate"]), 2, format = "f")
P <- formatC(unlist(gee[, "p.value"]), 3, format = "f")
CI_up <- formatC(unlist(gee[, "conf.high"]), 2, format = "f")
CI_low <- formatC(unlist(gee[, "conf.low"]), 2, format = "f")
table2 <- data.frame()
for(i in 1:length(Beta)) {
  # table1[i, "Variable"] <- gee[i, "term"]
  table2[i, "Beta"] <- paste0(Beta[i], " (", CI_low[i], ", ", CI_up[i], ")")
  table2[i, "p_value"] <- P[i]
}
table <- cbind(table, table1, table2)
write.csv(table, "test/gee_table_interation.csv", fileEncoding = "CP950", row.names=FALSE)

# create a flextable object
ft <- flextable(data = table)
ft <- autofit(ft)
#ft <- add_footer_lines(ft, paste0("#All result of Adj-Beta were adjusted by ",
#                                  gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "test/gee_table_interation.docx")




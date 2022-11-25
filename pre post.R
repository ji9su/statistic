
library(flextable)
library(ggm)

# pre_post analyse
# Continuous : paired-t test 
# Categorical : mcnemar
pre_post_function <- function(data, time_1, time_2, Continuous_variables = NA, Categorical_variable = NA) {
  
  grop_number <- levels(factor(data[,"time"]))
  pre_post_table <- as.data.frame(matrix(NA, nrow = 1, ncol = length(grop_number) + 2))
  colnames(pre_post_table) <- c(paste0("time", ":", grop_number), "mean difference", "p-value")
  
  if(!all(is.na(Continuous_variables))) {
    for(i in 1:length(Continuous_variables)) {
      pre_post_data = data.frame(time_1 = as.numeric(as.character(data[data[,"time"] %in% time_1, Continuous_variables[i]])),
                                 time_2 = as.numeric(as.character(data[data[,"time"] %in% time_2, Continuous_variables[i]])))
      
      pre_post_data = pre_post_data[!is.na(pre_post_data$time_1) & !is.na(pre_post_data$time_2),]
      
      m1 = mean(pre_post_data$time_1)
      sd1 = sd(pre_post_data$time_1)
      
      m2 = mean(pre_post_data$time_2)
      sd2 = sd(pre_post_data$time_2)
      
      m_diff = mean(pre_post_data$time_2 - pre_post_data$time_1)
      sd_diff = sd(pre_post_data$time_2 - pre_post_data$time_1)
      
      pre_post_data <- data.frame(group = c(rep(1, nrow(pre_post_data)), rep(2, nrow(pre_post_data))),
                                  score = c(pre_post_data$time_1, pre_post_data$time_2))
      
      paired_T  = t.test(score ~ group, data = pre_post_data, paired = TRUE)
      p_value  = paired_T$p.value
      
      m1 = formatC(m1, 2, format = "f")
      sd1 = formatC(sd1, 2, format = "f")
      
      m2 = formatC(m2, 2, format = "f")
      sd2 = formatC(sd2, 2, format = "f")
      
      m_diff = formatC(m_diff, 2, format = "f")
      sd_diff = formatC(sd_diff, 2, format = "f")
      
      p_value = formatC(p_value, 3, format = "f")
      if(p_value == "0.000") {p_value = "<0.001"}  
      
      pre_post_table[Continuous_variables[i], ] <- c(paste0(m1, " ± ", sd1), paste0(m2, " ± ", sd2), paste0(m_diff, " ± ", sd_diff), p_value)
      
    }
  }
  
  if(!is.na(Categorical_variable)) {
    for(j in 1:length(Categorical_variable)) {
      Categorical_number = levels(factor(data[,Categorical_variable[j]]))
      pre_post_data = data.frame(time_1 = as.integer(as.character(data[data$time == time_1, Categorical_variable[j]])),
                                 time_2 = as.integer(as.character(data[data$time == time_2, Categorical_variable[j]])))
      
      pre_post_data = pre_post_data[!is.na(pre_post_data$time_1) & !is.na(pre_post_data$time_2),]
      time_table_1 = table(pre_post_data$time_1)
      percentage_1 = time_table_1/sum(time_table_1) * 100
      percentage_1 = formatC(percentage_1, 1, format = "f")
      
      time_table_2 = table(pre_post_data$time_2)
      percentage_2 = time_table_2/sum(time_table_2) * 100
      percentage_2 = formatC(percentage_2, 1, format = "f")
      
      mcnemar_table = table(pre_post_data)
      mcnemar = mcnemar.test(mcnemar_table, correct = FALSE)
      p_value = mcnemar$p.value
      
      p_value = formatC(p_value, 3, format = "f")
      if(p_value == "0.000") {p_value = "<0.001"}
      
      pre_post_table[Categorical_variable[j], ] <- c("", "", "", p_value)
      for(l in 1:length(Categorical_number)) {
        pre_post_table[paste0(Categorical_variable[j], ":", Categorical_number[l]), ] <- c(paste0(time_table_1[l], "(", percentage_1[l], "%)"),
                                                                                           paste0(time_table_2[l], "(", percentage_2[l], "%)"),
                                                                                           "", "")
      }
    }
  }
  
  pre_post_table <- pre_post_table[2:nrow(pre_post_table),]
  Variable <- rownames(pre_post_table)
  pre_post_table <- cbind(Variable, pre_post_table)
  
  # create a flextable object
  ft <- flextable(data = pre_post_table)
  ft <- autofit(ft)
  # ft <- add_footer_lines(ft, "#: Testing by Fisher exact test, Wilcoxon Test, or Kruskal-Wallis Test, respectively.")
  # save it as a .docx file on your disk
  flextable::save_as_docx(ft, path = "result/pre_post_table.docx")
}


df <- read.csv('data/偏鄉減糖前後測.csv', header = T, fileEncoding = 'UTF-8')
colnames(df)
Continuous_variables <- c("hight", "weight", "BMI", "knowledge", "self")
Categorical_variable <- c("level", "eyes", colnames(df)[10:35], colnames(df)[37:41])
# Categorical_variable <- c("eyes")

pre_post_df = df[df$time %in% c(1, 2),]
time_1 = 1
time_2 = 2

pre_post_function(data = pre_post_df, time_1 = time_1, time_2 = time_2,
                  Continuous_variables = Continuous_variables, Categorical_variable = Categorical_variable)



colnames(data)
pc <- pcor(c("self.efficacy", "confidence", "group"), var(data))
pc
pcor.test(pc, 1, 72)


df_2 = data[data$次數 == 2, ]

name = as.character(unique(df_2$X.1.您的姓名))
item = c("知識總分", "態度總分", "行為總分")
for(j in 1:length(item)) {
  for(i in 1:length(name)) {
    df_2[df_2$X.1.您的姓名 == name[i], paste0("btw_", item[j])] = df_2[df_2$X.1.您的姓名 == name[i], item[j]] - data[data$次數 == 1 & data$X.1.您的姓名 == name[i], item[j]] 
    
  }
}

data = df_2

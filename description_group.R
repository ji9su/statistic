library(flextable)

data = read.csv("data/Example data.csv", fileEncoding = "CP950", header = TRUE, sep = ",")

description_group <- function(data, group, Continuous_variables, Categorical_variable) {
  
  grop_number <- levels(factor(data[,group]))
  description_table <- as.data.frame(matrix(NA, nrow = 1, ncol = length(grop_number) + 1))
  colnames(description_table) <- c(paste0(group, ":", grop_number), "p-value")
  
  ###Continuous
  
  N <- length(data[!is.na(data[,group]), group])
  
  for(j in 1:length(Continuous_variables)) {
    dim_description <- dim(description_table)
    for(i in 1:(dim_description[2]-1)) {
      M <- mean(data[data[,group] == grop_number[i], Continuous_variables[j]], na.rm = TRUE)
      S <- sd(data[data[,group] == grop_number[i], Continuous_variables[j]], na.rm = TRUE)
      M <- formatC(M, 2, format = "f")
      S <- formatC(S, 2, format = "f")
      description_table[j,i] <- paste0(M, " ± ", S)
    }
    
    if(length(grop_number) >=3 ) {
      if(N > 25) {
        Variance.table = aov(data[,Continuous_variables[j]]~as.factor(data[,group]))
        model = anova(Variance.table)
        p_value = model$`Pr(>F)`
        p_value <- formatC(p_value[1], 3, format = "f")
        description_table[j,"p-value"] = p_value
      } else {
        model = kruskal.test(data[,Continuous_variables[j]] ~ data[,group])
        p_value = model$p.value
        p_value <- formatC(p_value[1], 3, format = "f")
        description_table[j,"p-value"] = paste0(p_value, "#")
      }
    } else {
      if(N　> 25) {
        #var.result = var.test(data[,Continuous_variables[j]] ~ data[,group])
        #if (var.result[['p.value']] < 0.05) {
        #  model = t.test(data[,Continuous_variables[j]] ~ data[,group], var.equal = FALSE)
        #  p_value = model$p.value
        #} else {
        model = t.test(data[,Continuous_variables[j]] ~ data[,group], var.equal = TRUE)
        p_value = model$p.value
        p_value <- formatC(p_value[1], 3, format = "f")
        description_table[j,"p-value"] = p_value
        #}
      } else {
        model = wilcox.test(data[,Continuous_variables[j]] ~ data[,group])
        p_value = model$p.value
        p_value <- formatC(p_value[1], 3, format = "f")
        description_table[j,"p-value"] = paste0(p_value, "#")
      }
    }
    
    if(j == 1) {
      rownames(description_table) <- Continuous_variables[j]
    } else {
      rownames(description_table)[dim_description[1] + 1] <- Continuous_variables[j]
    }
  }
  
  ###Categorical 
  
  for(j in 1:length(Categorical_variable)) {
    
    table <- table(data[,c(Categorical_variable[j], group)])
    marginal.n1 = rep(NA, nrow(table))
    for (i in 1:nrow(table)) {
      marginal.n1[i] = sum(table[i,])
    }
    
    marginal.n2 = rep(NA, ncol(table))
    for (i in 1:ncol(table)) {
      marginal.n2[i] = sum(table[,i])
    }
    expected.TABLE = marginal.n1%*%t(marginal.n2)/sum(table)
    
    expected_result = expected.TABLE > 5
    
    if(sum(expected_result)/length(expected_result) > 0.8) {
      model <- chisq.test(table)
      p_value <- formatC(model$p.value, 3, format = "f") 
      dim_description <- dim(description_table)
      description_table[dim_description[1] + 1 ,"p-value"] <- p_value
      rownames(description_table)[dim_description[1] + 1] <- Categorical_variable[j]
    } else {
      model <- fisher.test(table)
      p_value <- formatC(model$p.value, 3, format = "f") 
      dim_description <- dim(description_table)
      description_table[dim_description[1] + 1 ,"p-value"] <- paste0(p_value, "#")
      rownames(description_table)[dim_description[1] + 1] <- Categorical_variable[j]
    }
    
    #if(j == 1){
    #  description_table[dim_description[1], "p-value"] <- p_value
    #  rownames(description_table)[dim_description[1]] <- Categorical_variable[j]
    #} else {
    #  description_table[dim_description[1] + 1 ,"p-value"] <- p_value
    #  rownames(description_table)[dim_description[1] + 1] <- Categorical_variable[j]
    #}
    
    for(k in 1:length(grop_number)) {
      percentage <- table[,k]/marginal.n2[k]*100
      if(k == 1){
        Categorical_percentage <- percentage
      } else {
        Categorical_percentage <- rbind(Categorical_percentage, percentage)
      }
    }
    Categorical_percentage <- formatC(Categorical_percentage, 1, format = "f")
    Categorical_number <- levels(factor(data[,Categorical_variable[j]]))
    description_table[paste0(Categorical_variable[j], Categorical_number), 1:(dim_description[2]-1)] <- paste0(table, " (", t(Categorical_percentage), "%)")
    
    description_table[is.na(description_table)] <- ""
  }
  
  Variable <- rownames(description_table)
  description_table <- cbind(Variable, description_table)
  
  return(description_table)
}

colnames(data)

group <- "Income"   # only Categorical variables
Continuous_variables <- c("eGFR", "SBP", "DBP")
Categorical_variable <- c("Education")

description_group_table <- description_group(data, group, Continuous_variables, Categorical_variable)

write.csv(description_group_table, "result/description_group_table.csv", fileEncoding = "UTF-8", row.names=FALSE)

# create a flextable object
ft <- flextable(data = description_group_table)
ft <- autofit(ft)
ft <- add_footer_lines(ft, "#: Testing by Fisher exact test, Wilcoxon Test, or Kruskal-Wallis Test, respectively.")
# save it as a .docx file on your disk
flextable::save_as_docx(ft, path = "result/description_group_table.docx")


library(flextable)
library("polycor")

data<- read.csv("test/量表.csv", header = TRUE, sep = ",", fileEncoding = "CP950")

my_func.correlation = function (x, y) {
  
  if (length(x) != length(y)) {
    
    cat("x與y不等長")
    
  } else {
    
    
    lvl.x = levels(factor(x))
    n.lvl.x = length(lvl.x)
    
    lvl.y = levels(factor(y))
    n.lvl.y = length(lvl.y)
    
    if (n.lvl.x > 10 & n.lvl.y > 10) {
      
      n.sample = sum(!is.na(x) & !is.na(y))
      
      if (n.sample >= 25) {
        
        result = cor.test(x, y, method = "pearson")
        method = "pearson"
        
      } else {
        
        result = cor.test(x, y, method = "spearman")
        method = "spearman"
        
      }
      
      rho = result$estimate
      p_val = result$p.value
      
    } else {
      
      if (n.lvl.x < 10) {
        
        if (n.lvl.y < 10) {
          
          result = polychor(x, y, std.err=TRUE)
          Z = result$rho/sqrt(result$var)
          CHISQ = Z^2
          p_val = pchisq(CHISQ, df = 1, lower.tail = FALSE)
          method = "polychor"
          
        } else {
          
          result = polyserial(y, x, std.err=TRUE)
          p_val = pchisq(result$chisq, result$df, lower.tail = FALSE)
          method = "polyserial"
          
        }
        
      } else {
        
        result = polyserial(x, y, std.err=TRUE)
        p_val = pchisq(result$chisq, result$df, lower.tail = FALSE)
        method = "polyserial"
        
      }
      
      rho = result$rho
      
    }
    
    my_list = list(rho, p_val, method)
    names(my_list) = c('rho', 'p_val', 'method')
    
    my_list
    
  }
  
}

my_func.correlation_matrix = function (data) {
  
  correlation_matrix = matrix(1, nrow = ncol(data), ncol = ncol(data))
  rownames(correlation_matrix) = colnames(data)
  colnames(correlation_matrix) = colnames(data)
  
  for (i in 1:ncol(data)) {
    
    for (j in 1:ncol(data)) {
      
      if (j < i) {
        
        result_list = my_func.correlation(data[,i],data[,j])
        correlation_matrix[j,i] = paste0(formatC(result_list$rho, 3, format = "f"), " (", formatC(result_list$p_val, 3, format = "f"), ")")
        #print(result_list$rho)
        #print(result_list$p_val)
        #correlation_matrix[i,j] = result_list$p_val
        correlation_matrix[i,j] = "-"
        
      }
      
    }
    
  }
  
  correlation_matrix
  
}


correlation_table <- my_func.correlation_matrix(data = data)
Variable <- rownames(correlation_table)
correlation_table <- cbind(Variable, correlation_table)
write.csv(correlation_table, "test/correlation_table.csv", fileEncoding = "UTF-8", row.names=FALSE)


# create a flextable object
ft <- flextable(data = as.data.frame(correlation_table))
ft <- autofit(ft)
#ft <- add_footer_lines(ft, paste0("#All result of Adj-Beta were adjusted by ",
#                                  gsub("\\+", ", ",gsub(" ","", adjustment_variable)),"."))
# save it as a .docx file on your disk

flextable::save_as_docx(ft, path = "test/correlation_table.docx")

colnames(data)

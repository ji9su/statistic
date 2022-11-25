
path <- function(data, independent, dependent, adjustment= NULL) {
  
  if(is.null(adjustment)) {
    model <- summary(lm.beta(lm(data[, dependent] ~  data[, independent])))
  } else {
    model <- summary(lm.beta(lm(data[, dependent] ~  data[, independent] + data[, adjustment])))
  }
  
  
  # Coefficients + qt(0.975, df = df) * model$coefficients[2,"Std. Error"]
  # Coefficients - qt(0.975, df = df) * model$coefficients[2,"Std. Error"]
  
  Coefficients <- formatC(c(model$coefficients[2:(2+length(adjustment)), 2]), 3, format = "f")
  sd <- formatC(c(model$coefficients[2:(2+length(adjustment)),"Std. Error"]), 2, format = "f")
  p_value <- model$coefficients[2:(2+length(adjustment)), 5]
  
  p_str <- c()
  for(i in 1:length(p_value)) {
    if(p_value[i] < 0.001) {
      p_str <- c(p_str, "***")
    } else if(p_value[i] < 0.01) {
      p_str <- c(p_str, "**")
    } else if(p_value[i] < 0.05) {
      p_str <- c(p_str, "*")
    } else {
      p_str <- c(p_str, "")
    }
  }
  
  return(list(Coefficients = Coefficients, sd = sd, p_str = p_str))
}

path_plot <- function(data, independent_variable, adjustment_variable, dependent_variable, bootse, bootLLCI, bootULCI) {
  
  name = paste0("result/path_plot/", independent_variable, "_", adjustment_variable, "_", dependent_variable, ".jpg")
  jpeg(name, width = 967, height = 548)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))
  
  # variable
  x = c(1.5, 1.5, 3.5, 3.5, 1.5)
  y = c(1, 3, 3, 1, 1)
  lines(x, y, lwd = 2)
  
  # adjustment
  x = c(4, 4, 6, 6, 4)
  y = c(6, 8, 8, 6, 6)
  lines(x, y, lwd = 2)
  
  # dependent
  x = c(6.5, 6.5, 8.5, 8.5, 6.5)
  y = c(1, 3, 3, 1, 1)
  lines(x, y, lwd = 2)
  
  arrows(x0 = 3.5, y0 = 2, x1 = 6.5, y1 = 2, angle = 20, lwd = 2)
  arrows(x0 = 2.5, y0 = 3, x1 = 4, y1 = 7, angle = 20, lwd = 2)
  arrows(x0 = 6, y0 = 7, x1 = 7.5, y1 = 3, angle = 20, lwd = 2)
  
  
  text(2.5, 2, independent)
  text(5, 7, adjustment_variable)
  text(7.5, 2, dependent_variable)
  
  
  # a
  a_list <- path(data = data, independent = independent_variable, dependent = adjustment_variable)
  a_str <- paste0("a : ", a_list$Coefficients, " (",  a_list$sd, ")", a_list$p_str)
  text(2.5, 5, a_str)
  
  # c
  c_list <- path(data = data, independent = independent_variable, dependent = dependent_variable)
  c_str <- paste0("c : ", c_list$Coefficients, " (",  c_list$sd, ")", c_list$p_str)
  text(2.5, 5, a_str)
  
  # b, c'
  b_c_list <- path(data = data, independent = independent_variable, 
                   adjustment = adjustment_variable, dependent = dependent_variable)
  b_str <- paste0("b : ", b_c_list$Coefficients[2], " (",  b_c_list$sd[2], ")", b_c_list$p_str[2])
  text(7.5, 5, b_str)
  
  
  c_dot_list_str <- paste0("cˊ : ", b_c_list$Coefficients[1], " (",  b_c_list$sd[1], ")", b_c_list$p_str[1])
  Coef <- as.numeric(a_list$Coefficients) * as.numeric(b_c_list$Coefficients[2])
  Coef_str <- formatC(c(Coef), 3, format = "f")
  text(5, 2.5, paste0(c_str, "  ", c_dot_list_str))
  text(5, 1.5, paste0("a × b : ", Coef_str, "(", bootse, ")\n Bias-corrected 95% CI : ", bootLLCI, ", ", bootULCI))
  dev.off()
  
}

data <- read.csv('data/災難準備度與因應能力.csv', header = T, fileEncoding = 'CP950')
colnames(data)

independent_variable <- "STRESS"
adjustment_variable <- "EFFICACY"
dependent_variable <- "SECONDTRAMA"
bootse = "0.02"
bootLLCI = "-0.07"
bootULCI = "0.01"

path_plot(data, independent_variable, adjustment_variable, dependent_variable, bootse, bootLLCI, bootULCI)




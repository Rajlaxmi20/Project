

#' Research question 1
#'
#' @description This function helps to run the two sample t test in order to answer whether the height and weight are linearly related.
#' @param file_name This is the name of the data set
#'
#' @return A statistical test to answer the research question.
#' @export
#' @import stats utils readr magrittr ggplot2


question1 <- function(file_name) {
  Name <- read.csv(file_name)
  #hypothesis
  hypothesis1 <- cat("HYPOTHESIS", "\n") #outputs the objects
  cat(" Null hypothesis - H0: Beta = 0 \n Alternative hypothesis - H1: Beta =! 0 \n where Beta is the slope" , "\n")

  assumptions1 <-
    (mod <- lm(weight ~ height, data = Name)) #performs the linear regression
  Y_vs_X <-  ggplot(data = Name, mapping = aes(x = height, y = weight)) + #creates an empty plot
    geom_point() + #creates the scatter plot
    labs(title = "Scatter plot for height vs weight", x = "height", y = "weight") #used to add title for x and y axis and graph


  res_vs_fitvals <- ggplot(data = data.frame( #creates an empty plot
    resid <- mod$residuals, #saves the residuals as resid
    fitted <- mod$fitted.values)) + #saves the fitted values as fitted
    geom_point(mapping = aes(x = fitted, y = resid)) + #creates scatter plot
    geom_hline(aes(yintercept = 0), colour = "red") #creates a horizontal line at y = 0
  labs(title = "Residuals vs fitted values", x = "Fitted values", y = "Residuals") #labels the axes and the graph



  histogram <- ggplot(data = data.frame(resid <- mod$residuals)) + #creates an empty plot
    geom_histogram(aes(x = resid), binwidth = 2) + #produces histogram
    labs(title = "Histogram of Residuals", x = "Residuals") #labels the x axis and the graph

  print(Y_vs_X / (res_vs_fitvals + histogram)) #the patchwork package is used to position the graphs on the same screen


  fit1 <- cat("\n LINEAR REGRESSION RESULTS\n")
  mod <- lm(weight ~ height, data = Name) #performs the linear regression when file name is entered
  sum1 <- broom::tidy(summary(mod), conf.int = TRUE)
  sum1

  p_value1 <- sum1$p.value[2] #p-value

  est_slope1 <- sum1$estimate[2] #estimated slope of beta hat

  t1 <- sum1$statistic[2] #t value

  lowerbound <- sum1$conf.low[2]
  upperbound <- sum1$conf.high[2]

  DF1 <- mod$df.residual#degrees of freedom

  results <- glue::glue("The estimated slope is {est_slope1}
                         The number of degrees of freedom are {DF1}
                         The test statistic equals {t1}
                         The p-value equals {p_value1}
                         The lower bound at 5% significance level is {lowerbound}
                         The upper bound at 5% significance level is {upperbound}")
  print(results)


  decision1 <- cat("DECISION", "\n")
  if({p_value1} < 0.05) {
    cat("Reject null hypothesis", "\n")
  } else {cat("Do not reject null hypothesis", "\n")}
  decision1

  conclusion1 <- cat("CONCLUSION", "\n")
  if({p_value1} < 0.05) {
    cat("As beta is non zero, there is a linear relationship between height and weight.", "\n")
  } else {cat("As beta is zero, there is no linear relationship between height and weight" , "\n")
  }
}


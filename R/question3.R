
#' Research question 3
#'
#' @description Answers whether there any association between gender and the amount of physical activity
#' @param file_name This is the name of the data set
#'
#' @return A statistical test to answer the research question.
#' @export
#' @import stats utils readr magrittr ggplot2
#'

question3 <- function(file_name) {
  Name <- read.csv(file_name)
  hypothesis3 <- cat("HYPOTHESIS", "\n") #outputs the objects
  cat(" Null hypothesis - H0: Gender and physical activity are not related \n Alternative hypothesis - H1: Gender and physical activity are  related", "\n")

  fit3 <- cat("\n CHI SQUARED TEST RESULTS\n")
  chi_squared_test <- chisq.test(Name$gender, Name$phys, correct = FALSE) #performs chi-squared test
  sum3 <- broom::tidy(chisq.test(Name$gender, Name$phys, correct = FALSE)) #performs the two sample t test
  sum3

  t3 <- sum3$statistic #test statistic
  p_value3 <- sum3$p.value #p-value


  results3 <- glue::glue(" The test statistic equals {t3}
                         The p-value equals {p_value3}")

  print(results3)

  decision3 <- cat("DECISION", "\n")
  if({p_value3} < 0.05) {
    cat("Reject null hypothesis", "\n")
  } else {cat("Do not reject null hypothesis", "\n")}
  decision3

  conclusion3 <- cat("CONCLUSION", "\n")
  if({p_value3} < 0.05) {
    cat("As null hypothesis is rejected,  the gender and physical ability are dependent", "\n")
  } else {cat("As null hypothesis is not rejected,  the gender and physical ability are independent" , "\n")
  }
}










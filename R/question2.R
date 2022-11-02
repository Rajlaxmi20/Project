
#' Research question 2
#'
#' @description This function helps to run the two sample t test.
#' @param file_name This is the name of the data set
#'
#' @return A statistical test to answer the research question.
#' @export
#' @import stats utils readr magrittr ggplot2
#'

question2 <- function(file_name) {
  Name <- read.csv(file_name)
  #hypothesis
  hypothesis2 <- cat("HYPOTHESIS", "\n") #outputs the objects
  cat(" Null hypothesis - H0: mu1 = mu2 \n Alternative hypothesis - H1: mu1 =! mu2 \n where mu1 and mu2 are the means of the male and female height ", "\n")

  assumptions2 <- qq_plot <- ggplot(data = Name, mapping = aes(sample = height)) + geom_qq() + geom_qq_line() + #produces qq plot
    facet_wrap(~gender) +  xlab("Theoretical") + ylab("sample") + #labels the x and y axes
    ggtitle("QQ plots") #labels the graph

  box_plots<- ggplot(data = Name, aes(y = height, x = gender)) + geom_boxplot() +
    coord_flip() + xlab("Height") + ylab("Gender") + ggtitle("Box plots") #produces empty plot with boxplot on it

  print(qq_plot / box_plots)

  fit2 <- cat("\n TWO SAMPLE T TEST RESULTS\n")
  t_test <- t.test(height~gender,data = Name, var.equal = TRUE) #performs the two sample t test
  sum2 <- broom::tidy(t.test(height~gender,data = Name, var.equal = TRUE)) #performs the two sample t test
  sum2

  t2 <- sum2$statistic #test statistic
  p_value2 <- sum2$p.value #p-value
  lowerbound2 <- sum2$conf.low
  upperbound2 <- sum2$conf.high


  results2 <- glue::glue(" The test statistic equals {t2}
                         The p-value equals {p_value2}
                         The lower bound at 5% significance level is {lowerbound2}
                         The upper bound at 5% significance level is {upperbound2}")
  print(results2)


  decision2 <- cat("DECISION", "\n")
  if({p_value2} < 0.05) {
    cat("Reject null hypothesis", "\n")
  } else {cat("Do not reject null hypothesis", "\n")}
  decision2

  conclusion2 <- cat("CONCLUSION", "\n")
  if({p_value2} < 0.05) {
    cat("As null hypothesis is rejected,  the mean height of male and female is not the same", "\n")
  } else {cat("As null hypothesis is not rejected, the mean height of male and female is same" , "\n")
  }
}

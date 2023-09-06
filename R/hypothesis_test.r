#' Hypothesis testing

#' @description
#' `f_ttest` is used to calculate the p-value of an equality test (based on a 
#' Student t distribution) on linear regression (`lm` class) coefficients.

#' @param l_lreg is the linear regression model (`lm` class)
#' @param i_coef_num is an integer used to select a linear regression coefficient.
#' By example, if the model is a simple linear regression, then 1 and 2 correspond
#' to the intercept and the slope respectively.
#' @param n_val is the numeric value used by the equality test. By selecting 
#' `n_val` = 0, the null hypothesis is to test if the linear regression coefficient
#' (selected from `i_coef_num`) is equal to 0.

#' @encoding UTF-8
 
f_ttest <- function(l_lreg, i_coef_num, n_val){
	df_coef <- stats::coef(summary(l_lreg))
	n_tstat <- (df_coef[i_coef_num, 1] - n_val) / df_coef[i_coef_num, 2]
	return(2 * stats::pt(abs(n_tstat), l_lreg$df.residual, lower.tail = F))
}

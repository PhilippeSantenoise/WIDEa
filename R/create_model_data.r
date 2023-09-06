#' Creating model data

#' @description
#' `f_create_model_data` returns a data frame with 1 or 2 columns: (1) fitted
#' values and (2) residual variance if "sigma" is informed in loaded data 
#' ("Model parameter loading" section).

#' @param v_f_val is the fitted values (calculated from the `f_check_model`
#' function).
#' @param v_vfun_val is the residual variance (calculated from the `f_check_model`
#' function).

#' @encoding UTF-8

f_create_model_data <- function (v_f_val, v_vfun_val) {
	if (length(v_vfun_val) > 0) {
		df_model <-  data.frame("fit" = v_f_val, "variance" = v_vfun_val)
	}
	else {
		df_model <- data.frame("fit" = v_f_val)
	}
	
	return (df_model)
}

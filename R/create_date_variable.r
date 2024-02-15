#' Creating the date variable

#' @description
#' `f_create_date_variable` is used to create the date variable (only used for the
#'  temporal data type).

#' @param df_all are data saved in the e_data environment (e_data$all).
#' @param s_x_var is the X variable name
#' @param s_date_format is the date format (values from the format field in
#' "Variable selection").

#' @encoding UTF-8

f_create_date_variable <- function (df_all, s_x_var, s_date_format) {
	df_all[, s_x_var] <- gsub("/| |:|-|[.]|h|T|Z", "", as.vector(as.character(df_all[, s_x_var])))
	v_date <- as.character(format(strptime(df_all[, s_x_var], format = s_date_format)))
	return (v_date)
}

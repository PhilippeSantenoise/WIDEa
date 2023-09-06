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
	df_all[, s_x_var] <- as.vector(df_all[, s_x_var])
	v_range <- c()
	
	if (is.numeric(df_all[, s_x_var]) & length(grep("[.]", df_all[, s_x_var])) == 0) {
		v_range <- range(df_all[, s_x_var])
	}
	else {
		df_all[, s_x_var] <- gsub("/| |:|-|[.]|h", "", df_all[, s_x_var])
	}
	
	v_date <- as.character(strptime(df_all[, s_x_var], format = s_date_format))
	i_num <- length(which(is.na(v_date)))
	
	if (length(v_range) > 0) {
		if (v_range[1] > 0 & v_range[2] < unclass(as.Date(Sys.time()) + 1)) {
			if (i_num > 0) {
				v_date <- as.character(as.Date(df_all[, s_x_var], origin = "1970-01-01", tz = "GMT"))
			}
		}
	}
	
	return (v_date)
}

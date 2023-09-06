#' Inventory of statistical methods (Statistics tab) available for all data types

#' @description
#' `f_stat_method_ini` returns a list including (1) a data frame with six columns
#' (statistic method information) and (2) two lists with information on preliminary
#' checks executed for statistic methods.
#' \cr\cr The four columns are:
#' \cr("data_type") The data type;
#' \cr("name") The method name; 
#' \cr("check_process") A value used to define which methods have (= 1) or not (= 0)
#' preliminary checks and which methods have returned an error after preliminary
#' checks (= -1);
#' \cr("message") The method name returned by a warning/error message (corresponding
#' to preliminary checks);
#' \cr("code") The code associated to preliminary checks applied on statistical 
#' methods. Statistical methods with a same code have same preliminary checks;
#' \cr("click") A binary value updated when a method is added/removed (check boxes
#' in Statistics tab) in the current graph.
#' \cr\cr The two lists return information about (i) group levels (Group selectize
#' input) added to the main plotly graph legend after preliminary checks have been
#' applied and (ii) warning/error messages (described by a code: 1, 0 or -1). The 
#' warning/error message is used to inform users about group levels which
#' preliminary checks failed. 0 (initial value) means no preliminary checks have
#' been executed for selected statistical methods. The code is equal to 1 (-1 resp.)
#' if a (no resp.) message must be displayed after preliminary checks. 

#' @encoding UTF-8

f_stat_method_ini <- function() {
	# Dataframe with 6 columns
	# ------------------------
	
	df_inv <- data.frame(
		"data_type" = c(
			rep("normal", 5),
			"ir"
		),
		"name" = c(
			"lreg", "conf_ellipsoid", "centroid", "dens_curve", "norm_dens_curve", 
			"mean_spect"
		),
		"leg_name" = c(
			"lreg", "ellipsoid", "centroid", "curve", "normal curve", 
			"mean"
		),
		"check_process" = c(
			rep(1, 2), 0, rep(1, 2),
			0
		),
		"message" = c(
			"linear regression", "confidence ellipsoid", NA, "density curve", "normal density curve", 
			NA
		),
		"code" = c(
			rep("cp1", 2), NA, rep("cp2", 2),
			NA
		)
	)
	
	df_inv$click <- 0
	
	# List of methods with preliminary checks
	# ---------------------------------------
	
	l_level <- eval(parse(text = paste0("list(", paste(paste0(as.vector(sort(unique(df_inv$code[!is.na(df_inv$code)]))), " = c()"), collapse = ", "), ")")))
	l_message <- eval(parse(text = paste0("list(", paste(paste0(df_inv[which(df_inv$check_process == 1), "name"], " = 0"), collapse = ", "), ")")))
	
	return(list(inv = df_inv, level = l_level, message = l_message))
}

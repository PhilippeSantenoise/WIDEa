#' Checking process on Graphic tab inputs (tp1)

#' @description
#' `f_check_tp1_inputs` is used to check tp1 input values (decimal number, histplot
#' bin width, Y-scale border fraction) and returns an error/warning message if one
#' of these input values are not correctly informed.

#' @param l_opt_name is the list of tp1 input IDs ("dec_num", "bw", "fraction").
#' @param o_plot is a reactive value including data information of the main plotly
#' graph.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.

#' @encoding UTF-8

f_check_tp1_inputs <- function (l_opt_name, o_plot = NULL, o_parameter = NULL) {
	v_message <- c()
	
	for (i in 1:length(l_opt_name)) {
		# Decimal number
		
		if (names(l_opt_name)[i] == "dec_num") {
			if (is.na(l_opt_name[[i]])) {
				v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
			}
			else {
				if (is.numeric(l_opt_name[[i]]) == F | length(grep("[.]", l_opt_name[[i]])) > 0) {
					v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
				}
				else {
					if (l_opt_name[[i]] < 0) {
						v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
					}
				}
			}
		}
		
		# Bin width (histplot)
		
		if (names(l_opt_name)[i] == "bw") {
			if (!is.numeric(l_opt_name[[i]])) {
				v_message <- c(v_message, "Incorrect bin width")
			}
			else {
				if (!is.na(shiny::isolate(o_parameter$f))) {
					n_val <- abs(diff(range(as.vector(shiny::isolate(o_plot$data)[, ".f."]))))
				}
				else {
					n_val <- abs(diff(range(as.vector(shiny::isolate(o_plot$data)[, shiny::isolate(o_parameter$x)]))))
				}
				
				if (l_opt_name[[i]] <= 0 | l_opt_name[[i]] >= n_val) {
					v_message <- c(v_message, paste0("The bin width must be between 0 and ", round(n_val, digits = 2)))
				}
			}
		}
	
		# Y-scale border fraction (temporal/IR data type)
		
		if (names(l_opt_name)[i] == "fraction") {
			if (is.na(l_opt_name[[i]])) {
				v_message <- c(v_message, "Fraction value is not numeric. So, the value is replaced by the standard value: 0.05.")
			}
			else {
				if (l_opt_name[[i]] < 0) {
					v_message <- c(v_message, "Fraction value is inferior to the minimal value (0). So, the value is replaced by the minimal value.")
				}
				
				if (l_opt_name[[i]] > 0.1) {
					v_message <- c(v_message, "Fraction value is superior to the maximal value (0.1). So, the value is replaced by the maximal value.")
				}
			}
		}
	}
	
	return(v_message)
}


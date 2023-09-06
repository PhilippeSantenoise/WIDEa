#' Data frame including error/warning messages associated to statistic methods added
#' to the (main) plotly graph 

#' @description
#' `f_create_stat_method_message` returns a data frame including all error/warning 
#' messages for the selected statistic methods (`v_stat_method`). An error/warning
#' message is returned if the checking process associated to statistic methods has
#' failed (insufficient data, etc.). If a Group variable (selectize input) is added 
#' to the main plotly graph, then a warning message can be returned when the
#' checking process has not failed for all group levels (else an error message will
#' be returned).

#' @param df_stat_method is the statistic method inventory created with the 
#' `f_stat_method_ini` function.
#' @param l_stat_method_level is a list including group levels (Group selectize 
#' input) for which the checking process has not failed.
#' @param v_stat_method is the vector of statistic method name (UI's input ID).
#' @param o_parameter is a reactive value including plotly graph information.
#' @param df_all are data save in o_plot$data.

#' @encoding UTF-8

f_create_stat_method_message <- function(df_stat_method, l_stat_method_level, v_stat_method, o_parameter, df_all) {
	df_stat_method <- df_stat_method[which(df_stat_method$name %in% v_stat_method), c("name", "message", "code")]
	v_message <- c()
	v_type <- c()
	
	for (i in as.vector(unique(df_stat_method$code))) {
		v_name <- paste(as.vector(unique(df_stat_method[which(df_stat_method$code == i), "message"])), collapse = ", ")
		
		if (length(l_stat_method_level[[i]]) == 0) { # error message
			v_type <- c(v_type, "error")
			
			if (i == "cp1") {
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 3) or the standard deviation of ", ifelse(shiny::isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to 0", ifelse(!is.na(shiny::isolate(o_parameter$group)), " for each group", "")))
			}
			else { # cp2
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 2) or the standard deviation of X variable is equal to 0", ifelse(!is.na(shiny::isolate(o_parameter$group)), " for each group", "")))
			}
		}
		else { # warning message
			v_type <- c(v_type, "warning")
			v_group <- as.vector(unique(df_all[, shiny::isolate(o_parameter$group)]))
			v_pos <- which(!v_group %in% l_stat_method_level[[i]])
			
			if (i == "cp1") {
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 3) or the standard deviation of ", ifelse(shiny::isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to 0 for one or more groups (", paste(v_group[v_pos], collapse = ", "), ")"))
			}
			else { # cp2
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 2) or the standard deviation of X variable is equal to 0 for one or more groups (", paste(v_group[v_pos], collapse = ", "), ")"))
			}
		}
	}
	
	return(data.frame("message" = v_message, "type" = v_type))
}

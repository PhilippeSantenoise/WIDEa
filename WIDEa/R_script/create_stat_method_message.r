

f_create_stat_method_message <- function(df_stat_method, l_stat_method_level, v_stat_method, o_parameter) {
	df_stat_method <- df_stat_method[which(df_stat_method$name %in% v_stat_method), c("name", "message", "code")]
	v_message <- c()
	v_type <- c()
	
	for (i in as.vector(unique(df_stat_method$code))) {
		v_name <- paste(as.vector(unique(df_stat_method[which(df_stat_method$code == i), "message"])), collapse = ", ")
		
		if (length(l_stat_method_level[[i]]) == 0) { # error message
			v_type <- c(v_type, "error")
			
			if (i == "cp1") {
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to 0", ifelse(!is.na(isolate(o_parameter$group)), " for each group", "")))
			}
			else { # cp2
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 2) or the standard deviation of X variable is equal to 0", ifelse(!is.na(isolate(o_parameter$group)), " for each group", "")))
			}
		}
		else { # warning message
			v_type <- c(v_type, "warning")
			v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
			v_pos <- which(!v_group %in% l_stat_method_level[[i]])
			
			if (i == "cp1") {
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to 0 for one or more groups (", paste(v_group[v_pos], collapse = ", "), ")"))
			}
			else { # cp2
				v_message <- c(v_message, paste0("Error with statistical method (", v_name, "): insufficient size of data (< 2) or the standard deviation of X variable is equal to 0 for one or more groups (", paste(v_group[v_pos], collapse = ", "), ")"))
			}
		}
	}
	
	return(data.frame("message" = v_message, "type" = v_type))
}

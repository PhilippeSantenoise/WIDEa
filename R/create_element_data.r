#' @importFrom shiny isolate
NULL

#' Creating data related to elements added to the main plotly graph 

#' @description
#' `f_create_element_data` is used to create data including the name (only temporal)
#' or the number (normal/ir) of elements name added to the main plotly graph when
#' the display button (left panel) is clicked. This data is used to determine the
#' position of current flags in the main plotly graph (added with a click event).
#' The function returns data in different formats: an integer value (normal), a
#' vector (temporal) and a data frame (ir).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param i_proc_num is the process number (only used for the temporal/ir data
#' type). 
#' @param o_plot is a reactive value including data information corresponding to the
#' main plotly graph.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_cond is a reactive value including binary values used as conditions in
#' the server (related to action buttons).
#' @param s_mode is the value corresponding to the "Mode" parameter in the Graphic
#' tab (3 values: "line", "marker", "line_marker").

#' @encoding UTF-8

f_create_element_data <- function (s_data_type = "normal", i_proc_num = 1, o_parameter, o_cond, o_plot, s_mode = "line") {
	if (s_data_type == "normal") {
		df_all <- isolate(o_plot$data)
		i_elt_num <- ifelse(!is.na(isolate(o_parameter$group)), length(as.vector(unique(df_all[, isolate(o_parameter$group)]))), 1)
		
		if (isolate(o_cond$qc2) == 1) {
			i_elt_num <- i_elt_num + 1
		}
		
		return (i_elt_num)
	}
	else if (s_data_type == "temporal") {
		if (i_proc_num == 1) {
			v_elt <- isolate(o_parameter$y)
			
			if (isolate(o_plot$add_pt)) {
				v_elt <- c(v_elt, isolate(o_plot$var_pt))
			}
			
			if (isolate(o_cond$flag) == 1) {
				v_add_var_name <- paste0(rep(isolate(o_parameter$y), each = 8), "_", rep(1:8, length(isolate(o_parameter$y))))
				
				df_freq <- as.data.frame(addmargins(table(c(isolate(o_plot$var_qc1), isolate(o_plot$var_qc2)))))
				df_freq <- df_freq[-dim(df_freq)[1],]
				names(df_freq) <- c("var_name", "num")
				
				v_freq_name_1 <- eval(parse(text = paste0("c(", paste(paste0("rep(\"", df_freq$var_name, "\", ", df_freq$num * 2, ")"), collapse = ", "), ")"))) 
				v_freq_name_2 <- eval(parse(text = paste0("c(", paste(paste0("1:", df_freq$num * 2), collapse = ", "), ")"))) 
				
				v_freq_name <- paste(v_freq_name_1, v_freq_name_2, sep = "_")
				v_add_var_name <- v_add_var_name[which(v_add_var_name %in% v_freq_name)]
				v_add_var_name <- substr(v_add_var_name, 1, nchar(v_add_var_name) - 2) 
				v_elt <- c(v_elt, v_add_var_name)
			}
		}
		else { # process = 2
			i_pos_1 <- isolate(o_plot$elt_pt_pos)[1]
			i_pos_2 <- isolate(o_plot$elt_pt_pos)[2]
			
			if (s_mode == "line") {
				if ((i_pos_1 - 1) < length(isolate(o_plot$elt))) {
					v_elt <- c(isolate(o_plot$elt)[1:(i_pos_1 - 1)], isolate(o_plot$var_pt), isolate(o_plot$elt)[i_pos_1:length(isolate(o_plot$elt))]) 
				}
				else {
					v_elt <- c(isolate(o_plot$elt)[1:(i_pos_1 - 1)], isolate(o_plot$var_pt))
				}
			}
			else { # mode = "marker" or "line_marker"
				if (isolate(o_plot$add_pt)) {
					v_elt <- isolate(o_plot$elt)[-c(i_pos_1:i_pos_2)]
				}
				else {
					v_elt <- isolate(o_plot$elt)
				}
			}
		}
		
		return (v_elt)
	}
	else { # ir
		if (i_proc_num == 1) {
			df_elt <- data.frame("type" = c("line", "point"), "no_flag" = rep(0, 2), "qc1_flag" = rep(0, 2), "qc2_flag" = rep(0, 2))
			df_elt[1, 2] <- length(c(3:dim(isolate(o_plot$data))[2]))
			
			if (isolate(o_cond$flag) == 1) {
				if (isolate(o_cond$qc1) == 1) {
					df_elt[1, 3] <- length(c(3:dim(isolate(o_plot$data_qc1))[2]))
				}
				
				if (isolate(o_cond$qc2) == 1) {
					df_elt[1, 4] <- length(c(3:dim(isolate(o_plot$data_qc2))[2]))
				}
			}
			
			if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
				if (s_mode == "line") {
					df_elt[2, 2] <- dim(isolate(o_plot$data))[2] - 2
					
					if (isolate(o_cond$flag) == 1) {
						if (isolate(o_cond$qc1) == 1) {
							df_elt[2, 3] <- dim(isolate(o_plot$data_qc1))[2] - 2
						}
						
						if (isolate(o_cond$qc2) == 1) {
							df_elt[2, 4] <- length(unique(as.vector(isolate(o_plot$id_group)$qc2_flag$group)))
						}
					}
				}
			}
		}
		else { # process = 2
			df_elt <- isolate(o_plot$elt)
			
			if (s_mode == "line") {
				df_elt[2, 2] <- dim(isolate(o_plot$data))[2] - 2
				
				if (isolate(o_cond$flag) == 1) {
					if (isolate(o_cond$qc1) == 1) {
						df_elt[2, 3] <- dim(isolate(o_plot$data_qc1))[2] - 2
					}
					
					if (isolate(o_cond$qc2) == 1) {
						df_elt[2, 4] <- length(unique(as.vector(isolate(o_plot$id_group)$qc2_flag$group)))
					}
				}
			}
			else { # mode = "marker" or "line_marker"
				if (isolate(o_plot$add_pt)) {
					df_elt[2, c(2:4)] <- rep(0, 3)
				}
			}
		}
		
		return (df_elt)
	}
}

#' @importFrom shiny isolate
NULL

#' Calculation of the Y axis range for the temporal/ir data type

#' @description
#' `f_calcul_y_axis_range` is related to the "Y scale" parameter in the Graphic tab
#' (top panel) and allows to calculate the Y axis range when the auto/manual option
#' is selected. The function returns a list with the calculated Y range (two ranges
#' returned for the auto option: range with/without fraction) and a warning message
#' (if a problem occurs).

#' @param s_data_type is the data type (2 values: "temporal", "ir").
#' @param s_scale is the value of the "Y scale" radio buttons ("auto" or "manual").
#' @param df_all_orig are data saved in the e_data environment ("all" or "sub" if
#' sub-data is created).
#' @param o_plot is a reactive value including data information corresponding to the
#' main plotly graph.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_cond is a reactive value including binary values used as conditions in
#' the server (related to action buttons).
#' @param o_zoom is a reactive value including zoom coordinates of the main plotly
#' graph. 
#' @param df_click_legend are legend item data information (name and status).
#' @param n_frac is a fraction of the main plotly graph height (value between 0 and
#' 0.1).
#' @param v_range is the vector including values filled in "min" and "max" inputs
#' ("Y scale" radio buttons: manual option).

#' @encoding UTF-8

f_calcul_y_axis_range <- function (s_data_type = "temporal", s_scale = "auto", df_all_orig, o_parameter, o_zoom, o_plot, o_cond, df_click_legend, n_frac = 0.05, v_range = NULL) {
	s_message <- character(0)
	
	if (s_scale == "auto") {
		v_y_range_saved <- isolate(o_plot$y_coord)
		
		if (!is.null(v_y_range_saved)) {
			v_y_range <- v_y_range_saved + c(-1, 1) * n_frac * diff(v_y_range_saved)
		}
		else {
			i_num <- dim(df_click_legend)[1]
			
			if (s_data_type == "temporal") {
				if (length(which(df_click_legend$statut == "T")) > 0) {
					df_all <- isolate(o_plot$data)
					
					if (is.null(isolate(o_zoom$coord))) {
						v_row <- c(1:dim(df_all)[1])
					}
					else {
						if (length(which(df_all[, paste0(isolate(o_parameter$x), "_trf")] < as.numeric(as.POSIXct(isolate(o_zoom$coord)[2], tz = "GMT")))) == 0 | length(which(df_all[, paste0(isolate(o_parameter$x), "_trf")] > as.numeric(as.POSIXct(isolate(o_zoom$coord)[1], tz = "GMT")))) == 0) {
							s_message <- "The Y scale is reseted because the selected date range is out of valid range"
							v_row <- c(1:dim(df_all)[1])
						}
						else {
							v_row <- which(df_all[, paste0(isolate(o_parameter$x), "_trf")] >= as.numeric(as.POSIXct(isolate(o_zoom$coord)[1], tz = "GMT")) & df_all[, paste0(isolate(o_parameter$x), "_trf")] <= as.numeric(as.POSIXct(isolate(o_zoom$coord)[2], tz = "GMT")))
						}
					}
					
					v_name <- as.vector(df_click_legend[which(df_click_legend$statut == "T"), "name"])
					
					if (i_num == 1) {
						v_y_range <- suppressWarnings(range(df_all[v_row, v_name], na.rm = T))
						
						if (Inf %in% v_y_range) {
							v_y_range <- range(df_all[, v_name], na.rm = T)
						}
						
						v_y_range_saved <- v_y_range
						v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
					}
					else {
						v_pos <- which(v_name %in% names(df_all))
						
						if (length(v_pos) > 0) {
							v_y_range <- suppressWarnings(range(df_all[v_row, v_name[v_pos]], na.rm = T))
						}
						else {
							v_y_range <- c()
						}
						
						v_name_qc2 <- NULL
						
						if (isolate(o_cond$flag) == 1) {
							v_name_qc <- isolate(o_plot$leg_name_qc)
							v_name_qc1 <- NULL
							v_pos <- which(v_name_qc %in% v_name)
							
							if (length(v_pos) > 0) {
								v_name_qc <- v_name_qc[v_pos]
								
								if (length(v_y_range) == 0) {
									v_pos <- which(substr(v_name_qc, nchar(v_name_qc) - 5, nchar(v_name_qc)) == "qc = 1")
								
									if (length(v_pos) > 0) {
										v_name_qc1 <- substr(v_name_qc[v_pos], 1, nchar(v_name_qc) - 7)
										df_qc1 <- isolate(o_plot$data_qc1)
										v_y_range <- c(v_y_range, suppressWarnings(range(df_qc1[v_row, v_name_qc1], na.rm = T)))
									}
								}
								
								v_pos <- which(substr(v_name_qc, nchar(v_name_qc) - 5, nchar(v_name_qc)) == "qc = 2")
								
								if (length(v_pos) > 0) {
									v_name_qc2 <- substr(v_name_qc[v_pos], 1, nchar(v_name_qc) - 7)
									df_qc2 <- isolate(o_plot$data_qc2)
									v_y_range <- c(v_y_range, suppressWarnings(range(df_qc2[v_row, v_name_qc2], na.rm = T)))
								}
							}
						}
						
						v_pos <- which(v_y_range %in% c(-Inf, Inf))
						
						if (length(v_pos) > 0) {
							v_y_range <- v_y_range[-v_pos]
						}
						
						if (length(v_y_range) == 0) {
							if (length(which(v_name %in% names(df_all))) > 0) {
								v_y_range <- range(df_all[, v_name], na.rm = T)
							}
							else {
								if (length(v_name_qc1) > 0) {
									v_y_range <- range(df_qc1[, v_name_qc1], na.rm = T)
								}
							}
							
							if (length(v_name_qc2) > 0) {
								v_y_range <- c(v_y_range, range(df_qc2[, v_name_qc2], na.rm = T))
							}
						}
						
						v_y_range <- range(v_y_range)
						v_y_range_saved <- v_y_range
						v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
					}
				}
				else {
					v_y_range <- range(df_all_orig[!is.na(df_all_orig[, isolate(o_parameter$x)]), isolate(o_parameter$y)], na.rm = T)
					v_y_range_saved <- v_y_range
					v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
				}
			}
			else { # ir
				df_all <- isolate(o_plot$data)
				
				if (length(which(df_click_legend$statut == "T")) > 0) {
					if (is.null(isolate(o_zoom$coord))) {
						v_row <- c(1:dim(df_all)[1])
					}
					else {
						if (length(which(as.vector(df_all$Frequency) < isolate(o_zoom$coord)[2])) == 0 | length(which(as.vector(df_all$Frequency) > isolate(o_zoom$coord)[1])) == 0) {
							s_message <- "The Y scale is reseted because the selected frequency range is out of valid range"
							v_row <- c(1:dim(df_all)[1])
						}
						else {
							v_row <- min(which(as.vector(df_all$Frequency) <= isolate(o_zoom$coord)[1])):max(which(as.vector(df_all$Frequency) >= isolate(o_zoom$coord)[2]))
						}
					}
					
					if (i_num == 1) {
						v_y_range <- suppressWarnings(range(df_all[v_row, -c(1, 2)], na.rm = T))
						
						if (Inf %in% v_y_range) {
							v_y_range <- range(df_all[, -c(1, 2)], na.rm = T)
						}
						
						v_y_range_saved <- v_y_range
						v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
					}
					else {
						v_name <- as.vector(df_click_legend[which(df_click_legend$statut == "T"), "name"])
						l_id_group <- isolate(o_plot$id_group)
						v_group <- as.vector(l_id_group$no_flag$group)
						v_pos_1 <- which(v_group %in% v_name)
						
						if (length(v_pos_1) > 0) {
							v_y_range <- suppressWarnings(range(df_all[v_row, v_pos_1 + 2], na.rm = T))
						}
						else {
							v_y_range <- c()
						}
						
						if (length(v_y_range) == 0 & isolate(o_parameter$mean_spect)) {
							v_pos <- which(paste0(v_group, " (mean)") %in% v_name)
							
							if (length(v_pos) > 0) {
								v_colname <- names(df_all)[-c(1, 2)]
								
								l_y_range <- lapply(unique(v_group[v_pos]), function(x) {
									return(range(as.vector(rowMeans(df_all[v_row, v_colname[which(v_group == x)]]))))
								})
								
								v_y_range <- c(v_y_range, range(as.vector(unlist(l_y_range))))
							}
						}
						
						if (length(v_y_range) == 0 & "qc1_flag" %in% names(l_id_group)) {
							v_group <- as.vector(l_id_group$qc1_flag$group)
							v_pos <- which(v_group %in% v_name)
							df_qc1 <- isolate(o_plot$data_qc1)
							
							if (length(v_pos) > 0) {
								v_y_range <- c(v_y_range, suppressWarnings(range(df_qc1[v_row, v_pos + 2], na.rm = T)))
							}
						}
						
						if ("qc2_flag" %in% names(l_id_group)) {
							v_group <- as.vector(l_id_group$qc2_flag$group)
							v_pos_2 <- which(v_group %in% v_name)
							df_qc2 <- isolate(o_plot$data_qc2)
							
							if (length(v_pos_2) > 0) {
								v_y_range <- c(v_y_range, suppressWarnings(range(df_qc2[v_row, v_pos_2 + 2], na.rm = T)))
							}
						}
						
						v_pos <- which(v_y_range %in% c(-Inf, Inf))
						
						if (length(v_pos) > 0) {
							v_y_range <- v_y_range[-v_pos]
						}
						
						if (length(v_y_range) == 0) {
							v_y_range <- range(df_all[, v_pos_1 + 2], na.rm = T)
							
							if ("qc2_flag" %in% names(l_id_group)) {
								v_y_range <- c(v_y_range, range(df_qc2[, v_pos_2 + 2], na.rm = T))
							}
						}
						
						v_y_range <- range(v_y_range)
						v_y_range_saved <- v_y_range
						v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
					}
				}
				else {
					# v_code <- as.vector(e_data$code_freq$Code)
					v_code <- as.vector(df_all$Code)
					v_y_range <- range(df_all_orig[, which(names(df_all_orig) %in% v_code)], na.rm = T)
					v_y_range_saved <- v_y_range
					v_y_range <- v_y_range + n_frac * c(-1, 1) * diff(v_y_range)
				}
			}
		}
		
		return(list(v_y_range, v_y_range_saved, s_message))
	}
	else { # manual
		if (s_data_type == "temporal") {
			v_range_orig <- range(df_all_orig[, isolate(o_parameter$y)], na.rm = T)
		}
		else { # ir
			df_all <- isolate(o_plot$data)
			# v_code <- as.vector(e_data$code_freq$Code)
			v_code <- as.vector(df_all$Code)
			v_range_orig <- range(df_all_orig[, which(names(df_all_orig) %in% v_code)], na.rm = T)
		}
		
		n_ymin_orig <- v_range_orig[1]
		n_ymax_orig <- v_range_orig[2]
		n_ymin_0 <- v_range[1]
		n_ymax_0 <- v_range[2]
		
		if (is.na(n_ymin_0) | is.null(n_ymin_0) | n_ymin_0 == "") {
			n_ymin <- n_ymin_orig
			s_message <- paste0("Minimal value is missing for y scale. So, the value is replaced by the global minimal value: ", n_ymin, ".")
		}
		else {
			n_ymin <- tryCatch({suppressWarnings(as.numeric(n_ymin_0))}, error = function(e) FALSE)
			
			if (!is.na(n_ymin) & !is.logical(n_ymin)) {
				if (n_ymin >= n_ymax_orig) {
					n_ymin <- n_ymin_orig
					s_message <- paste0("Minimal value cannot be superior or equal to the global maximal value (", n_ymax_orig, ") of y scale. So, the value is replaced by the global minimal value: ", n_ymin, ".")
				}
			}
			else {
				n_ymin <- n_ymin_orig
				s_message <- paste0("Minimal value is not numeric for y scale. So, the value is replaced by the global minimal value: ", n_ymin, ".")
			}
		}
		
		if (is.na(n_ymax_0) | is.null(n_ymax_0) | n_ymax_0 == "") {
			n_ymax <- n_ymax_orig
			s_message <- ifelse(is.na(s_message), paste0("Maximal value is missing for y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."), paste0(s_message, "<br/>Maximal value is missing for y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."))
		}
		else {
			n_ymax <- tryCatch({suppressWarnings(as.numeric(n_ymax_0))}, error = function(e) FALSE)
			
			if (!is.na(n_ymax) & !is.logical(n_ymax)) {
				if (n_ymax <= n_ymin_orig) {
					n_ymax <- n_ymax_orig
					s_message <- ifelse(is.na(s_message), paste0("Maximal value cannot be inferior or equal to the global minimal value (", n_ymin_orig, ") of y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."), paste0(s_message, "<br/>Maximal value cannot be inferior or equal to the global minimal value (", n_ymin_orig, ") of y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."))
				}
			}
			else {
				n_ymax <- n_ymax_orig
				s_message <- ifelse(is.na(s_message), paste0("Maximal value is not numeric for y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."), paste0(s_message, "<br/>Maximal value is not numeric for y scale. So, the value is replaced by the global maximal value: ", n_ymax, "."))
			}
		}
		
		n_diff <- n_ymin - n_ymax
		
		if (n_diff >= 0) {
			if (n_diff == 0) {
				n_ymin <- n_ymin_orig
				n_ymax <- n_ymax_orig
				s_message <- ifelse(is.na(s_message), paste0("Minimal and maximal values of y scale are equal. So, values are replaced by the global minimal and maximal values : ", n_ymin, " and ", n_ymax, " (resp.)."), paste0(s_message, "<br/>Minimal and maximal values of y scale are equal. So, values are replaced by the global minimal and maximal values : ", n_ymin, " and ", n_ymax, " (resp.)."))
			}
			else {
				n_ymin <- n_ymax_0
				n_ymax <- n_ymin_0
				s_message <- ifelse(is.na(s_message), paste0("Minimal value is superior to maximal value for y scale. So, values are reversed."), paste0(s_message, "<br/>Minimal value is superior to maximal value for y scale. So, values are reversed."))
			}
		}
		
		return(list(c(n_ymin, n_ymax), s_message))
	}
}

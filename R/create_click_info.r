#' @importFrom shiny isolate
NULL

#' Creating a list of data information associated to a plotly click event 

#' @description
#' `f_create_click_info` only concerns the normal/ir data type and allows to save a 
#' list of data information associated to a click event.
#' \cr\cr For the normal data type, the list includes: 
#' \cr(1) ("x", "y", "z") axes (selected from the "variable" checking boxes in the
#' Flag tab;
#' \cr(2) Variable name corresponding to the selected axes;
#' \cr(3) ID (if an ID variable is selected) or row number from data saved in e_data 
#' environment (e_data$all).
#' \cr\cr For the ir data type, the list includes:
#' \cr(1) ID (if an ID variable is selected) or row number from data saved in the 
#' o_plot reactive value (o_plot$id_group);
#' \cr(2) Row number from data saved in the o_plot reactive value (o_plot$data);
#' \cr(3) A boolean value returned by the condition "no qc = 1 flags already saved 
#' with this ID or row number;
#' \cr(4) A warning message returned if a qc = 1 flag is clicked and "add_flag"
#' value is selected from the "action" radio button in the Flag tab (empty message
#' else).

#' @param s_data_type is the data type (2 values: "normal", "ir").
#' @param s_action is the value of the "action" radio button in the Flag tab (2
#' values: "add_flag", "replace_qc"). This input is only used for the ir data type.
#' @param s_dim_num is the dimension number for the normal data type (2 values:
#' "2d", "3d").
#' @param v_xy_flag is a vector of values returned by the "variable" checking boxes
#' (Flag tab) for ("x", "y") axes.
#' @param b_z_flag is a boolean value returned by the "variable" checking boxes
#' (Flag tab) for the "z" axis.
#' @param s_flag_name is the flag data name (saved in the o_flag reactive value:
#' o_flag$name).
#' @param e_data is an environment created in the "WIDEa_launcher" R script. This
#' input is only used for the normal data type.
#' @param o_click_ev are data saved when a click event is executed in the main
#'  plotly graph ("plotly_click" with the `event_data` function).
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_plot is a reactive value including main plotly data information.
#' @param o_cond is a reactive value including binary values used as conditions in
#' the server (related to action buttons).
#' @param v_sub_row is the vector including row numbers of sub-data (NULL as default
#' value, if no sub-data are created). 

#' @encoding UTF-8

f_create_click_info <- function (s_data_type = "ir", s_action = "add_flag", s_dim_num = NULL, v_xy_flag = NULL, b_z_flag = F, s_flag_name = NA, e_data = NULL, v_sub_row = NULL, o_click_ev, o_parameter = NULL, o_plot, o_cond) {
	if (s_data_type == "normal") {
		v_pos <- which(c("flag_x", "flag_y") %in% v_xy_flag)
		v_axis <- c("x", "y")
		v_name <- c(isolate(o_parameter$x), isolate(o_parameter$y))
		
		if (s_dim_num == "3d") {
			if (b_z_flag) {
				v_pos <- c(v_pos, 3)
			}
			
			v_axis <- c(v_axis, "z")
			v_name <- c(v_name, isolate(o_parameter$z))
		}
		
		if (!is.na(s_flag_name)) { 
			if (substr(s_flag_name, nchar(s_flag_name) - 9, nchar(s_flag_name) - 4) == "withID") { # search the "withID" term in flag data name
				s_id_var <- names(e_data$flag)[1]
				eval(parse(text = paste0("v_num <- e_data$all[which(", paste(paste0("e_data$all$", v_name, " == o_click_ev[[\"", v_axis, "\"]]"), collapse = " & "), "), s_id_var]")))
			}
			else {
				eval(parse(text = paste0("v_num <- which(", paste(paste0("e_data$all$", v_name, " == o_click_ev[[\"", v_axis, "\"]]"), collapse = " & "), ")")))
				
				if (length(v_sub_row) > 0) {
					v_num <- v_num[which(v_num %in% v_sub_row)]
				}
			}
		}
		else {
			if (!is.na(isolate(o_parameter$id))) {
				eval(parse(text = paste0("v_num <- e_data$all[which(", paste(paste0("e_data$all$", v_name, " == o_click_ev[[\"", v_axis, "\"]]"), collapse = " & "), "), isolate(o_parameter$id)]")))
			}
			else {
				eval(parse(text = paste0("v_num <- which(", paste(paste0("e_data$all$", v_name, " == o_click_ev[[\"", v_axis, "\"]]"), collapse = " & "), ")")))
				
				if (length(v_sub_row) > 0) {
					v_num <- v_num[which(v_num %in% v_sub_row)]
				}
			}
		}
		
		v_axis <- v_axis[v_pos]
		v_name <- v_name[v_pos]
		
		return(list(v_axis, v_name, v_num))
	}
	else { # ir
		df_elt <- isolate(o_plot$elt)
		i_sum_1 <- sum(as.vector(df_elt[, 2]))
		i_sum_2 <- i_sum_1 + sum(as.vector(df_elt[, 3]))
		
		s_id <- character(0)
		i_pos <- integer(0)
		b_cond <- F
		s_message <- character(0)
		
		if (s_action == "add_flag") {
			if ((o_click_ev[["curveNumber"]] + 1) <= i_sum_1) {
				v_name <- names(isolate(o_plot$data))[-c(1:2)]
				
				if (isolate(o_plot$add_pt) == T) {
					v_name <- rep(v_name, 2)
				}
				
				i_pos <- which(names(isolate(o_plot$data))[-c(1:2)] == v_name[o_click_ev[["curveNumber"]] + 1])
				s_id <- as.vector(isolate(o_plot$id_group)$no_flag[i_pos, "id"])
				
				if (isolate(o_cond$qc1) == 1) {
					b_cond <- !s_id %in% as.vector(isolate(o_plot$id_group)$qc1_flag[, "id"])
				}
				else {
					b_cond <- T
				}
			}
			
			if ((o_click_ev[["curveNumber"]] + 1) <= i_sum_2 & isolate(o_cond$qc1) == 1 & b_cond == F) {
				s_message <- "Flag already exists"
			}
		} # replace_qc
		else {
			if ((o_click_ev[["curveNumber"]] + 1) <= i_sum_2 & isolate(o_cond$qc1) == 1) {
				if (isolate(o_plot$add_pt) == T) {
					v_name <- c(rep(names(isolate(o_plot$data))[-c(1:2)], 2), rep(names(isolate(o_plot$data_qc1))[-c(1:2)], 2))
				}
				else {
					v_name <- c(names(isolate(o_plot$data))[-c(1:2)], names(isolate(o_plot$data_qc1))[-c(1:2)])
				}
				
				if ((o_click_ev[["curveNumber"]] + 1) <= i_sum_1) {
					i_pos <- which(names(isolate(o_plot$data))[-c(1:2)] == v_name[o_click_ev[["curveNumber"]] + 1])
					s_id <- as.vector(isolate(o_plot$id_group)$no_flag[i_pos, "id"])
					b_cond <- s_id %in% as.vector(isolate(o_plot$id_group)$qc1_flag[, "id"])
				}
				else {
					i_pos <- which(names(isolate(o_plot$data_qc1))[-c(1:2)] == v_name[o_click_ev[["curveNumber"]] + 1])
					s_id <- as.vector(isolate(o_plot$id_group)$qc1_flag[i_pos, "id"])
					i_pos <- which(as.vector(isolate(o_plot$id_group)$no_flag$id) == s_id)
					b_cond <- T
				}
			}
		}
		
		return(list(s_id, i_pos, b_cond, s_message))
	}
}

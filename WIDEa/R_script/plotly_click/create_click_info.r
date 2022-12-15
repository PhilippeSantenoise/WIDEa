#########################################################################################################
# Copyright 2021 - INRAE - Philippe Santenoise
#
# This R script is a part of WIDEa
#
# WIDEa is a free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# WIDEa is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with WIDEa (see the file License.txt). If not, see
# <http://www.gnu.org/licenses/>
#
#
# Description : the function only concerns the normal/ir data type and allows to save a list of 
#               informations associated to a click event,
#               (normal) - axes ("x", "y", "z")
#                        - variable name corresponding to axes
#                        - ID (if an ID variable is selected) or row number from data saved in e_data 
#                          environment (e_data$all)
#                        The two first informations depend on the value of the "variable" checking boxes 
#                        in the Flag tab.
#               (ir)     - ID (if an ID variable is selected) or row number from data saved in o_plot 
#                          reactive value (o_plot$id_group)
#                        - row number from data saved in o_plot reactive value (o_plot$data)
#                        - a boolean value returned by the condition : there are no qc = 1 flags already saved 
#                          saved with this ID or row number
#                        - a warning message returned if a qc = 1 flag is clicked and "add_flag" value is 
#                          selected from "action" radio button in the Flag tab (empty message else) 
#
# Creation date : May 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (2 values: "normal", "ir")
# s_action: value of "action" radio button in the Flag tab (2 values: "add_flag", "replace_qc"). This input is only used for the ir data type 
# s_dim_num: dimension number for the normal data type (2 values: "2d", "3d")
# (v_xy_flag, b_z_flag): value returned by the "variable" checking boxes in the Flag tab.  
# s_flag_name: flag data name (saved in o_flag reactive value: o_flag$name)
# e_data: environment created in the R script "WIDEa_launcher". This input is only used for the normal data type.
# o_click_ev: data saved when a click event is executed in the graph ("plotly_click" with the event_data function)
# (o_parameter, o_plot, o_cond): reactive values created in the R script "WIDEa_launcher"

# Output:
# -------
# return the corresponding list

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

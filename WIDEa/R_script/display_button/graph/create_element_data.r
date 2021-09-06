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
# Description : the function allows to create an indicator on the name (only temporal)/number (normal/ir)
#               of elements (number or variable name) added to the graph when the display button (left 
#               panel) is clicked. Elements corresponding to Flag/Statistics tabs are not taken in 
#               account. This indicator allows to know the position of elements corresponding to flag
#               data (qc = 1 and 2) or not and is used to add/remove current flags on the graph (from a
#               click event on the graph and Flag tab buttons).
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir") 
# i_proc_num: process number (only used for the temporal/ir data type)
# df_all: data saved in o_plot reactive value (o_plot$data). This input is only used for the noraml data type
# (o_parameter, o_cond, o_plot): reactive values from the R script "WIDEa_launcher"
# s_mode: value corresponding to the "Mode" parameter in the Graphic tab (3 values: "line", "marker", "line_marker")

# Output:
# -------
# return the indicator as an integer (normal), vector (temporal) or data.frame (ir)

f_create_element_data <- function (s_data_type = "normal", i_proc_num = 1, df_all, o_parameter, o_cond, o_plot, s_mode = "line") {
	if (s_data_type == "normal") {
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
				
				eval(parse(text = paste0("v_freq_name_1 <- c(", paste(paste0("rep(\"", df_freq$var_name, "\", ", df_freq$num * 2, ")"), collapse = ", "), ")"))) 
				eval(parse(text = paste0("v_freq_name_2 <- c(", paste(paste0("1:", df_freq$num * 2), collapse = ", "), ")"))) 
				
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

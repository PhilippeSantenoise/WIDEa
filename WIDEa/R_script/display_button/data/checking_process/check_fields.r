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
# Description : Function used to check if graph variable fields are all filled
#
# Creation date : September 2022
#########################################################################################################


# Inputs:
# -------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# s_plot_type: plot type (5 values: "plot", "boxplot", "histplot", "barplot", "corplot")
# s_dim_num: dimension number (2 values: "2d", "3d")
# s_model: model process (3 values: "none", "calib", "valid")
# l_input_id_value: list of input used to display the graph with id/value information (id as name)

# Output:
# The function returns a (named) vector of boolean values. Each boolean value is associated to a graph variable and is true if the field are not filled.


f_check_fields <- function (s_data_type, s_plot_type, s_dim_num, s_model, l_input_id_value) {
	if (s_data_type == "normal") {
		if (s_plot_type == "plot") {
			if (s_dim_num == "2d") {
				if (s_model == "none") {
					v_cond <- c(l_input_id_value$id == "yes" & (length(which(l_input_id_value$var_id == "")) == 1 | is.null(l_input_id_value$var_id)), is.null(l_input_id_value$var_x), l_input_id_value$f_radio == "yes" & (length(which(l_input_id_value$f_text == "")) == 1 | length(l_input_id_value$f_text) == 0), is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group))))
					names(v_cond) <- c("ID", "X", "f(x)", "Y", "g(y)", "Group")
				}
				else if (s_model == "calib") {
					v_cond <- c(l_input_id_value$ref_radio == "yes" & is.null(l_input_id_value$ref), l_input_id_value$wres_radio == "yes" & (length(which(l_input_id_value$wres_vfun == "")) == 1 | length(l_input_id_value$wres_vfun) == 0), l_input_id_value$wres_radio == "yes" & l_input_id_value$wres_cbox & is.null(l_input_id_value$wres_group), is.null(l_input_id_value$var_x), length(which(l_input_id_value$f_text == "")) == 1 | length(l_input_id_value$f_text) == 0, length(which(l_input_id_value$var_y == "")) == 1 | is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
					names(v_cond) <- c("Random", "Weighted residuals - variance function", "Weighted residuals - group", "X", "f(x)", "Y", "g(y)", "Group")
				}
				else {
					v_cond <- c(l_input_id_value$ref_radio == "yes" & is.null(l_input_id_value$ref), is.null(l_input_id_value$var_x), length(which(l_input_id_value$f_text == "")) == 1 | length(l_input_id_value$f_text) == 0, length(which(l_input_id_value$var_y == "")) == 1 | is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
					names(v_cond) <- c("Random", "X", "f(x)", "Y", "g(y)", "Group")
				}
			}
			else {
				v_cond <- c(l_input_id_value$id == "yes" & (length(which(l_input_id_value$var_id == "")) == 1 | is.null(l_input_id_value$var_id)), is.null(l_input_id_value$var_x), l_input_id_value$f_radio == "yes" & (length(which(l_input_id_value$f_text == "")) == 1 | length(l_input_id_value$f_text) == 0), is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0), is.null(l_input_id_value$var_z), l_input_id_value$h_radio == "yes" & (length(which(l_input_id_value$h_text == "")) == 1 | length(l_input_id_value$h_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
				names(v_cond) <- c("ID", "X", "f(x)", "Y", "g(y)", "Z", "h(z)", "Group")
			}
		}
		else if (s_plot_type == "boxplot") {
			v_cond <- c((!l_input_id_value$concat1 & (length(which(l_input_id_value$var_x == "")) == 1 | is.null(l_input_id_value$var_x))) | (l_input_id_value$concat1 & is.null(l_input_id_value$var_x)), is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
			names(v_cond) <- c("X", "Y", "g(y)", "Group")
		}
		else if (s_plot_type == "histplot") {
			v_cond <- c(is.null(l_input_id_value$var_x), l_input_id_value$f_radio == "yes" & (length(which(l_input_id_value$f_text == "")) == 1 | length(l_input_id_value$f_text) == 0), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
			names(v_cond) <- c("X", "f(x)", "Group")
		}
		else if (s_plot_type == "barplot") {
			v_cond <- c((!l_input_id_value$concat1 & (length(which(l_input_id_value$var_x == "")) == 1 | is.null(l_input_id_value$var_x))) | (l_input_id_value$concat1 & is.null(l_input_id_value$var_x)), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
			names(v_cond) <- c("X", "Group")
		}
		else {
			v_cond <- c(is.null(l_input_id_value$var_y), l_input_id_value$group == "yes" & ((!l_input_id_value$concat2 & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 & is.null(l_input_id_value$var_group)))) 
			names(v_cond) <- c("Y", "Group")
		}
	}
	else if (s_data_type == "temporal") {
		v_cond <- c(length(which(l_input_id_value$var_x == "")) == 1 | is.null(l_input_id_value$var_x), is.null(l_input_id_value$var_y), l_input_id_value$g_radio == "yes" & (length(which(l_input_id_value$g_text == "")) == 1 | length(l_input_id_value$g_text) == 0)) 
		names(v_cond) <- c("X", "Y", "g(y)")
	}
	else { # ir
		v_cond <- c(l_input_id_value$id == "yes" & (length(which(l_input_id_value$var_id == "")) == 1 | is.null(l_input_id_value$var_id)), l_input_id_value$group == "yes" & ((l_input_id_value$concat2 == F & (length(which(l_input_id_value$var_group == "")) == 1 | is.null(l_input_id_value$var_group))) | (l_input_id_value$concat2 == T & is.null(l_input_id_value$var_group))))
		names(v_cond) <- c("ID", "Group")
	}
	
	return(v_cond)
}

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
# Description : function used to create the graph color palette
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# df_all: data created from the function "f_prepare_data" (process = 1). This input only concern the normal/ir data type 
# o_parameter: reactive value from the R script "WIDEa_launcher"

# Output:
# -------
# return the graph color palette (vector)

f_create_col_palette <- function (s_data_type = "normal", df_all = data.frame(), o_parameter) {
	if (s_data_type == "normal") {
		if (!is.na(isolate(o_parameter$group))) {
			if (isolate(o_parameter$plot_type) %in% c("boxplot", "barplot")) {
				i_num <- length(as.vector(unique(df_all[, isolate(o_parameter$x)])))
			}
			else {
				i_num <- length(as.vector(unique(df_all[, isolate(o_parameter$group)])))
			}
		}
		else {
			i_num <- 1
		}
	}
	else if (s_data_type == "temporal") {
		i_num <- length(isolate(o_parameter$y))
	}
	else { # ir
		if (!is.na(isolate(o_parameter$group))) {
			i_num <- length(as.vector(unique(df_all[, isolate(o_parameter$group)])))
		}
		else {
			i_num <- 1
		}
	}
	
	if (i_num > 1) {
		v_color <- hue_pal()(i_num)
	}
	else {
		v_color <- "dodgerblue2"
	}
	
	return (v_color)
}

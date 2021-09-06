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
# Description : function used to edit graph axis range or the camera position when a 3D plot is selected
#               (normal data type) 
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# (o_parameter, o_zoom): reactive values from the R script "WIDEa_launcher"
# v_freq_range: spectra frequency range (ir data type)

# Output:
# -------
# return informations on (X, Y, Z) axis in a list

f_edit_axis_layout <- function (s_data_type = "normal", o_parameter, o_zoom, v_freq_range = NULL) {
	if (s_data_type == "normal") {
		l_axis_layout <- NULL
		
		if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
			if (!is.null(isolate(o_zoom$coord))) {
				if (!is.na(isolate(o_parameter$dim_num)) & isolate(o_parameter$dim_num) == "3d") {
					l_axis_layout <- isolate(o_zoom$coord)
				}
				else {
					if (length(which(!is.na(isolate(o_zoom$coord)[[1]]))) > 0) {
						v_x_range <- isolate(o_zoom$coord)[[1]]
					}
					else {
						v_x_range <- NULL
					}
					
					if (length(which(!is.na(isolate(o_zoom$coord)[[2]]))) > 0) {
						v_y_range <- isolate(o_zoom$coord)[[2]]
					}
					else {
						v_y_range <- NULL
					}
					
					l_axis_layout <- list(v_x_range, v_y_range)
				}
			}
		}
	}
	else { # temporal & ir
		if (!is.null(isolate(o_zoom$coord))) {
			v_x_range <- isolate(o_zoom$coord)
		}
		else {
			if (s_data_type == "temporal") {
				v_x_range <- NULL
			}
			else { # ir
				v_x_range <- rev(v_freq_range)
			}
		}
		
		l_axis_layout <- list(v_x_range, NULL)
	}
	
	return(l_axis_layout)
}

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
# Description : (corplot, normal data type) the function allows to create new data to be displayed in a  
#               modal window (showModal) when a click event is executed on a XY cell of the correlation 
#               matrix.
#               
# Creation date : May 2021
#########################################################################################################


# Input:
# ------
# df_all: data saved in o_plot reactive value (o_plot$data)
# o_parameter: reactive values created in the R script "WIDEa_launcher"
# s_x_var: variable name corresponding to the selected cell in X axis 
# s_y_var: variable name corresponding to the selected cell in Y axis

# Output:
# -------
# return data

f_create_modal_data <- function (df_all, o_parameter, s_x_var, s_y_var) {
	if (!is.na(isolate(o_parameter$group))) {
		df_all <-  df_all[which(df_all[, isolate(o_parameter$group)] == isolate(o_parameter$select_graph)),]
	}
	
	df_all <- df_all[, c(s_x_var, s_y_var)]
	df_all <- df_all[!is.na(df_all[, 1]) & !is.na(df_all[, 2]),]
	
	return(df_all)
}

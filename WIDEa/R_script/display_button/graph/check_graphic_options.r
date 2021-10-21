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
# Description : function used to execute a checking process on parameters selected from the Graphic tab
#               (opacity, decimal number and bin width)
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# l_opt_name: list of selected parameters ("op" for opacity, "dec_num" for decimal number, "bw" for bin width)
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"

# Output:
# -------
# return an error message only if one of these parameters are not correctly informed

f_check_graphic_opt <- function (l_opt_name, o_plot = NULL, o_parameter) {
	v_message <- c()
	
	for (i in 1:length(l_opt_name)) {
		# Opacity
		
		if (names(l_opt_name)[i] == "op") {
			if (is.na(l_opt_name[[i]])) {
				v_message <- c(v_message, "The opacity value is not numeric")
			}
			else {
				n_max <- ifelse(isolate(o_parameter$plot_type) %in% c("plot", "boxplot", NA), 1, 0.7)
				
				if (l_opt_name[[i]] < 0 | l_opt_name[[i]] > n_max) {
					v_message <- c(v_message, paste0("The opacity value must be between 0 and ", n_max))
				}
			}
		}
		
		# Decimal number
		
		if (names(l_opt_name)[i] == "dec_num") {
			if (is.na(l_opt_name[[i]])) {
				v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
			}
			else {
				if (is.numeric(l_opt_name[[i]]) == F | length(grep("[.]", l_opt_name[[i]])) > 0) {
					v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
				}
				else {
					if (l_opt_name[[i]] < 0) {
						v_message <- c(v_message, "The decimal number must be an integer superior or equal to 0")
					}
				}
			}
		}
		
		# Bin width (histplot)
		
		if (names(l_opt_name)[i] == "bw") {
			if (!is.numeric(l_opt_name[[i]])) {
				v_message <- c(v_message, "Incorrect bin width")
			}
			else {
				n_val <- abs(diff(range(as.vector(isolate(o_plot$data)[, isolate(o_parameter$x)]))))
				
				if (l_opt_name[[i]] <= 0 | l_opt_name[[i]] >= n_val) {
					v_message <- c(v_message, paste0("The bin width must be between 0 and ", round(n_val, digits = 2)))
				}
			}
		}
	}
	
	return(v_message)
}

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
# Description : function used to execute a checking process on tp1 (Graphic tab) inputs
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# l_opt_name: list of selected parameters ("op" for opacity, "dec_num" for decimal number, "bw" for bin width)
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"

# Output:
# -------
# return an error/warning message only if one of these parameters are not correctly informed


f_check_tp1_inputs <- function (l_opt_name, o_plot = NULL, o_parameter = NULL) {
	v_message <- c()
	
	for (i in 1:length(l_opt_name)) {
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
				if (!is.na(isolate(o_parameter$f))) {
					n_val <- abs(diff(range(as.vector(isolate(o_plot$data)[, ".f."]))))
				}
				else {
					n_val <- abs(diff(range(as.vector(isolate(o_plot$data)[, isolate(o_parameter$x)]))))
				}
				
				if (l_opt_name[[i]] <= 0 | l_opt_name[[i]] >= n_val) {
					v_message <- c(v_message, paste0("The bin width must be between 0 and ", round(n_val, digits = 2)))
				}
			}
		}
	
		# Y-scale border fraction (temporal/IR data type)
		
		if (names(l_opt_name)[i] == "fraction") {
			if (is.na(l_opt_name[[i]])) {
				v_message <- c(v_message, "Fraction value is not numeric. So, the value is replaced by the standard value: 0.05.")
			}
			else {
				if (l_opt_name[[i]] < 0) {
					v_message <- c(v_message, "Fraction value is inferior to the minimal value (0). So, the value is replaced by the minimal value.")
				}
				
				if (l_opt_name[[i]] > 0.1) {
					v_message <- c(v_message, "Fraction value is superior to the maximal value (0.1). So, the value is replaced by the maximal value.")
				}
			}
		}
	}
	
	return(v_message)
}


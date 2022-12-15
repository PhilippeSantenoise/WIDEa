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
# Description : function used to create model data
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# v_f_val: fitted values (calculated from the f_check_model function)
# v_vfun_val: residual variance (calculated from the f_check_model function)

# Output:
# -------
# return a data frame with 1 or 2 columns (fitted values and residual variance if "sigma" informed in data loaded from the "Model parameter loading" section)

f_create_model_data <- function (v_f_val, v_vfun_val) {
	if (length(v_vfun_val) > 0) {
		df_model <-  data.frame("fit" = v_f_val, "variance" = v_vfun_val)
	}
	else {
		df_model <- data.frame("fit" = v_f_val)
	}
	
	return (df_model)
}

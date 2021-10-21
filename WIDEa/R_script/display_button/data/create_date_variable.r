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
# Description : function used to create the date variable (only used for the temporal data type)
#
# Creation date : May 2021
#########################################################################################################


# Input:
# -------
# df_all: data saved in the e_data environment (e_data$all) 
# s_x_var: X variable name
# s_date_format: date format (values from the format field in "Variable selection")

# Output:
# -------
# return the date variable

f_create_date_variable <- function (df_all, s_x_var, s_date_format) {
	df_all[, s_x_var] <- as.vector(df_all[, s_x_var])
	v_range <- c()
	
	if (is.numeric(df_all[, s_x_var]) & length(grep("[.]", df_all[, s_x_var])) == 0) {
		v_range <- range(df_all[, s_x_var])
	}
	else {
		df_all[, s_x_var] <- gsub("/| |:|-|[.]|h", "", df_all[, s_x_var])
	}
	
	v_date <- as.character(strptime(df_all[, s_x_var], format = s_date_format))
	i_num <- length(which(is.na(v_date)))
	
	if (length(v_range) > 0) {
		if (v_range[1] > 0 & v_range[2] < unclass(as.Date(Sys.time()) + 1)) {
			if (i_num > 0) {
				v_date <- as.character(as.Date(df_all[, s_x_var], origin = "1970-01-01", tz = "GMT"))
			}
		}
	}
	
	return (v_date)
}

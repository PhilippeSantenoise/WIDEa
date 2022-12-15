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
# Description : function used to create the concatenated variable (only used for the normal/ir data type)
#
# Creation date : September 2022
#########################################################################################################


# Input:
# -------
# df_1: data saved in the e_data environment (e_data$all or e_data$sub) 
# v_var: vector of variables

# Output:
# -------
# return the date variable

f_create_concat_variable <- function (df_1, v_var) {
	eval(parse(text = paste0("v_row <- unique(c(", paste(paste0("which(is.na(as.vector(df_1$", v_var, ")))"), collapse = ", "), "))")))
	eval(parse(text = paste0("v_out <- as.vector(paste(", paste(paste0("as.vector(df_1$", v_var, ")"), collapse = ", "), ", sep = \" \"))")))
	if (length(v_row) > 0) {v_out[v_row] <- NA}
	return(v_out)
}

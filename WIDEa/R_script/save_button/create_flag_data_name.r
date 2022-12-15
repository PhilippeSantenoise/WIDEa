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
# Description : function used to create the flag data name. The name is composed of 3 elements, 
#               - the data name loaded from the "select data" field (left panel);
#               - the data type;
#               - the term "withID" if an ID variable is precised.
#
# Creation date : May 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# v_split_path: vector corresponding to loaded data path splitted by the "/" symbol
# s_id_var: ID variable name (default = NA, if the corresponding field is not filled) 

# Output:
# -------
# return the flag data name

f_create_flag_data_name <- function (s_data_type = "normal", v_split_path, s_id_var = NA) {
	v_split_path_new <- unlist(strsplit(v_split_path[length(v_split_path)], split = "[.]"))
	
	if (s_data_type == "normal") {
		if (!is.na(s_id_var)) {
			s_flag_data_name <- paste0(paste(v_split_path_new[1:(length(v_split_path_new) - 1)], collapse = "."), "_norm_flag_withID.", v_split_path_new[length(v_split_path_new)])
		}
		else {
			s_flag_data_name <- paste0(paste(v_split_path_new[1:(length(v_split_path_new) - 1)], collapse = "."), "_norm_flag.", v_split_path_new[length(v_split_path_new)])
		}
	}
	else if (s_data_type == "temporal") {
		s_flag_data_name <- paste0(paste(v_split_path_new[1:(length(v_split_path_new) - 1)], collapse = "."), "_temp_flag.", v_split_path_new[length(v_split_path_new)])
	}
	else { # ir
		if (!is.na(s_id_var)) {
			s_flag_data_name <- paste0(paste(v_split_path_new[1:(length(v_split_path_new) - 1)], collapse = "."), "_ir_flag_withID.", v_split_path_new[length(v_split_path_new)])
		}
		else {
			s_flag_data_name <- paste0(paste(v_split_path_new[1:(length(v_split_path_new) - 1)], collapse = "."), "_ir_flag.", v_split_path_new[length(v_split_path_new)])
		}
	}
	
	return(s_flag_data_name)
}

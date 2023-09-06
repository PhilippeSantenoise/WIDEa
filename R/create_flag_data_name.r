#' Creating the flag data name

#' @description
#' `f_create_flag_data_name` used to create the flag data name. The name is composed
#' of 3 elements: (1) the data name loaded from the "select data" field (left
#' panel), (2) the data type and (3) the term "withID" if an ID variable is
#' precised.

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param v_split_path is a vector corresponding to loaded data path splitted by the
#' "/" symbol. 
#' @param s_id_var is the ID variable name (NA as default value, if the ID field is
#' not filled).

#' @encoding UTF-8

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

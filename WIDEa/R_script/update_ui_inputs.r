####################################################################################
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
# Description: (Server) Functions used to update UI's inputs
#                       
# Creation date: May 2022
####################################################################################

# Function inputs
# l_id_max_item: list of ID (selectize) inputs with the maximum item value
# v_choices: vector of choices for selectize inputs
# v_selected: vector of selected values for selectize inputs
# i_max_item: maximum item value for selectize inputs
# s_id: UI's input ID
# s_value: value for text inputs
# b_remove_modal: remove an existing modal window (used with expand buttons)
# s_selected: value for radio button inputs
# b_value: value for check box inputs
# df_match_id: data frame with matching IDs between UI's input and o_parameter reactive value (created with the function f_create_match_id_data)
# i_delay: add a delay (in ms) to update commands (no delay as default value)
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# s_plot_type: plot type (5 values: "plot", "boxplot", "histplot", "barplot", "corplot")
# s_mode: graph mode (3 values: "marker", "line", "line_marker")
# l_current_option: list with information ("choices" and "selected") on "edit_option" selectize input. "choices"/"selected" return the vector of choices and the selected value of the selectize input respectively. 


# Create command lines to update the maxitem option for selectize inputs 
f_update_maxitem_selectize_input <- function (l_id_max_item, v_choices) {
	s_choices <- paste0("c(", paste(paste0("\"", v_choices, "\""), collapse = ", "), ")")
	s_cmd <- paste(paste0("updateSelectizeInput(session, \"", names(l_id_max_item), "\", choices = ", s_choices, ", options = list(maxOptions = 9999, maxItems = ", as.vector(unlist(l_id_max_item)), "))"), sep = "; ")
	return(s_cmd)
}

# Create command lines to update a selectize input 
f_update_selectize_input <- function (s_id, v_choices, v_selected, i_max_item = NULL) {
	s_choices <- paste0("c(", paste(paste0("\"", v_choices, "\""), collapse = ", "), ")")
	s_selected <- paste0("c(", paste(paste0("\"", v_selected, "\""), collapse = ", "), ")")
	s_cmd <- paste0("updateSelectizeInput(session, \"", s_id, "\", choices = ", s_choices, ", selected = ", s_selected, ifelse(!is.null(i_max_item), paste0(", options = list(maxOptions = 9999, maxItems = ", i_max_item, ")"), ""), ")")
	return(s_cmd)
}

# Create command lines to update a text input
f_update_text_field_input <- function (s_id, s_value, b_remove_modal = F) {
	s_cmd <- paste0("updateTextInput(session, \"", s_id, "\", value = \"", s_value, "\")", ifelse(b_remove_modal, "; removeModal()", ""))
	return(s_cmd)
}

# Create command lines to update a radio button input
f_update_radio_button_input <- function (s_id, s_selected) {
	s_cmd <- paste0("updateRadioButtons(session, \"", s_id, "\", selected = \"", s_selected, "\")")
	return(s_cmd)
}

# Create command lines to update a check box input
f_update_check_box_input <- function (s_id, b_value) {
	s_cmd <- paste0("updateCheckboxInput(session, \"", s_id, "\", value = ", b_value, ")")
	return(s_cmd)
}

# Create command lines to update option (label, color/opacity, point type/size) selectize input
f_update_option_selectize_input <- function (s_data_type, s_plot_type, s_mode = "marker", l_current_option) {
	v_choices <- c("label", ifelse(s_plot_type != "corplot", "color/opacity", NA), ifelse((s_data_type == "normal" & s_plot_type %in% c("plot", "boxplot")) | (s_data_type != "normal" & s_mode != "line"), "point type/size", NA))
	v_choices <- v_choices[!is.na(v_choices)]
	
	if (length(which(l_current_option$choices %in% v_choices)) == length(l_current_option$choices) & length(which(v_choices %in% l_current_option$choices)) == length(v_choices)) {
		s_cmd <- character(0)
	}
	else {
		s_cmd <- f_update_selectize_input("edit_option", v_choices, ifelse(l_current_option$selected %in% v_choices, l_current_option$selected, v_choices[1]))
	}
	
	return(list(v_choices, s_cmd))
}

# Aggregate all command lines to update the vector of specified inputs
f_update_inputs <- function (l_id_value, df_all = NULL, l_selectize_option = list("concat" = rep(0, 2), "sub_data" = 0), i_delay = 0) {
	v_name <- names(l_id_value)
	l_id <- f_create_all_input_id_list("type")
	eval(parse(text = paste0("v_type <- ", paste(paste0("ifelse(v_name %in% l_id$", names(l_id)[-1], ", \"", names(l_id)[-1], "\""), collapse = ", "), ", \"", names(l_id)[1], "\"))))))"))) 
	
	v_cmd <- as.vector(unlist(lapply(unique(v_type), function(x) {
		v_sub_name <- v_name[which(v_type == x)]
		v_cmd_x <- c()
		
		if (x == "Selectize") {
			l_selectize_id_value <- f_create_input_value_list("selectize", df_all, l_selectize_option$concat, l_selectize_option$sub_data)
			v_choices <- paste0("l_selectize_id_value[[2]][[l_selectize_id_value[[1]]$", v_sub_name, "]]")
			v_max_item <- rep("NULL", length(v_sub_name))
			v_pos <- which(paste0("concat", 1:2) %in% v_name)
			if (length(v_pos) > 0) {eval(parse(text = paste(paste0("v_max_item[which(v_sub_name == \"", paste0("var_", c("x", "group"))[v_pos], "\")] <- ifelse(l_id_value[[\"", paste0("concat", 1:2)[v_pos], "\"]], 9999, 1)"), collapse = "; ")))} 
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_update_selectize_input(\"", v_sub_name, "\", ", v_choices, ", l_id_value$", v_sub_name, ", ", v_max_item, ")"), collapse = "; ")))
		}
		else {
			s_fun_name <- ifelse(x == "Text", "text_field", ifelse(x == "Radio", "radio_button", "check_box"))
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_update_", s_fun_name, "_input(\"", v_sub_name, "\", l_id_value$", v_sub_name, ")"), collapse = "; ")))
		}
		
		return(v_cmd_x)
	})))
	
	if (i_delay > 0) {
		s_cmd <- paste0("shinyjs::delay(", i_delay, ", {", paste(v_cmd, collapse = "; "), "})")
	}
	else {
		s_cmd <- paste(v_cmd, collapse = "; ")
	}
	
	return(s_cmd)
}

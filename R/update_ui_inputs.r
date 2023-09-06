#' List of functions used to update UI's inputs

#' @description
#' `f_update_maxitem_selectize_input` is used to create command lines to update the 
#' maxitem option for selectize inputs.
#' \cr`f_update_selectize_input` is used to create command lines to update a 
#' selectize input. 
#' \cr`f_update_text_field_input` is used to create command lines to update a text 
#' input. 
#' \cr`f_update_radio_button_input` is used to create command lines to update a 
#' radio button input. 
#' \cr`f_update_check_box_input` is used to create command lines to update a check 
#' box input. 
#' \cr`f_update_option_selectize_input` is used to create command lines to update 
#' option (label, color/opacity, point type/size) selectize input. 
#' \cr`f_update_inputs` is used to aggregate all command lines to update the vector 
#' of specified inputs. 

#' @param l_id_max_item is the list of ID (selectize) inputs with the maximum item
#' value.
#' @param v_choices is the vector of choices for selectize inputs.
#' @param v_selected is the vector of selected values for selectize inputs.
#' @param i_max_item is the maximum item value for selectize inputs.
#' @param s_id is the UI's input ID.
#' @param s_value is the value for text inputs.
#' @param b_remove_modal is a boolean value used to remove an existing modal window
#' (used with expand buttons).
#' @param s_selected is the value for radio button inputs.
#' @param b_value is the value for check box inputs.
#' @param i_delay is used to add a delay (in ms) and update commands (no delay as
#' default value).
#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_plot_type is the plot type (5 values: "plot", "boxplot", "histplot",
#' "barplot", "corplot").
#' @param s_mode is the plotly graph mode (3 values: "marker", "line",
#' "line_marker").
#' @param l_current_option is a list with information ("choices" and "selected") on
#' "edit_option" selectize input. "choices"/"selected" return the vector of choices
#' and the selected value of the selectize input respectively.
#' @param l_id_value is a list including UI's input IDs and values.
#' @param df_all are data saved in the e_data environment.
#' @param l_selectize_option is a list including binary values related to the
#' concatenation check boxes (1: checked; 0 else) and the sub-data create button (1:
#' clicked; 0 else).

#' @encoding UTF-8
 
f_update_maxitem_selectize_input <- function (l_id_max_item, v_choices) {
	s_choices <- paste0("c(", paste(paste0("\"", v_choices, "\""), collapse = ", "), ")")
	s_cmd <- paste(paste0("updateSelectizeInput(session, \"", names(l_id_max_item), "\", choices = ", s_choices, ", options = list(maxOptions = 9999, maxItems = ", as.vector(unlist(l_id_max_item)), "))"), sep = "; ")
	return(s_cmd)
}

#' @rdname f_update_maxitem_selectize_input 
f_update_selectize_input <- function (s_id, v_choices, v_selected, i_max_item = NULL) {
	s_choices <- paste0("c(", paste(paste0("\"", v_choices, "\""), collapse = ", "), ")")
	s_selected <- paste0("c(", paste(paste0("\"", v_selected, "\""), collapse = ", "), ")")
	s_cmd <- paste0("updateSelectizeInput(session, \"", s_id, "\", choices = ", s_choices, ", selected = ", s_selected, ifelse(!is.null(i_max_item), paste0(", options = list(maxOptions = 9999, maxItems = ", i_max_item, ")"), ""), ")")
	return(s_cmd)
}

#' @rdname f_update_maxitem_selectize_input
f_update_text_field_input <- function (s_id, s_value, b_remove_modal = F) {
	s_cmd <- paste0("updateTextInput(session, \"", s_id, "\", value = \"", s_value, "\")", ifelse(b_remove_modal, "; removeModal()", ""))
	return(s_cmd)
}

#' @rdname f_update_maxitem_selectize_input
f_update_radio_button_input <- function (s_id, s_selected) {
	s_cmd <- paste0("updateRadioButtons(session, \"", s_id, "\", selected = \"", s_selected, "\")")
	return(s_cmd)
}

#' @rdname f_update_maxitem_selectize_input
f_update_check_box_input <- function (s_id, b_value) {
	s_cmd <- paste0("updateCheckboxInput(session, \"", s_id, "\", value = ", b_value, ")")
	return(s_cmd)
}

#' @rdname f_update_maxitem_selectize_input
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

#' @rdname f_update_maxitem_selectize_input
f_update_inputs <- function (l_id_value, df_all = NULL, l_selectize_option = list("concat" = rep(0, 2), "sub_data" = 0), i_delay = 0) {
	v_name <- names(l_id_value)
	l_id <- f_create_all_input_id_list("type")
	v_type <- eval(parse(text = paste0(paste(paste0("ifelse(v_name %in% l_id$", names(l_id)[-1], ", \"", names(l_id)[-1], "\""), collapse = ", "), ", \"", names(l_id)[1], "\"))))))"))) 
	
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

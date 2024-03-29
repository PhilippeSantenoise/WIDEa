#' List of functions used to enable/disable UI's inputs

#' @description
#' `f_on_off_selectize_input` is used to create command lines to disable/enable a
#' selectize input. 
#' \cr`f_on_off_field_input` is used to create command lines to disable/enable a
#' field input (text, numeric).
#' \cr`f_on_off_action_button_input` is used to create command lines to
#' disable/enable an action button input.
#' \cr`f_on_off_radio_button_input` is used to create command lines to
#' disable/enable a radio button input.
#' \cr`f_on_off_check_box_input` is used to create command lines to disable/enable a
#' check box input. 
#' \cr`f_on_off_check_box_group_input` is used to create command lines to
#' disable/enable a check box group input.
#' \cr`f_init_input_status_data` is used to create an input id/status data frame
#' with initial value. Data are saved in the reactive value named o_input_status. 
#' \cr`f_create_input_status_list` is used to create the list of id/status inputs to
#' be disabled/enabled.
#' \cr`f_update_input_status_list` is used to check the input id/status data frame
#' (o_input_status) and update the list of inputs to be disabled/enabled. Return a
#' NULL value if all inputs are already enabled/disabled. 
#' \cr`f_update_sp_input_value_list` is used to update the list of id/value special
#' inputs. 
#' \cr`f_update_input_status_data` is used to update the input id/status data frame
#' (saved in o_input_status). 
#' \cr`f_on_off_inputs` is used to aggregate all command lines to disable/enable the
#' list of specified inputs. 

#' @param s_id is the input ID.
#' @param v_id is the vector of input IDs.
#' @param v_status is the vector of input status. 
#' @param i_status is a binary value corresponding to input status (0: disable ;
#' 1: enable).
#' @param v_choices is the vector of choices for selectize inputs (only used if 
#' `i_status` = 1).
#' @param i_max_item is the maximum number of items for a selectize input.
#' @param s_type is the field input type ("Text" or "Numeric").
#' @param n_value is the default value when a numeric input is enabled.
#' @param b_selected is a boolean value used to add a selected value for selectize
#' input (if `i_status` = 1).
#' @param s_selected is the selected value of radio button input.
#' @param l_id_status is the list of input IDs with status information obtained by
#' the o_on_off reactive value.
#' @param df_id_status is the data frame of input IDs with status information
#' obtained by the o_input_status reactive value.
#' @param b_plot_select is a boolean value related to plot_type and dim_num radio
#' buttons (T: clicked).
#' @param l_selectize_option is the selectize input options (five options). The
#' first option is the variable name of loaded data (e_data$all). The second option
#' is a vector of binary values given by o_cond reactive value (concat1, concat2). 
#' The third option is a list of inputs with ID and maximum number of items. The
#' fourth option is a binary value associated to sub-data (= 1 if created). The
#" fifth option is the variable name selected from the "vname" input (sub data
#' creation section).
#' @param i_sp_num is an integer (3 values: 0, 1 and 2) used to execute or not a
#' special enabling/desabling of UI's inputs. 0 is the default and corresponds to no
#' special enabling/desabling. 2 is only used for radio buttons.
#' @param l_sp_id_values is the list of id/value inputs associated to `i_sp_num`
#' value.
#' @param i_update is a binary value associated to o_cond$update reactive value.
#' @param df_all is the data frame saved in e_data environment (e_data$all).
#' @param s_style is the action button input style (values: NULL as default,
#' "default", "danger", "succes").
#' @param b_hide is a boolean value used to add a command line to hide the input.
#' @param v_selected is the vector of selected values for the check box group
#' inputs.

#' @encoding UTF-8

f_on_off_selectize_input <- function (s_id, v_choices, b_selected = F, i_max_item = NULL, i_status, i_sp_num = 0, i_update = 0, b_hide = F) {
	if (i_status == 0) {
		s_cmd <- paste0(ifelse(i_sp_num == 1, paste0("shinyjs::enable(\"", s_id, "\"); "), ""), "shiny::updateSelectizeInput(session, \"", s_id, "\", choices = \" \"", ifelse(!is.null(i_max_item), paste0(", options = list(maxOptions = 9999, maxItems = ", i_max_item, ")"), ""), "); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))", ifelse(b_hide, paste0("; shinyjs::delay(100, shinyjs::hide(\"", s_id, "\"))"), ""))
	}
	else {
		s_choices <- paste0("c(", paste(paste0("\"", v_choices, "\""), collapse = ", "), ")")
		s_cmd <- paste0("shinyjs::enable(\"", s_id, "\")", ifelse(i_update == 0, paste0("; shiny::updateSelectizeInput(session, \"", s_id, "\", choices = ", s_choices, ifelse(b_selected, paste0(", selected = \"", v_choices[1], "\""), ""), ifelse(!is.null(i_max_item), paste0(", options = list(maxOptions = 9999, maxItems = ", i_max_item, ")"), ""), ")"), ""), ifelse(i_sp_num == 1, paste0("; shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))"), ""))
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input
f_on_off_field_input <- function (s_id, s_type, i_status, n_value, b_hide = F) {
	if (i_status == 0) {
		s_cmd <- paste0("shiny::update", s_type, "Input(session, \"", s_id, "\", value = ", ifelse(s_type == "Text", "character", "numeric"), "(0)); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))", ifelse(b_hide, paste0("; shinyjs::delay(100, shinyjs::hide(\"", s_id, "\"))"), ""))
	}
	else {
		s_cmd <- paste0("shinyjs::enable(\"", s_id, "\")", ifelse(s_type != "Text", paste0("; shiny::updateNumericInput(session, \"", s_id, "\", value = ", n_value, ")"), ""))
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input 
f_on_off_action_button_input <- function (s_id, i_status, s_style = NULL) {
	if (i_status == 0) {
		if (!is.null(s_style)) {
			s_cmd <- paste0("shinyBS::updateButton(session, \"", s_id, "\", style = \"", s_style, "\"); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))")
		}
		else {
			s_cmd <- paste0("shinyjs::disable(\"", s_id, "\")")
		}
	}
	else {
		s_cmd <- paste0("shinyjs::enable(\"", s_id, "\")")
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input 
f_on_off_radio_button_input <- function (s_id, s_selected, i_status, i_sp_num = 0) {
	if (i_sp_num == 1) {
		s_cmd <- paste0("shiny::updateRadioButtons(session, \"", s_id, "\", selected = \"", s_selected, "\")")
	}
	else {
		if (i_status == 0) {
			s_cmd <- paste0("shiny::updateRadioButtons(session, \"", s_id, "\", selected = \"", s_selected, "\"); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))")
		}
		else {
			s_cmd <- paste0(ifelse(i_sp_num == 2, paste0("shiny::updateRadioButtons(session, \"", s_id, "\", selected = \"", s_selected, "\"); "), ""), "shinyjs::enable(\"", s_id, "\")")
		}
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input 
f_on_off_check_box_input <- function (s_id, i_status) {
	if (i_status == 0) {
		s_cmd <- paste0("shiny::updateCheckboxInput(session, \"", s_id, "\", value = F); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))")
	}
	else {
		s_cmd <- paste0("shinyjs::enable(\"", s_id, "\")")
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input 
f_on_off_check_box_group_input <- function (s_id, v_selected, i_status) {
	if (i_status == 0) {
		s_cmd <- paste0("shiny::updateCheckboxGroupInput(session, \"", s_id, "\", selected = character(0)); shinyjs::delay(100, shinyjs::disable(\"", s_id, "\"))")
	}
	else {
		s_selected <- paste0("c(", paste(paste0("\"", v_selected, "\""), collapse = ", "), ")")
		s_cmd <- paste0("shinyjs::enable(\"", s_id, "\"); shiny::updateCheckboxGroupInput(session, \"", s_id, "\", selected = ", s_selected, ")")
	}
	
	return(s_cmd)
}

#' @rdname f_on_off_selectize_input 
f_init_input_status_data <- function() {
	l_id <- f_create_all_input_id_list("section")
	v_panel <- eval(parse(text = paste0("c(", paste(paste0("rep(names(l_id[", 1:length(l_id), "]), length(l_id[[", 1:length(l_id), "]]))"), collapse = ", "), ")")))
	v_id <- as.vector(unlist(l_id))
	return(data.frame("panel" = v_panel, "id" = v_id, "status" = rep(0, length(v_id)), "special" = rep(0, length(v_id))))
}

#' @rdname f_on_off_selectize_input 
f_create_input_status_list <- function(v_id, v_status) {
	return(eval(parse(text = paste0("list(", paste(paste0(v_id, " = ", v_status), collapse = ", "), ")"))))
}

#' @rdname f_on_off_selectize_input    
f_update_input_status_list <- function(l_id_status, df_id_status, b_plot_select = F) {
	v_id <- as.vector(df_id_status[df_id_status$id %in% names(l_id_status), "id"])
	l_id_status <- l_id_status[v_id]
	v_pos <- which((as.vector(unlist(l_id_status)) - as.vector(df_id_status[df_id_status$id %in% names(l_id_status), "status"])) != 0)
	
	if (length(v_pos) > 0) {
		v_pos2 <- which(c("display_button", "rel_symbol", "vvalue1") %in% names(l_id_status)[v_pos])
		
		if (length(v_pos2) > 0) { # add inputs ("f_radio", "g_radio", "h_radio", "webgl", "vtype") with a special desabling (number = 2, 3)
			v_cond <- rep(F, 3)
			if (length(which(c(1, 2) %in% v_pos2)) == 2 | (length(v_pos2) == 1 & 2 %in% v_pos2)) {v_cond[1] <- ifelse(l_id_status[["rel_symbol"]] == 0, T, F)}
			if (!v_cond[1] & 1 %in% v_pos2) {v_cond[2] <- ifelse(l_id_status[["display_button"]] == 0, T, F)}
			if (3 %in% v_pos2) {v_cond[3] <- ifelse(l_id_status[["vvalue1"]] == 1, T, F)}
			
			if (T %in% v_cond) {
				l_sp_id_num <- f_create_input_sp_num_list()
				v_name <- names(l_sp_id_num)[which(as.vector(unlist(l_sp_id_num)) %in% c(2, 3))]
				if (v_cond[2] | v_cond[3]) {v_name <- eval(parse(text = paste0("v_name[", ifelse(v_cond[2], "-", ""), "which(v_name == \"vtype\")]")))}
				v_row <- which(df_id_status$id %in% v_name & df_id_status$special == 1)
				if (length(v_row) > 0) {v_pos <- sort(c(v_pos, which(names(l_id_status) %in% df_id_status[v_row, "id"])))}
			}
		}
		
		if ("ref_radio" %in% names(l_id_status)[v_pos]) { # add input ("f_radio") with a special desabling (number = 2) when a calibration/validation model is selected
			if (l_id_status[["ref_radio"]] == 1) {
				v_pos <- sort(c(v_pos, which(names(l_id_status) == "f_radio")))
			}
		}
		
		if (b_plot_select & !"ref_radio" %in% names(l_id_status)[v_pos]) { # plot_type/dim_num radio button is clicked
			v_row <- which(df_id_status$id %in% paste0(c("f", "g", "h"), "_text"))
			
			if (length(which(c("f_radio", "g_radio") %in% names(l_id_status)[v_pos])) > 0) { # remove (f_radio, g_radio) input enabling if status = 0 and special = 1 
				v_row <- c(v_row, which(df_id_status$id %in% c("f_radio", "g_radio") & df_id_status$status == 0 & df_id_status$special == 1))
			}
			
			if (length(v_row) > 0) {v_pos <- v_pos[!v_pos %in% which(names(l_id_status) %in% df_id_status[v_row, "id"])]}
		}
	
		if (length(v_pos) > 0) {
			return(l_id_status[v_pos])
		}
		else {
			return(NULL)
		}
	}
	else {
		return(NULL)
	}
}

#' @rdname f_on_off_selectize_input 
f_update_sp_input_value_list <- function(l_sp_id_values, l_id_status, df_id_status) {
	l_sp_id_num <- f_create_input_sp_num_list()
	v_status <- as.vector(unlist(l_id_status))
	v_pos <- which(v_status == 0 | (names(l_id_status) %in% names(l_sp_id_num)[which(as.vector(unlist(l_sp_id_num)) %in% c(2, 3))] & v_status == 1))
	
	if (length(v_pos) > 0) {
		df_id_sp <- df_id_status[df_id_status$id %in% names(l_id_status)[v_pos], c("id", "special")]
		v_pos <- which(df_id_sp$special == 1)
		
		if (length(v_pos) > 0) {
			eval(parse(text = paste(paste0("l_sp_id_values$", df_id_sp[v_pos, "id"], " <- NA"), collapse = "; ")))
		}
	}
	
	return(l_sp_id_values)
}

#' @rdname f_on_off_selectize_input 
f_update_input_status_data <- function(l_id_status, df_id_status, l_sp_id_values = list()) {
	df_id_status[df_id_status$id %in% names(l_id_status), "status"] <- as.vector(unlist(l_id_status[as.vector(df_id_status[df_id_status$id %in% names(l_id_status), "id"])]))
	
	if (length(l_sp_id_values) > 0) {
		v_pos <- which(is.na(as.vector(unlist(l_sp_id_values))))
		
		if (length(v_pos) > 0) {
			df_id_status[df_id_status$id %in% names(l_sp_id_values)[v_pos], "special"] <- 0
			
			if (length(v_pos) < length(names(l_sp_id_values))) {
				df_id_status[df_id_status$id %in% names(l_sp_id_values)[-v_pos], "special"] <- 1
			}
		}
		else {
			df_id_status[df_id_status$id %in% names(l_sp_id_values), "special"] <- 1
		}
	}
	
	return(df_id_status)
}

#' @rdname f_on_off_selectize_input 
f_on_off_inputs <- function(l_id_status, df_all = NULL, l_selectize_option = list("sub_var_name" = NULL, "concat" = rep(0, 2), "max_item" = list(), "sub_data" = 0, "update" = 0), l_sp_id_values = list()) {
	l_id <- f_create_all_input_id_list("type")
	v_sp_name <- c()
	
	if (length(l_sp_id_values) > 0) { # add input ("vtype") with special command lines (number = 3)
		v_pos <- which(!names(l_sp_id_values) %in% names(l_id_status))
		
		if (length(v_pos) > 0) {
			v_sp_name <- names(l_sp_id_values)[v_pos] 
			eval(parse(text = paste(paste0("l_id_status$", v_sp_name, " <- 0"), collapse = "; ")))
		}
	}
	
	v_name <- names(l_id_status)
	v_type <- eval(parse(text = paste0(paste(paste0("ifelse(v_name %in% l_id$", names(l_id)[-1], ", \"", names(l_id)[-1], "\""), collapse = ", "), ", \"", names(l_id)[1], "\"))))))"))) 
	
	v_cmd <- as.vector(unlist(lapply(unique(v_type), function(x) {
		v_sub_name <- v_name[which(v_type == x)]
		v_status <- as.vector(unlist(l_id_status[v_sub_name]))
		v_cmd_x <- c()
		
		if (x == "Selectize") {
			s_sub_var_type <- ifelse("rel_symbol" %in% names(l_id_status), ifelse(l_id_status[["rel_symbol"]] == 1 & "vtype" %in% names(l_sp_id_values), "quant", "qualit"), "qualit")
			l_id_value <- f_create_input_value_list("selectize", df_all, l_selectize_option$concat, l_selectize_option$sub_data, 0, l_selectize_option$sub_var_name, s_sub_var_type)
			v_max_item <- rep("NULL", length(v_sub_name))
			v_pos <- which(v_sub_name %in% names(l_selectize_option$max_item))
			if (length(v_pos) > 0) {v_max_item[v_pos] <- as.vector(unlist(l_selectize_option$max_item[v_sub_name[v_pos]]))}
			v_choices <- paste0("l_id_value[[2]][[l_id_value[[1]]$", v_sub_name, "]]")
			v_pos <- which(v_sub_name %in% names(l_sp_id_values) & v_status == 1)
			if (length(v_pos) > 0) {v_choices[v_pos] <- paste0("l_sp_id_values[[\"", v_sub_name, "\"]]")}
			v_selected <- rep("F", length(v_sub_name))
			v_pos <- which(v_sub_name == "vname" & v_status == 1) # add a selected value for "vname" input (subdata creation)
			if (length(v_pos) > 0) {v_selected[v_pos] <- "T"}
			v_hide <- rep("F", length(v_sub_name))
			b_cond <- ifelse("rel_symbol" %in% v_sub_name, ifelse(l_id_status[["rel_symbol"]] == 0, T, F), F)
			v_pos <- which(v_sub_name == "vvalue1" & v_status == 0) # add a command line to hide "vvalue1" input
			if (length(v_pos) > 0 & !b_cond) {v_hide[v_pos] <- "T"}
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_selectize_input(\"", v_sub_name, "\", ", v_choices, ", ", v_selected, ", ", v_max_item, ", l_id_status[[\"", v_sub_name, "\"]], ", ifelse(v_sub_name %in% names(l_sp_id_values), 1, 0), ", l_selectize_option$update, ", v_hide, ")"), collapse = "; ")))
		}
		else if (x %in% c("Numeric", "Text")) {
			v_value <- NA
			if (x == "Numeric") {v_value <- f_create_input_value_list("numeric")[v_sub_name]}
			v_hide <- rep("F", length(v_sub_name))
			v_pos <- which(v_sub_name == "vvalue2" & v_status == 0) # add a command line to hide "vvalue2" input
			if (length(v_pos) > 0) {v_hide[v_pos] <- "T"}
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_field_input(\"", v_sub_name, "\", \"", x, "\", l_id_status[[\"", v_sub_name, "\"]], " , v_value, ", ", v_hide, ")"), collapse = "; ")))
		}
		else if (x == "Action") {
			v_style <- rep("NULL", length(v_sub_name))
			v_pos <- which(v_sub_name == "create_button")
			if (length(v_pos) > 0) {v_style[v_pos] <- "\"default\""}
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_action_button_input(\"", v_sub_name, "\", l_id_status[[\"", v_sub_name, "\"]], ", v_style, ")"), collapse = "; ")))
		}
		else if (x == "Radio") {
			v_selected <- f_create_input_value_list("radio_button")[v_sub_name]
			v_pos <- which(v_sub_name %in% names(l_sp_id_values)[!is.na(as.vector(unlist(l_sp_id_values)))])
			if (length(v_pos) > 0) {v_selected[v_pos] <- as.vector(unlist(l_sp_id_values[v_sub_name[v_pos]]))}
			v_special <- rep(0, length(v_sub_name))
			v_pos <- which(v_sub_name %in% names(l_sp_id_values))
			
			if (length(v_pos) > 0) {
				l_sp_id_num <- f_create_input_sp_num_list()
				v_num <- as.vector(unlist(l_sp_id_num[v_sub_name[v_pos]]))
				v_special[v_pos] <- ifelse(v_sub_name[v_pos] %in% v_sp_name | (v_status[v_pos] == 0 & v_num %in% c(2, 3) & (is.na(l_sp_id_values[v_sub_name[v_pos]]) | "ref_radio" %in% v_sub_name[v_pos])), 1, v_num)
			}
			
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_radio_button_input(\"", v_sub_name, "\", \"", v_selected, "\", l_id_status[[\"", v_sub_name, "\"]], ", v_special, ")"), collapse = "; ")))
		}
		else if (x == "CheckBox") {
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_check_box_input(\"", v_sub_name, "\", l_id_status[[\"", v_sub_name, "\"]])"), collapse = "; ")))
		}
		else { # check box group input
			l_id_value <- f_create_input_value_list("check_box_group")
			eval(parse(text = paste(paste0("v_cmd_x[", 1:length(v_sub_name), "] <- f_on_off_check_box_group_input(\"", v_sub_name, "\", l_id_value[[\"", v_sub_name, "\"]], l_id_status[[\"", v_sub_name, "\"]])"), collapse = "; ")))
		}
		
		return(v_cmd_x)
	})))
	
	s_cmd <- paste(v_cmd, collapse = "; ")
	return(s_cmd)
}

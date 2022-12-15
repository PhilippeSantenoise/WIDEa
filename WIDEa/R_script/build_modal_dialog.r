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
# Description: (Server) Functions used to build a modal dialog box/popup window
#                       
# Creation date: June 2022
####################################################################################

# Function inputs:
# s_title: window title
# s_size: window size
# s_id: UI's input ID
# i_window_num: window number. Five values available: 1 = sub-data condition formula, 2 = f(x), 3 = g(y), 4 = h(z), 5 = weighted residuals function  
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# s_plot_type: plot type
# s_model: model type (3 values: "none", "calib", "valid")
# b_group: boolean value associated to the group radio button input (T if a group variable is added, F else)
# l_var: list of variables
# b_info_var: boolean value used to add a datatable including a warning message on missing variable(s)
# b_code_var: boolean value used to add a datatable including the code/variable used to write the function (f(x), g(y), h(z), weighted residuals)
# b_info_cond: boolean value used to add a datatable including sub-data condition information
# o_sdata_cond : reactive value including all conditions added
# l_new_value: list of values associated to the new condtion. Values are given by the following inputs respectively: "vname", "vtype", "rel_symbol", "vvalue1"/"vvalue2".
# s_selection: string value used as datatable option (row datatable selection)   
# s_option: option value (3 values: "label", "color/opacity", "point type/size"). The value is associated to the "edit_option" selectize input.
# s_sub_option: sub-option value available from "color/opacity" and "point type/size" options (NULL when the "label" option is selected). The 
#               sub-option has 2 values ("color", "opacity") for the "color/opacity" option and is associated to the "color_opacity" radio 
#               button input. The sub-option has 2 values ("type", "size") for the "point type/size" option and is associated to the 
#               "point_type_size" radio button input. 
# o_option_value: reactive values used to graph label/color/pch edition (o_label_text, o_name_option)
# s_exec: execution command used to update label data (2 values: "clear", "add"). This command is used to clear/add value in label data 
# v_row: vector of row number of label data used to apply the clear/add command (s_exec) 
# s_custom_text: string value added in label/color/pch data when the add command is selected 
# s_dim_num: string value associated to the "dim_num" radio button (2 values: "2d", "3d")
# df_all: data saved in o_plot reactive value (o_plot$data)
# o_parameter: reactive values created in the R script "WIDEa_launcher"
# s_x_cell_var: variable name corresponding to the selected cell in X axis (corplot) 
# s_y_cell_var: variable name corresponding to the selected cell in Y axis (corlplot)
# df_xy_cell: data associated to a click event on a XY cell of the correlation matrix (corplot, normal data type) 
# i_graph_num: number associated to the XY cell graph type (0: XY scatter-plot + X histplot + Y histplot; 1: XY scatter-plot; 2: X histplot; 3: Y histplot)
 

# Create command lines to build a modal dialog used to expand a text area. The text area is used to express the (sub-data) condition formula, 
# f(x), g(y), h(z) or the weighted residuals function.
f_build_modal_dialog_expand <- function (s_title = NULL, s_size = "l", s_id, i_window_num = 1, b_info_var = F, b_code_var = F, b_info_cond = F) {
	s_cmd <- paste0("showModal(modalDialog(title = \"", s_title, "\", 
		easyClose = F, 
		size = \"", s_size, "\", 
		textAreaInput(\"", s_id, "_modal\", NULL, input$", s_id, ", width = \"855px\", height = validateCssUnit(\"auto\"), resize = \"none\"),",
		ifelse(b_info_var, "HTML(\"<br>\"), DT::dataTableOutput(\"modal_info_var\"),", ""),
		ifelse(b_code_var, "HTML(\"<br>\"), DT::dataTableOutput(\"modal_code_var\"),", ""),
		ifelse(b_info_cond, "HTML(\"<br>\"), DT::dataTableOutput(\"condInfo_temp\"),", ""),
		"footer = tagList(actionButton(\"ok_button_exp", i_window_num, "\", \"Ok\"), actionButton(\"close_button_exp", i_window_num, "\", \"Close\"))
	))")
	
	return(s_cmd)
}

# Create a new dataset (in datatable format) including all (sub-data) conditions
f_create_cond_data <- function(o_sdata_cond, s_selection = "multiple") {
	v_value <- as.vector(unlist(lapply(1:length(isolate(o_sdata_cond$value)), function(i) {
		if (isolate(o_sdata_cond$var_type)[i] == "quant") {
			return(isolate(o_sdata_cond$value)[[i]])
		}
		else {
			return(paste(paste0("\"", isolate(o_sdata_cond$value)[[i]], "\""), collapse = ", "))
		}
	})))
	
	df_info <- data.frame("Condition" = paste0("c", 1:length(isolate(o_sdata_cond$var_name))), "Variable_name" = isolate(o_sdata_cond$var_name), "Relation_symbol" = isolate(o_sdata_cond$var_rel), "Value" = v_value)
	
	return(
		df_info %>%
		DT::datatable(
			rownames = F, 
			container = htmltools::withTags(table(class = 'display',
				thead(
					tr(
						th("Condition"),
						th("Variable name"),
						th("Relation symbol"),
						th("Value")  
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_info),
				scroller = T,
				bFilter = 0,
				initComplete = JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = s_selection
		) %>%
		formatStyle("Condition", fontWeight = "bold", backgroundColor = "#90A4AE")
	)
}

# Check if a new (sub-data) condition already exists. An error message (NULL as default) is returned by the function.  
f_check_cond_data <- function(o_sdata_cond, l_new_value) {
	s_e_message <- character(0)
	v_pos <- c(which(isolate(o_sdata_cond$var_name) == l_new_value[[1]]), which(isolate(o_sdata_cond$var_type) == l_new_value[[2]]), which(isolate(o_sdata_cond$var_rel) == l_new_value[[3]]))
	
	if (length(v_pos) > 0) {
		df_freq <- as.data.frame(addmargins(table(v_pos)))
		df_freq <- df_freq[-dim(df_freq)[1],]
		v_pos <- which(df_freq[, 2] == 3)
		
		if (length(v_pos) > 0) {
			v_num <- as.numeric(as.vector(df_freq[v_pos, 1]))
			
			if (l_new_value[[2]] == "qualit") {	
				v_val <- as.vector(unlist(lapply(v_num, function(x) {
					if (length(l_new_value[[4]]) == length(isolate(o_sdata_cond$value)[[x]])) {
						v_cond <- which(l_new_value[[4]] %in% isolate(o_sdata_cond$value)[[x]])
						return(ifelse(length(v_cond) == length(isolate(o_sdata_cond$value)[[x]]), 1, -1))
					}
					else {
						return (-1)
					}
				})))
				
				i_pos <- v_num[which(v_val == 1)]
			}
			else {
				v_val <- as.vector(unlist(lapply(v_num, function(x) {
					return(as.numeric(input$vvalue2) == isolate(o_sdata_cond$value)[[x]])
				})))
				
				i_pos <- v_num[which(v_val)]
			}
			
			if (length(i_pos) > 0) {
				s_e_message <- paste0("This condition has already been added (see c", i_pos, " with the info/clear button)")    
			}
		}
	}
	
	return(s_e_message)
}

# Create a list of two datasets (in datatable format) including (1) a warning message on missing variable(s) and (2) the code/variable used to write the function (f(x), g(y), h(z), weighted residuals)
f_create_modal_code_var_list <- function(s_data_type, s_model, i_window_num, l_var) {
	if (length(l_var) == 2) { # variables associated to f(x) (with random effects) or the weighted residuals function (with group) when a model is added
		if (is.null(l_var[[1]]) & is.null(l_var[[2]])) {
			v_code <- ""
			v_var <- paste0("No X/", ifelse(i_window_num == 2, "Random", "Weighted residuals group"), " variable is selected") 
		}
		else {
			if (is.null(l_var[[1]]) | is.null(l_var[[2]])) {
				if (i_window_num == 2) { # f(x)
					if (is.null(l_var[[1]])) {
						v_code <- c("", paste0("re", 1:length(l_var[[2]])))
						v_var <- c("No X variable is selected", l_var[[2]])
					}
					else {
						v_code <- c("", paste0("x", 1:length(l_var[[1]])))
						v_var <- c("No Random variable is selected", l_var[[1]])
					}
				}
				else { # weighted residuals
					if (is.null(l_var[[1]])) {
						v_code <- c("", paste0("gr", 1:length(l_var[[2]])))
						v_var <- c("No X variable is selected", l_var[[2]])
					}
					else {
						v_code <- c("", paste0("x", 1:length(l_var[[1]])))
						v_var <- c("No Weighted residuals group variable is selected", l_var[[1]])
					}
				}
			}
			else {
				if (i_window_num == 2) { # f(x)
					v_code <- c(paste0("x", 1:length(l_var[[1]])), paste0("re", 1:length(l_var[[2]])))
				}
				else { # weighted residuals
					v_code <- c(paste0("x", 1:length(l_var[[1]])), paste0("gr", 1:length(l_var[[2]])))
				}
				
				v_var <- c(l_var[[1]], l_var[[2]])
			}
		}
	}
	else {
		if (is.null(l_var[[1]])) {
			v_code <- ""
			v_var <- paste0("No ", ifelse(i_window_num %in% c(2, 5), "X", ifelse(i_window_num == 3, "Y", "Z")), " variable is selected") 
		}
		else {
			v_code <- ifelse(i_window_num %in% c(2, 5), "x", ifelse(i_window_num == 3, "y", "z"))
			
			if (s_data_type != "temporal") {
				if ((i_window_num != 3 & s_model != "none") | s_model == "none") {v_code <- paste0(v_code, 1:length(l_var[[1]]))}  
				v_var <- l_var[[1]]
			}
			else {
				v_var <- "All variables are designated by the same code"
			}
		}
	}
	
	df_code_var <- data.frame("Code" = v_code, "Variable" = v_var)
	dt_out1 <- NULL
	dt_out2 <- NULL
	v_pos <- which(df_code_var$Code == "")
	
	if (length(v_pos) > 0) {
		dt_out1 <- as.data.frame(df_code_var[v_pos, "Variable"]) %>%
		DT::datatable(
			rownames = F, 
			container = htmltools::withTags(table(class = 'display',
				thead(
					tr(th("Information"))
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = 1,
				bFilter = 0,
				initComplete = JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = "none"
		)
		
		df_code_var <- df_code_var[-v_pos,]
	}
	
	if (dim(df_code_var)[1] > 0) {
		dt_out2 <- df_code_var %>%
		DT::datatable(
			rownames = F, 
			container = htmltools::withTags(table(class = 'display',
				thead(
					tr(
						th("Code"),
						th("Variable")
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_code_var),
				scroller = T,
				bFilter = 0,
				initComplete = JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = "none"
		) %>%
		formatStyle("Code", fontWeight = "bold", backgroundColor = "#90A4AE")
	}
	
	return(list(dt_out1, dt_out2))
}

# Create a dataset (in datatable format) with customized labels/colors/point characteristics 
f_create_option_data <- function(s_option = "label", s_sub_option = NULL, o_option_value, s_data_type = NULL, s_plot_type = NULL, b_group = F, i_display = 0) {
	if (s_option == "label") {
		df_out <- data.frame("Label" = o_option_value$label_temp, "Text" = o_option_value$text_temp)
		
		dt_out <- df_out %>%
		DT::datatable(
			rownames = F, 
			container = htmltools::withTags(table(class = 'display',
				thead(
					tr(
						th("Label"),
						th("Text (custom)")  
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_out),
				scroller = T,
				bFilter = 0,
				initComplete = JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = "multiple"
		) %>%
		formatStyle("Label", fontWeight = "bold", backgroundColor = "#90A4AE")
	}
	else if (s_option == "color/opacity") {
		if (s_sub_option == "color") {
			df_out <- data.frame("Column" = o_option_value$name, "Hexadecimal_custom" = o_option_value$color_temp, "Color_custom" = rep("", length(o_option_value$name)))
			
			if (i_display == 1) {
				df_out$Color_default <- rep("", length(o_option_value$name))
				
				if (s_data_type != "temporal") {
					v_pos <- which(o_option_value$name %in% names(o_option_value$color_default))
					if (nrow(df_out) > length(v_pos)) {df_out$Color_default[c(1:nrow(df_out))[-v_pos]] <- "missing"}
				}
			}
			
			v_pos <- which(o_option_value$color_temp != "")
			
			if (length(v_pos) > 0) {
				if (i_display == 1) {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							thead(
								tr(
									th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
									th("Hexadecimal (custom)"),
									th("Color (custom)"),
									th("Color (default)")
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					formatStyle("Color_default", "Column", color = styleEqual(o_option_value$name, ifelse(df_out$Color_default == "missing", "red", "black")), backgroundColor = styleEqual(names(o_option_value$color_default), o_option_value$color_default)) %>%
					formatStyle("Color_custom", "Column", backgroundColor = styleEqual(o_option_value$name[which(o_option_value$color_temp != "")], o_option_value$color_temp[which(o_option_value$color_temp != "")]))
				}
				else {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							thead(
								tr(
									th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
									th("Hexadecimal (custom)"),
									th("Color (custom)")
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					formatStyle("Color_custom", "Column", backgroundColor = styleEqual(o_option_value$name[which(o_option_value$color_temp != "")], o_option_value$color_temp[which(o_option_value$color_temp != "")]))
				}
			}
			else {
				if (i_display == 1) {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							thead(
								tr(
									th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
									th("Hexadecimal (custom)"),
									th("Color (custom)"),
									th("Color (default)")
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					formatStyle("Color_default", "Column", color = styleEqual(o_option_value$name, ifelse(df_out$Color_default == "missing", "red", "black")), backgroundColor = styleEqual(names(o_option_value$color_default), o_option_value$color_default))
				}
				else {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							thead(
								tr(
									th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
									th("Hexadecimal (custom)"),
									th("Color (custom)")
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
				}
			}
		}
		else { # opacity
			df_out <- data.frame("Column" = o_option_value$name, "Opacity_custom" = o_option_value$opacity_temp)
			
			if (i_display == 1) {
				if (s_data_type != "temporal") {
					df_out$Opacity_default <- NA
					v_pos <- which(o_option_value$name %in% names(o_option_value$opacity_default))
					df_out$Opacity_default[v_pos] <- o_option_value$opacity_default
					if (nrow(df_out) > length(v_pos)) {df_out$Opacity_default[c(1:nrow(df_out))[-v_pos]] <- "missing"}
				}
				else {
					df_out$Opacity_default <- o_option_value$opacity_default
				}
			}
			
			if (i_display == 1) {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Opacity (custom)"),
								th("Opacity (default)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				formatStyle("Opacity_default", "Column", color = styleEqual(o_option_value$name, ifelse(df_out$Opacity_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Opacity (custom)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
			}
		}
	}
	else { # point type/size
		if (s_sub_option == "type") {
			df_out <- data.frame("Column" = o_option_value$name, "Point_type_custom" = o_option_value$point_type_temp)
			
			if (i_display == 1) {
				if (s_data_type != "temporal") {
					df_out$Point_type_default <- NA
					v_pos <- which(o_option_value$name %in% names(o_option_value$point_type_default))
					df_out$Point_type_default[v_pos] <- o_option_value$point_type_default
					if (nrow(df_out) > length(v_pos)) {df_out$Point_type_default[c(1:nrow(df_out))[-v_pos]] <- "missing"}
				}
				else {
					df_out$Point_type_default <- o_option_value$point_type_default
				}
			}
			
			if (i_display == 1) {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type  == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Point type (custom)"),
								th("Point type (default)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				formatStyle("Point_type_default", "Column", color = styleEqual(o_option_value$name, ifelse(df_out$Point_type_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Point type (custom)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
			}
		}
		else { # size
			df_out <- data.frame("Column" = o_option_value$name, "Point_size_custom" = o_option_value$point_size_temp)
			
			if (i_display == 1) {
				if (s_data_type != "temporal") {
					df_out$Point_size_default <- NA
					v_pos <- which(o_option_value$name %in% names(o_option_value$point_size_default))
					df_out$Point_size_default[v_pos] <- o_option_value$point_size_default
					if (nrow(df_out) > length(v_pos)) {df_out$Point_size_default[c(1:nrow(df_out))[-v_pos]] <- "missing"}
				}
				else {
					df_out$Point_size_default <- o_option_value$point_size_default
				}
			}
			
			if (i_display == 1) {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type  == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Point size (custom)"),
								th("Point size (default)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				formatStyle("Point_size_default", "Column", color = styleEqual(o_option_value$name, ifelse(df_out$Point_size_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						thead(
							tr(
								th(ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
								th("Point size (custom)")
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
			}
		}
	}
	
	return(dt_out)
}

# Update the dataset (in datatable format) with customized labels/colors/point characteristics 
f_update_option_data <- function(s_option = "label", s_sub_option = NULL, s_exec = "clear", o_option_value, v_row, s_custom_text = NULL, s_data_type = NULL, s_plot_type = NULL, b_group = F, i_display = 0) {
	if (s_exec %in% c("clear", "add")) {
		s_w_message <- character(0)
		dt_out <- NULL
		
		if (s_option == "label") {
			v_pos <- which(o_option_value$text_temp[v_row] == ifelse(s_exec == "clear", "", s_custom_text))
			
			if (length(v_pos) > 0) {
				s_w_message <- paste0("(", ifelse(length(v_pos) > 1, paste0(paste(o_option_value$label_temp[v_row[v_pos]], collapse = ", "), "): labels are"), paste0(o_option_value$label_temp[v_row[v_pos]], "): label is")), " already ", ifelse(s_exec == "clear", "deleted", "added"))
				v_row <- v_row[-v_pos]
			}
			
			if (length(v_row) > 0) {
				o_option_value$text_temp[v_row] <- ifelse(s_exec == "clear", "", s_custom_text)
				dt_out <- f_create_option_data(s_option, NULL, o_option_value)
			}
			
			return(list(s_w_message, o_option_value$text_temp, dt_out))
		}
		else if (s_option == "color/opacity") {
			if (s_sub_option == "color") {
				v_pos <- which(o_option_value$color_temp[v_row] == ifelse(s_exec == "clear", "", s_custom_text))
				
				if (length(v_pos) > 0) {
					s_w_message <- paste0("Color", ifelse(length(v_pos) > 1, "s are", " is"), " already ", ifelse(s_exec == "clear", "deleted", "added"), " for the following ", ifelse(s_data_type == "temporal", "variable", "level"), ifelse(length(v_pos) > 1, paste0("s: ", paste(o_option_value$name[v_row[v_pos]], collapse = ", ")), paste0(": ", o_option_value$name[v_row[v_pos]])))
					v_row <- v_row[-v_pos]
				}
				
				if (length(v_row) > 0) {
					o_option_value$color_temp[v_row] <- ifelse(s_exec == "clear", "", s_custom_text)
					dt_out <- f_create_option_data(s_option, s_sub_option, o_option_value, s_data_type, s_plot_type, b_group, i_display)
				}
				
				return(list(s_w_message, o_option_value$color_temp, dt_out))
			}
			else { # opacity
				v_pos <- which(o_option_value$opacity_temp[v_row] == ifelse(s_exec == "clear", "", s_custom_text))
				
				if (length(v_pos) > 0) {
					s_w_message <- paste0("Opacit", ifelse(length(v_pos) > 1, "ies are", "y is"), " already ", ifelse(s_exec == "clear", "deleted", "added"), " for the following ", ifelse(s_data_type == "temporal", "variable", "level"), ifelse(length(v_pos) > 1, paste0("s: ", paste(o_option_value$name[v_row[v_pos]], collapse = ", ")), paste0(": ", o_option_value$name[v_row[v_pos]])))
					v_row <- v_row[-v_pos]
				}
				
				if (length(v_row) > 0) {
					o_option_value$opacity_temp[v_row] <- ifelse(s_exec == "clear", "", s_custom_text)
					dt_out <- f_create_option_data(s_option, s_sub_option, o_option_value, s_data_type, s_plot_type, b_group, i_display)
				}
				
				return(list(s_w_message, o_option_value$opacity_temp, dt_out))
			}
		}
		else { # point type/size
			if (s_sub_option == "type") {
				v_pos <- which(o_option_value$point_type_temp[v_row] == ifelse(s_exec == "clear", "", s_custom_text))
				
				if (length(v_pos) > 0) {
					s_w_message <- paste0("Point type", ifelse(length(v_pos) > 1, "s are", " is"), " already ", ifelse(s_exec == "clear", "deleted", "added"), " for the following ", ifelse(s_data_type == "temporal", "variable", "level"), ifelse(length(v_pos) > 1, paste0("s: ", paste(o_option_value$name[v_row[v_pos]], collapse = ", ")), paste0(": ", o_option_value$name[v_row[v_pos]])))
					v_row <- v_row[-v_pos]
				}
				
				if (length(v_row) > 0) {
					o_option_value$point_type_temp[v_row] <- ifelse(s_exec == "clear", "", s_custom_text)
					dt_out <- f_create_option_data(s_option, s_sub_option, o_option_value, s_data_type, s_plot_type, b_group, i_display)
				}
				
				return(list(s_w_message, o_option_value$point_type_temp, dt_out))
			}
			else { # size
				v_pos <- which(o_option_value$point_size_temp[v_row] == ifelse(s_exec == "clear", "", s_custom_text))
				
				if (length(v_pos) > 0) {
					s_w_message <- paste0("Point size", ifelse(length(v_pos) > 1, "s are", " is"), " already ", ifelse(s_exec == "clear", "deleted", "added"), " for the following ", ifelse(s_data_type == "temporal", "variable", "level"), ifelse(length(v_pos) > 1, paste0("s: ", paste(o_option_value$name[v_row[v_pos]], collapse = ", ")), paste0(": ", o_option_value$name[v_row[v_pos]])))
					v_row <- v_row[-v_pos]
				}
				
				if (length(v_row) > 0) {
					o_option_value$point_size_temp[v_row] <- ifelse(s_exec == "clear", "", s_custom_text)
					dt_out <- f_create_option_data(s_option, s_sub_option, o_option_value, s_data_type, s_plot_type, b_group, i_display)
				}
				
				return(list(s_w_message, o_option_value$point_size_temp, dt_out))
			}
		}
	}
	else {
		return("unknown execution command (s_exec)")
	}
}

# Create a plotly as point type inventory
f_create_point_type_plotly <- function(s_dim_num = "2d") {
	if (s_dim_num == "2d") {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open", "star", "star-open", "hourglass-open", "bowtie-open", "circle-cross-open", "circle-x-open", "square-cross-open", "square-x-open", "diamond-cross-open", "diamond-x-open", "cross-thin-open", "x-thin-open", "asterisk-open", "hash-open")
		df_info <- data.frame(x = rep(c(1:8), 3), y = rep(c(3:1), each = 8), type = v_symbol, num = c(1:24))
	}
	else {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
		df_info <- data.frame(x = c(1:8), y = rep(1, 8), type = v_symbol, num = c(1:8))
	}
	
	ply_type <- plot_ly() %>% config(displaylogo = F, doubleClick = F, displayModeBar = F, staticPlot = T)
	ply_type <- add_trace(p = ply_type, x = df_info[, "x"], y = df_info[, "y"], hoverinfo = "none", type = "scatter", mode = "markers", marker = list(symbol = df_info[, "type"], size = 12, color = "black"))
	ply_type <- add_annotations(p = ply_type, x = df_info$x - 0.3, y = df_info$y, xref = "x", yref = "y", text = paste0(df_info$num, ":"), showarrow = F)
	ply_type <- layout(p = ply_type, xaxis = list(title = NULL, showticklabels = F, showgrid = F, zerolinecolor = "#ffff", fixedrange = T), yaxis = list(title = NULL, showticklabels = F, showgrid = F, zerolinecolor = "#ffff", fixedrange = T), showlegend = F, margin = list(l = 0, r = 0, b = 0, t = 0))
	return(ply_type)
}

# Create data associated to a click event on a XY cell of the correlation matrix (corplot, normal data type)
f_create_xy_cell_data <- function (df_all, o_parameter, s_x_cell_var, s_y_cell_var) {
	if (!is.na(isolate(o_parameter$group))) {df_all <-  df_all[which(df_all[, isolate(o_parameter$group)] == isolate(o_parameter$select_graph)),]}
	df_all <- df_all[, c(s_x_cell_var, s_y_cell_var)]
	df_all <- df_all[!is.na(df_all[, 1]) & !is.na(df_all[, 2]),]
	return(df_all)
}

# Create a plotly associated to a click event on a XY cell of the correlation matrix (corplot, normal data type)
f_create_xy_cell_plotly <- function (df_xy_cell,  i_graph_num) {
	s_x_var <- names(df_xy_cell)[1]
	s_y_var <- names(df_xy_cell)[2]
	b_cond <- i_graph_num %in% c(0, 1) & sd(df_xy_cell[, s_x_var]) > 0 & sd(df_xy_cell[, s_y_var]) > 0
	
	if (b_cond) { # add linear regression to XY scatter-plot
		eval(parse(text = paste0("l_lreg <- lm(", s_y_var, " ~ ", s_x_var, ", data = df_xy_cell)")))
		v_x <- seq(min(df_xy_cell[, s_x_var]), max(df_xy_cell[, s_x_var]), length.out = 1000) # sequence generation (size = 1000) from X variable
		n_intercept <- coef(l_lreg)[[1]]
		s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
		n_slope <- coef(l_lreg)[[2]]
		s_slope <- f_numeric_trsf(n_slope, b_pval = F)
		v_y <- n_intercept + n_slope * v_x
		n_test <- f_ttest(l_lreg, 2, 0) # slope = 0 t-test
		s_test <- f_numeric_trsf(n_test, b_pval = T)
		s_text <- paste0(s_y_var, " = a + b . ", s_x_var, "<br>a = ", s_intercept, "<br>b = ", s_slope, "<br>b = 0 test: p-value ", s_test)
	}
	
	if (i_graph_num == 0) {
		v_text <- paste0(s_x_var, ": ", df_xy_cell[, 1], "<br>", s_y_var, ": ", df_xy_cell[, 2])
		ply_1_1 <- plot_ly()
		ply_1_1 <- add_trace(p = ply_1_1, x = df_xy_cell[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_x_var)
		ply_1_1 <- layout(p = ply_1_1, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1_2 <- plot_ly()
		ply_1_2 <- add_trace(p = ply_1_2, x = df_xy_cell[, 1], y = df_xy_cell[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = v_text)
		
		if (b_cond) {
			ply_1_2 <- add_trace(p = ply_1_2, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 1), width = 3, dash = "dash"), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = s_text, showlegend = F)
		}
		
		ply_1_2 <- layout(p = ply_1_2, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		
		ply_1_3 <- plot_ly()
		ply_1_3 <- add_trace(p = ply_1_3, y = df_xy_cell[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_y_var)
		ply_1_3 <- layout(p = ply_1_3, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1 <- subplot(list(ply_1_1, plotly_empty(), ply_1_2, ply_1_3), nrows = 2, heights = c(0.3, 0.7), widths = c(0.7, 0.3), margin = 0, shareX = T, shareY = T, titleX = T, titleY = T) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		ply_1 <- layout(p = ply_1, showlegend = F)
	}
	else {
		ply_1 <- plot_ly() %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		
		if (i_graph_num == 1) {
			v_text <- paste0(s_x_var, ": ", df_xy_cell[, 1], "<br>", s_y_var, ": ", df_xy_cell[, 2])
			ply_1 <- add_trace(p = ply_1, x = df_xy_cell[, 1], y = df_xy_cell[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = v_text)
			
			if (b_cond) {
				ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 1), width = 3, dash = "dash"), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = s_text, showlegend = F)
			}
			
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		}
		else if (i_graph_num == 2) {
			ply_1 <- add_trace(p = ply_1, x = df_xy_cell[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_x_var)
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
		else {
			ply_1 <- add_trace(p = ply_1, x = df_xy_cell[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_y_var)
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_y_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
	}
	
	return(ply_1)
}
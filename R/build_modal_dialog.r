#' @importFrom htmltools tag
#' @importFrom stats sd coef lm
#' @importFrom grDevices adjustcolor
#' @importFrom plotly plotly_empty 
NULL

#' Functions associated to a modal dialog box/popup window

#' @description
#' `f_build_modal_dialog_expand` returns a command line used to build a modal 
#' dialog. The modal dialog allows to expand a text area. The text area. The text
#' area is used to express the (sub-data) condition formula, f(x), g(y), h(z) or the
#' weighted residuals function.
#' \cr`f_create_cond_data` returns a dataset (in datatable format) including all
#' (sub-data) conditions.
#' \cr`f_check_cond_data` is used to check if a new (sub-data) condition already
#' exists. An error message (NULL as default) is returned by the function.
#' \cr`f_create_modal_code_var_list` returns a list of two datasets (in datatable
#' format) including (1) a warning message on missing variable(s) and (2) the
#' code/variable used to write the function (f(x), g(y), h(z), weighted residuals).
#' \cr`f_create_option_data` allows to create a new dataset (in datatable format)
#' with customized labels/colors/point characteristics.
#' \cr`f_update_option_data` allows to update the existing dataset (in datatable
#' format) with customized labels/colors/point characteristics.
#' \cr`f_create_point_type_plotly` returns a plotly used as a point type inventory.
#' \cr`f_create_xy_cell_data` returns data associated to a click event on a XY cell
#' of the correlation matrix (corplot, normal data type).
#' \cr`f_create_xy_cell_plotly` returns a plotly associated to a click event on a XY
#' cell of the correlation matrix (corplot, normal data type).

#' @param s_title is the tittle of the modal dialog window (NULL as default value).
#' @param s_size is the size of the modal dialog window. The size is a string value
#' and three values are avaible: "l" (large), "m" (medium) and "s" (small).
#' @param s_id is the UI's input ID.
#' @param i_window_num is an integer corresponding to the window number. Five values
#' are available: 1 (sub-data condition formula), 2 (f(x)), 3 (g(y)), 4 (h(z)) and 5 
#' (weighted residuals function).  
#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_plot_type is the plot type (4 values: "plot", "boxplot", "histplot", 
#' "barplot").
#' @param s_model is the model type (3 values: "none", "calib", "valid").
#' @param b_group is a boolean value associated to the group radio button input (T 
#' if a group variable is added, F else).
#' @param l_var is the list of variables filled in the X, Y, Z and weigthed 
#' residual group selectize inputs.
#' @param b_info_var is a boolean value used to select a datatable including a 
#' warning message on missing variable(s).
#' @param b_code_var is a boolean value used to select a datatable including the 
#' code/variable columns. Variables in the datatable are listed in `l_var` and are
#' associated to an unique code used to write the following function: f(x), g(y),
#' h(z), weighted residuals.
#' @param b_info_cond is a boolean value used to select a datatable including 
#' sub-data condition information.
#' @param o_sdata_cond is a reactive value including all conditions added for 
#' sub-data.
#' @param l_new_value is a list of values associated to the new (sub-data) condtion.
#' Values are given by the following inputs respectively: "vname", "vtype",
#' "rel_symbol", "vvalue1"/"vvalue2".
#' @param s_selection is a string value used as datatable option (row datatable 
#' selection).   
#' @param s_option is a string value (3 values: "label", "color/opacity", 
#' "point type/size"). The value is associated to the "edit_option" selectize input.
#' @param s_sub_option is a string value available from "color/opacity" and 
#' "point type/size" options (NULL when the "label" option is selected). The
#' sub-option has 2 values ("color", "opacity") for the "color/opacity" option and 
#' is associated to the "color_opacity" radio button input. The sub-option has 2
#' values ("type", "size") for the "point type/size" option and is associated to the 
#' "point_type_size" radio button input. 
#' @param o_option_value is a reactive value used to graph label/color/pch edition
#' (o_label_text, o_name_option).
#' @param s_exec is a string value used to update label data (2 values: "clear", 
#' "add"). This command is used to clear/add value in label data. 
#' @param v_row is the vector of row number of label data used to apply the 
#' clear/add command (s_exec). 
#' @param s_custom_text is a string value added in label/color/pch data when the add
#' command is selected. 
#' @param s_dim_num is a string value associated to the "dim_num" radio button (2 
#' values: "2d", "3d").
#' @param df_all is data saved in o_plot reactive value (o_plot$data).
#' @param o_parameter is a reactive value used to build the plotly graph. 
#' @param s_x_cell_var is a variable name corresponding to the selected cell in X 
#' axis (corplot).
#' @param s_y_cell_var is a variable name corresponding to the selected cell in Y
#' axis (corlplot).
#' @param df_xy_cell is data associated to a click event on a XY cell of the 
#' correlation matrix (corplot, normal data type). 
#' @param i_graph_num is an integer value associated to the XY cell graph type (0: 
#' XY scatter-plot + X histplot + Y histplot; 1: XY scatter-plot; 2: X histplot; 3:
#' Y histplot).
#' @param i_display is a binary value related to the display button status (1:
#' clicked; 0: not clicked).

#' @encoding UTF-8

f_build_modal_dialog_expand <- function (s_title = NULL, s_size = "l", s_id, i_window_num = 1, b_info_var = F, b_code_var = F, b_info_cond = F) {
	s_cmd <- paste0("shiny::showModal(shiny::modalDialog(title = \"", s_title, "\", 
		easyClose = F, 
		size = \"", s_size, "\", 
		shiny::textAreaInput(\"", s_id, "_modal\", NULL, input$", s_id, ", width = \"855px\", height = htmltools::validateCssUnit(\"auto\"), resize = \"none\"),",
		ifelse(b_info_var, "htmltools::HTML(\"<br>\"), DT::dataTableOutput(\"modal_info_var\"),", ""),
		ifelse(b_code_var, "htmltools::HTML(\"<br>\"), DT::dataTableOutput(\"modal_code_var\"),", ""),
		ifelse(b_info_cond, "htmltools::HTML(\"<br>\"), DT::dataTableOutput(\"condInfo_temp\"),", ""),
		"footer = htmltools::tagList(shiny::actionButton(\"ok_button_exp", i_window_num, "\", \"Ok\"), shiny::actionButton(\"close_button_exp", i_window_num, "\", \"Close\"))
	))")
	
	return(s_cmd)
}

#' @rdname f_build_modal_dialog_expand
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
				tag("thead",
					list(
						tag("tr", 
							list(
								tag("th", "Condition"),
								tag("th", "Variable name"),
								tag("th", "Relation symbol"),
								tag("th", "Value")  
							)
						)
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_info),
				scroller = T,
				bFilter = 0,
				initComplete = htmlwidgets::JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = s_selection
		) %>%
		DT::formatStyle("Condition", fontWeight = "bold", backgroundColor = "#90A4AE")
	)
}

#' @rdname f_build_modal_dialog_expand
f_check_cond_data <- function(o_sdata_cond, l_new_value) {
	s_e_message <- character(0)
	v_pos <- c(which(isolate(o_sdata_cond$var_name) == l_new_value[[1]]), which(isolate(o_sdata_cond$var_type) == l_new_value[[2]]), which(isolate(o_sdata_cond$var_rel) == l_new_value[[3]]))
	
	if (length(v_pos) > 0) {
		df_freq <- as.data.frame(stats::addmargins(table(v_pos)))
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
					return(as.numeric(l_new_value[[4]]) == isolate(o_sdata_cond$value)[[x]])
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

#' @rdname f_build_modal_dialog_expand
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
				tag("thead", 
					list(
						tag("tr", 
							list(
								tag("th", "Information")
							)
						)
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = 1,
				bFilter = 0,
				initComplete = htmlwidgets::JS(
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
				tag("thead",
					list(
						tag("tr",
							list(
								tag("th", "Code"),
								tag("th", "Variable")
							)
						)
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_code_var),
				scroller = T,
				bFilter = 0,
				initComplete = htmlwidgets::JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = "none"
		) %>%
		DT::formatStyle("Code", fontWeight = "bold", backgroundColor = "#90A4AE")
	}
	
	return(list(dt_out1, dt_out2))
}

#' @rdname f_build_modal_dialog_expand
f_create_option_data <- function(s_option = "label", s_sub_option = NULL, o_option_value, s_data_type = NULL, s_plot_type = NULL, b_group = F, i_display = 0) {
	if (s_option == "label") {
		df_out <- data.frame("Label" = o_option_value$label_temp, "Text" = o_option_value$text_temp)
		
		dt_out <- df_out %>%
		DT::datatable(
			rownames = F, 
			container = htmltools::withTags(table(class = 'display',
				tag("thead",
					list(
						tag("tr",
							list(
								tag("th", "Label"),
								tag("th", "Text (custom)")  
							)
						)
					)
				)
			)),
			options = list(
				dom = "ft", 
				pageLength = nrow(df_out),
				scroller = T,
				bFilter = 0,
				initComplete = htmlwidgets::JS(
					"function(settings, json) {",
					"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
					"}"
				)
			),
			selection = "multiple"
		) %>%
		DT::formatStyle("Label", fontWeight = "bold", backgroundColor = "#90A4AE")
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
							tag("thead", 
								list(
									tag("tr",
										list(
											tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
											tag("th", "Hexadecimal (custom)"),
											tag("th", "Color (custom)"),
											tag("th", "Color (default)")
										)
									)
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = htmlwidgets::JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					DT::formatStyle("Color_default", "Column", color = DT::styleEqual(o_option_value$name, ifelse(df_out$Color_default == "missing", "red", "black")), backgroundColor = DT::styleEqual(names(o_option_value$color_default), o_option_value$color_default)) %>%
					DT::formatStyle("Color_custom", "Column", backgroundColor = DT::styleEqual(o_option_value$name[which(o_option_value$color_temp != "")], o_option_value$color_temp[which(o_option_value$color_temp != "")]))
				}
				else {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							tag("thead",
								list(
									tag("tr",
										list(
											tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
											tag("th", "Hexadecimal (custom)"),
											tag("th", "Color (custom)")
										)
									)
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = htmlwidgets::JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					DT::formatStyle("Color_custom", "Column", backgroundColor = DT::styleEqual(o_option_value$name[which(o_option_value$color_temp != "")], o_option_value$color_temp[which(o_option_value$color_temp != "")]))
				}
			}
			else {
				if (i_display == 1) {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							tag("thead",
								list(
									tag("tr",
										list(
											tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
											tag("th", "Hexadecimal (custom)"),
											tag("th", "Color (custom)"),
											tag("th", "Color (default)")
										)
									)
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = htmlwidgets::JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
					DT::formatStyle("Color_default", "Column", color = DT::styleEqual(o_option_value$name, ifelse(df_out$Color_default == "missing", "red", "black")), backgroundColor = DT::styleEqual(names(o_option_value$color_default), o_option_value$color_default))
				}
				else {
					dt_out <- df_out %>%
					DT::datatable(
						rownames = F, 
						container = htmltools::withTags(table(class = 'display',
							tag("thead",
								list(
									tag("tr",
										list(
											tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
											tag("th", "Hexadecimal (custom)"),
											tag("th", "Color (custom)")
										)
									)
								)
							)
						)),
						options = list(
							dom = "ft", 
							pageLength = nrow(df_out),
							scroller = T,
							bFilter = 0,
							initComplete = htmlwidgets::JS(
								"function(settings, json) {",
								"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
								"}"
							)
						),
						selection = "multiple"
					) %>%
					DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
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
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Opacity (custom)"),
										tag("th", "Opacity (default)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				DT::formatStyle("Opacity_default", "Column", color = DT::styleEqual(o_option_value$name, ifelse(df_out$Opacity_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type %in% c("boxplot", "barplot"), "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Opacity (custom)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
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
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type  == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Point type (custom)"),
										tag("th", "Point type (default)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				DT::formatStyle("Point_type_default", "Column", color = DT::styleEqual(o_option_value$name, ifelse(df_out$Point_type_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Point type (custom)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
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
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type  == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Point size (custom)"),
										tag("th", "Point size (default)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE") %>%
				DT::formatStyle("Point_size_default", "Column", color = DT::styleEqual(o_option_value$name, ifelse(df_out$Point_size_default == "missing", "red", "black")))
			}
			else {
				dt_out <- df_out %>%
				DT::datatable(
					rownames = F, 
					container = htmltools::withTags(table(class = 'display',
						tag("thead",
							list(
								tag("tr",
									list(
										tag("th", ifelse(s_data_type == "temporal", "Variable (Y)", ifelse(s_plot_type == "boxplot", "Level (X)", ifelse(b_group, "Level (Group)", "Level")))),
										tag("th", "Point size (custom)")
									)
								)
							)
						)
					)),
					options = list(
						dom = "ft", 
						pageLength = nrow(df_out),
						scroller = T,
						bFilter = 0,
						initComplete = htmlwidgets::JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#367BB4FF', 'color': '#fff'});",
							"}"
						)
					),
					selection = "multiple"
				) %>%
				DT::formatStyle("Column", fontWeight = "bold", backgroundColor = "#90A4AE")
			}
		}
	}
	
	return(dt_out)
}

#' @rdname f_build_modal_dialog_expand
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

#' @rdname f_build_modal_dialog_expand
f_create_point_type_plotly <- function(s_dim_num = "2d") {
	if (s_dim_num == "2d") {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open", "star", "star-open", "hourglass-open", "bowtie-open", "circle-cross-open", "circle-x-open", "square-cross-open", "square-x-open", "diamond-cross-open", "diamond-x-open", "cross-thin-open", "x-thin-open", "asterisk-open", "hash-open")
		df_info <- data.frame(x = rep(c(1:8), 3), y = rep(c(3:1), each = 8), type = v_symbol, num = c(1:24))
	}
	else {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
		df_info <- data.frame(x = c(1:8), y = rep(1, 8), type = v_symbol, num = c(1:8))
	}
	
	ply_type <- plotly::plot_ly() %>% plotly::config(displaylogo = F, doubleClick = F, displayModeBar = F, staticPlot = T)
	ply_type <- plotly::add_trace(p = ply_type, x = df_info[, "x"], y = df_info[, "y"], hoverinfo = "none", type = "scatter", mode = "markers", marker = list(symbol = df_info[, "type"], size = 12, color = "black"))
	ply_type <- plotly::add_annotations(p = ply_type, x = df_info$x - 0.3, y = df_info$y, xref = "x", yref = "y", text = paste0(df_info$num, ":"), showarrow = F)
	ply_type <- plotly::layout(p = ply_type, xaxis = list(title = NULL, showticklabels = F, showgrid = F, zerolinecolor = "#ffff", fixedrange = T), yaxis = list(title = NULL, showticklabels = F, showgrid = F, zerolinecolor = "#ffff", fixedrange = T), showlegend = F, margin = list(l = 0, r = 0, b = 0, t = 0))
	return(ply_type)
}

#' @rdname f_build_modal_dialog_expand
f_create_xy_cell_data <- function (df_all, o_parameter, s_x_cell_var, s_y_cell_var) {
	if (!is.na(isolate(o_parameter$group))) {df_all <-  df_all[which(df_all[, isolate(o_parameter$group)] == isolate(o_parameter$select_graph)),]}
	df_all <- df_all[, c(s_x_cell_var, s_y_cell_var)]
	df_all <- df_all[!is.na(df_all[, 1]) & !is.na(df_all[, 2]),]
	return(df_all)
}

#' @rdname f_build_modal_dialog_expand
f_create_xy_cell_plotly <- function (df_xy_cell,  i_graph_num) {
	s_x_var <- names(df_xy_cell)[1]
	s_y_var <- names(df_xy_cell)[2]
	b_cond <- i_graph_num %in% c(0, 1) & sd(df_xy_cell[, s_x_var]) > 0 & sd(df_xy_cell[, s_y_var]) > 0
	
	if (b_cond) { # add linear regression to XY scatter-plot
		l_lreg <- eval(parse(text = paste0("lm(", s_y_var, " ~ ", s_x_var, ", data = df_xy_cell)")))
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
		ply_1_1 <- plotly::plot_ly()
		ply_1_1 <- plotly::add_trace(p = ply_1_1, x = df_xy_cell[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_x_var)
		ply_1_1 <- plotly::layout(p = ply_1_1, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1_2 <- plotly::plot_ly()
		ply_1_2 <- plotly::add_trace(p = ply_1_2, x = df_xy_cell[, 1], y = df_xy_cell[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = v_text)
		
		if (b_cond) {
			ply_1_2 <- plotly::add_trace(p = ply_1_2, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 1), width = 3, dash = "dash"), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = s_text, showlegend = F)
		}
		
		ply_1_2 <- plotly::layout(p = ply_1_2, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		
		ply_1_3 <- plotly::plot_ly()
		ply_1_3 <- plotly::add_trace(p = ply_1_3, y = df_xy_cell[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_y_var)
		ply_1_3 <- plotly::layout(p = ply_1_3, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1 <- plotly::subplot(list(ply_1_1, plotly_empty(), ply_1_2, ply_1_3), nrows = 2, heights = c(0.3, 0.7), widths = c(0.7, 0.3), margin = 0, shareX = T, shareY = T, titleX = T, titleY = T) %>% plotly::config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		ply_1 <- plotly::layout(p = ply_1, showlegend = F)
	}
	else {
		ply_1 <- plotly::plot_ly() %>% plotly::config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		
		if (i_graph_num == 1) {
			v_text <- paste0(s_x_var, ": ", df_xy_cell[, 1], "<br>", s_y_var, ": ", df_xy_cell[, 2])
			ply_1 <- plotly::add_trace(p = ply_1, x = df_xy_cell[, 1], y = df_xy_cell[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = v_text)
			
			if (b_cond) {
				ply_1 <- plotly::add_trace(p = ply_1, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 1), width = 3, dash = "dash"), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text', text = s_text, showlegend = F)
			}
			
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		}
		else if (i_graph_num == 2) {
			ply_1 <- plotly::add_trace(p = ply_1, x = df_xy_cell[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_x_var)
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
		else {
			ply_1 <- plotly::add_trace(p = ply_1, x = df_xy_cell[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), name = s_y_var)
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_y_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
	}
	
	return(ply_1)
}
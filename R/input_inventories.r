#' List of functions associated to UI's inputs 

#' @description
#' `f_create_all_input_id_list` is used to create a list with all UI's input IDs
#' ordered by panel section or type (selectize, numeric, text, action button, radio
#' button, check box).
#' \cr`f_create_match_id_data` is used to create a data frame with matching IDs
#' between UI's input and o_parameter reactive value. Data are used to update inputs
#' in the variable selection section (left panel).
#' \cr`f_create_input_value_list` allows to create a list with (selectize, radio
#' button, check box group) input default values.
#' \cr`f_create_input_sp_num_list` allows to create a list for radio button inputs
#' with number used as a special enabling/desabling.
#' \cr`f_create_input_maxitem_list` is used to create a list for selectize input IDs
#' ("var_x", "var_y") with new maximum item values to be replaced.
#' \cr`f_update_input_maxitem_list` is used to update lists of selectize input ID 
#' with the maximum item value (saved in o_selectize_input reactive value).
#' \cr`f_create_varsel_input_id_vector` is used to create a vector of input IDs 
#' associated to the variable selection section (left panel). The input IDs are used
#' to save the status (enabled/disabled) once the display button is clicked.  
#' \cr`f_create_t3_input_id_vector` is used to create a vector of input IDs
#' associated to the Statistics tab.
#' \cr`f_create_input_id_vector` allows to create a vector of input IDs enabled when
#' a data/graph type and/or model procedure is selected.

#' @param s_by is a string value used to order UI's input inventory (two values: 
#' "section" as default, "type").
#' @param s_type is a string value associated to the input type (four values: 
#' "selectize" as default, "radio_button", "check_box_group", "numeric").
#' @param df_all is a data frame saved in e_data environment (e_data$all).
#' @param v_concat is a vector of two binary values (given by o_cond reactive
#' value: concat1, concat2) used to remove variables named ".concat1." and 
#' ".concat2." (respectively) from e_data$all.
#' @param i_concat is an integer associated to o_cond$concat1 reactive value.
#' @param b_concat1 is a boolean value associated to concat1 check box.
#' @param b_concat2 is a boolean value associated to concat2 check box.
#' @param i_sub_data is a binary value associated to sub-data (e_data$sub). The
#' value is equals to 1 if sub-data are created.
#' @param i_only_value is an integer value used to return specific values of 
#' selectize inputs (values: 0, 1 or 2). O returns a list of all selectize input
#' choices. 1 only returns the vector of variable names available from df_all
#' (e_data environment). 2 only returns the vector of unique ordered values 
#' associated to the variable selected from the "vname" selectize input (subdata
#' creation section).
#' @param s_sub_var_name is a string value returned by the "vname" selectize input
#' (NULL as default value).
#' @param s_sub_var_type is a string value returned by the "vtype" radio button
#' input (2 values: "qualit", "quanti"). 
#' @param s_data is the data type (three values: "normal", "temporal", "ir").
#' @param s_graph is the graph type (six values: "plot_2d", "plot_3d", "histplot",
#' "corplot", "boxplot", "barplot").
#' @param s_model is the model procedure (three values: "none" as default, "calib",
#' "valid"). This option is only available if s_data is "normal" and s_graph is
#' "plot_2d".
#' @param b_display is a boolean value associated to the display button (clicked =
#' T; not clicked = F).
#' @param b_load2 is a boolean value associated to the load2 button (clicked = T;
#' not clicked = F).
#' @param b_load3 is a boolean value associated to the load3 button (clicked = T;
#' not clicked = F).
#' @param o_selectize_input is a reactive value with max item information for
#' selectize inputs (ID: "var_x", "var_y").
#' @param l_id_status is a list of input IDs with status information obtained by the
#' reactive value o_on_off.
#' @param df_id_status is a data frame of input IDs with status information obtained
#' by the reactive value o_input_status.
#' @param b_flag is a boolean value associated to e_data$flag (T if flag data exist
#' in e_data environment, F else).

#' @encoding UTF-8

f_create_all_input_id_list <- function(s_by = "section") {
	v_fun_name <- c("f", "g", "h")
	v_graphic_opt <- c("bw", "dec_num")
	v_stat_opt <- c("lreg", "conf_ellipsoid", "centroid", "box_mean_sd", "dens_curve", "norm_dens_curve", "mean_spect")
	
	if (s_by == "section") {
		# data loading input
		v_lp_s1_input_id <- c(paste0(c(paste0("load", 1:2), "browse2", "edit_dopt2"), "_button"), "data_path2")
		# sub-data creation input
		v_lp_s2_input_id <- c("subdata_option", "vname", "vtype", "rel_symbol", paste0("vvalue", 1:2), paste0(c("c_info_clear", "c_add", "expand1", "create"), "_button"), "c_formula")
		# normal plot selection input
		v_lp_s3_input_id <- c("plot_type", "dim_num", "model")
		# model parameter loading input
		v_lp_s4_input_id <- c("data_path3", paste0(c("browse3", "load3", "edit_dopt3"), "_button"))
		# variable selection input
		v_lp_s5_input_id <- c("id", "group", paste0("var_", c("id", "x", "y", "z", "group")), paste0(c(v_fun_name, "ref", "wres"), "_radio"), paste0(v_fun_name, "_text"), paste0("concat", 1:2), "date_format", "ref", "wres_cbox", "wres_group", "wres_vfun", "display_button")
		# top panel input (tab 1: Graphic)
		v_tp_t1_input_id <- c("webgl", "mode", paste0(v_graphic_opt, "_radio"), v_graphic_opt, paste0(v_graphic_opt, "_button"), "y_scale", "fraction", "ymin", "ymax", "y_scale_button", "edit_option", "edit_option_button")
		# top panel input (tab 2: Flag)
		v_tp_t2_input_id <- c("action", paste0("var_flag_", 1:2), "draw", "qc", "comment", paste0(c(paste0("clear", 1:2), "save"), "_button"))
		# top panel input (tab 3: Statistics)
		v_tp_t3_input_id <- v_stat_opt
		
		return(list("lp_s1" = v_lp_s1_input_id, "lp_s2" = v_lp_s2_input_id, "lp_s3" = v_lp_s3_input_id, "lp_s4" = v_lp_s4_input_id, "lp_s5" = v_lp_s5_input_id, "tp_t1" = v_tp_t1_input_id, "tp_t2" = v_tp_t2_input_id, "tp_t3" = v_tp_t3_input_id))
	}
	else if (s_by == "type") {
		# selectize input
		v_t1_input_id <- c("vname", "rel_symbol", "vvalue1", paste0("var_", c("id", "x", "y", "z", "group")), "ref", "date_format", "wres_group", "edit_option")
		# numeric input
		v_t2_input_id <- c("fraction", "dec_num")
		# text input
		v_t3_input_id <- c(paste0("data_path", 2:3), "vvalue2", "c_formula", paste0(v_fun_name, "_text"), "wres_vfun", "bw", "ymin", "ymax", "comment")
		# action button input
		v_t4_input_id <- paste0(c("hs_bpanel", paste0("load", 1:3), paste0("browse", 2:3), paste0("edit_dopt", 2:3), "c_info_clear", "c_add", "expand1", "create", "display", v_graphic_opt, "y_scale", paste0("clear", 1:2), "save", "edit_option"), "_button")
		# radio button input
		v_t5_input_id <- c("vtype", "plot_type", "dim_num", "model", "id", paste0(c(v_fun_name, "ref", "wres", v_graphic_opt), "_radio"), "group", "webgl", "mode", "y_scale", "action", "draw", "qc")
		# check box input
		v_t6_input_id <- c("flag", "subdata_option", paste0("concat", 1:2), "wres_cbox", "var_flag_2", v_stat_opt)
		# check box group input
		v_t7_input_id <- "var_flag_1"
		
		return(list("Selectize" = v_t1_input_id, "Numeric" = v_t2_input_id, "Text" = v_t3_input_id, "Action" = v_t4_input_id, "Radio" = v_t5_input_id, "CheckBox" = v_t6_input_id, "CheckBoxGroup" = v_t7_input_id))
	}
	else {
		return("s_by value is incorrect")
	}
}

#' @rdname f_create_all_input_id_list
f_create_match_id_data <- function(b_concat1 = F, b_concat2 = F) {
	df_out <- data.frame("input" = c("var_id", "ref", "concat1", "var_x", "f_text", "date_format", "var_y", "g_text", "var_z", "h_text", "wres_group", "wres_vfun", "concat2", "var_group"), "parameter" = c("id", "ref", "concat1", ifelse(b_concat1, "concat1_group", "x"), "f", "date_format", "y", "g", "z", "h", "wres_group", "wres_vfun", "concat2", ifelse(b_concat2, "concat2_group", "group")))
	return(df_out)
}

#' @rdname f_create_all_input_id_list 
f_create_input_value_list <- function(s_type = "selectize", df_all = NULL, v_concat = rep(0, 2), i_sub_data = 0, i_only_value = 0, s_sub_var_name = NULL, s_sub_var_type = "qualit") {
	if (s_type == "selectize") {
		v_var_name <- names(df_all)
		v_value <- NULL
		
		if (i_sub_data == 0) {
			v_pos <- which(v_concat == 0)
			if (!is.null(v_var_name) & length(v_pos) > 0) {v_var_name <- v_var_name[!v_var_name %in% c(".concat1.", ".concat2.")[v_pos]]}
		}
		
		if (!is.null(v_var_name) & i_only_value != 1) {
			i_pos <- ifelse(length(which(v_var_name %in% s_sub_var_name)) > 0, which(v_var_name %in% s_sub_var_name), 1)
			v_value <- unique(as.vector(df_all[, i_pos]))
			v_value <- v_value[!is.na(v_value)]
			v_value <- v_value[order(v_value)]
		}
		
		if (i_only_value == 0) {
			l_id <- eval(parse(text = paste0("list(", paste(paste0("\"", c("vname", paste0("var_", c("id", "x", "y", "z", "group")), "ref", "wres_group"), "\" = 1"), collapse = ", "), ", \"date_format\" = 2, \"rel_symbol\" = 3, \"vvalue1\" = 4, \"edit_option\" = 5)")))
			l_value <- eval(parse(text = paste0("list(v_var_name, c(\"%Y%m%d\", \"%Y%m%d%H%M\", \"%Y%m%d%H%M%S\", \"%d%m%Y\", \"%d%m%Y%H%M\", \"%d%m%Y%H%M%S\"), ", ifelse(s_sub_var_type == "qualit", "c(\"%in%\", \"!%in%\")", "c(\"=\", \"!=\", \"<\", \">\", \"<=\", \">=\")"), ", v_value, c(\"label\", \"color/opacity\", \"point type/size\"))")))
			return(list(l_id, l_value))
		}
		else if (i_only_value == 1) {
			return(v_var_name)
		}
		else {
			return(v_value)
		}
	}
	else if (s_type == "radio_button") {
		v_fun_name <- c("f", "g", "h")
		v_graphic_opt <- c("bw", "dec_num")
		l_id_value <- list()
		eval(parse(text = paste(paste0("l_id_value$", c(paste0(c(v_fun_name, "ref", "wres"), "_radio"), "id", "group", paste0(v_graphic_opt, "_radio"), "vtype", "plot_type", "dim_num", "model", "y_scale", "webgl", "mode", "action", "draw", "qc"), " <- \"", c(rep("no", length(v_fun_name) + 4), rep("auto", length(v_graphic_opt)), "qualit", "plot", "2d", rep("none", 2), "yes", "marker", "add_flag", "pt", "2"), "\""), collapse = "; ")))
		return(l_id_value)
	}
	else if (s_type == "check_box_group") {
		return(list("var_flag_1" = c("flag_x", "flag_y")))
	}
	else if (s_type == "numeric") {
		return(list("fraction" = 0.05, "dec_num" = 2))
	}
	else {
		return("s_type value is incorrect")
	}
}

#' @rdname f_create_all_input_id_list 
f_create_input_sp_num_list <- function() {
	l_out <- list("id" = 1, "f_radio" = 2, "g_radio" = 2, "h_radio" = 2, "webgl" = 2, "vtype" = 3)
	return(l_out)
}

#' @rdname f_create_all_input_id_list
f_create_input_maxitem_list <- function(s_data = "normal", s_graph = "plot_2d", s_model = "none", b_concat1 = F) {
	if (s_data %in% c("normal", "temporal")) {
		if (s_data == "normal") {
			if (s_graph %in% c("plot_2d", "plot_3d", "histplot", "corplot", "boxplot", "barplot")) {
				if (s_model != "none") {
					if (s_graph != "plot_2d") {
						return("s_model is used only if s_graph is plot_2d")
					}
					else {
						return(list("var_y" = 1))
					}
				}
				else {
					l_out <- list()
					
					if (s_graph %in% c("plot_2d", "plot_3d", "boxplot")) {
						l_out$var_x <- ifelse(s_graph == "boxplot" & !b_concat1, 1, 9999)
						l_out$var_y <- 9999
					}
					else if (s_graph %in% c("histplot", "barplot")) {
						l_out$var_x <- ifelse(s_graph == "barplot" & !b_concat1, 1, 9999)
					}
					else {
						l_out$var_y <- 9999
					}
					
					return(l_out)
				}
			}
			else {
				return("s_graph value is incorrect")
			}
		}
		else { 
			if (s_graph != "plot_2d" | s_model != "none") {
				return("s_graph and s_model is used only if s_data is normal")
			}
			else {
				return(list("var_x" = 1))
			}
		}
	}
	else {
		return("s_data value is incorrect (two values available: normal or temporal)")
	}
}

#' @rdname f_create_all_input_id_list
f_update_input_maxitem_list <- function(o_selectize_input, l_id_status = NULL, df_id_status = NULL) {
	l_1 <- o_selectize_input$max_item  
	l_2 <- o_selectize_input$max_item_current
	
	if (is.null(l_id_status)) {
		l_3 <- NULL
		
		if (!is.null(df_id_status)) {
			v_pos <- which(df_id_status[df_id_status$id %in% names(l_1), "status"] == 1)
			
			if (length(v_pos) > 0) {
				v_name <- names(l_1)[v_pos]
				v_pos <- which((as.vector(unlist(l_1[v_name])) - as.vector(unlist(l_2[v_name]))) != 0)
				
				if (length(v_pos) > 0) {
					l_3 <- l_1[v_name[v_pos]]
					eval(parse(text = paste(paste0("l_2[[\"", v_name[v_pos], "\"]] <- l_1[[\"", v_name[v_pos], "\"]]"), collapse = "; ")))
				}
				
				v_pos <- c(v_pos, which((as.vector(unlist(l_1[v_name])) - as.vector(unlist(l_2[v_name]))) == 0))
				if (length(v_pos) > 0) {l_1 <- l_1[!names(l_1) %in% v_name[v_pos]]}
			}
		}
		
		return(list(l_1, l_2, l_3))
	}
	else {
		if (length(l_1) > 0) {
			v_pos <- which(names(l_1) %in% names(l_id_status) & (as.vector(unlist(l_1)) - as.vector(unlist(l_2[names(l_1)]))) != 0)
			
			if (length(v_pos) > 0) {
				l_1 <- l_1[v_pos]
				eval(parse(text = paste(paste0("l_2[[\"", names(l_1)[v_pos], "\"]] <- l_1[[\"", names(l_1)[v_pos], "\"]]"), collapse = "; ")))
			}
			else {
				l_1 <- list()
			}
		}
		
		v_pos <- which(as.vector(unlist(l_id_status)) == 0)
		
		if (length(v_pos) > 0) {
			v_name <- paste0("var_", c("x", "y"))
			v_pos <- which(v_name %in% names(l_id_status[v_pos]))
			
			if (length(v_pos) > 0) {
				v_name <- v_name[v_pos]
				v_pos <- which(as.vector(unlist(l_2[v_name])) != 9999)
				
				if (length(v_pos) > 0) {
					eval(parse(text = paste(paste0("l_1$", v_name[v_pos], " <- 9999"), collapse = "; ")))
					eval(parse(text = paste(paste0("l_2[[\"", v_name[v_pos], "\"]] <- 9999"), collapse = "; ")))
					l_1 <- l_1[sort(names(l_1))]
				}
			}
		}
		
		if (length(which(c("concat2", "var_group") %in% names(l_id_status))) == 2) {
			l_1$var_group <- 1
		}
		
		return(list(l_1, l_2))
	}
}

#' @rdname f_create_all_input_id_list
f_create_varsel_input_id_vector <- function(s_data = "normal", s_graph = "plot_2d", s_model = "none", b_flag = F) {
	if (s_data %in% c("normal", "ir")) {
		if (s_data == "normal") {
			if (s_model == "none") {
				if (s_graph %in% c("plot_2d", "plot_3d")) {
					v_out <- c("var_id", "f_radio", "f_text", "g_radio", "g_text")
					if (s_graph == "plot_3d") {v_out <- c(v_out, "h_radio", "h_text")}
				}
				else if (s_graph == "boxplot") {
					v_out <- c("g_radio", "g_text")
				}
				else if (s_graph == "histplot") {
					v_out <- c("f_radio", "f_text")
				}
				else { # corplot & barplot
					v_out <- c()
				}				
			}
			else {
				v_out <- c("ref", "g_text")
				if (s_model == "calib") {v_out <- c(v_out, paste0("wres_", c("cbox", "group", "vfun")))}
			}
		}
		else {
			v_out <- "var_id"
		}
		
		v_out <- c(v_out, "concat2", "var_group")
		if (b_flag) {v_out <- v_out[v_out != "var_id"]}
	}
	else {
		v_out <- "g_text"
	}
	
	return(v_out)
}

#' @rdname f_create_all_input_id_list
f_create_t3_input_id_vector <- function(s_data = "normal", s_graph = "plot_2d", s_model = "none") {
	v_out <- c()
	
	if (s_data == "normal") {
		if (s_model != "calib" | !s_graph %in% c("barplot", "corplot")) {
			if (s_graph == "plot_2d") { # model = none or valid
				v_out <- c("lreg", "conf_ellipsoid", "centroid")
			}
			else if (s_graph == "plot_3d") {
				v_out <- "centroid"
			}
			else if (s_graph == "boxplot") {
				v_out <- "box_mean_sd"
			}
			else { # histplot
				v_out <- c("dens_curve", "norm_dens_curve")
			}
		}
	}
	else {
		if (s_data == "ir") {
			v_out <- "mean_spect"
		}
	}
	
	return(v_out)
}

#' @rdname f_create_all_input_id_list
f_create_input_id_vector <- function(s_data = "normal", s_graph = "plot_2d", s_model = "none", b_load2 = F, b_load3 = F, b_display = F, i_concat = 0) {
	if (b_display) {
		if ((s_data == "normal" & s_graph %in% c("plot_2d", "plot_3d") & s_model == "none") | s_data %in% c("temporal", "ir")) {
			if (s_data == "normal") {
				v_out <- paste0("var_flag_", 1:ifelse(s_graph == "plot_2d", 1, 2))
			}
			else {
				v_out <- c("action", ifelse(s_data == "temporal", "draw", NA), "qc", "comment")
			}
			
			return(v_out[!is.na(v_out)])
		}
		else {
			return("b_display = T is only used for the followings conditions: (1) s_data is temporal or ir, (2) s_data is normal, s_graph is plot_2d or plot_3d and s_model is none")  
		}
	}
	else {
		if (s_data %in% c("normal", "temporal", "ir")) {
			if (s_data == "normal") {
				if (s_graph %in% c("plot_2d", "plot_3d", "histplot", "corplot", "boxplot", "barplot")) {
					if (s_model != "none") {
						if (s_graph != "plot_2d") {
							return("s_model is used only if s_graph is plot_2d")
						}
						else {
							if (!b_load3) {
								v_out <- c("webgl", "dec_num_radio", "edit_option", "edit_option_button")
								if (s_model == "valid") {v_out <- c(v_out, f_create_t3_input_id_vector(s_data, s_graph, s_model))}
							}
							else {
								v_out <- c(paste0("var_", c("x", "y")), paste0(c("g", "ref"), "_radio"), "group", "display_button")
								if (s_model == "calib") {v_out <- c(v_out, "wres_radio")}
							}
							
							return(v_out)
						}
					}
					else {
						if (s_graph == "plot_2d") {
							v_out <- c(paste0("var_", c("x", "y")), paste0(c("f", "g", "dec_num"), "_radio"), "group", "display_button", "webgl", "edit_option", "edit_option_button", f_create_t3_input_id_vector(s_data, s_graph, s_model))
						}
						else if (s_graph == "plot_3d") {
							v_out <- c(paste0("var_", c("x", "y", "z")), paste0(c("f", "g", "h", "dec_num"), "_radio"), "group", "display_button", "edit_option", "edit_option_button", f_create_t3_input_id_vector(s_data, s_graph, s_model))
						}
						else if (s_graph == "histplot") {
							v_out <- c("var_x", paste0(c("f", "bw"), "_radio"), "group", "display_button", "edit_option", "edit_option_button", f_create_t3_input_id_vector(s_data, s_graph, s_model))
						}
						else if (s_graph == "corplot") {
							v_out <- c("var_y", "group", "display_button", "edit_option", "edit_option_button")
						}
						else {
							if (s_graph == "boxplot") {
								v_out <- c(paste0("var_", c("x", "y")), paste0(c("g", "dec_num"), "_radio"), "group", "display_button", "edit_option", "edit_option_button", f_create_t3_input_id_vector(s_data, s_graph, s_model))
							}
							else {
								v_out <- c("var_x", "group", "display_button", "edit_option", "edit_option_button")
							}
							
							if (i_concat == 0) {v_out <- c(v_out, "concat1")}
						}
						
						return(v_out)
					}
				}
				else {
					return("s_graph value is incorrect")
				}
			}
			else { 
				if (s_graph != "plot_2d" | s_model != "none") {
					return("s_graph and s_model is used only if s_data is normal")
				}
				else {
					if (s_data == "temporal") {
						v_out <- c(paste0("var_", c("x", "y")), paste0(c("g", "dec_num"), "_radio"), "date_format", "display_button", "webgl", "mode", "y_scale", "edit_option", "edit_option_button")
					}
					else {
						if (b_load2) {
							v_out <- c("subdata_option", "dec_num_radio", "group", "display_button", "webgl", "mode", "y_scale", "edit_option", "edit_option_button", f_create_t3_input_id_vector(s_data, s_graph, s_model))
						}
						else {
							v_out <- c("subdata_option", "group")
						}
					}
					
					return(v_out)
				}
			}
		}
		else {
			return("s_data value is incorrect")
		}
	}
}

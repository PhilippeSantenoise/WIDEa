#' List of functions used to update reactive values

#' @description
#' `f_create_rv_inventory` is used to create inventory of reactive values with id
#' and initial value. This inventory is used to reset reactive values. 
#' \cr`f_create_rv_id_value_list` is used to create a list of id/value used to reset
#' specific reactive values.
#' \cr`f_update_rv` is used to create command lines to update reactive values.
#' \cr`f_update_graph_option_rv` is used to update reactive values associated to
#' graph option edition (color/opacity, point type/size, sorting).

#' @param df_rv_id_value is a data frame of four columns ("choice", "rv", "id",
#' "value") with reactive value information. The data frame is created from the
#' `f_create_rv_inventory` function.
#' @param s_choice is a value used to subset data with reactive value information
#' (column named "choice" in df_rv_id_value). Five values available: "load1",
#' "load2", "load3", "display", "lp_s1_s2".
#' @param l_rv_id_value is a list with reactive value information (including three
#' elements "rv", "id", "value").
#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_plot_type is the plot type (5 values: "plot", "boxplot", "histplot",
#' "barplot", "corplot").
#' @param i_display is a binary value associated to the display button (1 if
#' clicked, 0 else).
#' @param v_var_name is the vector of variable name filled in X/Group field (left 
#' panel: variable selection).
#' @param b_sorting2 is a boolean value. This parameter is associated to the 
#' sorting of the Group variable for boxplot/barplot (normal data type). Its value
#' is true if a Group variable is selected, false else.  
#' @param v_var_name2 is the vector of variable name filled in Group field and is
#' associated to `b_sorting2`.
#' @param df_all are data saved in e_data environment (e_data$all).
#' @param o_name_option is the reactive value associated to graph option (color,
#' opacity, point type/size, sorting) edition.
#' @param b_display_clear is a boolean value associated to the display button.
#' `b_display_clear` is true if the display/clear button is clicked and a graph is
#' already created, else false.
#' @param b_sub_data is a boolean value such that `b_sub_data` is true if a sub-data
#' exists and the "subdata_option" check box input is unchecked, false else.
#' @param s_option is the option value (3 values: "color/opacity",
#' "point type/size", "sorting"). `s_option` is associated to the "edit_option" 
#' selectize input.
#' @param v_concat is a vector with two boolean values associated to the 
#' concatenation box of X and Group variables 

#' @encoding UTF-8

f_create_rv_inventory <- function() {
	# reactive values associated to the display button
	df_out_1 <- data.frame(
		"choice" = rep("display", 64),
		"rv" = c("o_click_button", "o_click_legend", "o_zoom", "o_input_status", rep("o_parameter", 23), rep("o_cond", 9), rep("o_plot", 15), rep("o_picture_info", 4), rep("o_name_option", 6), rep("o_stat_method", 3)),
		"id" = c("display", "item", "coord", "display",
			"data_name", "plot_type", "dim_num", "model", "ref", "wres_group", "wres_vfun", "id", "concat1", "concat1_group", "x", "f", "date_format", "y", "g", "z", "h", "concat2", "concat2_group", "group", "quant_group", "min_max", "corplot_group", 
			"display", "flag", "qc1", "qc2", "save1", "save2", "selec_leg", "deselec_leg", "var_flag",
			"data", "model", "code_freq", "id_group", "y_coord", "add_pt", "pt_pos", "var_pt", "data_qc1", "data_qc2", "var_qc1", "var_qc2", "leg_name_qc", "elt", "elt_pt_pos",
			"filename", "format", "height", "width",
			"color_default", "opacity_default", "point_type_default", "point_size_default", "sorting_default", "sorting2_default",
			"inv", "level", "message"
		),
		"value" = c("0", rep("NULL", 2), "data.frame()",
			rep("NA", 8), "F", rep("NA", 8), "F", rep("NA", 2), "F", "data.frame()", "NA", 
			rep("0", 9),
			"data.frame()", "NULL", rep("NA", 2), "NULL", "T", rep("NA", 9),
			"\"Picture_name\"", "\"png\"", "800", "1000",
			rep("c()", 6),
			"l_stat_method_ini$inv", "l_stat_method_ini$level", "l_stat_method_ini$message"
		)
	)
	
	# reactive values reseted when data (e_data$all) are removed in e_data environment
	df_out_2 <- data.frame(
		"choice" = rep("lp_s1_s2", 22),
		"rv" = c("o_reset", "o_cond", rep("o_option", 3), rep("o_parameter", 17)),
		"id" = c("code", "flag_msg",
			"choice", "data", "plotly",
			"webgl", "mode", "autobw", "bw", "y_scale", "autodec_num", "dec_num", "xlab", "ylab", "zlab", "lreg", "conf_ellipsoid", "centroid", "boxmean", "dens_curve", "norm_dens_curve", "mean_spect"
		),
		"value" = c(rep("0", 2),
			"c()", rep("NULL", 2),
			"\"yes\"", "\"marker\"", "T", "NA", "\"none\"", "T", "NA", "\"x\"", "\"y\"", "\"z\"", rep("F", 3), "\"NULL\"", rep("F", 3)
		)
	)
	
	df_out <- rbind(df_out_1, df_out_2)
	return(df_out)
}

#' @rdname f_create_rv_inventory
f_create_rv_id_value_list <- function(df_rv_id_value, s_choice) {
	if (s_choice %in% c("display", "lp_s1_s2")) {
		df_choice <- df_rv_id_value[df_rv_id_value$choice == s_choice, ]
		return(list("rv" = as.vector(df_choice$rv), "id" = as.vector(df_choice$id), "value" = as.vector(df_choice$value)))
	}
	else {
		return ("invalid choice")
	}
}

#' @rdname f_create_rv_inventory
f_update_rv <- function(l_rv_id_value) {
	s_cmd <- paste(paste0(l_rv_id_value[["rv"]], "$", l_rv_id_value[["id"]], " <- ", l_rv_id_value[["value"]]), collapse = "; ")
	return(s_cmd)
}

#' @rdname f_create_rv_inventory
f_update_graph_option_rv <- function (o_name_option, s_option, i_display, s_data_type, s_plot_type, v_var_name, b_sorting2 = F, v_var_name2 = NULL, df_all = NULL, b_display_clear = F, b_sub_data = F, v_concat = rep(F, 2)) {
	l_out <- list()
	v_del <- c() 
	s_w_message <- character(0)
	s_e_message <- character(0)
	
	if (b_sub_data) { # condition: delete subdata (uncheck the "add conditions" box) 
		if (length(o_name_option$var3) > 0) { # Group variable: quantitative
			if (v_concat[2] | (!v_concat[2] & length(which(o_name_option$var3 %in% v_var_name)) == 0)) {
				v_del <- c(v_del, "var3", "pal_col_op", "pal_point")
			}
		}
		else { # X/Group variable: qualitative
			if (paste(o_name_option$var, collapse = "_") == paste(v_var_name, collapse = "_")) {
				eval(parse(text = paste0("v_name <- unique(as.vector(", ifelse(length(o_name_option$var) > 1 | v_concat[1], "f_create_concat_variable(df_all, v_var_name)", "df_all[, v_var_name]"), "))")))
				v_name <- v_name[!is.na(v_name)]
				v_name <- v_name[order(v_name)]
				
				if (length(o_name_option$name) < length(v_name)) {
					l_out$name <- v_name
					
					if (length(o_name_option$color) > 0) {
						l_out$color <- rep("", length(v_name))
						l_out$color[which(v_name %in% o_name_option$name)] <- o_name_option$color
					}
					
					if (length(o_name_option$opacity) > 0) {
						l_out$opacity <- rep("", length(v_name))
						l_out$opacity[which(v_name %in% o_name_option$name)] <- o_name_option$opacity
					}
					
					if (length(o_name_option$point_type) > 0) {
						l_out$point_type <- rep("", length(v_name))
						l_out$point_type[which(v_name %in% o_name_option$name)] <- o_name_option$point_type
					}
					
					if (length(o_name_option$point_size) > 0) {
						l_out$point_size <- rep("", length(v_name))
						l_out$point_size[which(v_name %in% o_name_option$name)] <- o_name_option$point_size
					}
					
					if (length(o_name_option$sorting) > 0) {
						l_out$sorting <- rep(0, length(v_name))
						l_out$sorting[which(v_name %in% o_name_option$name)] <- o_name_option$sorting
						v_add <- 1:length(v_name)
						if (length(which(v_name == 0)) > 0) {l_out$sorting[which(v_name == 0)] <- v_add[-which(v_add %in% o_name_option$sorting)]}
					}
				}
			}
			else {
				v_del <- c(v_del, "name", "color", "opacity", "point_type", "point_size", "sorting", "var")
			}
			
			if (b_sorting2) {
				if (paste(o_name_option$var2, collapse = "_") == paste(v_var_name2, collapse = "_")) {
					eval(parse(text = paste0("v_name2 <- unique(as.vector(", ifelse(length(o_name_option$var2) > 1 | v_concat[2], "f_create_concat_variable(df_all, v_var_name2)", "df_all[, v_var_name2]"), "))")))
					v_name2 <- v_name2[!is.na(v_name2)]
					v_name2 <- v_name2[order(v_name2)]
					
					if (length(o_name_option$name2) < length(v_name2)) {
						l_out$name2 <- v_name2
						
						if (length(o_name_option$sorting2) > 0) {
							l_out$sorting2 <- rep(0, length(v_name2))
							l_out$sorting2[which(v_name2 %in% o_name_option$name2)] <- o_name_option$sorting2
							v_add <- 1:length(v_name2)
							if (length(which(v_name2 == 0)) > 0) {l_out$sorting2[which(v_name2 == 0)] <- v_add[-which(v_add %in% o_name_option$sorting2)]}
						}
					}
				}
				else {
					v_del <- c(v_del, "name2", "sorting2", "var2")
				}
			}
		}
	}
	else {
		if (i_display == 1 & !b_display_clear) { # condition: click on the edit button (tp1: graph option) when a graph is already displayed 
			b_quant <- F
			if (s_data_type %in% c("normal", "ir") & length(which(!is.na(v_var_name))) == 1) {b_quant <- is.numeric(df_all[, v_var_name])}
			
			if (b_quant) { # Group variable: quantitative
				if (s_option == "color/opacity") {
					l_out$pal_col_op_temp <- list("color" = o_name_option$pal_col_op_default$color, "opacity" = o_name_option$pal_col_op_default$opacity)
					if (length(o_name_option$pal_col_op) > 0) {l_out$pal_col_op_temp <- o_name_option$pal_col_op}
				}
				else { # point type/size
					l_out$pal_point_temp <- list("type" = o_name_option$pal_point_default$type, "size" = o_name_option$pal_point_default$size, "size_coef" = o_name_option$pal_point_default$size_coef)
					if (length(o_name_option$pal_point) > 0) {l_out$pal_point_temp <- o_name_option$pal_point}
				}
				
				l_out$var3 <- v_var_name
				s_w_message <- "quantitative"
			}
			else { # X/Group variable: qualitative
				if (length(o_name_option$name) == 0) {
					if (s_data_type == "temporal") {
						l_out$name <- v_var_name
					}
					else { # normal/ir
						if (length(which(!is.na(v_var_name))) == 0 & s_plot_type %in% c("plot", "histplot")) {
							l_out$name <- "all"
						}
						else {
							eval(parse(text = paste0("v_name <- unique(as.vector(df_all[, \"", ifelse(s_plot_type %in% c("plot", "histplot") & ".concat2." %in% names(df_all), ".concat2.", ifelse(s_plot_type %in% c("boxplot", "barplot") & ".concat1." %in% names(df_all), ".concat1.", v_var_name)), "\"]))")))
							v_name <- v_name[!is.na(v_name)]
							v_name <- v_name[order(v_name)]
							l_out$name <- v_name
						}
					}
					
					if (s_option == "color/opacity") {
						eval(parse(text = paste(paste0("l_out$", c("color", "opacity"), "_temp <- rep(\"\", length(l_out$name))"), collapse = "; ")))
					}
					else if (s_option == "point type/size") {
						eval(parse(text = paste(paste0("l_out$", c("point_type", "point_size"), "_temp <- rep(\"\", length(l_out$name))"), collapse = "; ")))
					}
					else { # sorting
						l_out$sorting_temp <- 1:length(l_out$name)
					}
				}
				else {
					if (s_option == "color/opacity") {
						eval(parse(text = paste(paste0("l_out$", c("color", "opacity"), "_temp <- rep(\"\", length(o_name_option$name))"), collapse = "; ")))
					}
					else if (s_option == "point type/size") {
						eval(parse(text = paste(paste0("l_out$", c("point_type", "point_size"), "_temp <- rep(\"\", length(o_name_option$name))"), collapse = "; ")))
					}
					else { # sorting
						l_out$sorting_temp <- 1:length(o_name_option$name)
					}
				}
				
				if (length(which(!is.na(v_var_name))) > 0) {
					l_out$var <- v_var_name
				}
				else {
					v_del <- c(v_del, "var")
				}
				
				if (s_option == "color/opacity") {
					if (length(o_name_option$color) > 0) {l_out$color_temp <- o_name_option$color}
					if (length(o_name_option$opacity) > 0) {l_out$opacity_temp <- o_name_option$opacity}
				}
				else if (s_option == "point type/size") {
					if (length(o_name_option$point_type) > 0) {l_out$point_type_temp <- o_name_option$point_type}
					if (length(o_name_option$point_size) > 0) {l_out$point_size_temp <- o_name_option$point_size}
				}
				else { # sorting
					if (length(o_name_option$sorting) > 0) {l_out$sorting_temp <- o_name_option$sorting}
				}
				
				if (b_sorting2) { # normal: only for boxplot/barplot
					if (length(o_name_option$name2) == 0) {
						eval(parse(text = paste0("v_name2 <- unique(as.vector(df_all[, \"", ifelse(".concat2." %in% names(df_all), ".concat2.", v_var_name2), "\"]))")))
						v_name2 <- v_name2[!is.na(v_name2)]
						v_name2 <- v_name2[order(v_name2)]
						l_out$name2 <- v_name2
						l_out$sorting2_temp <- 1:length(l_out$name2)
					}
					else {
						l_out$sorting2_temp <- 1:length(o_name_option$name2)
					}
					
					if (length(which(!is.na(v_var_name2))) > 0) {
						l_out$var2 <- v_var_name2
					}
					else {
						v_del <- c(v_del, "var2")
					}
					
					if (length(o_name_option$sorting2) > 0) {l_out$sorting2_temp <- o_name_option$sorting2}
				}
				
				l_out$warning <- 1
			}
		}
		else {
			if (s_data_type == "temporal") {
				v_name <- v_var_name
				
				if (length(v_name) > 0) {
					# data frame with binary values: 1 if an error is detected (variable without value or qualitative variable)
					df_error = data.frame("var" = v_name, "no_value" = rep(0, length(v_name)), "qual" = rep(0, length(v_name)))
					
					v_size <- eval(parse(text = paste0("c(", paste(paste0("length(which(!is.na(df_all[, \"", v_name, "\"])))"), collapse = ", "), ")")))
					v_pos <- which(v_size == 0)
					
					if (length(v_pos) > 0) {
						df_error[v_pos, "no_value"] <- 1
						
						if ((nrow(df_error) - length(v_pos)) > 0) {
							v_row <- c(1:nrow(df_error))[-v_pos]
							v_qual <- eval(parse(text = paste0("c(", paste(paste0("!is.numeric(df_all[, \"", v_name[v_row], "\"])"), collapse = ", "), ")")))
							v_pos <- which(v_qual)
							if (length(v_pos) > 0) {df_error[v_row[v_pos], "qual"] <- 1}
						}
					}
					else {
						v_qual <- eval(parse(text = paste0("c(", paste(paste0("!is.numeric(df_all[, \"", v_name, "\"])"), collapse = ", "), ")")))
						v_pos <- which(v_qual)
						if (length(v_pos) > 0) {df_error[v_pos, "qual"] <- 1}
					}
				}
			}
			else { # normal/ir
				v_name <- NULL
				b_equal <- F
				b_quant <- F
				
				# data frame with binary values: 1 if an error is detected (empty field, variable without value or quantitative variable)
				df_error <- data.frame("var" = ifelse(s_plot_type %in% c("boxplot", "barplot"), "X", "Group"), "size" = length(v_var_name), "empty" = 0, "no_val" = 0, "quant" = 0)
				
				if (s_plot_type %in% c("boxplot", "barplot")) {
					if (length(v_var_name) > 0 & length(v_var_name2) > 0) { # check if variables filled in the X and Group fields are the same
						if (length(v_var_name) == length(v_var_name2) & length(which(v_var_name %in% v_var_name2)) == length(v_var_name2)) {
							b_equal <- T
						}
					}
					
					if (length(v_var_name) > 0) {
						if (!(b_equal & length(o_name_option$name) == 0)) {
							eval(parse(text = paste0("v_name <- unique(as.vector(", ifelse(length(v_var_name) > 1 | v_concat[1], "f_create_concat_variable(df_all, v_var_name)", "df_all[, v_var_name]"), "))")))
							v_name <- v_name[!is.na(v_name)]
							
							if (length(v_name) == 0 | is.numeric(v_name)) {
								v_name <- NULL
								df_error[1, ifelse(length(v_name) == 0, "no_val", "quant")] <- 1
							}
							else {
								v_name <- v_name[order(v_name)]
							}
						}
					}
					else {
						df_error[1, "empty"] <- 1
					}
					
					if (b_sorting2) {
						v_name2 <- NULL
						
						if (length(v_var_name2) > 0) {
							if (!(b_equal & length(o_name_option$name2) == 0)) {
								eval(parse(text = paste0("v_name2 <- unique(as.vector(", ifelse(length(v_var_name2) > 1 | v_concat[2], "f_create_concat_variable(df_all, v_var_name2)", "df_all[, v_var_name2]"), "))")))
								v_name2 <- v_name2[!is.na(v_name2)]
								
								if (length(v_name2) == 0 | is.numeric(v_name2)) {
									v_name2 <- NULL
									df_add <- data.frame("var" = "Group", "size" = length(v_var_name2), "empty" = 0, "no_val" = ifelse(length(v_name2) == 0, 1, 0), "quant" = ifelse(is.numeric(v_name2), 1, 0))
									df_error <- rbind(df_error, df_add)
								}
								else {
									v_name2 <- v_name2[order(v_name2)]
								}
							}
						}
						else {
							df_add <- data.frame("var" = "Group", "size" = length(v_var_name2), "empty" = 1, "no_val" = 0, "quant" = 0)
							df_error <- rbind(df_error, df_add)
						}
					}
				}
				else { # plot/histplot
					v_pos <- which(!is.na(v_var_name))
					
					if (length(v_pos) == 0) {
						if (length(v_var_name) > 0) {
							v_name <- "all"
						}
						else {
							df_error[1, "empty"] <- 1
						}
					}
					else {
						eval(parse(text = paste0("v_name <- unique(as.vector(", ifelse(length(v_pos) > 1 | v_concat[2], "f_create_concat_variable(df_all, v_var_name)", "df_all[, v_var_name]"), "))")))
						v_name <- v_name[!is.na(v_name)]
						
						if (length(v_name) == 0) {
							df_error[1, "no_val"] <- 1
						}
						else {
							if (is.numeric(v_name)) {
								if (s_plot_type == "histplot" | s_option == "sorting") {
									v_name <- NULL
									df_error[1, "quant"] <- 1
								}
								else { # plot
									b_quant <- T
									s_w_message <- "The Group variable is a quantitative variable.<br/>A quantitative variable can be transformed into a qualitative one by checking the concatenation box and re-selecting this variable from the input list."
								}
							}
							else {
								v_name <- v_name[order(v_name)]
							}
						}
					}
				}
			}
			
			if (b_display_clear) { # condition: click on the display/clear button
				if (length(o_name_option$var3) > 0) { # Group variable: quantitative
					if (v_concat[2] | (!v_concat[2] & length(which(o_name_option$var3 %in% v_var_name)) == 0)) {
						v_del <- c(v_del, "var3", "pal_col_op", "pal_point")
					}
				}
				else { # X/Group variable: qualitative
					if (length(v_name) > 0) {
						if (length(o_name_option$color) > 0 | length(o_name_option$opacity) > 0 | length(o_name_option$point_type) > 0 | length(o_name_option$point_size) > 0 | length(o_name_option$sorting) > 0) {
							if (s_data_type == "temporal") {
								v_pos <- which(v_name %in% o_name_option$name)
								
								if (length(o_name_option$color) > 0) {
									v_color <- o_name_option$color
									names(v_color) <- o_name_option$name
									
									if (length(v_pos) > 0) {
										l_out$color <- rep("", length(v_name))
										l_out$color[v_pos] <- as.vector(v_color[v_name[v_pos]])
									}
									else {
										v_del <- c(v_del, "color")
									}
								}
								
								if (length(o_name_option$opacity) > 0) {
									v_opacity <- o_name_option$opacity
									names(v_opacity) <- o_name_option$name
									
									if (length(v_pos) > 0) {
										l_out$opacity <- rep("", length(v_name))
										l_out$opacity[v_pos] <- as.vector(v_opacity[v_name[v_pos]])
									}
									else {
										v_del <- c(v_del, "opacity")
									}
								}
								
								if (length(o_name_option$point_type) > 0) {
									v_point_type <- o_name_option$point_type
									names(v_point_type) <- o_name_option$name
									
									if (length(v_pos) > 0) {
										l_out$point_type <- rep("", length(v_name))
										l_out$point_type[v_pos] <- as.vector(v_point_type[v_name[v_pos]])
									}
									else {
										v_del <- c(v_del, "point_type")
									}
								}
								
								if (length(o_name_option$point_size) > 0) {
									v_point_size <- o_name_option$point_size
									names(v_point_size) <- o_name_option$name
									
									if (length(v_pos) > 0) {
										l_out$point_size <- rep("", length(v_name))
										l_out$point_size[v_pos] <- as.vector(v_point_size[v_name[v_pos]])
									}
									else {
										v_del <- c(v_del, "point_size")
									}
								}
							}
							else { # normal/ir
								if (length(which(v_name %in% o_name_option$name)) != length(v_name) | length(which(o_name_option$name %in% v_name)) != length(o_name_option$name)) {
									v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "sorting", "var")
								}
								else {
									if (length(which(!is.na(v_var_name))) > 0) {
										if (length(which(v_var_name %in% o_name_option$var)) != length(v_var_name) | length(which(o_name_option$var %in% v_var_name)) != length(o_name_option$var)) {v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "sorting", "var")}
									}
									else {
										if (length(o_name_option$var) != 0) {v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "sorting", "var")}
									}
								}
							}
						}
						
						l_out$name <- v_name
					}
					else {
						v_del <- c(v_del, "name", "color", "opacity", "point_type", "point_size", "sorting", "var")
					}
					
					if (b_sorting2) { # normal: only for boxplot/barplot
						if (length(v_name2) > 0 & length(o_name_option$sorting2) > 0) {
							if (length(which(v_name2 %in% o_name_option$name2)) != length(v_name2) | length(which(o_name_option$name2 %in% v_name2)) != length(o_name_option$name2)) {
								v_del <- c(v_del, "sorting2", "var2")
							}
							else {
								if (length(which(!is.na(v_var_name2))) > 0) {
									if (length(which(v_var_name2 %in% o_name_option$var2)) != length(v_var_name2) | length(which(o_name_option$var2 %in% v_var_name2)) != length(o_name_option$var2)) {v_del <- c(v_del, "sorting2", "var2")}
								}
								else {
									if (length(o_name_option$var2) != 0) {v_del <- c(v_del, "sorting2", "var2")}
								}
							}
							
							l_out$name2 <- v_name2
						}
						else {
							v_del <- c(v_del, "name2", "sorting2", "var2")
						}
					}
				}
			}
			else { # condition: click on the edit button (tp1: graph option) when no graph is displayed 
				if (length(v_name) > 0) {
					if (b_quant) { # Group variable: quantitative
						if (s_option == "color/opacity") {
							l_out$pal_col_op_temp <- list("color" = o_name_option$pal_col_op_default$color, "opacity" = o_name_option$pal_col_op_default$opacity)
							if (length(o_name_option$pal_col_op) > 0) {l_out$pal_col_op_temp <- o_name_option$pal_col_op}
						}
						else { # point type/size
							l_out$pal_point_temp <- list("type" = o_name_option$pal_point_default$type, "size" = o_name_option$pal_point_default$size, "size_coef" = o_name_option$pal_point_default$size_coef)
							if (length(o_name_option$pal_point) > 0) {l_out$pal_point_temp <- o_name_option$pal_point}
						}
						
						l_out$var3 <- v_var_name
					}
					else { # X/Group variable: qualitative
						if (s_option == "color/opacity") {
							l_out$color_temp <- rep("", length(v_name))
							l_out$opacity_temp <- rep("", length(v_name))
						}
						else if (s_option == "point type/size") {
							l_out$point_type_temp <- rep("", length(v_name))
							l_out$point_size_temp <- rep("", length(v_name))
						}
						else { # sorting
							l_out$sorting_temp <- 1:length(v_name)
						}
					
						if (length(o_name_option$name) > 0 & ((s_option == "color/opacity" & (length(o_name_option$color) > 0 | length(o_name_option$opacity) > 0)) | (s_option == "point type/size" & (length(o_name_option$point_type) > 0 | length(o_name_option$point_size) > 0)) | (s_option == "sorting" & length(o_name_option$sorting) > 0))) {
							if (s_data_type == "temporal") {
								v_pos <- which(v_name %in% o_name_option$name)
								
								if (length(v_pos) > 0) {
									if (s_option == "color/opacity") {
										if (length(o_name_option$color) > 0) {
											v_color <- o_name_option$color
											names(v_color) <- o_name_option$name
											l_out$color_temp[v_pos] <- as.vector(v_color[v_name[v_pos]])
										}
										
										if (length(o_name_option$opacity) > 0) {
											v_opacity <- o_name_option$opacity
											names(v_opacity) <- o_name_option$name
											l_out$opacity_temp[v_pos] <- as.vector(v_opacity[v_name[v_pos]])
										}
									}
									else { # point type/size
										if (length(o_name_option$point_type) > 0) {
											v_point_type <- o_name_option$point_type
											names(v_point_type) <- o_name_option$name
											l_out$point_type_temp[v_pos] <- as.vector(v_point_type[v_name[v_pos]])
										}
										
										if (length(o_name_option$point_size) > 0) {
											v_point_size <- o_name_option$point_size
											names(v_point_size) <- o_name_option$name
											l_out$point_size_temp[v_pos] <- as.vector(v_point_size[v_name[v_pos]])
										}
									}
								}
							}
							else { # normal/ir
								if (length(which(v_name %in% o_name_option$name)) == length(v_name) & length(which(o_name_option$name %in% v_name)) == length(o_name_option$name)) {
									if (length(which(!is.na(v_var_name))) > 0) {
										b_cond <- length(which(v_var_name %in% o_name_option$var)) == length(v_var_name) & length(which(o_name_option$var %in% v_var_name)) == length(o_name_option$var) 
										
										if (b_cond) {
											if (s_option == "color/opacity") {
												if (length(o_name_option$color) > 0) {l_out$color_temp <- o_name_option$color}
												if (length(o_name_option$opacity) > 0) {l_out$opacity_temp <- o_name_option$opacity}
											}
											else if (s_option == "point type/size") {
												if (length(o_name_option$point_type) > 0) {l_out$point_type_temp <- o_name_option$point_type}
												if (length(o_name_option$point_size) > 0) {l_out$point_size_temp <- o_name_option$point_size}
											}
											else { # sorting
												if (length(o_name_option$sorting) > 0) {l_out$sorting_temp <- o_name_option$sorting}
											}
										}
									}
									else {
										if (length(o_name_option$var) == 0) {
											if (s_option == "color/opacity") {
												if (length(o_name_option$color) > 0) {l_out$color_temp <- o_name_option$color}
												if (length(o_name_option$opacity) > 0) {l_out$opacity_temp <- o_name_option$opacity}
											}
											else if (s_option == "point type/size") {
												if (length(o_name_option$point_type) > 0) {l_out$point_type_temp <- o_name_option$point_type}
												if (length(o_name_option$point_size) > 0) {l_out$point_size_temp <- o_name_option$point_size}
											}
											else { # sorting
												if (length(o_name_option$sorting) > 0) {l_out$sorting_temp <- o_name_option$sorting}
											}
										}
									}
								}
							}
						}
						
						l_out$name <- v_name
					}
				}
				else {
					v_del <- c(v_del, "name", "color_temp", "opacity_temp", "point_type_temp", "point_size_temp", "sorting_temp", "var3", "pal_col_op", "pal_col_op_temp", "pal_point", "pal_point_temp")
				}
				
				if (b_sorting2) { # normal: only for boxplot/barplot
					if (length(v_name2) > 0) {
						l_out$sorting2_temp <- 1:length(v_name2)
						
						if (length(o_name_option$name2) > 0 & length(o_name_option$sorting2) > 0) {
							if (length(which(v_name2 %in% o_name_option$name2)) == length(v_name2) & length(which(o_name_option$name2 %in% v_name2)) == length(o_name_option$name2)) {
								if (length(which(!is.na(v_var_name2))) > 0) {
									b_cond <- length(which(v_var_name2 %in% o_name_option$var2)) == length(v_var_name2) & length(which(o_name_option$var2 %in% v_var_name2)) == length(o_name_option$var2) 
									
									if (b_cond) {
										if (length(o_name_option$sorting2) > 0) {l_out$sorting2_temp <- o_name_option$sorting2}
									}
								}
								else {
									if (length(o_name_option$var2) == 0) {
										if (length(o_name_option$sorting2) > 0) {l_out$sorting2_temp <- o_name_option$sorting2}
									}
								}
							}
						}
						
						l_out$name2 <- v_name2
					}
					else {
						v_del <- c(v_del, "name2", "sorting2_temp")
					}
				}
				
				# create error message
				
				if (s_data_type == "temporal") {
					if (length(v_name) > 0) {
						v_sum <- as.vector(colSums(df_error[, -1]))
						
						if (sum(v_sum) > 0) {
							v_e_message <- c()
							v_pos <- which(v_sum > 0)
							
							if (1 %in% v_sum) { # variable with no value
								v_var <- df_error[which(df_error$no_val == 1), "var"]
								v_e_message <- c(v_e_message, paste0("The following variable", ifelse(length(v_var) > 1, "s have", " has"), " no value: ", paste(v_var, collapse = ", ")))
							}
							
							if (2 %in% v_sum) { # qualitative variable
								v_var <- df_error[which(df_error$qual == 1), "var"]
								v_e_message <- c(v_e_message, paste0("The following variable", ifelse(length(v_var) > 1, "s are not quantitative variables", " is not a quantitative variable"), ": ", paste(v_var, collapse = ", ")))
							}
							
							s_e_message <- paste0(paste(v_e_message, collapse = ".<br/>"), ifelse(length(v_e_message) > 1, ".", ""))
						}
					}
				}
				else { # normal/ir
					if (b_equal) {
						s_e_message <- paste0("Same variable", ifelse(length(v_var_name) > 0, "s", ""), " filled in X and Group fields")
					}
					else {
						v_sum <- as.vector(colSums(df_error[, -c(1, 2)]))
						
						if (sum(v_sum) > 0) {
							v_e_message <- c()
							v_pos <- which(v_sum > 0)
							
							if (1 %in% v_pos) { # empty field
								v_var <- df_error[which(df_error$empty == 1), "var"] 
								v_e_message <- c(v_e_message, paste0(ifelse(length(v_var) > 1, "X and Group variable fields are", paste0(v_var, " variable field is")), " empty"))
							}
							
							if (2 %in% v_pos) { # variable with no value
								v_var <- df_error[which(df_error$no_val == 1), "var"]
								v_size <- df_error[which(df_error$no_val == 1), "size"]
								v_pos <- which(v_size > 1)
								if (length(v_pos) > 0) {v_e_message <- c(v_e_message, paste0("The combination of ", v_var[v_pos], " variables has no value"))}
								if((length(v_size) - length(v_pos)) > 0) {v_e_message <- c(v_e_message, paste0(v_var[-v_pos], " variable has no value"))}
							}
							
							if (3 %in% v_pos) { # quantitative variable
								v_var <- df_error[which(df_error$quant == 1), "var"] 
								v_e_message <- c(v_e_message, paste0(ifelse(length(v_var) > 1, "X and Group variables are quantitative variables", paste0(v_var, " variable is a quantitative variable"))), "A quantitative variable can be transformed into a qualitative one by checking the concatenation box and re-selecting this variable from the input list")
							}
							
							s_e_message <- paste0(paste(v_e_message, collapse = ".<br/>"), ifelse(length(v_e_message) > 1, ".", ""))
						}
					}
				}
			}
			
			if (!b_quant) {
				if (length(which(!is.na(v_var_name))) > 0 & length(v_name) > 0) {
					l_out$var <- v_var_name
				}
				else {
					v_del <- c(v_del, "var")
				}

				if (b_sorting2) {
					if (length(which(!is.na(v_var_name2))) > 0 & length(v_name2) > 0) {
						l_out$var2 <- v_var_name2
					}
					else {
						v_del <- c(v_del, "var2")
					}
				}
			}
		}
	}
	
	return(list(l_out, unique(v_del), s_e_message, s_w_message))
}
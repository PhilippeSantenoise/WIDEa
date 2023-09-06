#' List of functions used to update reactive values

#' @description
#' `f_create_rv_inventory` is used to create inventory of reactive values with id
#' and initial value. This inventory is used to reset reactive values. 
#' \cr`f_create_rv_id_value_list` is used to create a list of id/value used to reset
#' specific reactive values.
#' \cr`f_update_rv` is used to create command lines to update reactive values.
#' \cr`f_update_graph_option_rv` is used to update reactive values associated to
#' graph option edition (color/opacity, point type/size).

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
#' @param v_var_name is the vector of variable name.
#' @param df_all are data saved in e_data environment (e_data$all).
#' @param o_name_option is the reactive value associated to graph option (color,
#' opacity, point type/size) edition.
#' @param b_display_clear is a boolean value associated to the display button.
#' `b_display_clear` is true if the display/clear button is clicked and a graph is
#' already created, else false.
#' @param b_sub_data is a boolean value such that `b_sub_data` is true if a sub-data
#' exists and the "subdata_option" check box input is unchecked, false else.
#' @param s_option is the option value (2 values: "color/opacity",
#' "point type/size"). `s_option` is associated to the "edit_option" selectize input.

#' @encoding UTF-8

f_create_rv_inventory <- function() {
	# reactive values associated to the display button
	df_out_1 <- data.frame(
		"choice" = rep("display", 62),
		"rv" = c("o_click_button", "o_click_legend", "o_zoom", "o_input_status", rep("o_parameter", 21), rep("o_cond", 8), rep("o_plot", 15), rep("o_picture_info", 4), rep("o_lreg_info", 3), rep("o_name_option", 4), rep("o_stat_method", 3)),
		"id" = c("display", "item", "coord", "display",
			"data_name", "plot_type", "dim_num", "model", "ref", "wres_group", "wres_vfun", "id", "concat1", "concat1_group", "x", "f", "date_format", "y", "g", "z", "h", "concat2", "concat2_group", "group", "corplot_group", 
			"display", "flag", "qc1", "qc2", "save1", "save2", "selec_leg", "deselec_leg",
			"data", "model", "code_freq", "id_group", "y_coord", "add_pt", "pt_pos", "var_pt", "data_qc1", "data_qc2", "var_qc1", "var_qc2", "leg_name_qc", "elt", "elt_pt_pos",
			"filename", "format", "height", "width",
			"xpos", "ypos", "elt",
			"color_default", "opacity_default", "point_type_default", "point_size_default",
			"inv", "level", "message"
		),
		"value" = c("0", rep("NULL", 2), "data.frame()",
			rep("NA", 8), "F", rep("NA", 8), "F", rep("NA", 3), 
			rep("0", 8),
			"data.frame()", "NULL", rep("NA", 2), "NULL", "T", rep("NA", 9),
			"\"Picture_name\"", "\"png\"", "800", "1000",
			"0", "1", "NA",
			rep("c()", 4),
			"l_stat_method_ini$inv", "l_stat_method_ini$level", "l_stat_method_ini$message"
		)
	)
	
	# reactive values reseted when data (e_data$all) are removed in e_data environment
	df_out_2 <- data.frame(
		"choice" = rep("lp_s1_s2", 20),
		"rv" = c("o_reset", "o_cond", "o_option", rep("o_parameter", 17)),
		"id" = c("code", "flag_msg",
			"choice",
			"webgl", "mode", "autobw", "bw", "y_scale", "autodec_num", "dec_num", "xlab", "ylab", "zlab", "lreg", "conf_ellipsoid", "centroid", "boxmean", "dens_curve", "norm_dens_curve", "mean_spect"
		),
		"value" = c(rep("0", 2),
			"c()",
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
f_update_graph_option_rv <- function (o_name_option, s_option, i_display, s_data_type, s_plot_type, v_var_name, df_all = NULL, b_display_clear = F, b_sub_data = F) {
	l_out <- list()
	v_del <- c() 
	
	if (b_sub_data) {
		if (paste(o_name_option$var, collapse = "_") == paste(v_var_name, collapse = "_")) {
			eval(parse(text = paste0("v_name <- unique(as.vector(", ifelse(length(o_name_option$var) > 1, "f_create_concat_variable(df_all, v_var_name)", "df_all[, v_var_name]"), "))")))
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
			}
		}
		else {
			v_del <- c(v_del, "name", "color", "opacity", "point_type", "point_size", "var")
		}
	}
	else {
		if (i_display == 1 & !b_display_clear) {
			if (length(o_name_option$name) == 0) {
				if (s_data_type == "temporal") {
					l_out$name <- v_var_name
				}
				else {
					if (length(which(!is.na(v_var_name))) == 0 & s_plot_type %in% c("plot", "histplot")) {
						l_out$name <- "all"
					}
					else {
						eval(parse(text = paste0("v_name <- unique(as.vector(df_all[, \"", ifelse(length(v_var_name) > 1, ifelse(s_plot_type %in% c("plot", "histplot"), ".concat2.", ".concat1."), v_var_name), "\"]))")))
						v_name <- v_name[order(v_name)]
						l_out$name <- v_name
					}
				}
				
				if (s_option == "color/opacity") {
					eval(parse(text = paste(paste0("l_out$", c("color", "opacity"), "_temp <- rep(\"\", length(l_out$name))"), collapse = "; ")))
				}
				else { # point type/size
					eval(parse(text = paste(paste0("l_out$", c("point_type", "point_size"), "_temp <- rep(\"\", length(l_out$name))"), collapse = "; ")))
				}
			}
			else {
				if (s_option == "color/opacity") {
					eval(parse(text = paste(paste0("l_out$", c("color", "opacity"), "_temp <- rep(\"\", length(o_name_option$name))"), collapse = "; ")))
				}
				else { # point type/size
					eval(parse(text = paste(paste0("l_out$", c("point_type", "point_size"), "_temp <- rep(\"\", length(o_name_option$name))"), collapse = "; ")))
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
			else { # point type/size
				if (length(o_name_option$point_type) > 0) {l_out$point_type_temp <- o_name_option$point_type}
				if (length(o_name_option$point_size) > 0) {l_out$point_size_temp <- o_name_option$point_size}
			}
			
			l_out$warning <- 1
		}
		else {
			if (s_data_type == "temporal") {
				v_name <- v_var_name
			}
			else {
				v_pos <- which(!is.na(v_var_name))
				
				if (length(v_pos) == 0 & s_plot_type %in% c("plot", "histplot")) {
					v_name <- "all"
				}
				else {
					v_name <- NULL
					
					if (length(v_pos) > 0) {
						eval(parse(text = paste0("v_name <- unique(as.vector(", ifelse(length(v_pos) > 1, "f_create_concat_variable(df_all, v_var_name)", "df_all[, v_var_name]"), "))")))
						v_name <- v_name[order(v_name)]
					}
				}
			}
			
			if (b_display_clear) {
				if (!is.null(v_name)) {
					if (length(o_name_option$color) > 0 | length(o_name_option$opacity) > 0 | length(o_name_option$point_type) > 0 | length(o_name_option$point_size) > 0) {
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
								v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "var")
							}
							else {
								if (length(which(!is.na(v_var_name))) > 0) {
									if (length(which(v_var_name %in% o_name_option$var)) != length(v_var_name) | length(which(o_name_option$var %in% v_var_name)) != length(o_name_option$var)) {v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "var")}
								}
								else {
									if (length(o_name_option$var) != 0) {v_del <- c(v_del, "color", "opacity", "point_type", "point_size", "var")}
								}
							}
						}
					}
					
					l_out$name <- v_name
				}
				else {
					v_del <- c(v_del, "name", "color", "opacity", "point_type", "point_size", "var")
				}
			}
			else {
				if (!is.null(v_name)) {
					if (s_option == "color/opacity") {
						l_out$color_temp <- rep("", length(v_name))
						l_out$opacity_temp <- rep("", length(v_name))
					}
					else { # point type/size
						l_out$point_type_temp <- rep("", length(v_name))
						l_out$point_size_temp <- rep("", length(v_name))
					}
					
					if (length(o_name_option$name) > 0 & ((s_option == "color/opacity" & (length(o_name_option$color) > 0 | length(o_name_option$opacity) > 0)) | (s_option == "point type/size" & (length(o_name_option$point_type) > 0 | length(o_name_option$point_size) > 0)))) {
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
											if (!is.null(o_name_option$color)) {l_out$color_temp <- o_name_option$color}
											if (!is.null(o_name_option$opacity)) {l_out$opacity_temp <- o_name_option$opacity}
										}
										else { # point type/size
											if (!is.null(o_name_option$point_type)) {l_out$point_type_temp <- o_name_option$point_type}
											if (!is.null(o_name_option$point_size)) {l_out$point_size_temp <- o_name_option$point_size}
										}
									}
								}
								else {
									if (length(o_name_option$var) == 0) {
										if (s_option == "color/opacity") {
											if (!is.null(o_name_option$color)) {l_out$color_temp <- o_name_option$color}
											if (!is.null(o_name_option$opacity)) {l_out$opacity_temp <- o_name_option$opacity}
										}
										else { # point type/size
											if (!is.null(o_name_option$point_type)) {l_out$point_type_temp <- o_name_option$point_type}
											if (!is.null(o_name_option$point_size)) {l_out$point_size_temp <- o_name_option$point_size}
										}
									}
								}
							}
						}
					}
					
					l_out$name <- v_name
				}
				else {
					v_del <- c(v_del, "name", "color_temp", "opacity_temp", "point_type_temp", "point_size_temp")
				}
			}
			
			if (length(which(!is.na(v_var_name))) > 0) {
				l_out$var <- v_var_name
			}
			else {
				v_del <- c(v_del, "var")
			}
		}
	}
	
	return(list(l_out, unique(v_del)))
}

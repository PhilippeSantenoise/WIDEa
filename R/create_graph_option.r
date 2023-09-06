#' @importFrom shiny isolate
NULL

#' List of functions used to build (color, opacity and point type/size) options
#' applied to the main plotly graph 

#' @description
#' `f_create_default_graph_opt_list` is used to create the list of graph default
#' color, opacity, point type and size.
#' \cr`f_create_graph_opt_list` is used to create the list of graph (custom) color,
#' opacity, point type and size (default is applied if no custom).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param df_all are data created from the `f_prepare_data` function (process = 1).
#' This input only concern the normal/ir data type. 
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_name_option is a reactive value with graph option (color, opacity, point
#' type/size) inventory (default, custom).

#' @encoding UTF-8

f_create_default_graph_opt_list <- function (s_data_type = "normal", df_all = data.frame(), o_parameter) {
	if (s_data_type %in% c("normal", "ir")) {
		if (o_parameter$plot_type %in% c("boxplot", "barplot")) {
			v_x <- unique(as.vector(df_all[, isolate(o_parameter$x)]))
			i_num <- length(v_x)
			v_name <- v_x[order(v_x)]
		}
		else {
			if (!is.na(isolate(o_parameter$group))) {
				v_group <- unique(as.vector(df_all[, isolate(o_parameter$group)]))
				i_num <- length(v_group)
				v_name <- v_group[order(v_group)]
			}
			else {
				i_num <- 1
				v_name <- "all"
			}
		}
	}
	else { # temporal
		i_num <- length(o_parameter$y)
		v_name <- o_parameter$y
	}
	
	if (i_num > 1) {
		v_color <- scales::hue_pal()(i_num)
	}
	else {
		v_color <- "#1C86EE"
	}
	
	v_opacity <- rep("0.7", i_num)
	v_point_type <- rep("1", i_num)
	v_point_size <- rep("6", i_num)
	eval(parse(text = paste(paste0("names(v_", c("color", "opacity", "point_type", "point_size"), ") <- v_name"), collapse = "; ")))
	return (list("color" = v_color, "opacity" = v_opacity, "point_type" = v_point_type, "point_size" = v_point_size))
}

#' @rdname f_create_default_graph_opt_list
f_create_graph_opt_list <- function(o_name_option) {
	v_color <- isolate(o_name_option$color_default)
	v_pos <- which(isolate(o_name_option$color) != "")
	
	if (length(v_pos) > 0) {
		v_custom_color <- isolate(o_name_option$color)[v_pos] 
		names(v_custom_color) <- isolate(o_name_option$name)[v_pos]
		v_pos <- which(names(v_color) %in% names(v_custom_color))
		if (length(v_pos) > 0) {v_color[names(v_color)[v_pos]] <- as.vector(v_custom_color[names(v_color)[v_pos]])}
	}
	
	v_opacity <- isolate(o_name_option$opacity_default)
	v_pos <- which(isolate(o_name_option$opacity) != "")
	
	if (length(v_pos) > 0) {
		v_custom_opacity <- isolate(o_name_option$opacity)[v_pos] 
		names(v_custom_opacity) <- isolate(o_name_option$name)[v_pos]
		v_pos <- which(names(v_opacity) %in% names(v_custom_opacity))
		if (length(v_pos) > 0) {v_opacity[names(v_opacity)[v_pos]] <- as.vector(v_custom_opacity[names(v_opacity)[v_pos]])}
	}
	
	v_point_type <- isolate(o_name_option$point_type_default)
	v_pos <- which(isolate(o_name_option$point_type) != "")
	
	if (length(v_pos) > 0) {
		v_custom_point_type <- isolate(o_name_option$point_type)[v_pos] 
		names(v_custom_point_type) <- isolate(o_name_option$name)[v_pos]
		v_pos <- which(names(v_point_type) %in% names(v_custom_point_type))
		if (length(v_pos) > 0) {v_point_type[names(v_point_type)[v_pos]] <- as.vector(v_custom_point_type[names(v_point_type)[v_pos]])}
	}
	
	v_point_size <- isolate(o_name_option$point_size_default)
	v_pos <- which(isolate(o_name_option$point_size) != "")
	
	if (length(v_pos) > 0) {
		v_custom_point_size <- isolate(o_name_option$point_size)[v_pos] 
		names(v_custom_point_size) <- isolate(o_name_option$name)[v_pos]
		v_pos <- which(names(v_point_size) %in% names(v_custom_point_size))
		if (length(v_pos) > 0) {v_point_size[names(v_point_size)[v_pos]] <- as.vector(v_custom_point_size[names(v_point_size)[v_pos]])}
	}
	
	return(list("color" = as.vector(v_color), "opacity" = as.vector(as.numeric(v_opacity)), "point_type" = as.vector(as.numeric(v_point_type)), "point_size" = as.vector(as.numeric(v_point_size))))
}
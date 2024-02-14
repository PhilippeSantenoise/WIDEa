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
#' type/size, sorting) inventory (default, custom).
#' @param b_quant is a boolean value associated to the Group variable (only for
#' normal/ir data type: plot). The value is True if the Group variable is
#' quantitative, False else.

#' @encoding UTF-8

f_create_default_graph_opt_list <- function (s_data_type = "normal", df_all = data.frame(), o_parameter) {
	v_sorting <- c()
	v_sorting2 <- c()
	
	if (s_data_type %in% c("normal", "ir")) {
		if (o_parameter$plot_type %in% c("boxplot", "barplot")) {
			v_x <- unique(as.vector(df_all[, isolate(o_parameter$x)]))
			i_num <- length(v_x)
			v_name <- v_x[order(v_x)]
			v_sorting <- 1:i_num
			names(v_sorting) <- v_name
			
			if (!is.na(isolate(o_parameter$group))) {
				v_group <- unique(as.vector(df_all[, isolate(o_parameter$group)]))
				v_sorting2 <- 1:length(v_group)
				names(v_sorting2) <- v_group[order(v_group)]
			}
		}
		else {
			if (!is.na(isolate(o_parameter$group))) {
				v_group <- unique(as.vector(df_all[, isolate(o_parameter$group)]))
				i_num <- length(v_group)
				v_name <- v_group[order(v_group)]
				v_sorting <- 1:i_num
				names(v_sorting) <- v_name
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
	return (list("color" = v_color, "opacity" = v_opacity, "point_type" = v_point_type, "point_size" = v_point_size, "sorting" = v_sorting, "sorting2" = v_sorting2))
}

#' @rdname f_create_default_graph_opt_list
f_create_graph_opt_list <- function(o_name_option, b_quant = F) {
	if (b_quant) {
		l_col_op <- isolate(o_name_option$pal_col_op_default)
		if (length(isolate(o_name_option$pal_col_op)) > 0) {l_col_op <- isolate(o_name_option$pal_col_op)}
		l_point <- isolate(o_name_option$pal_point_default)
		if (length(isolate(o_name_option$pal_point)) > 0) {l_point <- isolate(o_name_option$pal_point)}
		l_out <- list("pal_col_op" = l_col_op, "pal_point" = l_point)
	}
	else {
		v_color <- isolate(o_name_option$color_default)
		v_pos <- which(isolate(o_name_option$color) != "")
		
		if (length(v_pos) > 0) { # custom color
			v_custom_color <- isolate(o_name_option$color)[v_pos] 
			names(v_custom_color) <- isolate(o_name_option$name)[v_pos]
			v_pos <- which(names(v_color) %in% names(v_custom_color))
			if (length(v_pos) > 0) {v_color[names(v_color)[v_pos]] <- as.vector(v_custom_color[names(v_color)[v_pos]])}
		}
		
		v_opacity <- isolate(o_name_option$opacity_default)
		v_pos <- which(isolate(o_name_option$opacity) != "")
		
		if (length(v_pos) > 0) { # custom opacity
			v_custom_opacity <- isolate(o_name_option$opacity)[v_pos] 
			names(v_custom_opacity) <- isolate(o_name_option$name)[v_pos]
			v_pos <- which(names(v_opacity) %in% names(v_custom_opacity))
			if (length(v_pos) > 0) {v_opacity[names(v_opacity)[v_pos]] <- as.vector(v_custom_opacity[names(v_opacity)[v_pos]])}
		}
		
		v_point_type <- isolate(o_name_option$point_type_default)
		v_pos <- which(isolate(o_name_option$point_type) != "")
		
		if (length(v_pos) > 0) { # custom point type
			v_custom_point_type <- isolate(o_name_option$point_type)[v_pos] 
			names(v_custom_point_type) <- isolate(o_name_option$name)[v_pos]
			v_pos <- which(names(v_point_type) %in% names(v_custom_point_type))
			if (length(v_pos) > 0) {v_point_type[names(v_point_type)[v_pos]] <- as.vector(v_custom_point_type[names(v_point_type)[v_pos]])}
		}
		
		v_point_size <- isolate(o_name_option$point_size_default)
		v_pos <- which(isolate(o_name_option$point_size) != "")
		
		if (length(v_pos) > 0) { # custom point size
			v_custom_point_size <- isolate(o_name_option$point_size)[v_pos] 
			names(v_custom_point_size) <- isolate(o_name_option$name)[v_pos]
			v_pos <- which(names(v_point_size) %in% names(v_custom_point_size))
			if (length(v_pos) > 0) {v_point_size[names(v_point_size)[v_pos]] <- as.vector(v_custom_point_size[names(v_point_size)[v_pos]])}
		}
		
		l_out <- list("color" = as.vector(v_color), "opacity" = as.vector(as.numeric(v_opacity)), "point_type" = as.vector(as.numeric(v_point_type)), "point_size" = as.vector(as.numeric(v_point_size)))
		
		if (length(isolate(o_name_option$sorting_default)) > 0) { # custom values for the sorting of X/Group variable levels
			v_sorting <- isolate(o_name_option$sorting_default)
			v_custom_sorting <- isolate(o_name_option$sorting)
			v_name <- isolate(o_name_option$name)
			if (length(v_custom_sorting) > 0) {v_sorting[v_name[which(v_name %in% names(v_sorting))]] <- v_custom_sorting[which(v_name %in% names(v_sorting))]}
			l_out$sorting <- v_sorting
		}
		
		if (length(o_name_option$sorting2_default) > 0) { # custom values for the sorting of Group variable levels (boxplot, barplot)
			v_sorting <- o_name_option$sorting2_default
			v_custom_sorting <- isolate(o_name_option$sorting2)
			v_name <- isolate(o_name_option$name2)
			if (length(v_custom_sorting) > 0) {v_sorting[v_name[which(v_name %in% names(v_sorting))]] <- v_custom_sorting[which(v_name %in% names(v_sorting))]}
			l_out$sorting2 <- v_sorting
		}
	}
	
	return(l_out)
}
#########################################################################################################
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
# Description : Creating the graph option (default, custom) : color, opacity, point type/size
#
# Creation date : March 2021
#########################################################################################################


# Inputs:
# -------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# df_all: data created from the function "f_prepare_data" (process = 1). This input only concern the normal/ir data type 
# o_parameter: reactive value with graph parameters
# o_name_option: reactive value with graph option (color, opacity, point type/size) inventory (default, custom)


# function used to create the list of graph default color, opacity, point type and size
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
		v_color <- hue_pal()(i_num)
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

# function used to create the list of graph (custom) color, opacity, point type and size (default is applied if no custom)
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
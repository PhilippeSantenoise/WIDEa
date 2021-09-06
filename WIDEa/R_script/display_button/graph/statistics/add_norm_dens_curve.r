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
# Description : function used to compute a normal density curve and add results on the (plotly) graph.   
#               This function only concerns histplot of the normal data type.
#               
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# ply_1: plotly object created with the R script "Build_graph"
# v_group: (Group) variable modalities retained to execute the corresponding method. If no Group variable 
#          is selected, then v_group = "all". 
# df_click_legend: legend item informations (name and status)
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"
# i_w_message: binary value used to display (= 0) or not (= 1) the warning message returned by the function             

# Output:
# -------
# return a list containing a plotly object and warning/error messages

f_add_norm_dens_curve <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, i_w_message) {
	s_w_message <- NULL
	s_e_message <- NULL
	
	if (length(v_group) > 0) {
		df_all <- isolate(o_plot$data)
		n_op <- ifelse(isolate(o_parameter$autoop) == F, isolate(o_parameter$op), 0.5)
		
		if (!is.na(isolate(o_parameter$group))) {
			v_group_all <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
			v_group_all <- v_group_all[order(v_group_all)]
			
			if (length(v_group) < length(v_group_all)) {
				if (i_w_message == 0) {
					s_w_message <- paste0("Error with normal density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to zero for one or more groups (", paste(v_group_all[which(!v_group_all %in% v_group)], collapse = ", "), ")")
				}
			}
			
			v_color <- isolate(o_plot$color)[which(v_group_all %in% v_group)]
			l_val <- lapply(v_group, function(x) {
				v_x <- df_all[df_all[, isolate(o_parameter$group)] == x, isolate(o_parameter$x)]
				n_mean <- mean(v_x)
				n_sd <- sd(v_x)
				
				if (n_sd == 0) {
					return(list())
				}
				else {
					v_x_norm <- seq(n_mean - 3.5 * n_sd, n_mean + 3.5 * n_sd, length.out = 1000)
					v_y_norm <- dnorm(v_x_norm, n_mean, n_sd)
					return(list(v_x_norm, v_y_norm))
				}
			})
			
			v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (normal curve)")), "statut"]
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = l_val[[", 1:length(v_group), "]][[1]], y = l_val[[", 1:length(v_group), "]][[2]], name = \"", v_group, " (normal curve)\", type = \"scatter\", mode = \"lines\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op + 0.3), dash = \"dash\"), hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 0.7)), showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
		}
		else {
			v_x <- df_all[, isolate(o_parameter$x)]
			n_mean <- mean(v_x)
			n_sd <- sd(v_x)
			v_x_norm <- seq(n_mean - 3.5 * n_sd, n_mean + 3.5 * n_sd, length.out = 1000)
			v_y_norm <- dnorm(v_x_norm, n_mean, n_sd)
			v_visible <- df_click_legend[which(df_click_legend$name == "all (normal curve)"), "statut"]
			ply_1 <- add_trace(p = ply_1, x = v_x_norm, y = v_y_norm, name = "all (normal curve)", type = "scatter", mode = "lines", line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = n_op + 0.3), dash = "dash"), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), showlegend = T, visible = eval(parse(text = v_visible)))
		}
	}
	else {
		if (!is.na(isolate(o_parameter$group))) {
			s_e_message <- "Error with normal density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to zero for each group"
		}
		else {
			s_e_message <- "Error with normal density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to zero"
		}
	}
	
	return (list(ply_1, s_w_message, s_e_message))
}

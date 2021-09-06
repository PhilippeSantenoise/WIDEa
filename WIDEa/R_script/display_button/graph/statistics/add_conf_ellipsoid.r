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
# Description : function used to compute a (95 %) confidence ellipse and add results on the (plotly)  
#               graph. This function only concerns 2D plot of the normal data type (disabled if a 
#               calibration model is applied).
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

f_add_conf_ellipsoid <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, i_w_message) {
	s_w_message <- NULL
	s_e_message <- NULL
	
	if (length(v_group) > 0) {
		if (isolate(o_parameter$model) == "none") {
			eval(parse(text = paste0("df_all <- isolate(o_plot$data)[, c(isolate(o_parameter$x), isolate(o_parameter$y)", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ")]")))
		}
		else { # validation model
			eval(parse(text = paste0("df_all <- as.data.frame(cbind(isolate(o_plot$data)[, c(isolate(o_parameter$y)", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ")], isolate(o_plot$model)[, \"fit\"]))")))
			eval(parse(text = paste0("names(df_all) <- c(isolate(o_parameter$y), ", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ", \".fit.\")")))
		}
		
		if (!is.na(isolate(o_parameter$group))) {
			v_group_all <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
			v_group_all <- v_group_all[order(v_group_all)]
			
			l_val <- lapply(v_group, function(x) {
				df_group <- df_all[df_all[, isolate(o_parameter$group)] == x,]
				df_out <- dataEllipse(x = as.vector(df_group[, ifelse(isolate(o_parameter$model) == "none", isolate(o_parameter$x), ".fit.")]), y = as.vector(df_group[, isolate(o_parameter$y)]), levels = 0.95, draw = F, segments = 100)
				return(df_out)
			})
			
			v_pos_1 <- which(v_group_all %in% v_group) 
			v_pos_2 <- which(!v_group_all %in% v_group) 
			v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group_all[v_pos_1], " (ellipsoid)")), "statut"]
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = l_val[[", 1:length(l_val), "]][, 1], y = l_val[[", 1:length(l_val), "]][, 2], name = \"", v_group, " (ellipsoid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", isolate(o_plot$color)[v_pos_1], "\", alpha.f = 0.95), width = 1.5), fill = \"toself\", fillcolor = list(color = adjustcolor(\"", isolate(o_plot$color)[v_pos_1], "\", alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color)[v_pos_1], "\", alpha.f = 0.7)), hoverinfo = \"name\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
		
			if (length(v_pos_2) > 0) {
				if (i_w_message == 0) {
					s_w_message <- paste0("Error with confidence ellipsoid calculation: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero for one or more groups (", paste(v_group_all[v_pos_2], collapse = ", "), ")")
				}
			}
		}
		else {
			df_val <- dataEllipse(x = as.vector(df_all[, ifelse(isolate(o_parameter$model) == "none", isolate(o_parameter$x), ".fit.")]), y = as.vector(df_all[, isolate(o_parameter$y)]), levels = 0.95, draw = F, segments = 100)
			eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (ellipsoid)"), "statut"])))
			ply_1  <- add_trace(p = ply_1, x = df_val[, 1], y = df_val[, 2], name = "all (ellipsoid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "lines", line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.95), width = 1.5), fill = "toself", fillcolor = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = "name", showlegend = T, visible = v_visible)
		}
	}
	else {
		if (!is.na(isolate(o_parameter$group))) {
			s_e_message <- paste0("Error with confidence ellipsoid calculation: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero for each group")
		}
		else {
			s_e_message <- paste0("Error with confidence ellipsoid calculation: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero")
		}
	}
	
	return (list(ply_1, s_w_message, s_e_message))
}

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
# v_color: graph color vector

# Output:
# -------
# return a plotly object

f_add_conf_ellipsoid <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, v_color) {
	df_all <- isolate(o_plot$data)
	
	if (isolate(o_parameter$model) == "valid") { # validation model
		df_model <- isolate(o_plot$model)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		df_all <- as.data.frame(df_all[, v_pos[!is.na(v_pos)]])
		names(df_all) <- c("y", "group")[which(!is.na(v_pos))]
		df_all$x <- as.vector(df_model$fit)
		rm(list = "df_model")
	}
	else { # no model
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "y", "group")[which(!is.na(v_pos))]
	}
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group_all <- as.vector(unique(df_all$group))
		v_group_all <- v_group_all[order(v_group_all)]
		
		l_val <- lapply(v_group, function(x) {
			df_group <- df_all[which(df_all$group == x),]
			df_out <- dataEllipse(x = as.vector(df_group$x), y = as.vector(df_group$y), levels = 0.95, draw = F, segments = 100)
			return(df_out)
		})
		
		v_pos_1 <- which(v_group_all %in% v_group) 
		v_pos_2 <- which(!v_group_all %in% v_group) 
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group_all[v_pos_1], " (ellipsoid)")), "statut"]
		eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = l_val[[", 1:length(l_val), "]][, 1], y = l_val[[", 1:length(l_val), "]][, 2], name = \"", v_group, " (ellipsoid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", v_color[v_pos_1], "\", alpha.f = 1), width = 1.5), fill = \"toself\", fillcolor = list(color = adjustcolor(\"", v_color[v_pos_1], "\", alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[v_pos_1], "\", alpha.f = 1)), hoverinfo = \"name\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
	}
	else {
		df_val <- dataEllipse(x = as.vector(df_all$x), y = as.vector(df_all$y), levels = 0.95, draw = F, segments = 100)
		eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (ellipsoid)"), "statut"])))
		ply_1  <- add_trace(p = ply_1, x = df_val[, 1], y = df_val[, 2], name = "all (ellipsoid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "lines", line = list(color = adjustcolor(v_color, alpha.f = 1), width = 1.5), fill = "toself", fillcolor = list(color = adjustcolor(v_color, alpha.f = 0.3)), hoverlabel = list(bgcolor = adjustcolor(v_color, alpha.f = 1)), hoverinfo = "name", showlegend = T, visible = v_visible)
	}
	
	return (ply_1)
}

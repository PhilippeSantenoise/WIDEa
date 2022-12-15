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
# Description : function used to compute centroid and add results on the (plotly) graph. 
#               This function only concerns 2D/3D plot of the normal data type (disabled if a calibration 
#               model is applied).
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# ply_1: plotly object created with the R script "Build_graph"
# df_click_legend: legend item informations (name and status)
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"
# l_graph_opt: list of graph option (color, opacity, point type/size) created by the f_create_graph_opt_vector function

# Output:
# -------
# return a plotly object

f_add_centroid <- function (ply_1, df_click_legend, o_plot, o_parameter, l_graph_opt) {
	df_all <- isolate(o_plot$data)
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
	
	if (length(which(o_parameter$dim_num == "3d")) > 0) {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
	}
	else {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open", "star", "star-open", "hourglass-open", "bowtie-open", "circle-cross-open", "circle-x-open", "square-cross-open", "square-x-open", "diamond-cross-open", "diamond-x-open", "cross-thin-open", "x-thin-open", "asterisk-open", "hash-open")
	}
	
	if (isolate(o_parameter$model) == "valid") { # validation model
		df_model <- isolate(o_plot$model)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		df_all <- as.data.frame(df_all[, v_pos[!is.na(v_pos)]])
		names(df_all) <- c("y", "group")[which(!is.na(v_pos))]
		df_all$x <- as.vector(df_model$fit)
		rm(list = "df_model")
	}
	else { # no model
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), ifelse(!is.na(isolate(o_parameter$h)), ".h.", isolate(o_parameter$z)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "y", "z", "group")[which(!is.na(v_pos))]
	}
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group <- as.vector(unique(df_all$group))
		v_group <- v_group[order(v_group)]
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (centroid)")), "statut"]
		
		if (isolate(o_parameter$dim_num) == "3d") {
			df_mean <- aggregate(cbind(x, y, z) ~ group, data = df_all, mean)
			v_text <- paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(df_mean$x, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean$y, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "z", "h(z)"), ": ", round(df_mean$z, digits = i_dec_num)) 
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[which(df_mean$group == \"", v_group, "\"), \"x\"], y = df_mean[which(df_mean$group == \"", v_group, "\"), \"y\"], z = df_mean[which(df_mean$group == \"", v_group, "\"), \"z\"], name = \"", v_group, " (centroid)\", type = \"scatter3d\", mode = \"markers\", marker = list(symbol = \"", v_symbol[l_graph_opt$point_type], "\", color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1), size = ", 2 * l_graph_opt$point_size, ", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1)), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; "))) 
		}
		else { # plot 2d or validation model
			df_mean <- aggregate(cbind(x, y) ~ group, data = df_all, mean)
			v_text <- paste0(ifelse(isolate(o_parameter$model) == "valid", "fit", ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), ": ", round(df_mean$x, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean$y, digits = i_dec_num))
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[which(df_mean$group == \"", v_group, "\"), \"x\"], y = df_mean[which(df_mean$group == \"", v_group, "\"), \"y\"], name = \"", v_group, " (centroid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(symbol = \"", v_symbol[l_graph_opt$point_type], "\", color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1), size = ", 2 * l_graph_opt$point_size, ", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1)), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
		}
	}
	else {
		eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (centroid)"), "statut"])))
		
		if (isolate(o_parameter$dim_num) == "3d") {
			ply_1  <- add_trace(p = ply_1, x = mean(df_all$x), y = mean(df_all$y), z = mean(df_all$z), name = "all (centroid)", type = "scatter3d", mode = "markers", marker = list(symbol = v_symbol[l_graph_opt$point_type], color = adjustcolor(l_graph_opt$color, alpha.f = 1), size = 2 * l_graph_opt$point_size, line = list(color = adjustcolor(l_graph_opt$color, alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = "text", text = paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(mean(df_all$x), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all$y), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$h)), "z", "h(z)"), ": ", round(mean(df_all$z), digits = i_dec_num)), showlegend = T, visible = v_visible)
		}
		else { # plot 2d or validation model
			ply_1  <- add_trace(p = ply_1, x = mean(df_all$x), y = mean(df_all$y), name = "all (centroid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(symbol = v_symbol[l_graph_opt$point_type], color = adjustcolor(l_graph_opt$color, alpha.f = 1), size = 2 * l_graph_opt$point_size, line = list(color = adjustcolor(l_graph_opt$color, alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = "text", text = paste0(ifelse(isolate(o_parameter$model) == "valid", "fit", ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), ": ", round(mean(df_all$x), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all$y), digits = i_dec_num)), showlegend = T, visible = v_visible)
		}
	}
	
	return (ply_1)
}

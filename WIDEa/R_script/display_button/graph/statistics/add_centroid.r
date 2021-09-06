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

# Output:
# -------
# return a plotly object

f_add_centroid <- function (ply_1, df_click_legend, o_plot, o_parameter) {
	df_all <- isolate(o_plot$data)
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
		v_group <- v_group[order(v_group)]
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (centroid)")), "statut"]
		
		if (isolate(o_parameter$model) == "none") {
			if (isolate(o_parameter$dim_num) == "2d") {
				eval(parse(text = paste0("df_mean <- aggregate(cbind(", isolate(o_parameter$x), ", ", isolate(o_parameter$y), ") ~ ", isolate(o_parameter$group), ", data = df_all, mean)"))) 
				v_text <- paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(df_mean[, isolate(o_parameter$x)], digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean[, isolate(o_parameter$y)], digits = i_dec_num))
				eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$x)], y = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$y)], name = \"", v_group, " (centroid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.95), width = 2), color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.4), size = 12, symbol = \"square\"), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
			}
			else { # plot 3D
				eval(parse(text = paste0("df_mean <- aggregate(cbind(", isolate(o_parameter$x), ", ", isolate(o_parameter$y), ", ", isolate(o_parameter$z), ") ~ ", isolate(o_parameter$group), ", data = df_all, mean)"))) 
				v_text <- paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(df_mean[, isolate(o_parameter$x)], digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean[, isolate(o_parameter$y)], digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "z", "h(z)"), ": ", round(df_mean[, isolate(o_parameter$z)], digits = i_dec_num)) 
				eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$x)], y = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$y)], z = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$z)], name = \"", v_group, " (centroid)\", type = \"scatter3d\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.95), width = 2), color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.4), size = 12, symbol = \"square\"), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; "))) 
			}
		}
		else { # validation model
			df_all <- as.data.frame(cbind(df_all[, c(isolate(o_parameter$y), isolate(o_parameter$group))], isolate(o_plot$model)[, "fit"]))
			names(df_all)[3] <- ".fit."
			
			eval(parse(text = paste0("df_mean <- aggregate(cbind(.fit., ", isolate(o_parameter$y), ") ~ ", isolate(o_parameter$group), ", data = df_model, mean)"))) 
			v_text <- paste0("fit: ", round(df_mean[, ".fit."], digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean[, isolate(o_parameter$y)], digits = i_dec_num)) 
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", \".fit.\"], y = df_mean[df_mean[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$y)], name = \"", v_group, " (centroid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.95), width = 2), color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.4), size = 12, symbol = \"square\"), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
		} 
	}
	else {
		eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (centroid)"), "statut"])))
		
		if (isolate(o_parameter$model) == "none") {
			if (isolate(o_parameter$dim_num) == "2d") {
				ply_1  <- add_trace(p = ply_1, x = mean(df_all[, isolate(o_parameter$x)]), y = mean(df_all[, isolate(o_parameter$y)]), name = "all (centroid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.95), width = 2), color = adjustcolor(isolate(o_plot$color), alpha.f = 0.4), size = 12, symbol = "square"), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = "text", text = paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(mean(df_all[, isolate(o_parameter$x)]), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all[, isolate(o_parameter$y)]), digits = i_dec_num)), showlegend = T, visible = v_visible)
			}
			else { # plot 3D
				ply_1  <- add_trace(p = ply_1, x = mean(df_all[, isolate(o_parameter$x)]), y = mean(df_all[, isolate(o_parameter$y)]), z = mean(df_all[, isolate(o_parameter$z)]), name = "all (centroid)", type = "scatter3d", mode = "markers", marker = list(line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.95), width = 2), color = adjustcolor(isolate(o_plot$color), alpha.f = 0.4), size = 12, symbol = "square"), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = "text", text = paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(mean(df_all[, isolate(o_parameter$x)]), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all[, isolate(o_parameter$y)]), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$h)), "z", "h(z)"), ": ", round(mean(df_all[, isolate(o_parameter$z)]), digits = i_dec_num)), showlegend = T, visible = v_visible)
			}
		}
		else { # validation model
			df_all <- as.data.frame(cbind(df_all[, isolate(o_parameter$y)], isolate(o_plot$model)[, "fit"]))
			names(df_all) <- c(isolate(o_parameter$y), ".fit.")
			
			ply_1  <- add_trace(p = ply_1, x = mean(df_all[, ".fit."]), y = mean(df_all[, isolate(o_parameter$y)]), name = "all (centroid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.95), width = 2), color = adjustcolor(isolate(o_plot$color), alpha.f = 0.4), size = 12, symbol = "square"), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = "text", text = paste0("fit: ", round(mean(df_all[, ".fit."]), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all[, isolate(o_parameter$y)]), digits = i_dec_num)), showlegend = T, visible = v_visible)
		}
	}
	
	return (ply_1)
}

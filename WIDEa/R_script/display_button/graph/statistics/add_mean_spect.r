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
# Description : function used to compute mean spectrum and add results on the (plotly) graph. This  
#               function only concerns the ir data type. 
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

f_add_mean_spect <- function (ply_1, df_click_legend, o_plot, o_parameter) {
	df_all <- isolate(o_plot$data)
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
	v_name <- names(df_all)[-c(1, 2)]
	
	l_id_group <- isolate(o_plot$id_group)
	v_group <- as.vector(l_id_group$no_flag$group)
	v_color <- unique(as.vector(l_id_group$no_flag$color))
	
	v_visible <- as.vector(df_click_legend[which(df_click_legend$name %in% paste0(unique(v_group), " (mean)")), "statut"])
	
	l_var <- lapply(unique(v_group), function(i) {
		df_out <- data.frame("y" = rep(NA, dim(df_all)[1]), "info" = rep(NA, dim(df_all)[1])) 
		df_out$y <- rowMeans(df_all[, v_name[which(v_group == i)]])
		df_out$info <- paste0("frequency: ", round(as.vector(df_all$Frequency), digits = i_dec_num), "<br>y: ", round(df_out$y, digits = i_dec_num))
		return(df_out)
	})
	
	eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all$Frequency, y = l_var[[", 1:length(l_var), "]]$y, type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = 1), width = 3, dash = \"dash\"), name = \"" , unique(v_group), " (mean)\", hoverinfo = 'text+name', text = l_var[[", 1:length(l_var), "]]$info, legendgroup = \"" , unique(v_group), " (mean)\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
	
	if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
		l_var <- lapply(unique(v_group), function(i) {
			df_out <- data.frame("y" = rep(NA, dim(df_all)[1]), "info" = rep(NA, dim(df_all)[1])) 
			df_out$y <- rowMeans(df_all[isolate(o_plot$pt_pos), v_name[which(v_group == i)]])
			df_out$info <- paste0("frequency: ", round(as.vector(df_all[isolate(o_plot$pt_pos), "Frequency"]), digits = i_dec_num), "<br>y: ", round(df_out$y, digits = i_dec_num))
			return(df_out)
		})
		
		eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_all[isolate(o_plot$pt_pos), \"Frequency\"]), y = l_var[[", 1:length(l_var), "]]$y, type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"", v_color, "\", alpha.f = 1), width = 3), color = adjustcolor(\"", v_color, "\", alpha.f = 0.3), size = 6), name = \"" , unique(v_group), " (mean)\", hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = 'text+name', text = l_var[[", 1:length(l_var), "]]$info, legendgroup = \"" , unique(v_group), " (mean)\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
	}
	
	return (ply_1)
}

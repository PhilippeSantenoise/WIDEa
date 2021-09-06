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
# Description : (corplot, normal data type) the function allows to create a new graph in a modal window 
#               (showModal) when a click event is executed on a XY cell of the correlation matrix. 
#               Several graph types can be displayed from the previous/next button of the modal window. 
#
# Creation date : May 2021
#########################################################################################################


# Input:
# ------
# df_all: data created by the f_create_modal_data function
# i_num: number associated to the graph type (0: XY scatter-plot + X histplot + Y histplot; 1: XY scatter-plot; 2: X histplot; 3: Y histplot)  
# s_x_var: variable name corresponding to the selected cell in X axis
# s_y_var: variable name corresponding to the selected cell in Y axis

# Output:
# -------
# return a plotly object

f_build_modal_graph <- function (df_all,  i_num, s_x_var, s_y_var) {
	if (i_num == 0) {
		v_text <- paste0(s_x_var, ": ", df_all[, 1], "<br>", s_y_var, ": ", df_all[, 2])
		ply_1_1 <- plot_ly()
		ply_1_1 <- add_trace(p = ply_1_1, x = df_all[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_x_var)
		ply_1_1 <- layout(p = ply_1_1, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		ply_1_2 <- plot_ly()
		ply_1_2 <- add_trace(p = ply_1_2, x = df_all[, 1], y = df_all[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverinfo = 'text', text = v_text)
		ply_1_2 <- layout(p = ply_1_2, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		ply_1_3 <- plot_ly()
		ply_1_3 <- add_trace(p = ply_1_3, y = df_all[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_y_var)
		ply_1_3 <- layout(p = ply_1_3, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		ply_1 <- subplot(list(ply_1_1, plotly_empty(), ply_1_2, ply_1_3), nrows = 2, heights = c(0.3, 0.7), widths = c(0.7, 0.3), margin = 0, shareX = T, shareY = T, titleX = T, titleY = T) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		ply_1 <- layout(p = ply_1, showlegend = F)
	}
	else {
		ply_1 <- plot_ly() %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		
		if (i_num == 1) {
			v_text <- paste0(s_x_var, ": ", df_all[, 1], "<br>", s_y_var, ": ", df_all[, 2])
			ply_1 <- add_trace(p = ply_1, x = df_all[, 1], y = df_all[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverinfo = 'text', text = v_text)
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		}
		else if (i_num == 2) {
			ply_1 <- add_trace(p = ply_1, x = df_all[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_x_var)
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
		else {
			ply_1 <- add_trace(p = ply_1, x = df_all[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_y_var)
			ply_1 <- layout(p = ply_1, xaxis = list(title = s_y_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
	}
	
	return(ply_1)
}

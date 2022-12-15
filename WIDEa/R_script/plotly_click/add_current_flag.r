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
# Description : the function allows to create a list of data correponding to the current flag. This list 
#               is only used to update the graph ("addTraces" with the plotlyProxyInvoke function). 
#
# Creation date : May 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# s_dim_num: dimension number for the normal data type (2 values: "2d", "3d")
# l_temp_coord: (X, Y) coordinates used to draw intervals (created from the f_create_current_flag_data function) for the temporal data type (default = NULL)  
# (o_parameter, o_plot) : reactive values created in the R script "WIDEa_launcher"
# o_click_ev: data associated to a click event ("plotly_click" with the event_data function) 
# i_flag_num: current flag number
# l_click_info: list of supplementary informations associated to a click event (created by the f_create_click_info function). This input is only used for 
#               the ir data type and allows to identify the spectrum 

# Output:
# -------
# return the corresponding list 

f_add_current_flag <- function (s_data_type = "normal", s_dim_num = "2d", l_temp_coord = NULL, o_parameter, o_plot = NULL, o_click_ev, i_flag_num, l_click_info = NULL) {
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
	
	if (s_data_type == "normal") {
		if (s_dim_num == "2d") {
			l_add_traces <- list(
				x = rep(o_click_ev[["x"]], 2), y = rep(o_click_ev[["y"]], 2), type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'markers', marker = list(line = list(color = "black", width = 2), color = adjustcolor("black", alpha.f = 0), size = 8), hoverinfo = 'text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>x: ", round(o_click_ev[["x"]], digits = i_dec_num), "<br>y: ", round(o_click_ev[["y"]], digits = i_dec_num)), showlegend = F
			)
		}
		else {
			l_add_traces <- list(
				x = rep(o_click_ev[["x"]], 2), y = rep(o_click_ev[["y"]], 2), z = rep(o_click_ev[["z"]], 2), type = 'scatter3d', mode = 'markers', marker = list(symbol = "circle-open", line = list(width = 2), color = adjustcolor("black", alpha.f = 1), size = 8), hoverinfo = 'text', text = paste0("(flag ", i_flag_num, ")<br>x: ", round(o_click_ev[["x"]], digits = i_dec_num), "<br>y: ", round(o_click_ev[["y"]], digits = i_dec_num), "<br>z: ", round(o_click_ev[["z"]], digits = i_dec_num)), showlegend = F
			)
		}
	}
	else if (s_data_type == "temporal") {
		if (is.null(l_temp_coord)) {
			l_add_traces <- list(
				x = rep(o_click_ev[["x"]], 2), y = rep(o_click_ev[["y"]], 2), name = isolate(o_plot$elt)[o_click_ev[["curveNumber"]] + 1], type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'markers', marker = list(line = list(color = "black", width = 2), color = adjustcolor("black", alpha.f = 0), size = 8), hoverinfo = 'x+text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>", round(o_click_ev[["y"]], digits = i_dec_num)), showlegend = F
			)
		}
		else {
			l_add_traces <- list(
				list(x = l_temp_coord[[1]]$x, y = l_temp_coord[[1]]$y, name = isolate(o_plot$elt)[o_click_ev[["curveNumber"]] + 1], type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'markers', marker = list(line = list(color = "black", width = 2), color = adjustcolor("black", alpha.f = 0), size = 8), hoverinfo = 'x+text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>", round(l_temp_coord[[1]]$y, digits = i_dec_num)), showlegend = F),
				list(x = l_temp_coord[[2]]$x, y = l_temp_coord[[2]]$y, name = isolate(o_plot$elt)[o_click_ev[["curveNumber"]] + 1], type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'lines', line = list(color = "black", dash = "dash"), hoverinfo = 'x+text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>", round(l_temp_coord[[2]]$y, digits = i_dec_num)), showlegend = F)
			)
		}
	}
	else { # ir
		s_id <- l_click_info[[1]]
		i_pos <- l_click_info[[2]]
		df_all <- isolate(o_plot$data)
		s_group <- as.vector(isolate(o_plot$id_group)$no_flag[i_pos, "group"])
		
		if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
			v_row <- isolate(o_plot$pt_pos)
			
			if (length(v_row) == 1) {
				i_rep <- 2
			}
			else {
				i_rep <- 1
			}
			
			l_add_traces <- list(
				list(x = df_all$Frequency, y = df_all[, i_pos + 2], name = s_group, type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'lines', line = list(color = "black", dash = "dash"), hoverinfo = 'text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>id: ", s_id, "<br>frequency: ", round(df_all$Frequency, digits = i_dec_num), "<br>y: ", round(df_all[, i_pos + 2], digits = i_dec_num)), showlegend = F),
				list(x = rep(df_all$Frequency[v_row], i_rep), y = rep(df_all[v_row, i_pos + 2], i_rep), name = s_group, type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'markers', marker = list(line = list(color = "black", width = 2), color = adjustcolor("black", alpha.f = 0), size = 8), hoverinfo = 'text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>id: ", s_id, "<br>frequency: ", round(df_all$Frequency[v_row], digits = i_dec_num), "<br>y: ", round(df_all[v_row, i_pos + 2], digits = i_dec_num)), showlegend = F)
			)
		}
		else {
			l_add_traces <- list(
				x = df_all$Frequency, y = df_all[, i_pos + 2], name = s_group, type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = 'lines', line = list(color = "black", dash = "dash"), hoverinfo = 'text', hoverlabel = list(bgcolor = "black"), text = paste0("(flag ", i_flag_num, ")<br>id: ", s_id, "<br>frequency: ", round(df_all$Frequency, digits = i_dec_num), "<br>y: ", round(df_all[, i_pos + 2], digits = i_dec_num)), showlegend = F
			)
		}
	}
	
	return (l_add_traces)
}

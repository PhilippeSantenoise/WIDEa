#' @importFrom shiny isolate
#' @importFrom grDevices adjustcolor 
NULL

#' Creating a list related to flag data information

#' @description
#' `f_add_current_flag` allows to create a list of data correponding to the current
#' flag. This list is only used to update the graph ("addTraces" with the
#' `plotlyProxyInvoke` function).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_dim_num is the dimension number for the normal data type (2 values:
#' "2d", "3d").
#' @param l_temp_coord is a the list of (X, Y) coordinates used to draw intervals
#' (created from the `f_create_current_flag_data` function) for the temporal data
#' type (default = NULL).
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_plot is a reactive value including main plotly data information.
#' @param o_click_ev are data associated to a click event ("plotly_click" with the
#' `event_data` function).
#' @param i_flag_num is an integer corresponding to the current flag number.
#' @param l_click_info is the list of supplementary informations associated to a
#' click event (created by the `f_create_click_info` function). This input is only 
#' used for the ir data type and allows to identify the spectrum.

#' @encoding UTF-8

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

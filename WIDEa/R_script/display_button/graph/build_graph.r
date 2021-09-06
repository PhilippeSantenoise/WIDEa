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
# Description : function used to build (plotly) graphs for all data types (normal, temporal, IR) 
#               
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# s_source: main panel (= graph area) source
# v_dimension: main panel dimension (width, height)
# (o_click_button, o_plot, o_parameter, o_picture_info): reactive values from the R script "WIDEa_launcher" 
# df_click_legend: legend item informations (name and status)
# l_axis_layout: camera eye position for 3D plot or (X, Y) range for other plot types (used only for 2D plot and histplot)

# Output:
# -------
# return a plotly object

f_build_graph <- function (s_data_type = "normal", s_source = "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout = NULL, o_picture_info) {
	df_all <- isolate(o_plot$data)
	
	if (s_data_type == "normal") {
		if (isolate(o_parameter$plot_type) == "plot") {
			n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.7)
			i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
			
			# (i) 2D/3D (scatter) plot
			# ........................
			
			if (isolate(o_parameter$model) == "none") {
				# (1) model: none
				# ...............
			
				if (isolate(o_parameter$dim_num) == "2d") {
					# (a) dimension number: 2
					# .......................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
				
					v_text <- paste0(isolate(o_parameter$xlab), ": ", round(df_all[, isolate(o_parameter$x)], digits = i_dec_num), "<br>", isolate(o_parameter$ylab), ": ", round(df_all[, isolate(o_parameter$y)], digits = i_dec_num))
					
					if (!is.na(isolate(o_parameter$group))) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$x)], y = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$y)], name = \"", v_group, "\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
					}
					else {
						eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all"), "statut"])))
						ply_1 <- add_trace(p = ply_1, x = df_all[, isolate(o_parameter$x)], y = df_all[, isolate(o_parameter$y)], name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor(isolate(o_plot$color), alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
						ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),"
						xaxis = list(title = isolate(o_parameter$xlab), range = l_axis_layout[[1]]),
						yaxis = list(title = isolate(o_parameter$ylab), range = l_axis_layout[[2]])
					)")))
				}
				else {
					# (b) dimension number: 3
					# .......................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
					
					v_text <- paste0(isolate(o_parameter$xlab), ": ", round(df_all[, isolate(o_parameter$x)], digits = i_dec_num), "<br>", isolate(o_parameter$ylab), ": ", round(df_all[, isolate(o_parameter$y)], digits = i_dec_num), "<br>", isolate(o_parameter$zlab), ": ", round(df_all[, isolate(o_parameter$z)], digits = i_dec_num))								
					
					if (!is.na(isolate(o_parameter$group))) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$x)], y = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$y)], z = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$z)], name = \"", v_group, "\", type = \"scatter3d\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
					}
					else {
						eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all"), "statut"])))
						ply_1 <- add_trace(p = ply_1, x = df_all[, isolate(o_parameter$x)], y = df_all[, isolate(o_parameter$y)], z = df_all[, isolate(o_parameter$z)], name = "all", type = "scatter3d", mode = "markers", marker = list(size = 6, color = adjustcolor(isolate(o_plot$color), alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = 'text', text = v_text, showlegend = T, visible = v_visible)
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1, ", ifelse(!is.na(isolate(o_parameter$title)), "title = isolate(o_parameter$title), ", ""), "scene = list(xaxis = list(title = isolate(o_parameter$xlab)), yaxis = list(title = isolate(o_parameter$ylab)), zaxis = list(title = isolate(o_parameter$zlab))", ifelse(is.null(l_axis_layout), "", ", camera = list(up = l_axis_layout[[1]], center = l_axis_layout[[2]], eye = l_axis_layout[[3]])"), "))")))
				}
			}
			else {
				v_y <- as.vector(df_all[, isolate(o_parameter$y)])
				v_fit <- as.vector(isolate(o_plot$model)[, "fit"])
				
				if (isolate(o_parameter$model) == "calib") {
					# (2) model: calibration
					# ......................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 110, v_dimension[2] - 330)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
					
					if ("variance" %in% names(isolate(o_plot$model))) {
						v_variance <- as.vector(isolate(o_plot$model)[, "variance"])
						s_ylab <- "Standardized residuals"
					}
					else {
						v_variance <- rep(1, length(v_fit))
						s_ylab <- "Residuals"
					}
					
					if (isolate(o_parameter$select_graph) == "QQplot") {
						# qqplot:
						
						v_qres <- quantile((v_y - v_fit) / sqrt(v_variance), p = ppoints(length(v_y)))
						v_qnorm <- qnorm(p = ppoints(length(v_y)), mean = 0, sd = 1)
						v_text <- paste0("qnorm: ", round(v_qnorm, digits = i_dec_num), "<br>qres: ", round(v_qres, digits = i_dec_num))
						ply_1 <- add_trace(p = ply_1, x = v_qnorm, y = v_qres, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverinfo = 'text+name', text = v_text, showlegend = T)
						
						# add graph layout:
						
						eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
							ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),"
							shapes = list(f_abline(xmin = -4, xmax = 4, a = 0, b = 1, dash = \"dash\")),
							xaxis = list(title = \"Theoretical quantiles (standard normal)\", zeroline = F, range = l_axis_layout[[1]]),
							yaxis = list(title = \"Sample quantiles (standardized residuals)\", zeroline = F, range = l_axis_layout[[2]])
						)")))
					}
					else {
						v_res <- (v_y - v_fit) / sqrt(v_variance)
						
						if (length(grep("fitted", isolate(o_parameter$select_graph))) > 0) {
							# (standardized) residuals vs fitted values:
							
							v_text <- paste0("fit: ", round(v_fit, digits = i_dec_num), "<br>res: ", round(v_res, digits = i_dec_num))
							
							if (!is.na(isolate(o_parameter$group))) {
								v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
								v_group <- v_group[order(v_group)]
								v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
								eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = round(v_fit[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], digits = i_dec_num), y = round(v_res[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], digits = i_dec_num), name = \"", v_group, "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
							}
							else {
								ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_res, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor(isolate(o_plot$color), alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = 'text+name', text = v_text, showlegend = T)
							}
							
							# add graph layout:
							
							eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
								ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""), "
								shapes = list(f_hline(y = 0, dash = \"dash\")", ifelse("variance" %in% names(isolate(o_plot$model)), ", f_hline(y = -2, dash = \"dash\"), f_hline(y = 2, dash = \"dash\")),", "),"), "
								xaxis = list(title = \"Fitted values\", range = l_axis_layout[[1]]),
								yaxis = list(title = s_ylab, zeroline = F, range = l_axis_layout[[2]])
							)")))
						}
						else {
							# (standardized) residuals vs independent variables:
							
							i_num <- as.numeric(substr(isolate(o_parameter$select_graph), nchar(isolate(o_parameter$select_graph)), nchar(isolate(o_parameter$select_graph))))
							v_x <- as.vector(df_all[, isolate(o_parameter$x)[i_num]])
							v_text <- paste0("x", i_num, ": ", round(v_x, digits = i_dec_num), "<br>res: ", round(v_res, digits = i_dec_num))
							
							if (!is.na(isolate(o_parameter$group))) {
								v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
								v_group <- v_group[order(v_group)]
								v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
								eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_x[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], y = v_res[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], name = \"", v_group, "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
							}
							else {
								ply_1 <- add_trace(p = ply_1, x = v_x, y = v_res, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor(isolate(o_plot$color), alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = 'text+name', text = v_text, showlegend = T)
							}
							
							# add graph layout:
							
							eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
								ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""), "
								shapes = list(f_hline(y = 0, dash = \"dash\")", ifelse("variance" %in% names(isolate(o_plot$model)), ", f_hline(y = -2, dash = \"dash\"), f_hline(y = 2, dash = \"dash\")),", "),"), "
								xaxis = list(title = \"x", i_num, "\", range = l_axis_layout[[1]]),
								yaxis = list(title = s_ylab, zeroline = F, range = l_axis_layout[[2]])
							)")))
						}
					}
				}
				else {
					# (3) model: validation
					# .....................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
					
					v_text <- paste0("fit: ", round(v_fit, digits = i_dec_num), "<br>", isolate(o_parameter$ylab), ": ", round(v_y, digits = i_dec_num))
					
					# observed values vs fitted values:
					
					if (!is.na(isolate(o_parameter$group))) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_fit[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], y = v_y[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], name = \"", v_group, "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group, "\")], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
					}
					else {
						ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_y, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor(isolate(o_plot$color), alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), hoverinfo = 'text+name', text = v_text, showlegend = T)
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
						ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),"
						shapes = list(f_abline(xmin = min(v_fit), xmax = max(v_fit), a = 0, b = 1, dash = \"dash\")),
						xaxis = list(title = \"Fitted values\", range = l_axis_layout[[1]]),
						yaxis = list(title = \"Observed values\", range = l_axis_layout[[2]])
					)")))
				}
			}
		}
		else if (isolate(o_parameter$plot_type) == "boxplot") {
			# (ii) boxplot
			# ............
			
			n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.7)
			i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
			
			if (!is.na(isolate(o_parameter$group))) {
				v_group_1 <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
				v_group_1 <- v_group_1[order(v_group_1)]
				v_group_2 <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
				v_group_2 <- v_group_2[order(v_group_2)]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group_2), "statut"]
				eval(parse(text = paste0("df_median <- aggregate(", isolate(o_parameter$y), " ~ ", isolate(o_parameter$x), ", data = df_all, median)")))
				df_median <- df_median[order(df_median[, isolate(o_parameter$x)]),]
				
				for (i in 1:length(v_group_1)) {
					eval(parse(text = paste0("ply_1_", i, " <- plot_ly(source = s_source, type = \"box\", width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c(\"sendDataToCloud\", \"zoom2d\", \"autoScale2d\", \"resetScale2d\", \"pan2d\", \"select2d\", \"lasso2d\", \"zoomIn2d\", \"zoomOut2d\", \"toggleSpikelines\", \"hoverClosestCartesian\", \"hoverCompareCartesian\"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))")))
					df_all_i <- df_all[df_all[, isolate(o_parameter$group)] == v_group_1[i],]
					df_num <- as.data.frame(addmargins(table(as.vector(df_all_i[, isolate(o_parameter$x)]))))
					df_num <- df_num[-dim(df_num)[1],]
					v_pos <- which(!v_group_2 %in% as.vector(df_num[, 1]))
					eval(parse(text = paste0("l_y <- list(", paste(rep("c()", length(v_group_2)), collapse = ", "), ")")))
					eval(parse(text = paste0("l_outliers <- list(", paste(rep("c()", length(v_group_2)), collapse = ", "), ")")))
					
					if (length(v_pos) > 0) {
						eval(parse(text = paste(paste0("l_y[[", c(1:length(v_group_2))[-v_pos], "]] <- as.vector(df_all_i[df_all_i[, isolate(o_parameter$x)] == \"", v_group_2[-v_pos], "\", isolate(o_parameter$y)])"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- as.vector(quantile(l_y[[", c(1:length(v_group_2))[-v_pos], "]], probs = c(0.25, 0.75), type = 5))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- c(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][1] - 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]])), l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][2] + 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]])))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- l_y[[", c(1:length(v_group_2))[-v_pos], "]][which(l_y[[", c(1:length(v_group_2))[-v_pos], "]] < l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][1] | l_y[[", c(1:length(v_group_2))[-v_pos], "]] > l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][2])]"), collapse = "; "))) 
					}
					else {
						eval(parse(text = paste(paste0("l_y[[", c(1:length(v_group_2)), "]] <- as.vector(df_all_i[df_all_i[, isolate(o_parameter$x)] == \"", v_group_2, "\", isolate(o_parameter$y)])"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- as.vector(quantile(l_y[[", c(1:length(v_group_2)), "]], probs = c(0.25, 0.75), type = 5))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- c(l_outliers[[", c(1:length(v_group_2)), "]][1] - 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2)), "]])), l_outliers[[", c(1:length(v_group_2)), "]][2] + 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2)), "]])))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- l_y[[", c(1:length(v_group_2)), "]][which(l_y[[", c(1:length(v_group_2)), "]] < l_outliers[[", c(1:length(v_group_2)), "]][1] | l_y[[", c(1:length(v_group_2)), "]] > l_outliers[[", c(1:length(v_group_2)), "]][2])]"), collapse = "; ")))
					}
					
					v_command <- as.vector(unlist(lapply(1:length(v_group_2), function(x) {
						if (length(v_pos) > 0) {
							if (x %in% v_pos) {
								v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = ", as.vector(df_median[, 2])[x], ", name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = n_op)), showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
								v_out <- c(v_out, paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = ", as.vector(df_median[, 2])[x], ", name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"white\", alpha.f = 1)), line = list(width = 3), showlegend = F, visible = ", v_visible[x], ")"))
							}
							else {
								v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = l_y[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = n_op)), marker = list(color = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0.7)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
								
								if (length(l_outliers[[x]]) > 0) {
									v_out <- paste0(v_out, paste0(" %>% add_markers(x = rep(\"", v_group_2[x], "\", length(l_outliers[[", x, "]])), y = l_outliers[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", type = \"scattergl\", marker = list(color = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", x, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[x], ")"))
								}
							}
						}
						else {
							v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = l_y[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = n_op)), marker = list(color = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0.7)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
							
							if (length(l_outliers[[x]]) > 0) {
								v_out <- paste0(v_out, paste0(" %>% add_markers(x = rep(\"", v_group_2[x], "\", length(l_outliers[[", x, "]])), y = l_outliers[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", type = \"scattergl\", marker = list(color = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color)[x], "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", x, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[x], ")"))
							}
						}
						
						return(v_out)
					})))
					
					# add graph layout per group:
					
					v_command <- c(v_command, paste0("ply_1_", i, " <- layout(p = ply_1_", i, ", margin = list(b = 0), xaxis = list(showticklabels = F, type = \"category\", title = \"", v_group_1[i], "\", fixedrange = T), yaxis = list(title = isolate(o_parameter$ylab), zeroline = F, fixedrange = T))"))
					eval(parse(text = paste(v_command, collapse = "; ")))
				}
				
				eval(parse(text = paste0("ply_1 <- subplot(list(", paste(paste0("ply_1_", 1:length(v_group_1)), collapse = ", "), "), shareY = T, shareX = F, titleX = T, titleY = T, margin = 0.01)")))
				
				# add global graph layout:
				
				eval(parse(text = ifelse(!is.na(isolate(o_parameter$title)), "ply_1 <- layout(p = ply_1, title = isolate(o_parameter$title))", "")))
			}
			else {
				ply_1 <- plot_ly(source = s_source, type = "box", width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
				v_group <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
				v_group <- v_group[order(v_group)]
				v_color <- hue_pal()(length(v_group))
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
				
				eval(parse(text = paste0("l_y <- list(", paste(paste0("as.vector(df_all[df_all[, isolate(o_parameter$x)] == \"", v_group, "\", isolate(o_parameter$y)])"), collapse = ", "), ")")))
				v_command_1 <- paste0("ply_1 <- add_boxplot(p = ply_1, y = l_y[[", 1:length(v_group), "]], name = \"", v_group, "\", legendgroup = \"", v_group, "\", color = I(adjustcolor(\"", v_color, "\", alpha.f = n_op)), marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 0.7)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = T, visible = ", v_visible, ")")
				eval(parse(text = paste0("l_outliers <- list(", paste(paste0("as.vector(quantile(l_y[[", 1:length(v_group), "]], probs = c(0.25, 0.75), type = 5))"), collapse = ", "), ")")))
				eval(parse(text = paste(paste0("l_outliers[[", 1:length(v_group), "]] <- c(l_outliers[[", 1:length(v_group), "]][1] - 1.5 * abs(diff(l_outliers[[", 1:length(v_group), "]])), l_outliers[[", 1:length(v_group), "]][2] + 1.5 * abs(diff(l_outliers[[", 1:length(v_group), "]])))"), collapse = "; ")))
				eval(parse(text = paste(paste0("l_outliers[[", 1:length(v_group), "]] <- l_y[[", 1:length(v_group), "]][which(l_y[[", 1:length(v_group), "]] < l_outliers[[", 1:length(v_group), "]][1] | l_y[[", 1:length(v_group), "]] > l_outliers[[", 1:length(v_group), "]][2])]"), collapse = "; ")))
				v_command_2 <- rep("", length(v_group))
				v_pos <- which(lengths(l_outliers) > 0)
				v_command_2[v_pos] <- paste0(" %>% add_markers(x = rep(\"", v_group[v_pos], "\", length(l_outliers[[", v_pos, "]])), y = l_outliers[[", v_pos, "]], name = \"", v_group[v_pos], "\", legendgroup = \"", v_group[v_pos], "\", type = \"scattergl\", marker = list(color = adjustcolor(\"", v_color[v_pos], "\", alpha.f = n_op)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[v_pos], "\", alpha.f = 0.7)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", v_pos, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[v_pos], ")")
				eval(parse(text = paste(paste0(v_command_1, v_command_2), collapse = "; ")))
				
				# add graph layout:
				
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(!is.na(isolate(o_parameter$title)), "title = isolate(o_parameter$title),", ""),
					"xaxis = list(type = \"category\", fixedrange = T), 
					yaxis = list(title = isolate(o_parameter$ylab), zeroline = F, fixedrange = T)
				)")))
			}
		}
		else if (isolate(o_parameter$plot_type) == "histplot") {
			# (iv) histplot
			# ..............
			
			n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.5)
			s_bw <- ifelse(isolate(o_parameter$autobin) == F & !is.na(isolate(o_parameter$bw)), paste0(", xbins = list(size = ", isolate(o_parameter$bw), ")"), "")
			
			ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
			
			if (!is.na(isolate(o_parameter$group))) {
				v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
				v_group <- v_group[order(v_group)]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"] 
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[df_all[, isolate(o_parameter$group)] == \"", v_group, "\", isolate(o_parameter$x)], name = \"", v_group, "\", type = \"histogram\", histnorm = \"probability density\", autobinx = isolate(o_parameter$autobin)", s_bw, ", marker = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op), line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op + 0.3), width = 2)), hoverlabel = list(bgcolor = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = 0.7)), showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
				
				# add graph layout:
			
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,
					barmode = \"overlay\",",
					ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),
					"xaxis = list(title = isolate(o_parameter$xlab), range = l_axis_layout[[1]]),
					yaxis = list(title = \"Density\", range = l_axis_layout[[2]])
				)")))
			}
			else {
				v_visible <- df_click_legend[which(df_click_legend$name == "all"), "statut"]
				eval(parse(text = paste0("ply_1 <- add_trace(p = ply_1, x = df_all[, isolate(o_parameter$x)], name = \"all\", type = \"histogram\", histnorm = \"probability density\", autobinx = isolate(o_parameter$autobin)", s_bw, ", marker = list(color = adjustcolor(isolate(o_plot$color), alpha.f = n_op), line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = n_op + 0.3), width = 2)), hoverlabel = list(bgcolor = adjustcolor(isolate(o_plot$color), alpha.f = 0.7)), showlegend = T, visible = ", v_visible, ")")))
				
				# add graph layout:
			
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),
					"xaxis = list(title = isolate(o_parameter$xlab), range = l_axis_layout[[1]]),
					yaxis = list(title = \"Density\", range = l_axis_layout[[2]])
				)")))
			}
		}
		else if (isolate(o_parameter$plot_type) == "barplot") {
			# (v) barplot
			# ............
			
			n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.5)
			
			if (!is.na(isolate(o_parameter$group))) {
				df_all$.num. <- 1
				v_group_1 <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
				v_group_1 <- v_group_1[order(v_group_1)]
				v_group_2 <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
				v_group_2 <- v_group_2[order(v_group_2)]
				df_num_all <- expand.grid(v_group_1, v_group_2)
				names(df_num_all) <- c(isolate(o_parameter$group), isolate(o_parameter$x))
				df_num_all$.num. <- 0
				df_num_all$.comb. <- paste(df_num_all[, isolate(o_parameter$group)], df_num_all[, isolate(o_parameter$x)], sep = "_")
				eval(parse(text = paste0("df_num <- aggregate(.num. ~ ", isolate(o_parameter$x), "/", isolate(o_parameter$group), ", data = df_all, sum)")))
				df_num$.comb. <- paste(df_num[, isolate(o_parameter$group)], df_num[, isolate(o_parameter$x)], sep = "_")
				
				if (dim(df_num_all)[1] > dim(df_num)[1]) {
					df_num <- rbind(df_num, df_num_all[which(!df_num_all[, ".comb."] %in% as.vector(df_num[, ".comb."])), c(isolate(o_parameter$x), isolate(o_parameter$group), ".num.", ".comb.")])
				}
				
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group_2), "statut"]
				
				for (i in 1:length(v_group_1)) {
					eval(parse(text = paste0("ply_1_", i, " <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c(\"sendDataToCloud\", \"zoom2d\", \"autoScale2d\", \"resetScale2d\", \"pan2d\", \"select2d\", \"lasso2d\", \"zoomIn2d\", \"zoomOut2d\", \"toggleSpikelines\", \"hoverClosestCartesian\", \"hoverCompareCartesian\"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))")))
					df_num_i <- df_num[df_num[, isolate(o_parameter$group)] == v_group_1[i],]
					eval(parse(text = paste(paste0("ply_1_", i, " <- add_trace(p = ply_1_", i, ", y = df_num_i[df_num_i[, isolate(o_parameter$x)] == \"", v_group_2, "\", \".num.\"], name = \"", v_group_2, "\", legendgroup = \"", v_group_2, "\", type = \"bar\", marker = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op), line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op + 0.3), width = 2)), hoverinfo = \"name+text\", text = df_num_i[df_num_i[, isolate(o_parameter$x)] == \"", v_group_2, "\", \".num.\"], textposition = \"auto\", showlegend = ", ifelse(i == 1, T, F), ", visible = ", v_visible, ")"), collapse = "; ")))
					
					# add graph layout per group:
					
					eval(parse(text = paste0("ply_1_", i, " <- layout(p = ply_1_", i, ", xaxis = list(showticklabels = F, title = \"", v_group_1[i], "\", fixedrange = T), yaxis = list(title = \"Count\", fixedrange = T))")))
				}
				
				eval(parse(text = paste0("ply_1 <- subplot(list(", paste(paste0("ply_1_", 1:length(v_group_1)), collapse = ", "), "), shareY = T, shareX = F, titleX = T, titleY = T, margin = 0.01)")))
			
				# add global graph layout:
				
				eval(parse(text = ifelse(!is.na(isolate(o_parameter$title)), "ply_1 <- layout(p = ply_1, title = isolate(o_parameter$title))", "")))
			}
			else {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
				v_x <- as.vector(df_all[, isolate(o_parameter$x)])
				v_group <- unique(v_x)
				v_group <- v_group[order(v_group)]
				df_num <- as.data.frame(addmargins(table(v_x)))
				names(df_num) <- c(isolate(o_parameter$x), ".num.")
				df_num <- df_num[-dim(df_num)[1],]
				v_color <- hue_pal()(length(v_group))
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, y = df_num[df_num[, isolate(o_parameter$x)] == \"", v_group, "\", \".num.\"], name = \"", v_group, "\", type = \"bar\", marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op), line = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op + 0.3), width = 2)), hoverinfo = \"name+text\", text = df_num[df_num[, isolate(o_parameter$x)] == \"", v_group, "\", \".num.\"], textposition = \"auto\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
				
				# add graph layout:
			
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),
					"xaxis = list(showticklabels = F, fixedrange = T),
					yaxis = list(title = \"Count\", fixedrange = T)
				)")))
			}
		}
		else {
			# (vi) corplot
			# ............
			
			if (!is.na(isolate(o_parameter$group))) {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 95, v_dimension[2] - 315)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
				df_all <-  df_all[which(df_all[, isolate(o_parameter$group)] == isolate(o_parameter$select_graph)),]
			}
			else {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 40, v_dimension[2] - 260)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
			}
			
			m_cor <- cor(df_all[, isolate(o_parameter$y)], use = "pairwise.complete.obs")
			m_cor[upper.tri(m_cor)] <- NA
			m_cor <- m_cor[nrow(m_cor):1,]
			
			l_text <- lapply(rev(isolate(o_parameter$y)[-1]), function(x) {
				v_del <- which(isolate(o_parameter$y) == x):length(isolate(o_parameter$y)) 
				v_colname <- isolate(o_parameter$y)[-v_del]
				v_cor <- round(as.vector(t(m_cor[x, v_colname])), digits = 2)
				v_cor <- ifelse(is.na(v_cor), "", paste0("<br>cor: ", v_cor))
				v_info <- paste0("x: ", v_colname, "<br>y: ", x, v_cor, "<br>")
				v_size <- eval(parse(text = paste0("c(", paste(paste0("nrow(df_all[!is.na(df_all$", x, ") & !is.na(df_all$", v_colname, "),])"), collapse = ", "), ")")))
				v_info <- c(paste0(v_info, "size: ", v_size), rep("", length(v_del)))
				return(eval(parse(text = paste0("list(", paste(paste0("\"", v_info, "\""), collapse = ", "), ")"))))
			})
			
			df_cor <- as.data.frame(combinations(x = isolate(o_parameter$y), k = 2, replace = F))
			names(df_cor) <- c("x", "y")
			df_cor$val <- eval(parse(text = paste0("c(", paste(paste0("round(cor(df_all[, \"", df_cor[, 1], "\"], df_all[, \"", df_cor[, 2], "\"], use = \"pairwise.complete.obs\"), digits = 2)"), collapse = ", "), ")")))
			df_cor <- df_cor[!is.na(df_cor$val),]
			
			ply_1 <- add_trace(p = ply_1, x = colnames(m_cor), y = rownames(m_cor), z = m_cor, zmin = -1, zmax = 1, colors = colorRamp(c("blue", "grey", "red")), xgap = 2, ygap = 2, type = "heatmap", hoverinfo = "text", text = l_text)
			ply_1 <- add_annotations(p = ply_1, x = df_cor$x, y = df_cor$y, text = df_cor$val, font = list(color = "white"), showarrow = F)
			
			# add graph layout:
			
			eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
				ifelse(!is.na(isolate(o_parameter$title)), "title =  isolate(o_parameter$title),", ""),
				"xaxis = list(tickangle = -45, showgrid = F, fixedrange = T), 
				yaxis = list(showgrid = F, fixedrange = T)
			)")))
		}
	}
	else if (s_data_type == "temporal") {
		n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.7)
		i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
		v_visible <- df_click_legend[which(df_click_legend$name %in% isolate(o_parameter$y)), "statut"] 
		
		ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 20, v_dimension[2] - 240)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
		
		if (isolate(o_parameter$mode) == "marker") {
			v_command <- paste0("df_all[!is.na(df_all$", isolate(o_parameter$y), "),]")
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), name = \"", isolate(o_parameter$y), "\", hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
		}
		else {
			v_command <- "df_all"
			
			if (isolate(o_parameter$mode) == "line") {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), name = \"", isolate(o_parameter$y), "\", hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
			}
			else {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$color), "\", alpha.f = n_op)), name = \"", isolate(o_parameter$y), "\", hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
			}
		}
		
		if (isolate(o_plot$add_pt)) {
			l_pt_pos <- isolate(o_plot$pt_pos)
			v_visible <- df_click_legend[which(df_click_legend$name %in% isolate(o_plot$var_pt)), "statut"]
			
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"",isolate(o_parameter$x), "\"], y = df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"", isolate(o_plot$var_pt), "\"], type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", isolate(o_plot$var_pt_color), "\", alpha.f = n_op)), name = \"", isolate(o_plot$var_pt), "\", hoverinfo = 'x+text+name', text = round(df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"", isolate(o_plot$var_pt), "\"], digits = i_dec_num), legendgroup = \"", isolate(o_plot$var_pt), "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
		}
		
		# add graph layout:
		
		ply_1 <- layout(p = ply_1,
			title = ifelse(!is.na(isolate(o_parameter$title)), isolate(o_parameter$title), ""), 
			xaxis = list(
				title = "",
				rangeselector = list(
					buttons = list(
						list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
						list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
						list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
						list(step = "all")
					)
				),
				rangeslider = list(type = "date", thickness = 0.1, borderwidth = 1),
				type = "date",
				range = l_axis_layout[[1]]
			),
			yaxis = list(title = isolate(o_parameter$ylab), range = l_axis_layout[[2]])
		)
	}
	else { # ir
		n_op <- ifelse(isolate(o_parameter$autoop) == F & !is.na(isolate(o_parameter$op)), isolate(o_parameter$op), 0.7)
		i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
		
		l_id_group <- isolate(o_plot$id_group)
		eval(parse(text = paste0("v_visible <- c(", paste(paste0("df_click_legend[which(df_click_legend$name == \"", as.vector(l_id_group$no_flag$group), "\"), \"statut\"]"), collapse = ", "), ")")))
		
		v_name <- names(df_all)[-c(1, 2)]
		v_id <- as.vector(l_id_group$no_flag$id)
		v_group <- as.vector(l_id_group$no_flag$group)
		v_show <- as.vector(l_id_group$no_flag$show)
		v_color <- as.vector(l_id_group$no_flag$color)
		
		l_text <- lapply(1:length(v_name), function(i) {
			v_out <- paste0("id: ", v_id[i], "<br>frequency : ", round(as.vector(df_all$Frequency), digits = i_dec_num), "<br>y: ", round(as.vector(df_all[, v_name[i]]), digits = i_dec_num))
			return(v_out)
		})
		
		ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 45, v_dimension[2] - 265)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)))
		
		if (isolate(o_parameter$mode) == "marker") {
			v_command <- paste0("df_all[!is.na(df_all$", v_name, "),]")
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = 6, color = adjustcolor(\"", v_color, "\", alpha.f = n_op)), name = \"" , v_group, "\", hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]][which(!is.na(df_all$", v_name, "))], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
		}
		else {
			v_command <- "df_all"
			
			if (isolate(o_parameter$mode) == "line") {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op)), name = \"" , v_group, "\", hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
			}
			else {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op)), marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op), size = 6), name = \"" , v_group, "\", hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
			}
		}
		
		if (isolate(o_plot$add_pt)) {
			l_text <- lapply(1:length(v_name), function(i) {
				v_out <- paste0("id: ", v_id[i], "<br>frequency : ", round(as.vector(df_all[isolate(o_plot$pt_pos), "Frequency"]), digits = i_dec_num), "<br>y: ", round(as.vector(df_all[isolate(o_plot$pt_pos), v_name[i]]), digits = i_dec_num))
				return(v_out)
			})
			
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_all[isolate(o_plot$pt_pos), \"Frequency\"]), y = as.vector(df_all[isolate(o_plot$pt_pos), \"", v_name, "\"]), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = n_op), size = 6), name = \"", v_group, "\", hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"", v_group, "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
		}
		
		# add graph layout & annotations:
		
		v_freq_range <- range(as.vector(df_all$Frequency))
		b_cond <- 4000 >= v_freq_range[1] & 4000 <= v_freq_range[2]
		
		eval(parse(text = paste0("ply_1 <- layout(p = ply_1,", 
			ifelse(!is.na(isolate(o_parameter$title)), "title = isolate(o_parameter$title),", ""), " 
			xaxis = list(
				title = \"Frequency (cm-1)\",
				rangeslider = list(thickness = 0.1, borderwidth = 1),
				range = l_axis_layout[[1]]
			),
			yaxis = list(title = isolate(o_parameter$ylab), range = l_axis_layout[[2]])",
			ifelse(b_cond, ", shapes = list(f_vline(x = 4000, dash = \"dash\"))", ""),
		")")))
		
		if (b_cond) {
			ply_1 <- add_annotations(ply_1, x = 4000, y = 1.05, xref = "x", yref = "paper", text = "NIR-MIR limit", showarrow = F)
		}
		
		#title = TeX(\"Frequency\\text{ (cm}^{-1}\\text{)}\")
	}
	
	return (ply_1)
}

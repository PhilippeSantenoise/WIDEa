#' @importFrom shiny isolate
#' @importFrom plotly plot_ly config add_trace add_boxplot add_markers add_annotations subplot layout colorbar
#' @importFrom grDevices adjustcolor colorRamp rgb 
#' @importFrom stats quantile ppoints qnorm aggregate median addmargins cor
NULL

#' Building the main plotly graph

#' @description
#' `f_build_graph` returns the main plotly graph for all data types.

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_source is the main panel (graph area) source. 
#' @param v_dimension is the vector of the main panel dimension (width, height).
#' @param o_click_button is a reactive value including binary values related to
#' several buttons: clicked (1) or not (0).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_picture_info is a reactive value including data information on the graph
#' picture.
#' @param o_label_text is a reactive value including data information on the graph
#' label edition.
#' @param df_click_legend are legend item data information (name and status).
#' @param l_axis_layout is the list including camera eye position for 3D plot or (X,
#' Y) range for other plot types (used only for 2D plot and histplot).
#' @param l_graph_opt is the list of graph option (color, opacity, point type/size,
#' sorting) created by the `f_create_graph_opt_vector` function. The background/grid
#' color is also added to the list (`l_graph_opt$bg_grid_color`).

#' @encoding UTF-8

f_build_graph <- function (s_data_type = "normal", s_source = "graphic", v_dimension, o_click_button, df_click_legend, o_plot, o_parameter, l_axis_layout = NULL, o_picture_info, o_label_text, l_graph_opt = NULL) {
	df_all <- isolate(o_plot$data)
	s_title <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "title")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "title")], ""), "") 
	
	if (length(which(o_parameter$dim_num == "3d")) > 0) {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
	}
	else {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open", "star", "star-open", "hourglass-open", "bowtie-open", "circle-cross-open", "circle-x-open", "square-cross-open", "square-x-open", "diamond-cross-open", "diamond-x-open", "cross-thin-open", "x-thin-open", "asterisk-open", "hash-open")
	}
	
	v_min_max <- c()
	
	if (s_data_type == "normal") {
		if (isolate(o_parameter$plot_type) == "plot") {
			i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
			
			# quantitative Group variable: point size
			
			if (isolate(o_parameter$quant_group) & !(isolate(o_parameter$model) == "calib" & length(which(isolate(o_parameter$select_graph) == "QQplot")) > 0)) {
				v_size <- l_graph_opt$pal_point$size
				
				if (abs(l_graph_opt$pal_point$size_coef) > 1) { # not constant size
					v_interval <- c(1, abs(l_graph_opt$pal_point$size_coef))
					if (l_graph_opt$pal_point$size_coef < 0) {v_interval <- rev(v_interval)} # descending size
					v_size <- v_size * scales::rescale(df_all[, isolate(o_parameter$group)], to = v_interval)
				}
			}

			# (i) 2D/3D (scatter) plot
			# ........................
			
			df_fun <- isolate(o_plot$fun)
			s_var_y <- ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y))
			
			if (isolate(o_parameter$model) == "none") {
				# (1) model: none
				# ...............
				
				s_xlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")], isolate(o_parameter$xlab)), isolate(o_parameter$xlab))
				s_ylab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")], isolate(o_parameter$ylab)), isolate(o_parameter$ylab))
				s_var_x <- ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x))
				v_round_x <- round(df_all[, s_var_x], digits = i_dec_num)
				v_round_y <- round(df_all[, s_var_y], digits = i_dec_num)
				
				if (isolate(o_parameter$dim_num) == "2d") {
					# (a) dimension number: 2
					# .......................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
					
					v_text <- paste0(isolate(o_parameter$xlab), ": ", v_round_x, "<br>", isolate(o_parameter$ylab), ": ", v_round_y)
					
					if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
						v_order <- order(l_graph_opt$sorting)
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\"), \"", s_var_x, "\"], y = df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\"), \"", s_var_y, "\"], name = \"", v_group[v_order], "\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_order], ", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
					}
					else {
						v_visible <- eval(parse(text = df_click_legend[which(df_click_legend$name == "all"), "statut"]))
						v_x <- as.vector(df_all[, s_var_x])
						v_y <- as.vector(df_all[, s_var_y])
						
						if (!isolate(o_parameter$quant_group)) {
							ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = l_graph_opt$point_size, color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), symbol = v_symbol[l_graph_opt$point_type], line = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
						}
						else { # quantitative Group variable
							v_group <- as.vector(df_all[, isolate(o_parameter$group)])
							
							if (isolate(o_parameter$webgl) == "yes") { # type = "scattergl"
								m_rgb <- colorRamp(l_graph_opt$pal_col_op$color)(scales::rescale(v_group))
								v_bgcolor <- rgb(m_rgb[, 1], m_rgb[, 2], m_rgb[, 3], maxColorValue = 255) 
								ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, color = v_group, name = "all", legendgroup = "all", type = "scattergl", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, hoverlabel = list(bgcolor = v_bgcolor), colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
							}
							else { # type = "scatter"
								ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, color = v_group, name = "all", legendgroup = "all", type = "scatter", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
							}
							
							if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(v_x), range(v_y))}
						}
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
						ifelse(s_title != "", "title =  s_title,", ""),"
						plot_bgcolor = l_graph_opt$bg_grid_color[1],
						xaxis = list(title = s_xlab, range = l_axis_layout[[1]], gridcolor = l_graph_opt$bg_grid_color[2]),
						yaxis = list(title = s_ylab, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
					)")))
				}
				else {
					# (b) dimension number: 3
					# .......................
					
					s_zlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "z")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "z")], isolate(o_parameter$zlab)), isolate(o_parameter$zlab))
					s_var_z <- ifelse(!is.na(isolate(o_parameter$h)), ".h.", isolate(o_parameter$z))
					v_round_z <- round(df_all[, s_var_z], digits = i_dec_num)
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
					
					v_text <- paste0(isolate(o_parameter$xlab), ": ", v_round_x, "<br>", isolate(o_parameter$ylab), ": ", v_round_y, "<br>", isolate(o_parameter$zlab), ": ", v_round_z)								
					
					if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
						v_order <- order(l_graph_opt$sorting)
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\"), \"", s_var_x, "\"], y = df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\"), \"", s_var_y, "\"], z = df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\"), \"", s_var_z, "\"], name = \"", v_group[v_order], "\", type = \"scatter3d\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_order], ", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
					}
					else {
						v_visible <- eval(parse(text = df_click_legend[which(df_click_legend$name == "all"), "statut"]))
						v_x <- as.vector(df_all[, s_var_x])
						v_y <- as.vector(df_all[, s_var_y])
						v_z <- as.vector(df_all[, s_var_z])
						
						if (!isolate(o_parameter$quant_group)) {
							ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, z = v_z, name = "all", type = "scatter3d", mode = "markers", marker = list(size = l_graph_opt$point_size, color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), symbol = v_symbol[l_graph_opt$point_type], line = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
						}
						else { # quantitative Group variable
							v_group <- as.vector(df_all[, isolate(o_parameter$group)])
							ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, z = v_z, color = v_group, name = "all", legendgroup = "all", type = "scatter3d", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T, visible = v_visible)
							if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(v_x), range(v_y), range(v_z))}
						}
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1, ", ifelse(s_title != "", "title = s_title, ", ""), "scene = list(xaxis = list(title = s_xlab, backgroundcolor = l_graph_opt$bg_grid_color[1], showbackground = T, gridcolor = l_graph_opt$bg_grid_color[2]), yaxis = list(title = s_ylab, backgroundcolor = l_graph_opt$bg_grid_color[1], showbackground = T, gridcolor = l_graph_opt$bg_grid_color[2]), zaxis = list(title = s_zlab, backgroundcolor = l_graph_opt$bg_grid_color[1], showbackground = T, gridcolor = l_graph_opt$bg_grid_color[2])", ifelse(is.null(l_axis_layout), "", ", camera = list(up = l_axis_layout[[1]], center = l_axis_layout[[2]], eye = l_axis_layout[[3]])"), "))")))
				}
			}
			else {
				v_y <- as.vector(df_all[, s_var_y])
				v_fit <- as.vector(isolate(o_plot$model)[, "fit"])
				
				if (isolate(o_parameter$model) == "calib") {
					# (2) model: calibration
					# ......................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 110, v_dimension[2] - 330)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
					
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
						ply_1 <- add_trace(p = ply_1, x = v_qnorm, y = v_qres, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 1)), hoverlabel = list(bgcolor = adjustcolor("dodgerblue2", alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T)
						
						# add graph layout:
						
						eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
							ifelse(s_title != "", "title =  s_title,", ""),"
							shapes = list(f_abline(xmin = -4, xmax = 4, a = 0, b = 1, dash = \"dash\")),
							plot_bgcolor = l_graph_opt$bg_grid_color[1],
							xaxis = list(title = \"Theoretical quantiles (standard normal)\", zeroline = F, range = l_axis_layout[[1]], gridcolor = l_graph_opt$bg_grid_color[2]),
							yaxis = list(title = \"Sample quantiles (standardized residuals)\", zeroline = F, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
						)")))
					}
					else {
						v_res <- (v_y - v_fit) / sqrt(v_variance)
						
						if (length(grep("fitted", isolate(o_parameter$select_graph))) > 0) {
							# (standardized) residuals vs fitted values:
							
							v_text <- paste0("fit: ", round(v_fit, digits = i_dec_num), "<br>res: ", round(v_res, digits = i_dec_num))
							
							if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
								v_order <- order(l_graph_opt$sorting)
								v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
								v_group <- v_group[order(v_group)]
								v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
								eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = round(v_fit[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], digits = i_dec_num), y = round(v_res[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], digits = i_dec_num), name = \"", v_group[v_order], "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_order], ", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
							}
							else {
								if (!isolate(o_parameter$quant_group)) {
									ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_res, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = l_graph_opt$point_size, color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), symbol = v_symbol[l_graph_opt$point_type], line = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T)
								}
								else { # quantitative Group variable
									v_group <- as.vector(df_all[, isolate(o_parameter$group)])
									
									if (isolate(o_parameter$webgl) == "yes") { # type = "scattergl"
										m_rgb <- colorRamp(l_graph_opt$pal_col_op$color)(scales::rescale(v_group))
										v_bgcolor <- rgb(m_rgb[, 1], m_rgb[, 2], m_rgb[, 3], maxColorValue = 255) 
										ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_res, color = v_group, name = "all", legendgroup = "all", type = "scattergl", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, hoverlabel = list(bgcolor = v_bgcolor), colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
									}
									else { # type = "scatter"
										ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_res, color = v_group, name = "all", legendgroup = "all", type = "scatter", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
									}
									
									if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(v_fit), range(v_res))}
								}
							}
							
							# add graph layout:
							
							eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
								ifelse(s_title != "", "title =  s_title,", ""), "
								shapes = list(f_hline(y = 0, dash = \"dash\")", ifelse("variance" %in% names(isolate(o_plot$model)), ", f_hline(y = -2, dash = \"dash\"), f_hline(y = 2, dash = \"dash\")),", "),"), "
								plot_bgcolor = l_graph_opt$bg_grid_color[1],
								xaxis = list(title = \"Fitted values\", range = l_axis_layout[[1]], gridcolor = l_graph_opt$bg_grid_color[2]),
								yaxis = list(title = s_ylab, zeroline = F, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
							)")))
						}
						else {
							# (standardized) residuals vs independent variables:
							
							i_num <- as.numeric(substr(isolate(o_parameter$select_graph), nchar(isolate(o_parameter$select_graph)), nchar(isolate(o_parameter$select_graph))))
							s_xlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")], paste0("x", i_num)), paste0("x", i_num))
							v_x <- as.vector(df_all[, isolate(o_parameter$x)[i_num]])
							v_text <- paste0("x", i_num, ": ", round(v_x, digits = i_dec_num), "<br>res: ", round(v_res, digits = i_dec_num))
							
							if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
								v_order <- order(l_graph_opt$sorting)
								v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
								v_group <- v_group[order(v_group)]
								v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
								eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_x[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], y = v_res[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], name = \"", v_group[v_order], "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_order], ", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
							}
							else {
								if (!isolate(o_parameter$quant_group)) {
									ply_1 <- add_trace(p = ply_1, x = v_x, y = v_res, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = l_graph_opt$point_size, color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), symbol = v_symbol[l_graph_opt$point_type], line = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T)
								}
								else { # quantitative Group variable
									v_group <- as.vector(df_all[, isolate(o_parameter$group)])
									
									if (isolate(o_parameter$webgl) == "yes") { # type = "scattergl"
										m_rgb <- colorRamp(l_graph_opt$pal_col_op$color)(scales::rescale(v_group))
										v_bgcolor <- rgb(m_rgb[, 1], m_rgb[, 2], m_rgb[, 3], maxColorValue = 255) 
										ply_1 <- add_trace(p = ply_1, x = v_x, y = v_res, color = v_group, name = "all", legendgroup = "all", type = "scattergl", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, hoverlabel = list(bgcolor = v_bgcolor), colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
									}
									else { # type = "scatter"
										ply_1 <- add_trace(p = ply_1, x = v_x, y = v_res, color = v_group, name = "all", legendgroup = "all", type = "scatter", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
									}
									
									if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(v_x), range(v_res))}
								}
							}
							
							# add graph layout:
							
							eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
								ifelse(s_title != "", "title =  s_title,", ""), "
								shapes = list(f_hline(y = 0, dash = \"dash\")", ifelse("variance" %in% names(isolate(o_plot$model)), ", f_hline(y = -2, dash = \"dash\"), f_hline(y = 2, dash = \"dash\")),", "),"), "
								plot_bgcolor = l_graph_opt$bg_grid_color[1],
								xaxis = list(title = s_xlab, range = l_axis_layout[[1]], gridcolor = l_graph_opt$bg_grid_color[2]),
								yaxis = list(title = s_ylab, zeroline = F, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
							)")))
						}
					}
				}
				else {
					# (3) model: validation
					# .....................
					
					ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
					
					v_text <- paste0("fit: ", round(v_fit, digits = i_dec_num), "<br>", isolate(o_parameter$ylab), ": ", round(v_y, digits = i_dec_num))
					
					# observed values vs fitted values:
					
					if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
						v_order <- order(l_graph_opt$sorting)
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_fit[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], y = v_y[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], name = \"", v_group[v_order], "\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_order], ", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = v_text[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_order], "\")], showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
					}
					else {
						if (!isolate(o_parameter$quant_group)) {
							ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_y, name = "all", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(size = l_graph_opt$point_size, color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), symbol = v_symbol[l_graph_opt$point_type], line = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), hoverinfo = 'text+name', text = v_text, showlegend = T)
						}
						else { # quantitative Group variable
							v_group <- as.vector(df_all[, isolate(o_parameter$group)])
							
							if (isolate(o_parameter$webgl) == "yes") { # type = "scattergl"
								m_rgb <- colorRamp(l_graph_opt$pal_col_op$color)(scales::rescale(v_group))
								v_bgcolor <- rgb(m_rgb[, 1], m_rgb[, 2], m_rgb[, 3], maxColorValue = 255) 
								ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_y, color = v_group, name = "all", legendgroup = "all", type = "scattergl", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, hoverlabel = list(bgcolor = v_bgcolor), colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
							}
							else { # type = "scatter"
								ply_1 <- add_trace(p = ply_1, x = v_fit, y = v_y, color = v_group, name = "all", legendgroup = "all", type = "scatter", mode = "markers", marker = list(size = v_size, symbol = v_symbol[l_graph_opt$pal_point$type], line = list(width = 1.5)), opacity = l_graph_opt$pal_col_op$opacity, colors = l_graph_opt$pal_col_op$color, hoverinfo = 'text+name', text = v_text, showlegend = T)
							}
							
							if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(range(v_fit), range(v_y))}
						}
					}
					
					# add graph layout:
					
					eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
						ifelse(s_title != "", "title =  s_title,", ""),"
						shapes = list(f_abline(xmin = min(v_fit), xmax = max(v_fit), a = 0, b = 1, dash = \"dash\")),
						plot_bgcolor = l_graph_opt$bg_grid_color[1],
						xaxis = list(title = \"Fitted values\", range = l_axis_layout[[1]], gridcolor = l_graph_opt$bg_grid_color[2]),
						yaxis = list(title = \"Observed values\", range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
					)")))
				}
			}
		}
		else if (isolate(o_parameter$plot_type) == "boxplot") {
			# (ii) boxplot
			# ............
			
			s_ylab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")], isolate(o_parameter$ylab)), isolate(o_parameter$ylab))
			i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
			
			if (!is.na(isolate(o_parameter$group))) {
				names(df_all)[match(c(isolate(o_parameter$x), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))] <- c("x", "y", "group")
				v_order_1 <- order(l_graph_opt$sorting2)
				v_order_2 <- order(l_graph_opt$sorting)
				v_group_1 <- as.vector(unique(df_all$group))
				v_group_1 <- v_group_1[order(v_group_1)][v_order_1]
				v_group_2 <- as.vector(unique(df_all$x))
				v_group_2 <- v_group_2[order(v_group_2)]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group_2), "statut"][v_order_2]
				v_group_2 <- v_group_2[v_order_2]
				df_median <- aggregate(y ~ x, data = df_all, median)
				# df_median <- df_median[order(df_median[, "x"]),]
				v_median <- as.vector(df_median[order(df_median[, "x"]), 2])[v_order_2]
				v_color <- l_graph_opt$color[v_order_2]
				v_opacity <- l_graph_opt$opacity[v_order_2]
				v_point_type <- l_graph_opt$point_type[v_order_2]
				v_point_size <- l_graph_opt$point_size[v_order_2]
				
				for (i in 1:length(v_group_1)) {
					eval(parse(text = paste0("ply_1_", i, " <- plot_ly(source = s_source, type = \"box\", width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c(\"sendDataToCloud\", \"zoom2d\", \"autoScale2d\", \"resetScale2d\", \"pan2d\", \"select2d\", \"lasso2d\", \"zoomIn2d\", \"zoomOut2d\", \"toggleSpikelines\", \"hoverClosestCartesian\", \"hoverCompareCartesian\"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)")))
					df_all_i <- df_all[which(df_all$group == v_group_1[i]),]
					df_num <- as.data.frame(addmargins(table(as.vector(df_all_i$x))))
					df_num <- df_num[-dim(df_num)[1],]
					v_pos <- which(!v_group_2 %in% as.vector(df_num[, 1]))
					l_y <- eval(parse(text = paste0("list(", paste(rep("c()", length(v_group_2)), collapse = ", "), ")")))
					l_outliers <- eval(parse(text = paste0("list(", paste(rep("c()", length(v_group_2)), collapse = ", "), ")")))
					
					if (length(v_pos) > 0) {
						eval(parse(text = paste(paste0("l_y[[", c(1:length(v_group_2))[-v_pos], "]] <- as.vector(df_all_i[which(df_all_i$x == \"", v_group_2[-v_pos], "\"), \"y\"])"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- as.vector(quantile(l_y[[", c(1:length(v_group_2))[-v_pos], "]], probs = c(0.25, 0.75), type = 5))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- c(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][1] - 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]])), l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][2] + 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2))[-v_pos], "]])))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2))[-v_pos], "]] <- l_y[[", c(1:length(v_group_2))[-v_pos], "]][which(l_y[[", c(1:length(v_group_2))[-v_pos], "]] < l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][1] | l_y[[", c(1:length(v_group_2))[-v_pos], "]] > l_outliers[[", c(1:length(v_group_2))[-v_pos], "]][2])]"), collapse = "; "))) 
					}
					else {
						eval(parse(text = paste(paste0("l_y[[", c(1:length(v_group_2)), "]] <- as.vector(df_all_i[which(df_all_i$x == \"", v_group_2, "\"), \"y\"])"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- as.vector(quantile(l_y[[", c(1:length(v_group_2)), "]], probs = c(0.25, 0.75), type = 5))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- c(l_outliers[[", c(1:length(v_group_2)), "]][1] - 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2)), "]])), l_outliers[[", c(1:length(v_group_2)), "]][2] + 1.5 * abs(diff(l_outliers[[", c(1:length(v_group_2)), "]])))"), collapse = "; ")))
						eval(parse(text = paste(paste0("l_outliers[[", c(1:length(v_group_2)), "]] <- l_y[[", c(1:length(v_group_2)), "]][which(l_y[[", c(1:length(v_group_2)), "]] < l_outliers[[", c(1:length(v_group_2)), "]][1] | l_y[[", c(1:length(v_group_2)), "]] > l_outliers[[", c(1:length(v_group_2)), "]][2])]"), collapse = "; ")))
					}
					
					v_command <- as.vector(unlist(lapply(1:length(v_group_2), function(x) {
						if (length(v_pos) > 0) {
							if (x %in% v_pos) {
								v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = ", v_median[x], ", name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], ")), showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
								v_out <- c(v_out, paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = ", v_median[x], ", name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(l_graph_opt$bg_grid_color[1]), line = list(width = 3), showlegend = F, visible = ", v_visible[x], ")"))
							}
							else {
								v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = l_y[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], ")), marker = list(color = adjustcolor(\"", v_color[x], "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[x], "\", alpha.f = 1)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
								
								if (length(l_outliers[[x]]) > 0) {
									v_out <- paste0(v_out, paste0(" %>% add_markers(x = rep(\"", v_group_2[x], "\", length(l_outliers[[", x, "]])), y = l_outliers[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", type = \"scattergl\", marker = list(size = ", v_point_size[x], ", color = adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], "), symbol = \"", v_symbol[v_point_type[x]], "\", line = list(color = adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[x], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", x, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[x], ")"))
								}
							}
						}
						else {
							v_out <- paste0("ply_1_", i, " <- add_boxplot(p = ply_1_", i, ", y = l_y[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", color = I(adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], ")), marker = list(color = adjustcolor(\"", v_color[x], "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[x], "\", alpha.f = 1)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = ", ifelse(i == 1, "T", "F"), ", visible = ", v_visible[x], ")")
							
							if (length(l_outliers[[x]]) > 0) {
								v_out <- paste0(v_out, paste0(" %>% add_markers(x = rep(\"", v_group_2[x], "\", length(l_outliers[[", x, "]])), y = l_outliers[[", x, "]], name = \"", v_group_2[x], "\", legendgroup = \"", v_group_2[x], "\", type = \"scattergl\", marker = list(size = ", v_point_size[x], ", color = adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], "), symbol = \"", v_symbol[v_point_type[x]], "\", line = list(color = adjustcolor(\"", v_color[x], "\", alpha.f = ", v_opacity[x], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[x], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", x, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[x], ")"))
							}
						}
						
						return(v_out)
					})))
					
					# add graph layout per group:
					
					v_command <- c(v_command, paste0("ply_1_", i, " <- layout(p = ply_1_", i, ", margin = list(b = 0), plot_bgcolor = l_graph_opt$bg_grid_color[1], xaxis = list(showticklabels = F, type = \"category\", title = \"", v_group_1[i], "\", fixedrange = T), yaxis = list(title = s_ylab, zeroline = F, fixedrange = T, gridcolor = l_graph_opt$bg_grid_color[2]))"))
					eval(parse(text = paste(v_command, collapse = "; ")))
				}
				
				eval(parse(text = paste0("ply_1 <- subplot(list(", paste(paste0("ply_1_", 1:length(v_group_1)), collapse = ", "), "), shareY = T, shareX = F, titleX = T, titleY = T, margin = 0.01)")))
				
				# add global graph layout:
				
				if (s_title != "") {ply_1 <- layout(p = ply_1, title = s_title)}
			}
			else {
				names(df_all)[match(c(isolate(o_parameter$x), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y))), names(df_all))] <- c("x", "y")
				ply_1 <- plot_ly(source = s_source, type = "box", width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
				v_order <- order(l_graph_opt$sorting)
				v_group <- as.vector(unique(df_all$x))
				v_group <- v_group[order(v_group)]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"][v_order]
				v_group <- v_group[v_order]
				v_color <- l_graph_opt$color[v_order]
				v_opacity <- l_graph_opt$opacity[v_order]
				v_point_type <- l_graph_opt$point_type[v_order]
				v_point_size <- l_graph_opt$point_size[v_order]
				l_y <- eval(parse(text = paste0("list(", paste(paste0("as.vector(df_all[which(df_all$x == \"", v_group, "\"), \"y\"])"), collapse = ", "), ")")))
				v_command_1 <- paste0("ply_1 <- add_boxplot(p = ply_1, y = l_y[[", 1:length(v_group), "]], name = \"", v_group, "\", legendgroup = \"", v_group, "\", color = I(adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, ")), marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = 0)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = \"y+name\", boxmean = ", isolate(o_parameter$boxmean), ", showlegend = T, visible = ", v_visible, ")")
				l_outliers <- eval(parse(text = paste0("list(", paste(paste0("as.vector(quantile(l_y[[", 1:length(v_group), "]], probs = c(0.25, 0.75), type = 5))"), collapse = ", "), ")")))
				eval(parse(text = paste(paste0("l_outliers[[", 1:length(v_group), "]] <- c(l_outliers[[", 1:length(v_group), "]][1] - 1.5 * abs(diff(l_outliers[[", 1:length(v_group), "]])), l_outliers[[", 1:length(v_group), "]][2] + 1.5 * abs(diff(l_outliers[[", 1:length(v_group), "]])))"), collapse = "; ")))
				eval(parse(text = paste(paste0("l_outliers[[", 1:length(v_group), "]] <- l_y[[", 1:length(v_group), "]][which(l_y[[", 1:length(v_group), "]] < l_outliers[[", 1:length(v_group), "]][1] | l_y[[", 1:length(v_group), "]] > l_outliers[[", 1:length(v_group), "]][2])]"), collapse = "; ")))
				v_command_2 <- rep("", length(v_group))
				v_pos <- which(lengths(l_outliers) > 0)
				
				if (length(v_pos) > 0) {
					v_command_2[v_pos] <- paste0(" %>% add_markers(x = rep(\"", v_group[v_pos], "\", length(l_outliers[[", v_pos, "]])), y = l_outliers[[", v_pos, "]], name = \"", v_group[v_pos], "\", legendgroup = \"", v_group[v_pos], "\", type = \"scattergl\", marker = list(size = ", v_point_size[v_pos], ", color = adjustcolor(\"", v_color[v_pos], "\", alpha.f = ", v_opacity[v_pos], "), symbol = \"", v_symbol[v_point_type[v_pos]], "\", line = list(color = adjustcolor(\"", v_color[v_pos], "\", alpha.f = ", v_opacity[v_pos], "), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color[v_pos], "\", alpha.f = 1)), hoverinfo = \"text+name\", text = paste0(\"", ifelse(is.na(isolate(o_parameter$g)), "y: ", "g(y): "), "\", round(l_outliers[[", v_pos, "]], digits = i_dec_num)), showlegend = F, visible = ", v_visible[v_pos], ")")
				}
				
				eval(parse(text = paste(paste0(v_command_1, v_command_2), collapse = "; ")))
				
				# add graph layout:
				
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(s_title != "", "title = s_title,", ""),
					"plot_bgcolor = l_graph_opt$bg_grid_color[1],
					xaxis = list(type = \"category\", fixedrange = T), 
					yaxis = list(title = s_ylab, zeroline = F, fixedrange = T, gridcolor = l_graph_opt$bg_grid_color[2])
				)")))
			}
		}
		else if (isolate(o_parameter$plot_type) == "histplot") {
			# (iv) histplot
			# ..............
			
			s_xlab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "x")], isolate(o_parameter$xlab)), isolate(o_parameter$xlab))
			s_bw <- ifelse(isolate(o_parameter$autobw) == F & !is.na(isolate(o_parameter$bw)), paste0(", xbins = list(size = ", isolate(o_parameter$bw), ")"), "")
			
			ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'select2d', 'lasso2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
			
			if (!is.na(isolate(o_parameter$group))) {
				names(df_all)[match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), isolate(o_parameter$group)), names(df_all))] <- c("x", "group")
				v_order <- order(l_graph_opt$sorting)
				v_group <- as.vector(unique(df_all$group))
				v_group <- v_group[order(v_group)]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"] 
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all$group == \"", v_group[v_order], "\"), \"x\"], name = \"", v_group[v_order], "\", type = \"histogram\", histnorm = \"probability density\", autobinx = isolate(o_parameter$autobw)", s_bw, ", marker = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", ifelse((l_graph_opt$opacity[v_order] + 0.3) > 1, 1, l_graph_opt$opacity[v_order] + 0.3), "), width = 2)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
				
				# add graph layout:
				
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,
					barmode = \"overlay\",",
					ifelse(s_title != "", "title =  s_title,", ""),
					"plot_bgcolor = l_graph_opt$bg_grid_color[1],
					xaxis = list(title = s_xlab, range = l_axis_layout[[1]]),
					yaxis = list(title = \"Density\", range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
				)")))
			}
			else {
				names(df_all) <- "x"
				v_visible <- df_click_legend[which(df_click_legend$name == "all"), "statut"]
				eval(parse(text = paste0("ply_1 <- add_trace(p = ply_1, x = df_all$x, name = \"all\", type = \"histogram\", histnorm = \"probability density\", autobinx = isolate(o_parameter$autobw)", s_bw, ", marker = list(color = adjustcolor(l_graph_opt$color, alpha.f = l_graph_opt$opacity), line = list(color = adjustcolor(l_graph_opt$color, alpha.f = ifelse((l_graph_opt$opacity + 0.3) > 1, 1, l_graph_opt$opacity + 0.3)), width = 2)), hoverlabel = list(bgcolor = adjustcolor(l_graph_opt$color, alpha.f = 1)), showlegend = T, visible = ", v_visible, ")")))
				
				# add graph layout:
			
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(s_title != "", "title =  s_title,", ""),
					"plot_bgcolor = l_graph_opt$bg_grid_color[1],
					xaxis = list(title = s_xlab, range = l_axis_layout[[1]]),
					yaxis = list(title = \"Density\", range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
				)")))
			}
		}
		else if (isolate(o_parameter$plot_type) == "barplot") {
			# (v) barplot
			# ............
			
			if (!is.na(isolate(o_parameter$group))) {
				df_all$.num. <- 1
				v_order_1 <- order(l_graph_opt$sorting2)
				v_order_2 <- order(l_graph_opt$sorting)
				v_group_1 <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
				v_group_1 <- v_group_1[order(v_group_1)]
				v_group_2 <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
				v_group_2 <- v_group_2[order(v_group_2)]
				df_num_all <- expand.grid(v_group_1, v_group_2)
				names(df_num_all) <- c(isolate(o_parameter$group), isolate(o_parameter$x))
				df_num_all$.num. <- 0
				df_num_all$.comb. <- paste(df_num_all[, isolate(o_parameter$group)], df_num_all[, isolate(o_parameter$x)], sep = "_")
				df_num <- eval(parse(text = paste0("aggregate(.num. ~ ", isolate(o_parameter$x), "/", isolate(o_parameter$group), ", data = df_all, sum)")))
				df_num$.comb. <- paste(df_num[, isolate(o_parameter$group)], df_num[, isolate(o_parameter$x)], sep = "_")
				
				if (dim(df_num_all)[1] > dim(df_num)[1]) {
					df_num <- rbind(df_num, df_num_all[which(!df_num_all[, ".comb."] %in% as.vector(df_num[, ".comb."])), c(isolate(o_parameter$x), isolate(o_parameter$group), ".num.", ".comb.")])
				}
				
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group_2), "statut"][v_order_2]
				v_group_1 <- v_group_1[v_order_1]
				v_group_2 <- v_group_2[v_order_2]
				v_color <- l_graph_opt$color[v_order_2]
				v_opacity <- l_graph_opt$opacity[v_order_2]
				
				for (i in 1:length(v_group_1)) {
					eval(parse(text = paste0("ply_1_", i, " <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c(\"sendDataToCloud\", \"zoom2d\", \"autoScale2d\", \"resetScale2d\", \"pan2d\", \"select2d\", \"lasso2d\", \"zoomIn2d\", \"zoomOut2d\", \"toggleSpikelines\", \"hoverClosestCartesian\", \"hoverCompareCartesian\"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)")))
					df_num_i <- df_num[df_num[, isolate(o_parameter$group)] == v_group_1[i],]
					eval(parse(text = paste(paste0("ply_1_", i, " <- add_trace(p = ply_1_", i, ", y = df_num_i[df_num_i[, isolate(o_parameter$x)] == \"", v_group_2, "\", \".num.\"], name = \"", v_group_2, "\", legendgroup = \"", v_group_2, "\", type = \"bar\", marker = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", ifelse((v_opacity + 0.3) > 1, 1, v_opacity + 0.3), "), width = 2)), hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = \"name+text\", text = df_num_i[df_num_i[, isolate(o_parameter$x)] == \"", v_group_2, "\", \".num.\"], textposition = \"auto\", showlegend = ", ifelse(i == 1, T, F), ", visible = ", v_visible, ")"), collapse = "; ")))
					
					# add graph layout per group:
					
					eval(parse(text = paste0("ply_1_", i, " <- layout(p = ply_1_", i, ", plot_bgcolor = l_graph_opt$bg_grid_color[1], xaxis = list(showticklabels = F, title = \"", v_group_1[i], "\", fixedrange = T), yaxis = list(title = \"Count\", fixedrange = T, gridcolor = l_graph_opt$bg_grid_color[2]))")))
				}
				
				eval(parse(text = paste0("ply_1 <- subplot(list(", paste(paste0("ply_1_", 1:length(v_group_1)), collapse = ", "), "), shareY = T, shareX = F, titleX = T, titleY = T, margin = 0.01)")))
			
				# add global graph layout:
				
				if (s_title != "") {ply_1 <- layout(p = ply_1, title = s_title)}
			}
			else {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 50, v_dimension[2] - 270)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
				v_x <- as.vector(df_all[, isolate(o_parameter$x)])
				v_order <- order(l_graph_opt$sorting)
				v_group <- unique(v_x)
				v_group <- v_group[order(v_group)]
				df_num <- as.data.frame(addmargins(table(v_x)))
				names(df_num) <- c(isolate(o_parameter$x), ".num.")
				df_num <- df_num[-dim(df_num)[1],]
				v_visible <- df_click_legend[which(df_click_legend$name %in% v_group), "statut"]
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, y = df_num[df_num[, isolate(o_parameter$x)] == \"", v_group[v_order], "\", \".num.\"], name = \"", v_group[v_order], "\", type = \"bar\", marker = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", l_graph_opt$opacity[v_order], "), line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = ", ifelse((l_graph_opt$opacity[v_order] + 0.3) > 1, 1, l_graph_opt$opacity[v_order] + 0.3), "), width = 2)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"name+text\", text = df_num[df_num[, isolate(o_parameter$x)] == \"", v_group[v_order], "\", \".num.\"], textposition = \"auto\", showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
				
				# add graph layout:
			
				eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
					ifelse(s_title != "", "title =  s_title,", ""),
					"plot_bgcolor = l_graph_opt$bg_grid_color[1],
					xaxis = list(showticklabels = F, fixedrange = T),
					yaxis = list(title = \"Count\", fixedrange = T, gridcolor = l_graph_opt$bg_grid_color[2])
				)")))
			}
		}
		else {
			# (vi) corplot
			# ............
			
			if (!is.na(isolate(o_parameter$group))) {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 95, v_dimension[2] - 315)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
				df_all <-  df_all[which(df_all[, isolate(o_parameter$group)] == isolate(o_parameter$select_graph)),]
			}
			else {
				ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 40, v_dimension[2] - 260)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c("sendDataToCloud", "zoom2d", "autoScale2d", "resetScale2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
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
			
			df_cor <- as.data.frame(arrangements::combinations(x = isolate(o_parameter$y), k = 2, replace = F))
			names(df_cor) <- c("x", "y")
			df_cor$val <- eval(parse(text = paste0("c(", paste(paste0("round(cor(df_all[, \"", df_cor[, 1], "\"], df_all[, \"", df_cor[, 2], "\"], use = \"pairwise.complete.obs\"), digits = 2)"), collapse = ", "), ")")))
			df_cor <- df_cor[!is.na(df_cor$val),]
			
			ply_1 <- add_trace(p = ply_1, x = colnames(m_cor), y = rownames(m_cor), z = m_cor, zmin = -1, zmax = 1, colors = colorRamp(c("blue", "grey", "red")), xgap = 2, ygap = 2, type = "heatmap", hoverinfo = "text", text = l_text)
			ply_1 <- add_annotations(p = ply_1, x = df_cor$x, y = df_cor$y, text = df_cor$val, font = list(color = "white"), showarrow = F)
			
			# add graph layout:
			
			eval(parse(text = paste0("ply_1 <- layout(p = ply_1,",
				ifelse(s_title != "", "title =  s_title,", ""),
				"plot_bgcolor = l_graph_opt$bg_grid_color[1],
				xaxis = list(tickangle = -45, showgrid = F, fixedrange = T), 
				yaxis = list(showgrid = F, fixedrange = T)
			)")))
		}
	}
	else if (s_data_type == "temporal") {
		s_ylab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")], isolate(o_parameter$ylab)), isolate(o_parameter$ylab))
		i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
		v_visible <- df_click_legend[which(df_click_legend$name %in% isolate(o_parameter$y)), "statut"] 
		
		ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 20, v_dimension[2] - 240)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
		
		if (isolate(o_parameter$mode) == "marker") {
			v_command <- paste0("df_all[!is.na(df_all$", isolate(o_parameter$y), "),]")
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size, ", color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, "), symbol = \"", v_symbol[l_graph_opt$point_type], "\", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, "), width = 1.5)), name = \"", isolate(o_parameter$y), "\", hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1)), hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
		}
		else {
			v_command <- "df_all"
			
			if (isolate(o_parameter$mode) == "line") {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, ")), name = \"", isolate(o_parameter$y), "\", hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1)), hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
			}
			else {
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~", isolate(o_parameter$x), ", y = ~", isolate(o_parameter$y), ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, ")), marker = list(size = ", l_graph_opt$point_size, ", color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, "), symbol = \"", v_symbol[l_graph_opt$point_type], "\", line = list(color = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = ", l_graph_opt$opacity, "), width = 1.5)), name = \"", isolate(o_parameter$y), "\", hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color, "\", alpha.f = 1)), hoverinfo = 'x+text+name', text = ~round(", isolate(o_parameter$y), ", digits = i_dec_num), legendgroup = \"", isolate(o_parameter$y), "\", visible = ", v_visible, ")"), collapse = "; ")))
			}
		}
		
		if (isolate(o_plot$add_pt)) {
			l_pt_pos <- isolate(o_plot$pt_pos)
			v_visible <- df_click_legend[which(df_click_legend$name %in% isolate(o_plot$var_pt)), "statut"]
			v_pos <- which(isolate(o_parameter$y) %in% isolate(o_plot$var_pt))
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"",isolate(o_parameter$x), "\"], y = df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"", isolate(o_plot$var_pt), "\"], type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", l_graph_opt$point_size[v_pos], ", color = adjustcolor(\"", l_graph_opt$color[v_pos], "\", alpha.f = ", l_graph_opt$opacity[v_pos], "), symbol = \"", v_symbol[l_graph_opt$point_type[v_pos]], "\", line = list(color = adjustcolor(\"", l_graph_opt$color[v_pos], "\", alpha.f = ", l_graph_opt$opacity[v_pos], "), width = 1.5)), name = \"", isolate(o_plot$var_pt), "\", hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_pos], "\", alpha.f = 1)), hoverinfo = 'x+text+name', text = round(df_all[l_pt_pos$", isolate(o_plot$var_pt), ", \"", isolate(o_plot$var_pt), "\"], digits = i_dec_num), legendgroup = \"", isolate(o_plot$var_pt), "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
		}
		
		# add graph layout:
		
		ply_1 <- layout(p = ply_1,
			title = s_title, 
			plot_bgcolor = l_graph_opt$bg_grid_color[1],
			xaxis = list(
				title = "",
				gridcolor = l_graph_opt$bg_grid_color[2],
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
			yaxis = list(title = s_ylab, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])
		)
	}
	else { # ir
		s_ylab <- ifelse(length(isolate(o_label_text$text)) > 0, ifelse(isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")] != "", isolate(o_label_text$text)[which(isolate(o_label_text$label) == "y")], isolate(o_parameter$ylab)), isolate(o_parameter$ylab))
		i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F & !is.na(isolate(o_parameter$dec_num)), isolate(o_parameter$dec_num), 2)
		
		df_id_group <- isolate(o_plot$id_group)$no_flag
		row.names(df_id_group) <- 1:nrow(df_id_group)
		df_id_group <- df_id_group[order(df_id_group$sorting),]
		v_row <- as.integer(row.names(df_id_group))
		
		v_name <- names(df_all)[-c(1, 2)][v_row]
		v_id <- as.vector(df_id_group$id)
		v_show <- as.vector(df_id_group$show)
		v_color <- as.vector(df_id_group$color)
		v_opacity <- as.vector(df_id_group$opacity)
		v_point_type <- as.vector(df_id_group$point_type)
		v_point_size <- as.vector(df_id_group$point_size)
		v_group <- as.vector(df_id_group$group)
		if (isolate(o_parameter$quant_group)) {v_group_val <- as.vector(df_id_group$group_val)}
		v_visible <- eval(parse(text = paste0("c(", paste(paste0("df_click_legend[which(df_click_legend$name == \"", v_group, "\"), \"statut\"]"), collapse = ", "), ")")))
		
		l_text <- lapply(1:length(v_name), function(i) {
			v_out <- paste0("id: ", v_id[i], "<br>frequency : ", round(as.vector(df_all$Frequency), digits = i_dec_num), "<br>y: ", round(as.vector(df_all[, v_name[i]]), digits = i_dec_num))
			return(v_out)
		})
		
		ply_1 <- plot_ly(source = s_source, width = ifelse(isolate(o_click_button$left_panel) == 1, v_dimension[1], v_dimension[1] - 300), height = ifelse(isolate(o_click_button$top_panel) == 1, v_dimension[2] - 45, v_dimension[2] - 265)) %>% config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'), toImageButtonOptions = list(format = isolate(o_picture_info$format), filename = isolate(o_picture_info$filename), height = isolate(o_picture_info$height), width = isolate(o_picture_info$width)), doubleClickDelay = 1e-16)
		
		if (isolate(o_parameter$mode) == "marker") {
			if (isolate(o_parameter$quant_group)) { # quantitative Group variable
				v_command <- paste0("!is.na(df_all$", v_name, ")")
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all[", v_command, ", \"Frequency\"], y = df_all[", v_command, ", \"", v_name, "\"], color = rep(", v_group_val, ", nrow(df_all[", v_command, ",])), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", v_point_size, ", symbol = \"", v_symbol[v_point_type], "\", line = list(width = 1.5)), opacity = ", v_opacity, ", name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"", v_color, "\"), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]][which(!is.na(df_all$", v_name, "))], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ", colors = l_graph_opt$color)"), collapse = "; ")))
			}
			else { # no/qualitative Group variable
				v_command <- paste0("df_all[!is.na(df_all$", v_name, "),]")
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", v_point_size, ", color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), symbol = \"", v_symbol[v_point_type], "\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), width = 1.5)), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]][which(!is.na(df_all$", v_name, "))], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
			}
		}
		else {
			v_command <- "df_all"
			
			if (isolate(o_parameter$mode) == "line") {
				if (isolate(o_parameter$quant_group)) { # quantitative Group variable
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all$Frequency, y = df_all$", v_name, ", color = rep(", v_group_val, ", nrow(df_all)), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", opacity = ", v_opacity, ", name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"", v_color, "\"), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ", colors = l_graph_opt$color)"), collapse = "; ")))
				}
				else { # no/qualitative Group variable
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, ")), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
				}
			}
			else {
				if (isolate(o_parameter$quant_group)) { # quantitative Group variable
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all$Frequency, y = df_all$", v_name, ", color = rep(", v_group_val, ", nrow(df_all)), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", opacity = ", v_opacity, ", marker = list(size = ", v_point_size, ", symbol = \"", v_symbol[v_point_type], "\", line = list(width = 1.5)), name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"", v_color, "\"), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ", colors = l_graph_opt$color)"), collapse = "; ")))
				}
				else { # no/qualitative Group variable
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, ")), marker = list(size = ", v_point_size, ", color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), symbol = \"", v_symbol[v_point_type], "\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), width = 1.5)), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
				}
			}
		}
		
		if (isolate(o_plot$add_pt)) {
			l_text <- lapply(1:length(v_name), function(i) {
				v_out <- paste0("id: ", v_id[i], "<br>frequency : ", round(as.vector(df_all[isolate(o_plot$pt_pos), "Frequency"]), digits = i_dec_num), "<br>y: ", round(as.vector(df_all[isolate(o_plot$pt_pos), v_name[i]]), digits = i_dec_num))
				return(v_out)
			})
			
			if (isolate(o_parameter$quant_group)) { # quantitative Group variable
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_all[isolate(o_plot$pt_pos), \"Frequency\"]), y = as.vector(df_all[isolate(o_plot$pt_pos), \"", v_name, "\"]), color = rep(", v_group_val, ", length(isolate(o_plot$pt_pos))), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", v_point_size, ", symbol = \"", v_symbol[v_point_type], "\", line = list(width = 1.5)), opacity = ", v_opacity, ", name = \"", v_group, "\", hoverlabel = list(bgcolor = \"", v_color, "\"), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"", v_group, "\", showlegend = F, visible = ", v_visible, ", colors = l_graph_opt$color)"), collapse = "; ")))
			}
			else { # no/qualitative Group variable
				eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_all[isolate(o_plot$pt_pos), \"Frequency\"]), y = as.vector(df_all[isolate(o_plot$pt_pos), \"", v_name, "\"]), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(size = ", v_point_size, ", color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), symbol = \"", v_symbol[v_point_type], "\", line = list(color = adjustcolor(\"", v_color, "\", alpha.f = ", v_opacity, "), width = 1.5)), name = \"", v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"", v_color, "\", alpha.f = 1)), hoverinfo = 'text+name', text = l_text[[", 1:length(v_name), "]], legendgroup = \"", v_group, "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
			}
		}
		
		# add graph layout & annotations:
		
		v_freq_range <- range(as.vector(df_all$Frequency))
		b_cond <- 4000 >= v_freq_range[1] & 4000 <= v_freq_range[2]
		
		eval(parse(text = paste0("ply_1 <- layout(p = ply_1,", 
			ifelse(s_title != "", "title = s_title,", ""), " 
			plot_bgcolor = l_graph_opt$bg_grid_color[1],
			xaxis = list(
				title = \"Frequency (cm-1)\",
				gridcolor = l_graph_opt$bg_grid_color[2],
				rangeslider = list(thickness = 0.1, borderwidth = 1),
				range = l_axis_layout[[1]]
			),
			yaxis = list(title = s_ylab, range = l_axis_layout[[2]], gridcolor = l_graph_opt$bg_grid_color[2])",
			ifelse(b_cond, ", shapes = list(f_vline(x = 4000, dash = \"dash\"))", ""),
		")")))
		
		if (b_cond) {
			ply_1 <- add_annotations(ply_1, x = 4000, y = 1.05, xref = "x", yref = "paper", text = "NIR-MIR limit", showarrow = F)
		}
		
		#title = TeX(\"Frequency\\text{ (cm}^{-1}\\text{)}\")
	}
	
	if (length(v_min_max) > 0) {
		return(list(ply_1, v_min_max))
	}
	else {
		return (ply_1)
	}
}

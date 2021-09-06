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
# Description : function used to add flag data on the (plotly) graph for each data type (normal, temporal 
#               and ir).  
#               Flag data are separated into two categories,  
#               (1) previous flags are data saved in e_data environment (e_data$flag) and splitted by qc 
#                   value in the o_plot reactive value (o_plot$data_qc1, o_plot$data_qc2);
#               (2) current flags are data saved in e_current_flag environment.
#               
#               This function only concerns 2D/3D plot for the normal 
#               data type (without adding models).
#               
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# ply_1: plotly object created with the f_build_graph function
# v_cond: binary vector corresponding to 3 elements (flag, qc1 and qc2). 
#         A zero value means no (qc1/qc2) flag data exist. If flag = 1, then qc1 = 1 and/or qc2 = 1 (only temporal/ir data type). 
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"
# df_click_legend: legend item informations (name and status)
# df_previous_flag: flag data saved in e_data environment (first category). This input is only used for the ir data type (default: NULL)
# e_current_flag: environment corresponding to flag data of the second category.     

# Output:
# -------
# return a plotly object

f_add_flag <- function(s_data_type = "normal", ply_1, v_cond = rep(0, 3), df_click_legend, o_plot, o_parameter, df_previous_flag = NULL, e_current_flag) {
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
	
	if (s_data_type == "normal") {
		if (isolate(o_parameter$plot_type) == "plot") {
			if (isolate(o_parameter$model) == "none") {
				if (isolate(o_parameter$dim_num) == "2d") { # 2D plot
					# (1) add "previous" flags:
					
					if (v_cond[3] == 1) {
						df_qc2 <- isolate(o_plot$data_qc2) 
						v_text <- paste0("x: ", round(df_qc2[, isolate(o_parameter$x)], digits = i_dec_num), "<br>y: ", round(df_qc2[, isolate(o_parameter$y)], digits = i_dec_num))
						eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "qc = 2"), "statut"])))
						
						ply_1 <- add_trace(p = ply_1, x = df_qc2[, isolate(o_parameter$x)], y = df_qc2[, isolate(o_parameter$y)], type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", name = "qc = 2", marker = list(line = list(color = "red", width = 2), color = adjustcolor("red", alpha.f = 0), size = 8), hoverlabel = list(bgcolor = "red"), hoverinfo = 'text', text = v_text, showlegend = T, visible = v_visible)
					}
					
					# (2) add "current" flags:
					
					if (length(e_current_flag) > 0) {
						df_all <- isolate(o_plot$data)
						v_id <- as.vector(e_current_flag$coord[, 1])
						df_all <- df_all[, c(ifelse(!is.na(isolate(o_parameter$id)), isolate(o_parameter$id), ".row_num."), isolate(o_parameter$x), isolate(o_parameter$y))]
						df_all <- df_all[which(df_all[, 1] %in% v_id), -1]
						df_all <- unique(df_all)
						v_text <- paste0("(flag ", 1:dim(df_all)[1], ")<br>x: ", round(df_all[, isolate(o_parameter$x)], digits = i_dec_num), "<br>y: ", round(df_all[, isolate(o_parameter$y)], digits = i_dec_num))
						
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = ", df_all[, isolate(o_parameter$x)], ", y = ", df_all[, isolate(o_parameter$y)], ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"black\", width = 2), color = adjustcolor(\"black\", alpha.f = 0), size = 8), hoverlabel = list(bgcolor = \"black\"), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = F)"), collapse = "; ")))
					}
				}
				else { # 3D plot
					# (1) add "previous" flags:
					
					if (v_cond[3] == 1) {
						df_qc2 <- isolate(o_plot$data_qc2)
						v_text <- paste0("x: ", round(df_qc2[, isolate(o_parameter$x)], digits = i_dec_num), "<br>y: ", round(df_qc2[, isolate(o_parameter$y)], digits = i_dec_num), "<br>z: ", round(df_qc2[, isolate(o_parameter$z)], digits = i_dec_num))
						eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "qc = 2"), "statut"])))
						
						ply_1 <- add_trace(p = ply_1, x = df_qc2[, isolate(o_parameter$x)], y = df_qc2[, isolate(o_parameter$y)], z = df_qc2[, isolate(o_parameter$z)], type = "scatter3d", mode = "markers", name = "qc = 2", marker = list(line = list(color = "red", width = 1), color = adjustcolor("red", alpha.f = 0.5), size = 8), hoverlabel = list(bgcolor = "red"), hoverinfo = 'text', text = v_text, showlegend = T, visible = v_visible)
					}
					
					# (2) add "current" flags:
					
					if (length(e_current_flag) > 0) {
						df_all <- isolate(o_plot$data)
						v_id <- as.vector(e_current_flag$coord[, 1])
						df_all <- df_all[, c(ifelse(!is.na(isolate(o_parameter$id)), isolate(o_parameter$id), ".row_num."), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z))]
						df_all <- df_all[which(df_all[, 1] %in% v_id), -1]
						df_all <- unique(df_all)
						v_text <- paste0("(flag ", 1:dim(df_all)[1], ")<br>x: ", round(df_all[, isolate(o_parameter$x)], digits = i_dec_num), "<br>y: ", round(df_all[, isolate(o_parameter$y)], digits = i_dec_num), "<br>z: ", round(df_all[, isolate(o_parameter$z)], digits = i_dec_num))
						
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = ", df_all[, isolate(o_parameter$x)], ", y = ", df_all[, isolate(o_parameter$y)], ", z = ", df_all[, isolate(o_parameter$z)], ", type = \"scatter3d\", mode = \"markers\", marker = list(line = list(color = \"black\", width = 1), color = adjustcolor(\"black\", alpha.f = 0.5), size = 8), hoverlabel = list(bgcolor = \"black\"), hoverinfo = \"text\", text = \"", v_text, "\", showlegend = F)"), collapse = "; ")))
					}
				}
			}
		}
	}
	else if (s_data_type == "temporal") {
		# (1) add "previous" flags:
		
		if (v_cond[1] == 1) {
			df_qc1 <- isolate(o_plot$data_qc1)
			df_qc2 <- isolate(o_plot$data_qc2)
			v_leg_name <- unique(substr(isolate(o_plot$leg_name_qc), 1, nchar(isolate(o_plot$leg_name_qc)) - 7))
			
			f_add_exist_flag <- function(x) {
				v_command <- c()
				
				# qc = 1:
				
				if (v_cond[2] == 1) {
					if (x %in% isolate(o_plot$var_qc1)) { 
						s_visible <- paste0(", visible = ", df_click_legend[which(df_click_legend$name == paste0(x, " qc = 1")), "statut"])
						v_command <- c(v_command, paste0("ply_1 <- add_trace(p = ply_1, x = df_qc1$",isolate(o_parameter$x), "[which(!is.na(df_qc1$", x, "_pt))], y = df_qc1$", x, "_pt[which(!is.na(df_qc1$", x, "_pt))], name = \"", x, " qc = 1\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), width = 2), color = adjustcolor(\"#FF9900\", alpha.f = 0), size = 8), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), text = df_qc1$", x, "_comment[which(!is.na(df_qc1$", x, "_pt))], legendgroup = \"", x, "_qc1\", showlegend = T", s_visible, ")"))
						v_command <- c(v_command, paste0("ply_1 <- add_trace(p = ply_1, x = df_qc1$",isolate(o_parameter$x), ", y = df_qc1$", x, ", name = \"", x, " qc = 1\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), dash = \"dash\"), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), text = df_qc1$", x, "_comment, legendgroup = \"", x, "_qc1\", showlegend = F", s_visible, ")")) 
					}
				}
				
				# qc = 2:
				
				if (v_cond[3] == 1) {
					if (x %in% isolate(o_plot$var_qc2)) {
						s_visible <- paste0(", visible = ", df_click_legend[which(df_click_legend$name == paste0(x, " qc = 2")), "statut"])
						v_command <- c(v_command, paste0("ply_1 <- add_trace(p = ply_1, x = df_qc2$",isolate(o_parameter$x), "[which(!is.na(df_qc2$", x, "_pt))], y = df_qc2$", x, "_pt[which(!is.na(df_qc2$", x, "_pt))], name = \"", x, " qc = 2\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"red\", width = 2), color = adjustcolor(\"red\", alpha.f = 0), size = 8), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"red\"), text = df_qc2$", x, "_comment[which(!is.na(df_qc2$", x, "_pt))], legendgroup = \"", x, "_qc2\", showlegend = T", s_visible, ")"))
						v_command <- c(v_command, paste0("ply_1 <- add_trace(p = ply_1, x = df_qc2$",isolate(o_parameter$x), ", y = df_qc2$", x, ", name = \"", x, " qc = 2\", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = \"red\", dash = \"dash\"), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"red\"), text = df_qc2$", x, "_comment, legendgroup = \"", x, "_qc2\", showlegend = F", s_visible, ")"))
					}
				}
				
				return(paste(v_command, collapse = "; "))
			}
			
			eval(parse(text = paste(unlist(lapply(v_leg_name, f_add_exist_flag)), collapse = "; ")))
		}
		
		# (2) add "current" flags:
		
		if (length(e_current_flag) > 0) {
			if ("coord" %in% ls(e_current_flag)) {
				df_all <- isolate(o_plot$data)
				df_flag <- e_current_flag$coord
				i_num1 <- max(as.vector(unique(df_flag$num)))
				df_flag$x1_trf <- as.numeric(as.POSIXct(df_flag$x1, tz = "GMT"))
				df_flag$x2_trf <- as.numeric(as.POSIXct(df_flag$x2, tz = "GMT"))
				
				l_param <- lapply(1:i_num1, function(x) {
					df_x <- df_flag[df_flag$num == x,]
					v_x_trf <- c(as.vector(df_x[, "x1_trf"]), as.vector(df_x[, "x2_trf"]))
					v_x_trf <- v_x_trf[!is.na(v_x_trf)]
					v_x_trf <- v_x_trf[order(v_x_trf)]
					s_var_name <- as.vector(df_x[1, "var_name"])
					v_y <- round(df_all[which(df_all[, paste0(isolate(o_parameter$x), "_trf")] %in% v_x_trf), s_var_name], digits = i_dec_num)
					v_comment_pt <- paste0("(flag ", x, ")<br>", v_y)
					
					if (length(v_x_trf) > 1) {
						v_x <- df_all[which(df_all[, paste0(isolate(o_parameter$x), "_trf")] == v_x_trf[1]):which(df_all[, paste0(isolate(o_parameter$x), "_trf")] == v_x_trf[length(v_x_trf)]), isolate(o_parameter$x)]
						v_y <- df_all[which(df_all[, isolate(o_parameter$x)] %in% v_x), s_var_name] 
						
						if (length(v_x_trf) > 2) {
							v_pos_pt <- which(is.na(df_x$x2))
							v_pos_mpt <- which(!is.na(df_x$x2)) 
							v_pos <- c()
							
							if (length(v_pos_pt) > 0) {
								v_pos <- c(v_pos, which(v_x %in% as.vector(df_x[v_pos_pt, "x1"])))
							}
							
							if (length(v_pos_mpt) > 0) {
								eval(parse(text = paste0("v_pos <- c(v_pos,", paste(paste0("which(v_x == \"", as.vector(df_x[v_pos_mpt, "x1"]), "\"):which(v_x == \"", as.vector(df_x[v_pos_mpt, "x2"]), "\")"), collapse = ", "), ")")))
								v_pos <- c(1:length(v_x))[-v_pos]
							}
							
							v_y[v_pos] <- NA
						}
						
						v_comment_mpt <- rep(NA, length(v_y))
						v_comment_mpt[which(!is.na(v_y))] <- paste0("(flag ", x, ")<br>", round(v_y[which(!is.na(v_y))], digits = i_dec_num))
						l_out <- list(v_x_trf, s_var_name, v_comment_pt, v_comment_mpt, v_y)
					}
					else {
						l_out <- list(v_x_trf, s_var_name, v_comment_pt)
					}
					
					return(l_out)
				})
				
				v_command <- paste0("ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all$", isolate(o_parameter$x), "_trf == l_param[[", 1:i_num1, "]][[1]][1]), isolate(o_parameter$x)], y = df_all[which(df_all$", isolate(o_parameter$x), "_trf == l_param[[", 1:i_num1, "]][[1]][1]), l_param[[", 1:i_num1, "]][[2]]], name = l_param[[", 1:i_num1, "]][[2]], type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"black\", width = 2), color = adjustcolor(\"black\", alpha.f = 0), size = 8), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"black\"), text = l_param[[", 1:i_num1, "]][[3]][1], showlegend = F)")
				v_num <- which(lengths(l_param) > 3)
				v_command[v_num] <- paste0(v_command[v_num], "; ")
				v_command[v_num] <- paste0(v_command[v_num], paste0("ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all$", isolate(o_parameter$x), "_trf %in% l_param[[", v_num, "]][[1]][-1]), isolate(o_parameter$x)], y = df_all[which(df_all$", isolate(o_parameter$x), "_trf %in% l_param[[", v_num, "]][[1]][-1]), l_param[[", v_num, "]][[2]]], name = l_param[[", v_num, "]][[2]], type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"black\", width = 2), color = adjustcolor(\"black\", alpha.f = 0), size = 8), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"black\"), text = l_param[[", v_num, "]][[3]][-1], showlegend = F); ply_1 <- add_trace(p = ply_1, x = df_all[which(df_all$", isolate(o_parameter$x), "_trf == l_param[[", v_num, "]][[1]][1]):which(df_all$", isolate(o_parameter$x), "_trf == l_param[[", v_num, "]][[1]][length(l_param[[", v_num, "]][[1]])]), isolate(o_parameter$x)], y = l_param[[", v_num, "]][[5]], name = l_param[[", v_num, "]][[2]], type = \"scatter\", mode = \"lines\", line = list(color = \"black\", dash = \"dash\"), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"black\"), text = l_param[[", v_num, "]][[4]], showlegend = F)"))
			}
			else {
				i_num1 <- 0
			}
			
			i_num2 <- max(as.vector(unique(e_current_flag$all_coord$num)))
			
			if (i_num2 > i_num1 | i_num1 == 0) {
				df_flag <- e_current_flag$all_coord[e_current_flag$all_coord$num == i_num2,]
				s_comment <- paste0("(flag ", i_num2, ")<br>", round(as.vector(df_flag$y), digits = i_dec_num))
				v_command <- c(v_command, paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_flag$x), y = as.vector(df_flag$y), name = as.vector(df_flag$var_name), type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"black\", width = 2), color = adjustcolor(\"black\", alpha.f = 0), size = 8), hoverinfo = \"x+text\", hoverlabel = list(bgcolor = \"black\"), text = s_comment, showlegend = F)"))
			}
			
			eval(parse(text = paste(v_command, collapse = "; ")))
		}
	}
	else { # ir
		l_id_group <- isolate(o_plot$id_group)
		
		# (1) add "previous" flags:
		
		if (v_cond[1] == 1) {
			if (v_cond[2] == 1) {
				eval(parse(text = paste0("v_visible <- c(", paste(paste0("df_click_legend[which(df_click_legend$name == \"", as.vector(l_id_group$qc1_flag$group), "\"), \"statut\"]"), collapse = ", "), ")")))
				df_qc1 <- isolate(o_plot$data_qc1)
				v_name <- names(df_qc1)[-c(1, 2)]
				v_id <- as.vector(l_id_group$qc1_flag$id)
				v_group <- as.vector(l_id_group$qc1_flag$group)
				v_show <- as.vector(l_id_group$qc1_flag$show)
				
				l_text <- lapply(1:length(v_name), function(i) {
					s_comment <- df_previous_flag[which(df_previous_flag[, 1] == v_id[i]), "comment"]
					s_comment <- ifelse(is.na(s_comment) | s_comment == "", "", paste0("<br>comment:<br>", s_comment))
					v_out <- paste0("(", v_group[i], ")<br>id: ", v_id[i], "<br>frequency : ", round(as.vector(df_qc1$Frequency), digits = i_dec_num), "<br>y: ", round(as.vector(df_qc1[, v_name[i]]), digits = i_dec_num), s_comment)
					return(v_out)
				})
				
				if (isolate(o_parameter$mode) == "marker") {
					v_command <- paste0("df_qc1[!is.na(df_qc1$", v_name, "),]")
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), width = 2), color = adjustcolor(\"#FF9900\", alpha.f = 0), size = 6), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]][which(!is.na(df_qc1$", v_name, "))], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
				}
				else {
					v_command <- "df_qc1"
					
					if (isolate(o_parameter$mode) == "line") {
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), dash = \"dash\"), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
					}
					else {
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), dash = \"dash\"), marker = list(line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), width = 2), color = adjustcolor(\"#FF9900\", alpha.f = 0), size = 6), name = \"" , v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
					}
				}
				
				if (isolate(o_plot$add_pt) == T) {
					l_text <- lapply(1:length(v_name), function(i) {
						s_comment <- df_previous_flag[which(df_previous_flag[, 1] == v_id[i]), "comment"]
						s_comment <- ifelse(is.na(s_comment) | s_comment == "", "", paste0("<br>comment:<br>", s_comment))
						v_out <- paste0("(", v_group[i], ")<br>id: ", v_id[i], "<br>frequency : ", round(as.vector(df_qc1[isolate(o_plot$pt_pos), "Frequency"]), digits = i_dec_num), "<br>y: ", round(as.vector(df_qc1[isolate(o_plot$pt_pos), v_name[i]]), digits = i_dec_num), s_comment)
						return(v_out)
					})
					
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = as.vector(df_qc1[isolate(o_plot$pt_pos), \"Frequency\"]), y = as.vector(df_qc1[isolate(o_plot$pt_pos), \"", v_name, "\"]), type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = adjustcolor(\"#FF9900\", alpha.f = 1), width = 2), color = adjustcolor(\"#FF9900\", alpha.f = 0), size = 6), name = \"", v_group, "\", hoverlabel = list(bgcolor = adjustcolor(\"#FF9900\", alpha.f = 1)), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]], legendgroup = \"", v_group, "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
				}
			}
			
			if (v_cond[3] == 1) {
				eval(parse(text = paste0("v_visible <- c(", paste(paste0("df_click_legend[which(df_click_legend$name == \"", as.vector(l_id_group$qc2_flag$group), "\"), \"statut\"]"), collapse = ", "), ")")))
				df_qc2 <- isolate(o_plot$data_qc2)
				v_name <- names(df_qc2)[-c(1, 2)]
				v_id <- as.vector(l_id_group$qc2_flag$id)
				v_group <- as.vector(l_id_group$qc2_flag$group)
				v_show <- as.vector(l_id_group$qc2_flag$show)
				
				l_text <- lapply(1:length(v_name), function(i) {
					s_comment <- df_previous_flag[which(df_previous_flag[, 1] == v_id[i]), "comment"]
					s_comment <- ifelse(is.na(s_comment) | s_comment == "", "", paste0("<br>comment:<br>", s_comment))
					v_out <- paste0("(", v_group[i], ")<br>id: ", v_id[i], "<br>frequency : ", round(as.vector(df_qc2$Frequency), digits = i_dec_num), "<br>y: ", round(as.vector(df_qc2[, v_name[i]]), digits = i_dec_num), s_comment)
					return(v_out)
				})
				
				if (isolate(o_parameter$mode) == "marker") {
					v_command <- paste0("df_qc2[!is.na(df_qc2$", v_name, "),]")
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"red\", width = 2), color = adjustcolor(\"red\", alpha.f = 0), size = 6), name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"red\"), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]][which(!is.na(df_qc2$", v_name, "))], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
				}
				else {
					v_command <- "df_qc2"
					
					if (isolate(o_parameter$mode) == "line") {
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = \"red\", dash = \"dash\"), name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"red\"), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
					}
					else {
						eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, data = ", v_command, ", x = ~Frequency, y = ~", v_name, ", type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines+markers\", line = list(color = \"red\", dash = \"dash\"), marker = list(line = list(color = \"red\", width = 2), color = adjustcolor(\"red\", alpha.f = 0), size = 6), name = \"" , v_group, "\", hoverlabel = list(bgcolor = \"red\"), hoverinfo = 'text', text = l_text[[", 1:length(v_name), "]], legendgroup = \"" , v_group, "\", showlegend = ", v_show, ", visible = ", v_visible, ")"), collapse = "; ")))
					}
				}
				
				if (isolate(o_plot$add_pt) == T) {
					v_group <- unique(as.vector(l_id_group$qc2_flag$group))
					v_visible <- as.vector(df_click_legend[which(df_click_legend$name %in% v_group), "statut"])
					
					l_var <- lapply(v_group, function(i) {
						v_pos <- which(l_id_group$qc2_flag$group == i)
						v_id <- as.vector(l_id_group$qc2_flag$id)[v_pos]
						df_out <- data.frame("x" = rep(NA, length(isolate(o_plot$pt_pos)) * length(v_pos)), "y" = rep(NA, length(isolate(o_plot$pt_pos)) * length(v_pos)), "info" = rep(NA, length(isolate(o_plot$pt_pos)) * length(v_pos))) 
						df_out$x <- rep(as.vector(df_qc2$Frequency[isolate(o_plot$pt_pos)]), length(v_pos))
						df_out$y <- as.vector(unlist(df_qc2[isolate(o_plot$pt_pos), v_pos + 2]))
						df_out$info <- paste0("(", i, ")<br>id: ", rep(v_id, each = length(isolate(o_plot$pt_pos))), "<br>frequency : ", round(df_out$x, digits = i_dec_num), "<br>y: ", round(df_out$y, digits = i_dec_num))
						return(df_out)
					})
					
					eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = l_var[[", 1:length(v_group), "]]$x, y = l_var[[", 1:length(v_group), "]]$y, type = \"" , ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(line = list(color = \"red\", width = 2), color = adjustcolor(\"red\", alpha.f = 0), size = 6), name = \"", v_group, "\", hoverlabel = list(bgcolor = \"red\"), hoverinfo = 'text', text = l_var[[", 1:length(v_group), "]]$info, legendgroup = \"", v_group, "\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
				}
			}
		}
		
		# (2) add "current" flags:
		
		if ("coord" %in% ls(e_current_flag)) {
			df_all <- isolate(o_plot$data)
			df_flag <- e_current_flag$coord
			
			if (length(which(!is.na(isolate(o_plot$pt_pos)))) > 0) {
				v_id <- rep(as.vector(df_flag[, 1]), each = 2)
			}
			else {
				v_id <- as.vector(df_flag[, 1])
			}
			
			l_command <- lapply(1:length(v_id), function(x) {
				i_pos <- which(l_id_group$no_flag$id == v_id[x])
				s_group <- as.vector(l_id_group$no_flag$group[i_pos])
				
				if (x == 1) {
					v_row <- 1:dim(df_all)[1]
					s_type <- "mode = \"lines\", line = list(color = \"black\", dash = \"dash\")"
				}
				else {
					if (v_id[x] == v_id[x - 1]) {
						v_row <- isolate(o_plot$pt_pos)
						s_type <- "mode = \"markers\", marker = list(line = list(color = \"black\", width = 2), color = adjustcolor(\"black\", alpha.f = 0), size = 8)"
					}
					else {
						v_row <- 1:dim(df_all)[1]
						s_type <- "mode = \"lines\", line = list(color = \"black\", dash = \"dash\")"
					}
				}
				
				v_text <- paste0("(flag ", which(df_flag$id == v_id[x]), ")<br>id: ", v_id[x], "<br>frequency: ", round(df_all$Frequency[v_row], digits = i_dec_num), "<br>y: ", round(df_all[v_row, i_pos + 2], digits = i_dec_num))
				return(list(i_pos, s_group, v_row, s_type, v_text))
			})
			
			eval(parse(text = paste0("v_type <- c(", paste(paste0("l_command[[", 1:length(l_command), "]][[4]]"), collapse = ", "), ")"))) 
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = df_all$Frequency[l_command[[", 1:length(l_command), "]][[3]]], y = df_all[l_command[[", 1:length(l_command), "]][[3]], l_command[[", 1:length(l_command), "]][[1]] + 2], name = l_command[[", 1:length(l_command), "]][[2]], type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", ", v_type, ", hoverinfo = \"text\", hoverlabel = list(bgcolor = \"black\"), text = l_command[[", 1:length(l_command), "]][[5]], showlegend = F)"), collapse = "; ")))
		}
	}
	
	return (ply_1)
}

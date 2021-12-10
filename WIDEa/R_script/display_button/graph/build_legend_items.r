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
# Description : function used to create data corresponding to legend items. Legend items are saved in a
#               two columns data.frame (item name and status: selected/unselected on the current graph).
#               This data.frame is returned for all data type. 
#               A second element is also returned for the normal data type and concern the o_legend_group 
#               reactive value (created in the R script "WIDEa_launcher"). This reactive value returns the  
#               name of legend items assigned to 4 statistical methods. If one of these boxes are checked
#               in the Statitics tab (top panel), then the function update the corresponding reactive
#               value.      
#               The function is separated into 2 processes,
#               (1) the first process is executed when a graph is created in the main panel by clicking 
#                   on the display button (left panel); 
#               (2) the second process is executed when the graph (already created) is updated by  
#                   clicking on one of parameters in the top panel (Graphic, Flag, Statistics tabs).
#
# Creation date : March 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# i_proc_num: process number (2 values: 1, 2) 
# (o_parameter, o_cond, o_legend_group, o_plot): reactive values from the R script "WIDEa_launcher"
# df_click_legend: legend item informations (name and status)
# l_traces: list created from the "traces" input (see R script "WIDEa_launcher"). The "traces" input allows to save the status of legend items (selected/unselected).
#           The "l_traces" input returns the name of legend items with an unselected status (= "legendonly") if the "traces" input is not empty (boolean value: T). 

# Output:
# -------
# return a data.frame or a list (normal data type) corresponding to legend item informations 

f_build_legend_items <- function (s_data_type = "normal", i_proc_num = 1, o_parameter, o_cond, o_legend_group, o_plot, df_click_legend = data.frame(), l_traces = list(F, c())) {
	if (s_data_type == "normal") {
		df_all <- isolate(o_plot$data)
		
		if (i_proc_num == 1) {
			i_elt_num <- integer(0)
			b_cond_1 <- isolate(o_parameter$lreg) == T | isolate(o_parameter$conf_ellipsoid) == T
			b_cond_2 <- isolate(o_parameter$dens_curve) == T | isolate(o_parameter$norm_dens_curve) == T
			
			if (b_cond_1 | b_cond_2) {
				if (!is.na(isolate(o_parameter$group))) {
					v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
					v_group <- v_group[order(v_group)]
					df_num <- as.data.frame(addmargins(table(df_all[, isolate(o_parameter$group)])))
					df_num <- df_num[-dim(df_num)[1],]
					df_num <- df_num[order(df_num[, 1]),]
					
					if (b_cond_1) {
						v_pos <- which(df_num[, 2] > 2)
						
						if (length(v_pos) > 0) {
							eval(parse(text = paste0("v_sd_x <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), isolate(o_parameter$x)]))"), collapse = ", "), ")")))
							eval(parse(text = paste0("v_sd_y <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), isolate(o_parameter$y)]))"), collapse = ", "), ")")))
							
							if (length(unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))) > 0) {
								v_pos <- v_pos[-unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))]
							}
							
							if (length(v_pos) > 0) {
								if (isolate(o_parameter$lreg)) {
									o_legend_group$lreg <- v_group[v_pos]
								}
								
								if (isolate(o_parameter$conf_ellipsoid)) {
									o_legend_group$conf_ellipsoid <- v_group[v_pos]
								}
							}
						}
					}
					else {
						v_pos <- which(df_num[, 2] > 1)
						
						if (length(v_pos) > 0) {
							eval(parse(text = paste0("v_sd <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), isolate(o_parameter$x)]))"), collapse = ", "), ")")))
							
							if (length(which(v_sd == 0)) > 0) {
								v_pos <- v_pos[-which(v_sd == 0)]
							}
							
							if (length(v_pos) > 0) {
								if (isolate(o_parameter$dens_curve)) {
									o_legend_group$dens_curve <- v_group[v_pos]
								}
								
								if (isolate(o_parameter$norm_dens_curve)) {
									o_legend_group$norm_dens_curve <- v_group[v_pos]
								}
							}
						}
					}
				}
				else {
					if (b_cond_1) {
						if (dim(df_all)[1] > 2) {
							if (sd(as.vector(df_all[, isolate(o_parameter$x)])) > 0 & sd(as.vector(df_all[, isolate(o_parameter$y)])) > 0) {
								if (isolate(o_parameter$lreg)) {
									o_legend_group$lreg <- "all"
								}
								
								if (isolate(o_parameter$conf_ellipsoid)) {
									o_legend_group$conf_ellipsoid <- "all"
								}
							}
						}
					}
					else {
						if (dim(df_all)[1] > 1) {
							if (sd(as.vector(df_all[, isolate(o_parameter$x)])) > 0) {
								if (isolate(o_parameter$dens_curve)) {
									o_legend_group$dens_curve <- "all"
								}
								
								if (isolate(o_parameter$norm_dens_curve)) {
									o_legend_group$norm_dens_curve <- "all"
								}
							}
						}
					}
				}
			}
			
			if (length(which(isolate(o_parameter$model) == "calib")) > 0) {
				if ("variance" %in% names(isolate(o_plot$model))) {
					df_click_legend <- data.frame("name" = "all", "statut" = "T")
				}
				else {
					if (!is.na(isolate(o_parameter$group))) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
					}
					else {
						v_group <- "all"
					}
					
					v_group <- v_group[order(v_group)]
					df_click_legend <- data.frame("name" = v_group, "statut" = rep("T", length(v_group)))
				}
			}
			else {
				if (!is.na(isolate(o_parameter$group))) {
					if (isolate(o_parameter$plot_type) %in% c("boxplot", "barplot")) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
					}
					else {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
					}
				}
				else {
					if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
						v_group <- "all"
					}
					else {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
					}
				}
			
				v_group <- v_group[order(v_group)]
				df_click_legend <- data.frame("name" = v_group, "statut" = rep("T", length(v_group)))
				
				if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
					if (isolate(o_parameter$plot_type) == "plot") {
						if (isolate(o_parameter$model) == "none") {
							if (isolate(o_cond$qc2) == 1) {
								df_add <- data.frame("name" = "qc = 2", "statut" = "T")
								df_click_legend <- rbind(df_click_legend, df_add) 
							}
						}
						
						if (isolate(o_parameter$lreg)) {
							if (length(isolate(o_legend_group$lreg)) > 0) {
								df_add <- data.frame("name" = paste0(isolate(o_legend_group$lreg), " (lreg)"), "statut" = rep("T", length(isolate(o_legend_group$lreg))))
								df_click_legend <- rbind(df_click_legend, df_add)
							}
						}
						
						if (isolate(o_parameter$conf_ellipsoid)) {
							if (length(isolate(o_legend_group$conf_ellipsoid)) > 0) {
								df_add <- data.frame("name" = paste0(isolate(o_legend_group$conf_ellipsoid), " (ellipsoid)"), "statut" = rep("T", length(isolate(o_legend_group$conf_ellipsoid))))
								df_click_legend <- rbind(df_click_legend, df_add)
							}
						}
						
						if (isolate(o_parameter$centroid)) {
							df_add <- data.frame("name" = paste0(v_group, " (centroid)"), "statut" = rep("T", length(v_group)))
							df_click_legend <- rbind(df_click_legend, df_add)
						}
					}
					else {
						if (isolate(o_parameter$dens_curve)) {
							if (length(isolate(o_legend_group$dens_curve)) > 0) {
								df_add <- data.frame("name" = paste0(isolate(o_legend_group$dens_curve), " (curve)"), "statut" = rep("T", length(isolate(o_legend_group$dens_curve))))
								df_click_legend <- rbind(df_click_legend, df_add)
							}
						}
						
						if (isolate(o_parameter$norm_dens_curve)) {
							if (length(isolate(o_legend_group$norm_dens_curve)) > 0) {
								df_add <- data.frame("name" = paste0(isolate(o_legend_group$norm_dens_curve), " (normal curve)"), "statut" = rep("T", length(isolate(o_legend_group$norm_dens_curve))))
								df_click_legend <- rbind(df_click_legend, df_add)
							}
						}
					}
				}
			}
		}
		else { # process = 2
			df_click_legend$statut <- "T"
			
			if (isolate(o_cond$stat) > 0) {
				v_legend_group <- c()
				
				if (isolate(o_parameter$plot_type) == "plot" & isolate(o_cond$qc2) == 1) {
					i_qc <- 1
				}
				else {
					i_qc <- 0
				}
				
				if (!is.na(isolate(o_parameter$group))) {
					v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
					v_group <- v_group[order(v_group)]
				}
				else {
					v_group <- "all"
				}
				
				if (isolate(o_parameter$plot_type) == "plot") {
					if (isolate(o_parameter$dim_num) == "3d") {
						if (isolate(o_parameter$centroid)) {
							df_add <- data.frame("name" = paste0(v_group, " (centroid)"), "statut" = rep("T", length(v_group)))
							df_click_legend <- rbind(df_click_legend, df_add)
						}
						else {
							df_click_legend <- df_click_legend[c(1:(length(v_group) + i_qc)),]
						}
					}
					else {
						v_pos_1 <- grep("[(]lreg[)]", df_click_legend$name)
						v_pos_2 <- grep("[(]ellipsoid[)]", df_click_legend$name)
						v_pos_3 <- grep("[(]centroid[)]", df_click_legend$name)
						v_num_1 <- c(ifelse(isolate(o_parameter$lreg), 1, 0), ifelse(isolate(o_parameter$conf_ellipsoid), 1, 0), ifelse(isolate(o_parameter$centroid), 1, 0))
						v_num_2 <- c(ifelse(length(v_pos_1) > 0, 1, 0), ifelse(length(v_pos_2) > 0, 1, 0), ifelse(length(v_pos_3) > 0, 1, 0))
						v_num <- v_num_1 + v_num_2
						v_pos <- which(v_num == 1)
						
						if (v_pos %in% c(1, 2)) {
							if (v_num_1[v_pos] == 1) {
								if (sum(v_num_1[1:2]) == 2) {
									if (v_pos == 1) {
										v_legend_group <- isolate(o_legend_group$conf_ellipsoid)
									}
									else {
										v_legend_group <- isolate(o_legend_group$lreg)
									}
								}
								else {
									if (!is.na(isolate(o_parameter$group))) {
										df_num <- as.data.frame(addmargins(table(df_all[, isolate(o_parameter$group)])))
										df_num <- df_num[-dim(df_num)[1],]
										df_num <- df_num[order(df_num[, 1]),]
										v_pos_4 <- which(df_num[, 2] > 2)
										
										if (length(v_pos_4) > 0) {
											eval(parse(text = paste0("v_sd_x <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos_4], "\"), isolate(o_parameter$x)]))"), collapse = ", "), ")")))
											eval(parse(text = paste0("v_sd_y <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos_4], "\"), isolate(o_parameter$y)]))"), collapse = ", "), ")")))
											
											if (length(unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))) > 0) {
												v_pos_4 <- v_pos_4[-unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))]
											}
											
											if (length(v_pos_4) > 0) {
												v_legend_group <- v_group[v_pos_4]
											}
										}
									}
									else {
										if (dim(df_all)[1] > 2) {
											if (sd(as.vector(df_all[, isolate(o_parameter$x)])) > 0 & sd(as.vector(df_all[, isolate(o_parameter$y)])) > 0) {
												v_legend_group <- "all"
											}
										}
									}
								}
								
								if (length(v_legend_group) > 0) {
									if (v_pos == 1) {
										df_add <- data.frame("name" = paste0(v_legend_group, " (lreg)"), "statut" = rep("T", length(v_legend_group)))
										eval(parse(text = paste0("df_click_legend <- rbind(df_click_legend[1:(length(v_group) + i_qc),], df_add", ifelse(length(v_pos_2) > 0, ", df_click_legend[v_pos_2,]", ""), ifelse(length(v_pos_3) > 0, ", df_click_legend[v_pos_3,]", ""), ")")))
									}
									else {
										df_add <- data.frame("name" = paste0(v_legend_group, " (ellipsoid)"), "statut" = rep("T", length(v_legend_group)))
										eval(parse(text = paste0("df_click_legend <- rbind(df_click_legend[1:(length(v_group) + i_qc),]", ifelse(length(v_pos_1) > 0, ", df_click_legend[v_pos_1,]", ""), ", df_add", ifelse(length(v_pos_3) > 0, ", df_click_legend[v_pos_3,]", ""), ")")))
									}
								}
							}
							else {
								if (v_pos == 1) {
									df_click_legend <- df_click_legend[-v_pos_1,]
								}
								else {
									df_click_legend <- df_click_legend[-v_pos_2,]
								}
							}
						}
						else {
							if (v_num_1[v_pos] == 1) {
								df_add <- data.frame("name" = paste0(v_group, " (centroid)"), "statut" = rep("T", length(v_group)))
								df_click_legend <- rbind(df_click_legend, df_add)
							}
							else {
								df_click_legend <- df_click_legend[-v_pos_3,]
							}
						}
					}
				}
				else {
					if (isolate(o_parameter$dens_curve) == T | isolate(o_parameter$norm_dens_curve) == T) {
						v_pos_1 <- grep("[(]curve[)]", df_click_legend$name)
						v_pos_2 <- grep("[(]normal curve[)]", df_click_legend$name)
					
						if (isolate(o_parameter$dens_curve) == T & isolate(o_parameter$norm_dens_curve) == T) {
							if (length(v_pos_1) == 0 | length(v_pos_2) == 0) {
								if (length(v_pos_1) == 0) {
									v_legend_group <- isolate(o_legend_group$norm_dens_curve)
									df_add <- data.frame("name" = paste0(v_legend_group, " (curve)"), "statut" = rep("T", length(v_legend_group)))
									df_click_legend <- rbind(df_click_legend[1:length(v_group),], df_add, df_click_legend[v_pos_2,])
								}
								else {
									v_legend_group <- isolate(o_legend_group$dens_curve)
									df_add <- data.frame("name" = paste0(v_legend_group, " (normal curve)"), "statut" = rep("T", length(v_legend_group)))
									df_click_legend <- rbind(df_click_legend, df_add)
								}
							}
						}
						else if (isolate(o_parameter$dens_curve) == T & isolate(o_parameter$norm_dens_curve) == F) {
							if (length(v_pos_1) == 0 | length(v_pos_2) > 0) {
								if (!is.na(isolate(o_parameter$group))) {
									if (length(v_pos_1) == 0) {
										df_num <- as.data.frame(addmargins(table(df_all[, isolate(o_parameter$group)])))
										df_num <- df_num[-dim(df_num)[1],]
										v_pos_3 <- which(df_num[, 2] > 1)
										
										if (length(v_pos_3) > 0) {
											v_legend_group <- v_group[which(v_group %in% df_num[v_pos_3, 1])]
											eval(parse(text = paste0("v_sd <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_legend_group, "\"), isolate(o_parameter$x)]))"), collapse = ", "), ")")))
											v_pos_3 <- which(v_sd > 0)
											
											if (length(v_pos_3) > 0) {
												v_legend_group <- v_legend_group[v_pos_3]
												df_add <- data.frame("name" = paste0(v_legend_group, " (curve)"), "statut" = rep("T", length(v_legend_group)))
												df_click_legend <- rbind(df_click_legend, df_add)
											}
										}
									}
									else {
										df_click_legend <- df_click_legend[-v_pos_2,]
									}
								}
								else {
									if (length(v_pos_1) == 0) {
										if (dim(df_all)[1] > 1) {
											if (sd(as.vector(df_all[, isolate(o_parameter$x)])) > 0) {
												v_legend_group <- "all"
												df_add <- data.frame("name" = "all (curve)", "statut" = "T")
												df_click_legend <- rbind(df_click_legend, df_add)
											}
										}
									}
									else {
										df_click_legend <- df_click_legend[-v_pos_2,]
									}
								}
							}
						}
						else {
							if (length(v_pos_1) > 0 | length(v_pos_2) == 0) {
								if (!is.na(isolate(o_parameter$group))) {
									if (length(v_pos_1) > 0) {
										df_click_legend <- df_click_legend[-v_pos_1,]
									}
									else {
										df_num <- as.data.frame(addmargins(table(df_all[, isolate(o_parameter$group)])))
										df_num <- df_num[-dim(df_num)[1],]
										v_pos_3 <- which(df_num[, 2] > 1)
										
										if (length(v_pos_3) > 0) {
											v_legend_group <- v_group[which(v_group %in% df_num[v_pos_3, 1])]
											eval(parse(text = paste0("v_sd <- c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_legend_group, "\"), isolate(o_parameter$x)]))"), collapse = ", "), ")")))
											v_pos_3 <- which(v_sd > 0)
											
											if (length(v_pos_3) > 0) {
												v_legend_group <- v_legend_group[v_pos_3]
												df_add <- data.frame("name" = paste0(v_legend_group, " (normal curve)"), "statut" = rep("T", length(v_legend_group)))
												df_click_legend <- rbind(df_click_legend, df_add)
											}
										}
									}
								}
								else {
									if (length(v_pos_1) > 0) {
										df_click_legend <- df_click_legend[-v_pos_1,]
									}
									else {
										if (dim(df_all)[1] > 1) {
											if (sd(as.vector(df_all[, isolate(o_parameter$x)])) > 0) {
												v_legend_group <- "all"
												df_add <- data.frame("name" = "all (normal curve)", "statut" = "T")
												df_click_legend <- rbind(df_click_legend, df_add)
											}
										}
									}
								}
							}
						}
					}
					else {
						df_click_legend <- df_click_legend[1:(length(v_group) + i_qc),]
					}
				}
				
				if (isolate(o_cond$stat) < 3) {
					s_stat_name <- list(c("lreg", "conf_ellipsoid"), c("dens_curve", "norm_dens_curve"))[[ifelse(isolate(o_parameter$plot_type) == "histplot", 2, 1)]][isolate(o_cond$stat)]
					o_legend_group[[s_stat_name]] <- v_legend_group
				}
			}
			
			if (l_traces[[1]]) {
				v_traces <- l_traces[[2]]
				
				if (length(v_traces) > 0) {
					if (isolate(o_cond$qc2) == 1) {
						if (!is.na(isolate(o_parameter$group))) {
							v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
							v_group <- v_group[order(v_group)]
						}
						else {
							v_group <- "all"
						}
						
						v_group <- c(v_group, "qc = 2")
						
						if (isolate(o_parameter$centroid)) {
							v_group_centroid <- v_group
						}
						
						if (length(isolate(o_legend_group$lreg)) > 0 | length(isolate(o_legend_group$conf_ellipsoid)) > 0 | isolate(o_parameter$centroid) == T) {
							if (length(isolate(o_legend_group$lreg)) > 0) {
								v_group <- c(v_group, paste0(isolate(o_legend_group$lreg), " (lreg)"))
							}
							
							if (length(isolate(o_legend_group$conf_ellipsoid)) > 0) {
								v_group <- c(v_group, paste0(isolate(o_legend_group$conf_ellipsoid), " (ellipsoid)"))
							}
							
							if (isolate(o_parameter$centroid)) {
								v_group <- c(v_group, paste0(v_group_centroid, " (centroid)"))
							}
						}
						
						v_pos <- which(v_group %in% v_traces)
					}
					else {
						if (paste(isolate(o_parameter$model), isolate(o_parameter$select_graph), sep = "_") != "calib_QQplot") {
							if (!is.na(isolate(o_parameter$group))) {
								if (isolate(o_parameter$plot_type) %in% c("boxplot", "barplot")) {
									v_group <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
								}
								else {
									v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
								}
								
								v_group <- v_group[order(v_group)]
								
								if (isolate(o_parameter$centroid)) {
									v_group_centroid <- v_group
								}
								
								if (length(isolate(o_legend_group$lreg)) > 0 | length(isolate(o_legend_group$conf_ellipsoid)) > 0 | isolate(o_parameter$centroid) == T | length(isolate(o_legend_group$dens_curve)) > 0 | length(isolate(o_legend_group$norm_dens_curve)) > 0) {
									if (length(isolate(o_legend_group$lreg)) > 0) {
										v_group <- c(v_group, paste0(isolate(o_legend_group$lreg), " (lreg)"))
									}
									
									if (length(isolate(o_legend_group$conf_ellipsoid)) > 0) {
										v_group <- c(v_group, paste0(isolate(o_legend_group$conf_ellipsoid), " (ellipsoid)"))
									}
									
									if (isolate(o_parameter$centroid)) {
										v_group <- c(v_group, paste0(v_group_centroid, " (centroid)"))
									}
									
									if (length(isolate(o_legend_group$dens_curve)) > 0) {
										v_group <- c(v_group, paste0(isolate(o_legend_group$dens_curve), " (curve)"))
									}
									
									if (length(isolate(o_legend_group$norm_dens_curve)) > 0) {
										v_group <- c(v_group, paste0(isolate(o_legend_group$norm_dens_curve), " (normal curve)"))
									}
								}
							}
							else {
								if (isolate(o_parameter$plot_type) %in% c("plot", "histplot")) {
									v_group <- "all"
									
									if (length(isolate(o_legend_group$lreg)) > 0 | length(isolate(o_legend_group$conf_ellipsoid)) > 0 | isolate(o_parameter$centroid) == T | length(isolate(o_legend_group$dens_curve)) > 0 | length(isolate(o_legend_group$norm_dens_curve)) > 0) {
										if (length(isolate(o_legend_group$lreg)) > 0) {
											v_group <- c(v_group, paste0(isolate(o_legend_group$lreg), " (lreg)"))
										}
										
										if (length(isolate(o_legend_group$conf_ellipsoid)) > 0) {
											v_group <- c(v_group, paste0(isolate(o_legend_group$conf_ellipsoid), " (ellipsoid)"))
										}
										
										if (isolate(o_parameter$centroid)) {
											v_group <- c(v_group, "all (centroid)")
										}
										
										if (length(isolate(o_legend_group$dens_curve)) > 0) {
											v_group <- c(v_group, paste0(isolate(o_legend_group$dens_curve), " (curve)"))
										}
										
										if (length(isolate(o_legend_group$norm_dens_curve)) > 0) {
											v_group <- c(v_group, paste0(isolate(o_legend_group$norm_dens_curve), " (normal curve)"))
										}
									}
								}
								else {
									v_group <- as.vector(unique(df_all[, isolate(o_parameter$x)]))
									v_group <- v_group[order(v_group)]
								}
							}
							
							v_pos <- which(v_group %in% v_traces)
						}
						else {
							v_pos <- c()
							df_click_legend <- data.frame("name" = "all", "statut" = "T")
						}
					}
					
					if (length(v_pos) > 0) {
						df_click_legend[v_pos, "statut"] <- "\"legendonly\""
					}
				}
			}
			else {
				if (!is.na(isolate(o_parameter$group)) & !is.na(isolate(o_parameter$model)) & isolate(o_parameter$model) == "calib") {
					if (isolate(o_parameter$select_graph) == "QQplot") {
						df_click_legend <- data.frame("name" = "all", "statut" = "T")
					}
					else {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						
						if (length(which(v_group %in% as.vector(df_click_legend$name))) != length(v_group)) {
							v_group <- v_group[order(v_group)]
							df_click_legend <- data.frame("name" = v_group, "statut" = rep("T", length(v_group)))
						}
					}
				}
			}
		}
		
		return(list(o_legend_group, df_click_legend))
	}
	else if (s_data_type == "temporal") {
		if (i_proc_num == 1) {
			if (isolate(o_cond$flag) == 1) {
				df_click_legend <- data.frame("name" = c(isolate(o_parameter$y), isolate(o_plot$leg_name_qc)), "statut" = rep("T", length(c(isolate(o_parameter$y), isolate(o_plot$leg_name_qc)))))
			}
			else {
				df_click_legend <- data.frame("name" = isolate(o_parameter$y), "statut" = rep("T", length(isolate(o_parameter$y))))
			}
			
			df_click_legend$name <- as.vector(df_click_legend$name)
			df_click_legend$statut <- as.vector(df_click_legend$statut)
		}
		else { # process = 2
			df_click_legend$statut <- "T"
			
			if (l_traces[[1]]) {
				v_traces <- l_traces[[2]]
				
				if (length(v_traces) > 0) {
					if (isolate(o_cond$flag) == 1) {
						v_pos <- which(c(isolate(o_parameter$y), isolate(o_plot$leg_name_qc)) %in% v_traces)
					}
					else {
						v_pos <- which(isolate(o_parameter$y) %in% v_traces)
					}
					
					if (length(v_pos) > 0) {
						df_click_legend[v_pos, "statut"] <- "\"legendonly\""
					}
				}
			}
		}
		
		return(df_click_legend)
	}
	else { # ir
		if (i_proc_num == 1) {
			v_name <- names(isolate(o_plot$elt))[which(as.vector(t(colSums(isolate(o_plot$elt)[, -1]))) > 0) + 1]
			eval(parse(text = paste0("v_elt <- c(", paste(paste0("as.vector(unique(isolate(o_plot$id_group)$", v_name, "$group))"), collapse = ", "), ")")))  
			
			if (isolate(o_parameter$mean_spect)) {
				v_pos <- c(grep("qc = 1", v_elt), grep("qc = 2", v_elt))
				
				if (length(v_pos) > 0) {
					v_elt <- c(v_elt, paste0(v_elt[-v_pos], " (mean)"))
				}
				else {
					v_elt <- c(v_elt, paste0(v_elt, " (mean)"))
				}
			}
			
			df_click_legend <- data.frame("name" = v_elt, "statut" = rep("T", length(v_elt)))
			df_click_legend$name <- as.vector(df_click_legend$name)
			df_click_legend$statut <- as.vector(df_click_legend$statut)
		}
		else { # process = 2
			df_click_legend$statut <- "T"
			v_pos <- grep("[(]mean[)]", df_click_legend$name)
			
			if (isolate(o_parameter$mean_spect)) {
				if (length(v_pos) == 0) {
					v_pos <- c(grep("qc = 1", df_click_legend$name), grep("qc = 2", df_click_legend$name))
					
					if (length(v_pos) > 0) {
						df_add <- data.frame("name" = paste0(df_click_legend$name[-v_pos], " (mean)"))
					}
					else {
						df_add <- data.frame("name" = paste0(df_click_legend$name, " (mean)"))
					}
					
					df_add$statut <- "T"
					df_click_legend <- rbind(df_click_legend, df_add)
				}
			}
			else {
				if (length(v_pos) > 0) {
					df_click_legend <- df_click_legend[-v_pos,]
				}
			}
			
			if (l_traces[[1]]) {
				v_traces <- l_traces[[2]]
				
				if (length(v_traces) > 0) {
					v_pos <- which(df_click_legend$name %in% v_traces)
					
					if (length(v_pos) > 0) {
						df_click_legend[v_pos, "statut"] <- "\"legendonly\""
					}
				}
			}
		}
		
		return(df_click_legend)
	}
}

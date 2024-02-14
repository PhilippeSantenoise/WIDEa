#' @importFrom shiny isolate
#' @importFrom stats sd
NULL

#' Building legend item data of the main plotly graph

#' @description
#' `f_build_legend_items` is used to build legend item data and update the
#' o_stat_method reactive value (created in the "WIDEa_server" R script) associated
#' to statistical methods (Statistics tab). Legend item data are saved in a two
#' columns data frame (item name and status: selected/unselected on the main plotly
#' graph). This data frame is returned for all data type. The function is separated
#' into 2 processes:
#' \cr(1) The first process is executed when a graph is created in the main panel by
#' clicking on the display button (left panel); 
#' \cr(2) The second process is executed when the current graph is updated by
#' (de)selecting a statistical method from the Statistics tab (top panel) and
#' changing the graph when a calibration model is added (normal data type).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param i_proc_num is the process number (2 values: 1, 2). 
#' @param o_plot is a reactive value including data information corresponding to the
#' main plotly graph.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param o_cond is a reactive value including binary values used as conditions in
#' the server (related to action buttons).
#' @param o_stat_method is a reactive value including data information on statistic
#' methods added to the main plotly graph. 
#' @param df_click_legend are legend item data information (name and status).

#' @encoding UTF-8

f_build_legend_items <- function (s_data_type = "normal", i_proc_num = 1, o_parameter, o_cond, o_plot, o_stat_method = NULL, df_click_legend = data.frame()) {
	if (s_data_type == "normal") {
		df_all <- isolate(o_plot$data)
		df_model <- isolate(o_plot$model)
		df_stat_method <- o_stat_method$inv
		l_stat_method_level <- o_stat_method$level
		l_stat_method_message <- o_stat_method$message
		df_message <- data.frame() # data frame including warning/error messages for statistical methods with preliminary checks
		v_stat_method_uncheck <- c() # vector used to uncheck the box of statistic method if an error message is returned
		v_pos <- which(df_stat_method$click == 1 & df_stat_method$check_process == 1)
		
		if (length(v_pos) > 0) {
			v_stat_method_name <- df_stat_method$name[v_pos]
			v_pos <- eval(parse(text = paste0("which(c(", paste(paste0("o_parameter$", v_stat_method_name), collapse = ", "), "))")))
			
			if (length(v_pos) > 0) { # update o_stat_method$level (levels for statistical methods after preliminary checks)
				v_name <- v_stat_method_name[v_pos]
				v_stat_method_cp <- as.vector(sort(unique(df_stat_method[which(df_stat_method$name %in% v_name), "code"])))
				v_pos <- which(as.vector(lengths(l_stat_method_level[v_stat_method_cp])) == 0)
				
				if (length(v_pos) > 0) {
					v_stat_method_cp <- v_stat_method_cp[v_pos]
					s_data_x <- ifelse(length(which(isolate(o_parameter$model) == "valid")) == 1, "df_model", "df_all")
					s_var_x <- ifelse(length(which(isolate(o_parameter$model) == "valid")) == 1, "fit", ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)))
					
					if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						df_num <- as.data.frame(stats::addmargins(table(df_all[, isolate(o_parameter$group)])))
						df_num <- df_num[-dim(df_num)[1],]
						df_num <- df_num[order(df_num[, 1]),]
					}
					
					if ("cp1" %in% v_stat_method_cp) { # check process cp1 for the following stat methods: lreg, ellipsoid
						s_var_y <- ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y))
						
						if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
							v_pos <- which(df_num[, 2] > 2)
						
							if (length(v_pos) > 0) {
								v_sd_x <- eval(parse(text = paste0("c(", paste(paste0("sd(as.vector(", s_data_x, "[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), \"", s_var_x, "\"]))"), collapse = ", "), ")")))
								v_sd_y <- eval(parse(text = paste0("c(", paste(paste0("sd(as.vector(df_all[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), \"", s_var_y, "\"]))"), collapse = ", "), ")")))
								if (length(unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))) > 0) {v_pos <- v_pos[-unique(c(which(v_sd_x == 0), which(v_sd_y == 0)))]}
								
								if (length(v_pos) > 0) {
									l_stat_method_level[["cp1"]] <- v_group[v_pos]
								}
								else {
									df_stat_method[which(df_stat_method$code == "cp1"), "check_process"] <- (-1)
								}
							}
							else {
								df_stat_method[which(df_stat_method$code == "cp1"), "check_process"] <- (-1)
							}
						}
						else {
							if (dim(df_all)[1] > 2) {
								b_sd <- eval(parse(text = paste0("sd(as.vector(", s_data_x, "[, \"", s_var_x, "\"])) > 0 & sd(as.vector(df_all[, \"", s_var_y, "\"])) > 0")))
								
								if (b_sd) {
									l_stat_method_level[["cp1"]] <- "all"
								}
								else {
									df_stat_method[which(df_stat_method$code == "cp1"), "check_process"] <- (-1)
								}
							}
							else {
								df_stat_method[which(df_stat_method$code == "cp1"), "check_process"] <- (-1)
							}
						}
					}
					
					if ("cp2" %in% v_stat_method_cp) { # check process cp2 for the following stat methods: dens_curve, norm_dens_curve
						if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
							v_pos <- which(df_num[, 2] > 1)
							
							if (length(v_pos) > 0) {
								v_sd <- eval(parse(text = paste0("c(", paste(paste0("sd(as.vector(", s_data_x, "[which(df_all[, isolate(o_parameter$group)] == \"", v_group[v_pos], "\"), \"", s_var_x, "\"]))"), collapse = ", "), ")")))
								if (length(which(v_sd == 0)) > 0) {v_pos <- v_pos[-which(v_sd == 0)]}
								
								if (length(v_pos) > 0) {
									l_stat_method_level[["cp2"]] <- v_group[v_pos]
								}
								else {
									df_stat_method[which(df_stat_method$code == "cp2"), "check_process"] <- (-1)
								}
							}
							else {
								df_stat_method[which(df_stat_method$code == "cp2"), "check_process"] <- (-1)
							}
						}
						else {
							if (dim(df_all)[1] > 1) {
								b_sd <- eval(parse(text = paste0("sd(as.vector(", s_data_x, "[, \"", s_var_x, "\"])) > 0")))
								
								if (b_sd) {
									l_stat_method_level[["cp2"]] <- "all"
								}
								else {
									df_stat_method[which(df_stat_method$code == "cp2"), "check_process"] <- (-1)
								}
							}
							else {
								df_stat_method[which(df_stat_method$code == "cp2"), "check_process"] <- (-1)
							}
						}
					}
				}
				
				v_pos <- which(df_stat_method$click == 1 & df_stat_method$check_process == (-1))
				if (length(v_pos) > 0) {v_stat_method_uncheck <- df_stat_method$name[v_pos]}
				
				# update l_stat_method_message
				
				if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
					v_pos <- which(as.vector(unlist(l_stat_method_message)) == 0)
					
					if (length(v_pos) > 0) {
						v_pos <- which(v_name %in% names(l_stat_method_message)[v_pos])
						
						if (length(v_pos) > 0) {
							v_name <- v_name[v_pos]
							v_code <- unique(as.vector(df_stat_method[df_stat_method$name %in% v_name, "code"]))
							v_pos <- which(as.vector(lengths(l_stat_method_level[v_code])) == length(as.vector(unique(df_all[, isolate(o_parameter$group)]))))
							
							if (length(v_pos) > 0) {
								v_name_ok <- as.vector(df_stat_method[df_stat_method$name %in% v_name & df_stat_method$code %in% v_code[v_pos], "name"])
								l_stat_method_message[v_name_ok] <- (-1)
							}
							
							if (length(v_code) > length(v_pos)) {
								if (length(v_pos) > 0) {v_code <- v_code[-v_pos]}
								v_name <- as.vector(df_stat_method[df_stat_method$name %in% v_name & df_stat_method$code %in% v_code, "name"])
								df_message <- f_create_stat_method_message(df_stat_method, l_stat_method_level, v_name, o_parameter, df_all)
								l_stat_method_message[v_name] <- 1
							}
						}
					}
				}
				else {
					if (length(v_stat_method_uncheck) > 0) {
						df_message <- f_create_stat_method_message(df_stat_method, l_stat_method_level, v_stat_method_uncheck, o_parameter, df_all)
					}
				}
			}
		}
		
		if (i_proc_num == 1) {  # build legend item data
			if (length(which(isolate(o_parameter$model) == "calib")) > 0) {
				if ("variance" %in% names(df_model)) {
					df_click_legend <- data.frame("name" = "all", "statut" = "T")
				}
				else {
					if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
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
				if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
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
				
				if (nrow(df_click_legend) == 0) {
					df_click_legend <- data.frame("name" = v_group, "statut" = rep("T", length(v_group)))
					if (isolate(o_parameter$plot_type) == "plot" & isolate(o_parameter$model) == "none" & isolate(o_cond$qc2) == 1) {df_click_legend <- rbind(df_click_legend, data.frame("name" = "qc = 2", "statut" = "T"))}
				}
				else {
					v_status <- c(df_click_legend[df_click_legend$name %in% v_group, "statut"], ifelse("qc = 2" %in% df_click_legend$name, df_click_legend[df_click_legend$name == "qc = 2", "statut"], "T"))
					df_click_legend <- data.frame("name" = c(v_group, "qc = 2"), "statut" = v_status)
				}
			}
			
			v_pos <- which(df_stat_method$click == 1 & df_stat_method$check_process != (-1))
			
			if (length(v_pos) > 0) { # add legend items associated to statistical methods
				v_name_clicked <- as.vector(df_stat_method$name[v_pos])
				v_leg_name_clicked <- as.vector(df_stat_method$leg_name[v_pos])
				v_name <- paste0(rep(v_group, length(v_name_clicked)), " (", rep(v_leg_name_clicked, each = length(v_group)), ")")
				v_pos <- which(df_stat_method$name %in% v_name_clicked & df_stat_method$check_process == 1) # statistic methods with check process 
				
				if (length(v_pos) > 0) {
					v_name_sub <- as.vector(df_stat_method$name[v_pos])
					v_leg_name_sub <- as.vector(df_stat_method$leg_name[v_pos])
					l_sub <- l_stat_method_level[as.vector(df_stat_method[which(df_stat_method$name %in% v_name_sub), "code"])]
					names(l_sub) <- v_name_sub
					v_size <- as.vector(lengths(l_sub))
					v_name_proc_1 <- eval(parse(text = paste0("c(", paste(paste0("rep(\"", names(l_sub), "\", ", v_size,")"), collapse = ", "), ")"))) 
					v_name_proc_1 <- paste0(as.vector(unlist(l_sub)), " (", v_leg_name_sub[match(v_name_proc_1, v_name_sub)], ")")
					v_name_proc_2 <- paste0(rep(v_group, length(v_leg_name_sub)), " (", rep(v_leg_name_sub, each = length(v_group)), ")")
					v_pos <- which(!v_name_proc_2 %in% v_name_proc_1)
					if (length(v_pos) > 0) {v_name <- v_name[!v_name %in% v_name_proc_2[v_pos]]}
				}
				
				df_click_legend <- rbind(df_click_legend, data.frame("name" = v_name, "statut" = rep("T", length(v_name))))
			}
		}
		else { # process = 2: update legend item data
			if (length(which(isolate(o_parameter$model) == "calib")) > 0) { # legend items for the calibration model
				if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
					if (isolate(o_parameter$select_graph) == "QQplot") {
						df_click_legend <- data.frame("name" = "all", "statut" = "T")
					}
					else {
						v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
						v_group <- v_group[order(v_group)]
						df_click_legend <- data.frame("name" = v_group, "statut" = rep("T", length(v_group)))
					}
				}
			}
			
			i_pos <- which(df_stat_method$click == 1)
			
			if (length(i_pos) > 0) { # legend items for the statistical method
				s_name <- as.vector(df_stat_method[i_pos, "name"])
				s_leg_name <- as.vector(df_stat_method[i_pos, "leg_name"])
				
				if (o_parameter[[s_name]]) {
					v_name <- as.vector(df_stat_method[df_stat_method$data_type == "normal", "name"])
					v_leg_name <- as.vector(df_stat_method[df_stat_method$data_type == "normal", "leg_name"])
					i_pos <- which(v_name == s_name)
					l_pos <- eval(parse(text = paste0("list(", paste(paste0("grep(\" [(]", v_leg_name, "[)]\", df_click_legend$name)"), collapse = ", "), ")")))
					v_size <- as.vector(lengths(l_pos))
					i_qc <- ifelse(isolate(o_parameter$plot_type) == "plot" & isolate(o_cond$qc2) == 1, 1, 0)
					i_nlevel <- ifelse(sum(v_size) == 0, nrow(df_click_legend), as.vector(unlist(l_pos))[1] - 1) - i_qc
					i_code <- as.vector(df_stat_method[df_stat_method$name == s_name, "code"])
					
					if (is.na(i_code)) {
						v_group <- as.vector(df_click_legend[1:i_nlevel, "name"])
					}
					else {
						v_group <- l_stat_method_level[[i_code]]
					}
					
					if (sum(v_size) == 0) {
						df_click_legend <- rbind(df_click_legend, data.frame("name" = paste0(v_group, " (", s_leg_name, ")"), "statut" = "T"))
					}
					else {
						b_cond <- ifelse(i_pos == 1, T, ifelse(sum(v_size[1:(i_pos - 1)]) == 0, T, F))
						
						if (b_cond) {
							df_click_legend <- rbind(df_click_legend[1:(i_nlevel + i_qc),], data.frame("name" = paste0(v_group, " (", s_leg_name, ")"), "statut" = "T"), df_click_legend[(i_nlevel + i_qc + 1):nrow(df_click_legend),])
						}
						else {
							b_cond <- ifelse(i_pos == length(l_pos), T, ifelse(sum(v_size[(i_pos + 1):length(l_pos)]) == 0, T, F))
							
							if (b_cond) {
								df_click_legend <- rbind(df_click_legend, data.frame("name" = paste0(v_group, " (", s_leg_name, ")"), "statut" = "T"))
							}
							else {
								v_pos_1 <- as.vector(unlist(l_pos[1:(i_pos - 1)]))
								v_pos_2 <- as.vector(unlist(l_pos[(i_pos + 1):length(l_pos)]))
								df_click_legend <- rbind(df_click_legend[1:v_pos_1[length(v_pos_1)],], data.frame("name" = paste0(v_group, " (", s_leg_name, ")"), "statut" = "T"), df_click_legend[v_pos_2[1]:nrow(df_click_legend),])
							}
						}
					}
				}
				else {
					v_pos <- grep(paste0(" [(]", s_leg_name, "[)]"), df_click_legend$name)
					df_click_legend <- df_click_legend[-v_pos,]
					df_stat_method[which(df_stat_method$name == s_name), "click"] <- 0
				}
			}
		}
		
		df_stat_method$click <- 0
		return(list(df_stat_method, l_stat_method_level, l_stat_method_message, df_message, v_stat_method_uncheck, df_click_legend))
	}
	else if (s_data_type == "temporal") {
		if (i_proc_num == 1) {
			if (isolate(o_cond$flag) == 1) {
				v_name <- c(isolate(o_parameter$y), isolate(o_plot$leg_name_qc))
				v_status <- rep("T", length(v_name))
				
				if (nrow(df_click_legend) == 0) {
					df_click_legend <- data.frame("name" = v_name, "statut" = v_status)
				}
				else {
					names(v_status) <- v_name
					v_pos <- which(df_click_legend$name %in% v_name)
					v_status[df_click_legend$name[v_pos]] <- df_click_legend[v_pos, "statut"]
					df_click_legend <- data.frame("name" = v_name, "statut" = as.vector(v_status))
				}
			}
			else {
				df_click_legend <- data.frame("name" = isolate(o_parameter$y), "statut" = rep("T", length(isolate(o_parameter$y))))
			}
			
			df_click_legend$name <- as.vector(df_click_legend$name)
			df_click_legend$statut <- as.vector(df_click_legend$statut)
		}
		
		return(df_click_legend)
	}
	else { # ir
		if (i_proc_num == 1) {
			v_name <- names(isolate(o_plot$elt))[which(as.vector(t(colSums(isolate(o_plot$elt)[, -1]))) > 0) + 1]
			v_elt <- eval(parse(text = paste0("c(", paste(paste0("as.vector(unique(isolate(o_plot$id_group)$", v_name, "$group))"), collapse = ", "), ")")))  
			
			if (isolate(o_parameter$mean_spect)) {
				v_pos <- c(grep("qc = 1", v_elt), grep("qc = 2", v_elt))
				
				if (length(v_pos) > 0) {
					v_elt <- c(v_elt, paste0(v_elt[-v_pos], " (mean)"))
				}
				else {
					v_elt <- c(v_elt, paste0(v_elt, " (mean)"))
				}
			}
			
			v_status <- rep("T", length(v_elt))
			
			if (nrow(df_click_legend) == 0) {
				df_click_legend <- data.frame("name" = v_elt, "statut" = v_status)
			}
			else {
				names(v_status) <- v_elt
				v_pos <- which(df_click_legend$name %in% v_elt)
				v_status[df_click_legend$name[v_pos]] <- df_click_legend[v_pos, "statut"]
				df_click_legend <- data.frame("name" = v_elt, "statut" = as.vector(v_status))
			}
			
			df_click_legend$name <- as.vector(df_click_legend$name)
			df_click_legend$statut <- as.vector(df_click_legend$statut)
		}
		else { # process = 2
			v_pos <- grep(" [(]mean[)]", df_click_legend$name)
			
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
		}
		
		return(df_click_legend)
	}
}

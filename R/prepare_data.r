#' @importFrom shiny isolate
NULL

#' Data preparation for the main plotly graph

#' @description
#' `f_prepare_data` is used to prepare data from data (raw/flag) saved in the e_data
#' environment. All data created from this function are saved in the o_plot
#' reactive value ("data" for raw data, "data_qc1" and "data_qc2" for flag data with
#' a qc = 1 or 2 value). The data preparation is performed in two (normal/ir data
#' type) or three (temporal data type) processes.
#' \cr(P1.1) Create data (raw and flag by qc value);  
#' \cr(P1.2) Remove qc = 2 flag data in raw data;
#' \cr(P1.3) Check if the raw data size is superior to 0; 
#' \cr(P1.4) Create a binary vector used to inform if flags (by qc values) exist (ir
#' data type);
#' \cr(P1.5) Create data corresponding to isolated points (ir data type);
#' \cr(P1.6) Check if the range of the variable code in raw data is the same as
#' code-frequency data (ir data type);
#' \cr(P1.7) Create data corresponding to isolated points (ir data type).
#' \cr(P2.1) Replace values of (X, Y, Z) variables by values of variables
#' transformed by (f, g, h) functions (normal/temporal data type);
#' \cr(P2.2) Transform data created in the process 1 (transposed data, data
#' including id/group informations) (ir data type).
#' \cr(P3) Create data corresponding to isolated points (temporal data type).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param i_proc_num is the process number (only used for the normal data type:
#' value = 1, 2).
#' @param df_all are data saved in the e_data environment (e_data$all or e_data$sub
#' if sub-data is created).
#' @param v_sub_row a vector of row number corresponding to sub-data (only used if
#' no ID variable is assocciated to flag data.
#' @param df_code_freq are data saved in the e_data environment (e_data$code_freq).
#' @param v_code_range is the vector of code range saved in the o_data_info
#' reactive value ("WIDEa_server" R script). This input only concerns the ir data
#' type.
#' @param df_flag are data saved in the e_data environment (e_data$flag).
#' @param s_flag_name is the flag data name saved in the o_flag reactive value
#' ("WIDEa_server" R script). This input only concerns the normal data type.
#' @param df_previous_flag are data saved in the e_previous_flag environment
#' (e_previous_flag$data).
#' @param l_qc is the list including qc = 1 and 2 data. This input is only used for
#' the ir data type (data created for the process 1 and required for the process 2).
#' @param l_id is the list including id data (id = row number if the ID variable is
#' not informed). This input is only used for the ir data type (data created for the 
#' process 1 and required for the process 2).
#' @param l_fun_val is the list including transformed variable values. This input is
#' only used for the normal/temporal data type (process 2).
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param l_graph_opt is the list of graph option (color, opacity, point type/size)
#' created by the f_create_graph_opt_vector function. This input is used for the ir
#' data type (process 2).

#' @encoding UTF-8

f_prepare_data <- function (s_data_type = "normal", i_proc_num = 1, df_all, v_sub_row = NULL, df_code_freq = NULL, v_code_range = NULL, df_flag = NULL, s_flag_name = NULL, df_previous_flag = NULL, l_qc = NULL, l_id = NULL, l_fun_val = NULL, o_parameter, l_graph_opt = NULL) {
	if (s_data_type == "normal") {
		if (i_proc_num == 1) {
			i_cond <- 0 # qc2
			df_qc2 <- data.frame()
			df_qc2_out <- data.frame()
			s_e_message <- character(0)
			s_w_message <- character(0)
			
			if (isolate(o_parameter$plot_type) != "barplot") {
				if (!is.null(df_flag)) {
					if (isolate(o_parameter$plot_type) == "plot") {
						if (isolate(o_parameter$dim_num) == "2d") {
							v_var <- c(isolate(o_parameter$x), isolate(o_parameter$y))
						}
						else {
							v_var <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z))
						}
						
						if (isolate(o_parameter$model) == "none") {v_var <- unique(v_var)}
					}
					else if (isolate(o_parameter$plot_type) == "boxplot") {
						v_var <- isolate(o_parameter$y)
					}
					else if (isolate(o_parameter$plot_type) == "histplot") {
						v_var <- isolate(o_parameter$x)
					}
					else {
						v_var <- isolate(o_parameter$y)
					}
					
					v_pos <- which(v_var %in% names(df_flag))
					v_var_add <- c()
					
					if (length(which(isolate(o_parameter$model) %in% c("calib", "valid"))) > 0) {
						if (length(which(!is.na(isolate(o_parameter$ref)))) > 0) {v_var_add <- c(v_var_add, isolate(o_parameter$ref))}
						if (length(which(!is.na(isolate(o_parameter$wres_group)))) > 0) {v_var_add <- c(v_var_add, isolate(o_parameter$wres_group))}
					}
					
					if (!is.na(isolate(o_parameter$group))) {v_var_add <- c(v_var_add, isolate(o_parameter$group))}
					v_var_add <- unique(v_var_add)
					v_var <- c(v_var, v_var_add)
					
					if (length(v_pos) > 0) {
						v_var_qc2 <- v_var[v_pos]
						df_flag <- df_flag[order(df_flag[, 1]),]
						v_sum <- rowSums(as.data.frame(df_flag[, v_var_qc2]))
						v_pos <- which(v_sum > 0)
						
						if (length(v_pos) > 0) {
							v_id <- as.vector(df_flag[v_pos, 1])
							
							if (isolate(o_parameter$data_name) == "sub") {
								if (substr(s_flag_name, nchar(s_flag_name) - 9, nchar(s_flag_name) - 4) == "withID") {
									v_sub_pos <- which(v_id %in% df_all[, names(df_flag)[1]])
								}
								else {
									v_sub_pos <- which(v_id %in% v_sub_row) 
								}
								
								if (length(v_sub_pos) > 0) {
									v_pos <- v_pos[v_sub_pos]
									v_id <- v_id[v_sub_pos]
								}
								else {
									v_id <- c()
								}
							}
							
							if (length(v_id) > 0) {
								if (substr(s_flag_name, nchar(s_flag_name) - 9, nchar(s_flag_name) - 4) == "withID") {
									df_all <- df_all[order(df_all[, names(df_flag)[1]]),]
									df_qc2 <- as.data.frame(df_all[which(df_all[, names(df_flag)[1]] %in% v_id), v_var]) 
									df_all[which(df_all[, names(df_flag)[1]] %in% v_id), v_var_qc2][df_flag[v_pos, v_var_qc2] == 1] <- NA
								}
								else {
									if (isolate(o_parameter$data_name) == "sub") {
										df_qc2 <- as.data.frame(df_all[which(v_sub_row %in% v_id), v_var])
										df_all[which(v_sub_row %in% v_id), v_var_qc2][df_flag[v_pos, v_var_qc2] == 1] <- NA
									}
									else {
										df_qc2 <- as.data.frame(df_all[v_id, v_var])
										df_all[v_id, v_var_qc2][df_flag[v_pos, v_var_qc2] == 1] <- NA
									}
								}
								
								if (length(v_var) == 1) {
									df_qc2 <- as.data.frame(df_qc2[!is.na(df_qc2)])
									names(df_qc2) <- v_var
								}
								else {
									df_qc2 <- eval(parse(text = paste0("df_qc2[", paste(paste0("!is.na(df_qc2[, \"", v_var, "\"])"), collapse = " & "), ",]")))
								}
								
								if (length(v_var_add) > 0) {
									df_qc2 <- unique(as.data.frame(df_qc2[, c(1:(dim(df_qc2)[2] - length(v_var_add)))]))
									if (dim(df_qc2)[2] == 1) {names(df_qc2) <- v_var[1]}
								}
								else {
									df_qc2 <- unique(df_qc2)
								}
							}
							
							
							if (dim(df_qc2)[1] > 0) {
								if (is.na(isolate(o_parameter$f)) & is.na(isolate(o_parameter$g)) & is.na(isolate(o_parameter$h)) & isolate(o_parameter$plot_type) == "plot") {
									i_cond <- 1
									df_qc2_out <- df_qc2
								}
								else {
									s_w_message <- paste0("Data with a qc = 2 flag (corresponding to ", dim(df_qc2)[1], " row", ifelse(dim(df_qc2)[1] > 1, "s", ""), ") are only displayed for a 2D/3D plot with no selected model and no function associated to (X, Y, Z) variables")
								}
							}
						}
					}
					
					if (dim(df_qc2)[1] == 0) {s_w_message <- "Data with a qc = 2 flag (no matching with the current selection) are only displayed for a 2D/3D plot (model: none) without (f, g, h) functions associated to (X, Y, Z) variables"}
				}
				
				if (!is.na(isolate(o_parameter$group)) & length(which(isolate(o_parameter$model) == "calib")) > 0) {
					if (length(which(is.na(df_all[, isolate(o_parameter$group)]))) > 0) {s_w_message <- ifelse(length(s_w_message) == 0, "Missing value(s) detected for the Group variable", paste0(s_w_message, "<br/>Missing value(s) detected for the Group variable"))}
				}
				
				if (isolate(o_parameter$plot_type) != "corplot") {
					if (isolate(o_parameter$plot_type) == "plot") {
						if (isolate(o_parameter$dim_num) == "2d") {
							if (isolate(o_parameter$model) == "none") {
								if (is.na(isolate(o_parameter$id))) {
									if (isolate(o_parameter$data_name) == "sub") {
										df_all$.row_num. <- v_sub_row
									}
									else {
										df_all$.row_num. <- c(1:dim(df_all)[1])
									}
								}
								
								df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", c(isolate(o_parameter$x), isolate(o_parameter$y)), "\"])"), collapse = " & "), ",]")))
							}
							else {
								df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", isolate(o_parameter$x), "\"])"), collapse = " & "), " & !is.na(df_all[, isolate(o_parameter$y)]),]")))
								if (length(which(!is.na(isolate(o_parameter$ref)))) > 0) {df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", isolate(o_parameter$ref), "\"])"), collapse = " & "), ",]")))}
								if (length(which(!is.na(isolate(o_parameter$wres_group)))) > 0) {df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", isolate(o_parameter$wres_group), "\"])"), collapse = " & "), ",]")))}
							}
						}
						else {
							if (is.na(isolate(o_parameter$id))) {
								if (isolate(o_parameter$data_name) == "sub") {
									df_all$.row_num. <- v_sub_row
								}
								else {
									df_all$.row_num. <- c(1:dim(df_all)[1])
								}
							}
							
							df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z)), "\"])"), collapse = " & "), ",]")))
						}
					}
					else if (isolate(o_parameter$plot_type) == "boxplot") {
						df_all <- eval(parse(text = paste0("df_all[!is.na(df_all[, isolate(o_parameter$x)]) & ", paste(paste0("!is.na(df_all[, \"", isolate(o_parameter$y), "\"])"), collapse = " & "), ",]")))
					}
					else {
						df_all <- eval(parse(text = paste0("df_all[", paste(paste0("!is.na(df_all[, \"", isolate(o_parameter$x), "\"])"), collapse = " & "), ",]")))
					}
				}
			}
			else {
				df_all <- df_all[!is.na(df_all[, isolate(o_parameter$x)]),]
			}
			
			if (!is.na(isolate(o_parameter$group))) {df_all <- df_all[!is.na(df_all[, isolate(o_parameter$group)]),]}
			if (dim(df_all)[1] == 0) {s_e_message <- paste0("No value is returned with the combination of the selected variables.", ifelse(dim(df_qc2)[1] > 0, " Rows with no missing value are marked with a qc = 2 flag.", ""))}
			return(list(df_all, df_qc2_out, i_cond, s_e_message, s_w_message))
		}
		else { # process = 2
			v_name <- c()
			v_fun_letter <- c("f", "g", "h")
			
			if (sum(lengths(l_fun_val)) > 0) {	
				v_pos <- which(!v_fun_letter %in% names(l_fun_val))
				
				if (length(v_pos) > 0) {
					v_name <- eval(parse(text = paste0("c(", paste(paste0("isolate(o_parameter$", c("x", "y", "z")[v_pos], ")"), collapse = ", "), ")")))
				}
				else {
					v_name <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z))
				}
			}
			else {
				v_name <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z))
			}
			
			if (isolate(o_parameter$plot_type) == "plot") {
				if (isolate(o_parameter$model) == "none" & is.na(isolate(o_parameter$id))) {v_name <- c(v_name, ".row_num.")}
			}
			
			v_name <- c(v_name, isolate(o_parameter$id), isolate(o_parameter$group))
			v_name <- v_name[!is.na(v_name)]
			v_pos <- which(v_fun_letter %in% names(l_fun_val))
			
			if (length(v_name) > 0){
				if (length(v_name) == 1) {
					df_all <- as.data.frame(df_all[, v_name])
					names(df_all) <- v_name
				}
				else {
					df_all <- df_all[, v_name]
				}
				
				if (length(v_pos) > 0) {eval(parse(text = paste(paste0("df_all$.", v_fun_letter[v_pos], ". <- l_fun_val$", v_fun_letter[v_pos]), collapse = "; ")))}
			}
			else {
				if (length(v_pos) > 0) {df_all <- eval(parse(text = paste0("data.frame(", paste(paste0("\".", v_fun_letter[v_pos], ".\" = l_fun_val$", v_fun_letter[v_pos]), collapse = ", "), ")"))) }
			}
			
			return(df_all)
		}
	}
	else if (s_data_type == "temporal") {
		if (i_proc_num == 1) {
			v_cond <- rep(0,3)
			df_qc1 <- NA
			df_qc2 <- NA
			v_var_qc1 <- NA
			v_var_qc2 <- NA
			v_leg_name_qc <- NA
			s_e_message <- character(0)
			s_w_message <- character(0)
			eval(parse(text = paste0("df_all$", isolate(o_parameter$x), "_trf <- as.numeric(as.POSIXct(df_all$", isolate(o_parameter$x), ", tz = \"GMT\"))")))
			df_all <- df_all[, c(1, dim(df_all)[2], 2:(dim(df_all)[2] - 1))]
			i_qc_num <- -1
			
			if (!is.null(df_flag)) {
				df_flag$date_start_trf <- as.numeric(as.POSIXct(df_flag$date_start, tz = "GMT"))
				df_flag$date_end_trf <- as.numeric(as.POSIXct(df_flag$date_end, tz = "GMT"))
				
				if (length(which(as.vector(unique(df_flag$var_name)) %in% isolate(o_parameter$y))) > 0) {
					if (!is.na(isolate(o_parameter$g))) {
						v_cond[1] <- 0
						v_var_qc1 <- as.vector(unique(df_flag[df_flag$qc == 1 & df_flag$var_name %in% isolate(o_parameter$y), "var_name"]))
						v_var_qc2 <- as.vector(unique(df_flag[df_flag$qc == 2 & df_flag$var_name %in% isolate(o_parameter$y), "var_name"]))
						i_qc_num <- length(unique(c(v_var_qc1, v_var_qc2)))
						
						if (length(v_var_qc2) > 0) {
							v_var_qc2 <- names(df_all)[which(names(df_all) %in% v_var_qc2)]
							df_qc <- as.data.frame(matrix(data = 0, nrow = dim(df_all)[1], ncol = dim(df_all)[2]))
							names(df_qc) <- c(isolate(o_parameter$x), paste0(isolate(o_parameter$x), "_trf"), paste0("qc_", names(df_all)[-c(1, 2)]))
							df_qc[, c(1,2)] <- df_all[, c(1,2)]
							
							for (i in 1:length(v_var_qc2)) {
								df_flag_select <- df_flag[which(df_flag$var_name == v_var_qc2[i]),]
								v_pos <- eval(parse(text = paste0("c(", paste(paste0("min(which(df_all$", isolate(o_parameter$x), "_trf >= ", as.vector(df_flag_select$date_start_trf), ")):max(which(df_all$", isolate(o_parameter$x), "_trf <= ", as.vector(df_flag_select$date_end_trf), "))"), collapse = ", "), ")")))
								v_qc <- eval(parse(text = paste0("c(", paste(paste0("rep(", df_flag_select$qc, ", length(min(which(df_all$", isolate(o_parameter$x), "_trf >= ", as.vector(df_flag_select$date_start_trf), ")):max(which(df_all$", isolate(o_parameter$x), "_trf <= ", as.vector(df_flag_select$date_end_trf), "))))"), collapse = ", "), ")"))) 
								df_qc[v_pos, paste0("qc_", v_var_qc2[i])] <- v_qc
							}
							
							df_all[, -c(1, 2)][df_qc[, -c(1, 2)] == 2] <- NA
							v_cond_NA <- eval(parse(text = paste0("c(", paste(paste0("length(which(is.na(df_all[, \"", isolate(o_parameter$y), "\"]))) == dim(df_all)[1]"), collapse = ", "), ")")))
							v_pos <- which(v_cond_NA == T)
							if (length(v_pos) > 0) {s_e_message <- paste0("All dates correspond to a qc = 2 flag for the following variable(s): ", paste(isolate(o_parameter$y)[v_pos], collapse = ", "))}
						}
						
						if (length(s_e_message) == 0) {
							if (i_qc_num >= 0) {
								s_w_message <- paste0("Data with a qc = (1, 2) flag (corresponding to ", i_qc_num, " Y variable", ifelse(i_qc_num > 1, "s", ""), ") are only displayed if no function is associated to Y variable", ifelse(i_qc_num > 1, "s", ""), "<br/>The flag tab is disabled because a function g is enabled")
							}
							else {
								s_w_message <- "The flag tab is disabled because a function g is enabled"
							}
						}
					}
					else {
						v_cond[1] <- 1
						v_var_qc1 <- as.vector(unique(df_flag[df_flag$qc == 1 & df_flag$var_name %in% isolate(o_parameter$y), "var_name"]))
						
						if (length(v_var_qc1) > 0) {
							v_cond[2] <- 1
							v_var_qc1 <- names(df_all)[which(names(df_all) %in% v_var_qc1)]
						}
						else {
							v_cond[2] <- 0
						}
						
						v_var_qc2 <- as.vector(unique(df_flag[df_flag$qc == 2 & df_flag$var_name %in% isolate(o_parameter$y), "var_name"]))
						
						if (length(v_var_qc2) > 0) {
							v_cond[3] <- 1
							v_var_qc2 <- names(df_all)[which(names(df_all) %in% v_var_qc2)]
						}
						else {
							v_cond[3] <- 0
						}
						
						v_var_qc <- c(v_var_qc1, v_var_qc2)
						v_var_all <- unique(c(v_var_qc1, v_var_qc2)) 
						v_var_all <- names(df_all)[which(names(df_all) %in% v_var_all)]
						df_qc <- as.data.frame(matrix(data = 0, nrow = dim(df_all)[1], ncol = dim(df_all)[2]))
						names(df_qc) <- c(isolate(o_parameter$x), paste0(isolate(o_parameter$x), "_trf"), paste0("qc_", names(df_all)[-c(1, 2)]))
						df_qc[, c(1,2)] <- df_all[, c(1,2)]
						df_comment <- as.data.frame(matrix(data = NA, nrow = dim(df_all)[1], ncol = dim(df_all)[2]))
						names(df_comment) <- c(isolate(o_parameter$x), paste0(isolate(o_parameter$x), "_trf"), paste0("comment_", names(df_all)[-c(1, 2)]))
						df_comment[, c(1,2)] <- df_all[, c(1,2)]
						
						for (i in 1:length(v_var_all)) {
							df_flag_select <- df_flag[which(df_flag$var_name == v_var_all[i]),]
							v_pos <- eval(parse(text = paste0("c(", paste(paste0("min(which(df_all$", isolate(o_parameter$x), "_trf >= ", as.vector(df_flag_select$date_start_trf), ")):max(which(df_all$", isolate(o_parameter$x), "_trf <= ", as.vector(df_flag_select$date_end_trf), "))"), collapse = ", "), ")")))
							v_qc <- eval(parse(text = paste0("c(", paste(paste0("rep(", df_flag_select$qc, ", length(min(which(df_all$", isolate(o_parameter$x), "_trf >= ", as.vector(df_flag_select$date_start_trf), ")):max(which(df_all$", isolate(o_parameter$x), "_trf <= ", as.vector(df_flag_select$date_end_trf), "))))"), collapse = ", "), ")"))) 
							v_comment <- eval(parse(text = paste0("c(", paste(paste0("rep(\"", df_flag_select$comment, "\", length(min(which(df_all$", isolate(o_parameter$x), "_trf >= ", as.vector(df_flag_select$date_start_trf), ")):max(which(df_all$", isolate(o_parameter$x), "_trf <= ", as.vector(df_flag_select$date_end_trf), "))))"), collapse = ", "), ")"))) 
							v_comment <- ifelse(v_comment %in% c("", "NA"), NA, v_comment)
							df_qc[v_pos, paste0("qc_", v_var_all[i])] <- v_qc
							df_comment[v_pos, paste0("comment_", v_var_all[i])] <- v_comment
						}
						
						v_leg_name_qc <- rep(v_var_all, each = 2)
						v_leg_name_qc <- paste0(v_leg_name_qc, rep(c(" qc = 1", " qc = 2"), length(v_var_all )))
						v_leg_name_select <- c()
						
						if (v_cond[2] == 1) {
							df_qc1 <- df_all 
							df_qc1[, -c(1, 2)][df_qc[, -c(1, 2)] != 1] <- NA 
							v_leg_name_select <- c(v_leg_name_select, paste0(v_var_qc1, " qc = 1"))
						}
						
						if (v_cond[3] == 1) {
							df_qc2 <- df_all
							df_qc2[, -c(1, 2)][df_qc[, -c(1, 2)] != 2] <- NA
							df_all[, -c(1, 2)][df_qc[, -c(1, 2)] == 2] <- NA
							v_leg_name_select <- c(v_leg_name_select, paste0(v_var_qc2, " qc = 2"))
						}
						
						v_cond_NA <- eval(parse(text = paste0("c(", paste(paste0("length(which(is.na(df_all[, \"", isolate(o_parameter$y), "\"]))) == dim(df_all)[1]"), collapse = ", "), ")")))
						v_pos <- which(v_cond_NA == T)
						
						if (length(v_pos) > 0) { 
							s_e_message <- paste0("All dates correspond to a qc = 2 flag for the following variable(s): ", paste(isolate(o_parameter$y)[v_pos], collapse = ", "))
						}
						else {
							if (length(v_leg_name_select) > 0) {v_leg_name_qc <- v_leg_name_qc[which(v_leg_name_qc %in% v_leg_name_select)]}
							i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
							
							for (i in 1:length(v_var_all)) {
								if (v_cond[2] == 1) {
									if (v_var_all[i] %in% v_var_qc1) {
										v_all <- as.vector(df_qc1[, v_var_all[i]])
										
										if (!is.null(df_previous_flag)) {
											df_add <- data.frame("x" = df_qc1[which(!is.na(v_all)), isolate(o_parameter$x)], "y" = v_all[!is.na(v_all)], "var_name" = rep(v_var_all[i], length(which(!is.na(v_all)))), "qc" = rep(1, length(which(!is.na(v_all)))))
											df_previous_flag <- rbind(df_previous_flag, df_add)
										}
										else {
											df_previous_flag <- data.frame("x" = df_qc1[which(!is.na(v_all)), isolate(o_parameter$x)], "y" = v_all[!is.na(v_all)], "var_name" = rep(v_var_all[i], length(which(!is.na(v_all)))), "qc" = rep(1, length(which(!is.na(v_all)))))
										}
										
										v_qc <- as.vector(df_qc[, paste0("qc_", v_var_all[i])])
										v_qc[which(is.na(v_all))] <- 0
										v_diff_1 <- c(diff(v_qc), 0) 
										v_diff_1[which(v_diff_1 != (-1))] <- 0
										v_diff_2 <- c(0, -diff(v_qc))
										v_diff_2[which(v_diff_2 != (-1))] <- 0
										v_diff <- v_diff_1 + v_diff_2
										v_all_pt <- v_all
										v_all_pt[which(v_diff == 0)] <- NA
										v_text <- ifelse(is.na(df_comment[, paste0("comment_", v_var_all[i])]), "", paste0("<br>comments:<br>", df_comment[, paste0("comment_", v_var_all[i])]))
										v_comment <- paste0("(", v_var_all[i], ")<br>", round(v_all, digits = i_dec_num), v_text)
										eval(parse(text = paste0("df_qc1$", v_var_all[i], "_pt <- v_all_pt")))
										eval(parse(text = paste0("df_qc1$", v_var_all[i], "_comment <- v_comment")))
									}
								}
								
								if (v_cond[3] == 1) {
									if (v_var_all[i] %in% v_var_qc2) {
										v_all <- as.vector(df_qc2[, v_var_all[i]])
										
										if (!is.null(df_previous_flag)) {
											df_add <- data.frame("x" = df_qc2[which(!is.na(v_all)), isolate(o_parameter$x)], "y" = v_all[!is.na(v_all)], "var_name" = rep(v_var_all[i], length(which(!is.na(v_all)))), "qc" = rep(2, length(which(!is.na(v_all)))))
											df_previous_flag <- rbind(df_previous_flag, df_add)
										}
										else {
											df_previous_flag <- data.frame("x" = df_qc2[which(!is.na(v_all)), isolate(o_parameter$x)], "y" = v_all[!is.na(v_all)], "var_name" = rep(v_var_all[i], length(which(!is.na(v_all)))), "qc" = rep(2, length(which(!is.na(v_all)))))
										}
										
										v_qc <- as.vector(df_qc[, paste0("qc_", v_var_all[i])])
										v_qc[which(is.na(v_all))] <- 0
										v_qc[which(v_qc == 2)] <- 1
										v_diff_1 <- c(diff(v_qc), 0) 
										v_diff_1[which(v_diff_1 != (-1))] <- 0
										v_diff_2 <- c(0, -diff(v_qc))
										v_diff_2[which(v_diff_2 != (-1))] <- 0
										v_diff <- v_diff_1 + v_diff_2
										v_all_pt <- v_all
										v_all_pt[which(v_diff == 0)] <- NA
										v_text <- ifelse(is.na(df_comment[, paste0("comment_", v_var_all[i])]), "", paste0("<br>comments:<br>", df_comment[, paste0("comment_", v_var_all[i])]))
										v_comment <- paste0("(", v_var_all[i], ")<br>", round(v_all, digits = i_dec_num), v_text)
										eval(parse(text = paste0("df_qc2$", v_var_all[i], "_pt <- v_all_pt")))
										eval(parse(text = paste0("df_qc2$", v_var_all[i], "_comment <- v_comment")))
									}
								}
							}
						}
					}
				}
				else {
					v_cond[1] <- 0
				}
			}
			
			return(list(df_all, df_previous_flag, df_qc1, df_qc2, v_cond, v_var_qc1, v_var_qc2, v_leg_name_qc, s_e_message, s_w_message))
		}
		else if (i_proc_num == 2) {
			eval(parse(text = paste(paste0("df_all[, isolate(o_parameter$y)[", 1:length(l_fun_val), "]] <- l_fun_val[[", 1:length(l_fun_val), "]]"), collapse = "; ")))
			return(df_all)
		}
		else { # process = 3
			l_pt_pos <- lapply(isolate(o_parameter$y), function (x) {which(is.na(c(NA, df_all[, x], NA)))[which(diff(which(is.na(c(NA, df_all[, x], NA)))) == 2)]})
			names(l_pt_pos) <- isolate(o_parameter$y)
			return(l_pt_pos)
		}
	}
	else { # ir
		if (i_proc_num == 1) {
			v_id_0 <- NULL
			v_id_1 <- NULL
			v_id_2 <- NULL
			v_cond <- rep(0,3)
			df_qc1 <- NA
			df_qc2 <- NA
			v_pt_pos <- NA
			s_e_message <- character(0)
			df_code_freq$Frequency <- as.vector(df_code_freq$Frequency)
			df_code_freq <- df_code_freq[order(df_code_freq$Frequency, decreasing = T),]
			v_code <- as.vector(df_code_freq$Code)
			v_pos <- which(v_code_range %in% v_code)
			
			if (length(v_pos) < 2) {
				if (length(v_pos) == 0) {
					v_code_range_out <- v_code[c(1, length(v_code))]
				}
				else {
					if (v_pos == 1) {
						v_code_range_out <- c(v_code[1], v_code_range[2]) 
					}
					else {
						v_code_range_out <- c(v_code_range[1], v_code[length(v_code)])
					}
				}
				
				s_e_message <- paste0("Code variables must be between ", paste(v_code_range_out, collapse = " and "), " in spectra data")
			}
			else {
				v_pos <- which(v_code == v_code_range[1])
				if (v_pos > 1) {v_code <- v_code[-c(1:(v_pos - 1))]}
				v_pos <- which(v_code == v_code_range[2])
				if (v_pos < length(v_code)) {v_code <- v_code[-c((v_pos + 1):length(v_code))]}
				df_code_freq <- df_code_freq[which(df_code_freq$Code %in% v_code),]
				v_pos <- which(v_code %in% names(df_all))
				
				if (length(v_pos) > 0) {
					v_code_verif <- v_code[v_pos]	
				}
				else {
					v_code_verif <- v_code
				}
				
				v_pos <- which(as.vector(t(colSums(df_all[, v_code_verif], na.rm = T))) == 0)
				v_code_NA <- c()
				
				if (length(v_pos) > 0) {
					v_nrow <- as.vector(unlist(lapply(v_pos, function(x) {
						return(length(which(!is.na(df_all[, v_code_verif[x]]))))
					})))
					
					if (length(which(v_nrow == 0)) > 0) {
						v_pos <- v_pos[which(v_nrow == 0)]
						v_code_NA <- v_code_verif[v_pos]
						v_code_verif <- v_code_verif[-v_pos]
					}
				}
				
				v_pos <- which(is.na(rowSums(df_all[, v_code_verif])))
				
				if (length(v_pos) > 0) {
					s_e_message <- "Spectra must have values for a same number of frequencies"
				}
				else {
					v_pos <- c(0, which(!v_code %in% names(df_all)), length(v_code) + 1)
					
					if (length(v_code_NA) > 0) {
						v_pos <- c(v_pos, which(v_code %in% v_code_NA))
						v_pos <- v_pos[order(v_pos)]
					}
					
					v_diff <- diff(v_pos) 
					if (length(which(v_diff == 2)) > 0) {v_pt_pos <- v_pos[which(v_diff == 2)] + 1}
					v_pos <- which(!v_code %in% names(df_all))
					if (length(v_pos) > 0) {eval(parse(text = paste(paste0("df_all$", v_code[v_pos], " <- NA"), collapse = "; ")))}
					v_var_name <- c(isolate(o_parameter$id), isolate(o_parameter$group), df_code_freq$Code)
					v_var_name <- v_var_name[!is.na(v_var_name)]
					df_all <- df_all[, v_var_name]
					
					if (!is.na(isolate(o_parameter$id))) {
						if (!is.na(isolate(o_parameter$group))) {
							if (length(which(is.na(df_all[, isolate(o_parameter$group)]))) > 0) {df_all <- df_all[!is.na(df_all[, isolate(o_parameter$group)]),]}
						}
						
						v_id_0 <- as.vector(df_all[, isolate(o_parameter$id)])
					}
					else {
						v_id_0 <- eval(parse(text = ifelse(isolate(o_parameter$data_name) == "sub", "v_sub_row", "1:dim(df_all)[1]")))
						v_pos_1 <- c()
						if (!is.na(isolate(o_parameter$group))) {v_pos_1 <- eval(parse(text = ifelse(isolate(o_parameter$data_name) == "sub", "v_sub_row[which(is.na(df_all[, isolate(o_parameter$group)]))]", "which(is.na(df_all[, isolate(o_parameter$group)]))")))}
					}
					
					if (!is.null(df_flag)) {
						v_id_1 <- as.vector(df_flag[which(df_flag$qc == 1), 1])			
						v_id_2 <- as.vector(df_flag[which(df_flag$qc == 2), 1])
						
						if (length(v_id_1) > 0 & length(which(v_id_1 %in% v_id_0)) > 0) {
							if (!is.na(isolate(o_parameter$id))) {
								v_pos_1 <- which(df_all[, isolate(o_parameter$id)] %in% v_id_1)
								
								if (length(v_pos_1) > 0) {
									df_qc1 <- df_all[v_pos_1,]
									v_id_1 <- as.vector(df_qc1[, isolate(o_parameter$id)])
									v_cond[2] <- 1
								}
							}
							else {
								v_id_1 <- v_id_1[which(v_id_1 %in% v_id_0)]
								
								if (length(v_pos_1) > 0) {
									v_pos_2 <- which(v_id_1 %in% v_pos_1)
									
									if (length(v_pos_2) < length(v_id_1) & length(v_pos_2) > 0) {
										v_id_1 <- v_id_1[-v_pos_2]
										df_qc1 <- df_all[which(v_id_0 %in% v_id_1),]
										v_cond[2] <- 1
									}
								}
								else {
									df_qc1 <- df_all[which(v_id_0 %in% v_id_1),]
									v_cond[2] <- 1
								}
							}
						}
						
						if (length(v_id_2) > 0 & length(which(v_id_2 %in% v_id_0)) > 0) {
							if (!is.na(isolate(o_parameter$id))) {
								v_pos_1 <- which(df_all[, isolate(o_parameter$id)] %in% v_id_2)
								
								if (length(v_pos_1) > 0) {
									df_qc2 <- df_all[v_pos_1,]
									v_id_2 <- as.vector(df_qc2[, isolate(o_parameter$id)])
									v_cond[3] <- 1
									df_all <- df_all[-v_pos_1,]
									v_id_0 <- as.vector(df_all[, isolate(o_parameter$id)])
								}
							}
							else {
								v_id_2 <- v_id_2[which(v_id_2 %in% v_id_0)]
								
								if (length(v_pos_1) > 0) {
									v_pos_2 <- which(v_id_2 %in% v_pos_1)
									
									if (length(v_pos_2) < length(v_id_2) & length(v_pos_2) > 0) {
										v_id_2 <- v_id_2[-v_pos_2]
										df_qc2 <- df_all[which(v_id_0 %in% v_id_2),]
										v_cond[3] <- 1
										df_all <- df_all[-which(v_id_0 %in% c(v_pos_1, v_id_2)),]
										v_id_0 <- v_id_0[-which(v_id_0 %in% c(v_pos_1, v_id_2))]
									}
								}
								else {
									df_qc2 <- df_all[which(v_id_0 %in% v_id_2),]
									v_cond[3] <- 1
									df_all <- df_all[-which(v_id_0 %in% v_id_2),]
									v_id_0 <- v_id_0[-which(v_id_0 %in% v_id_2)]
								}
							}
						}
						else {
							if (is.na(isolate(o_parameter$id))) {
								if (length(v_pos_1) > 0) {
									df_all <- df_all[-which(v_id_0 %in% v_pos_1),]
									v_id_0 <- v_id_0[-which(v_id_0 %in% v_pos_1)]
								}
							}
						}
						
						v_cond[1] <- 1
					}
					else {
						if (is.na(isolate(o_parameter$id))) {
							if (length(v_pos_1) > 0) {
								df_all <- df_all[-which(v_id_0 %in% v_pos_1),]
								v_id_0 <- v_id_0[-which(v_id_0 %in% v_pos_1)]
							}
						}
					}
					
					if (v_cond[3] == 1 & dim(df_all)[1] == 0) {
						s_e_message <- "All IDs correspond to a qc = 2 flag"
					}
				}	
			}
			
			return(list(df_all, df_code_freq, df_qc1, df_qc2, v_cond, list(v_id_0, v_id_1, v_id_2), v_pt_pos, s_e_message))
		}
		else { # process = 2
			df_qc1 <- l_qc[[1]]
			df_qc2 <- l_qc[[2]]
			l_id_group <- list()
			
			# (a) Without flag								
			
			df_all_tr <- as.data.frame(t(df_all[, as.vector(df_code_freq$Code)]))
			v_name <- paste0("col", 1:dim(df_all)[1])
			df_all_tr <- cbind(df_code_freq, df_all_tr)
			names(df_all_tr)[3:(length(v_name) + 2)] <- v_name
			df_id_group <- data.frame("id" = rep(NA, length(v_name)), "group" = rep(NA, length(v_name)), "show" = rep(F, length(v_name)), "color" = rep(NA, length(v_name)), "opacity" = rep(NA, length(v_name))) 
			eval(parse(text = paste(paste0("df_id_group[, ", 1:dim(df_id_group)[2], "] <- as.vector(df_id_group[, ", 1:dim(df_id_group)[2], "])"), collapse = "; "))) 
			
			if (is.na(isolate(o_parameter$group))) {
				df_id_group$id <- l_id[[1]]
				df_id_group$group <- rep("all", length(v_name))
				
				if (length(v_name) == 1) {
					df_id_group$show <- T
				}
				else {
					df_id_group$show <- c(T, rep(F, length(v_name) - 1))
				}
				
				df_id_group$color <- l_graph_opt$color
				df_id_group$opacity <- l_graph_opt$opacity
				df_id_group$point_type <- l_graph_opt$point_type
				df_id_group$point_size <- l_graph_opt$point_size
			}
			else {
				v_group <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
				v_group <- v_group[order(v_group)]
				
				l_name <- lapply(v_group, function(x) {
					return(which(as.vector(df_all[, isolate(o_parameter$group)]) == x))
				})
				
				df_id_group$id <- l_id[[1]][as.vector(unlist(l_name))]
				df_all_tr <- df_all_tr[, c("Code", "Frequency", v_name[as.vector(unlist(l_name))])]
				eval(parse(text = paste0("df_id_group$group <- c(", paste(paste0("rep(\"", v_group, "\", ", lengths(l_name), ")"), collapse = ", "), ")")))
				eval(parse(text = paste0("df_id_group$color <- c(", paste(paste0("rep(\"", l_graph_opt$color, "\", ", lengths(l_name), ")"), collapse = ", "), ")")))
				eval(parse(text = paste0("df_id_group$opacity <- c(", paste(paste0("rep(", l_graph_opt$opacity, ", ", lengths(l_name), ")"), collapse = ", "), ")")))
				eval(parse(text = paste0("df_id_group$point_type <- c(", paste(paste0("rep(", l_graph_opt$point_type, ", ", lengths(l_name), ")"), collapse = ", "), ")")))
				eval(parse(text = paste0("df_id_group$point_size <- c(", paste(paste0("rep(", l_graph_opt$point_size, ", ", lengths(l_name), ")"), collapse = ", "), ")")))
				
				if (length(v_name) == 1) {
					df_id_group$show <- T
				}
				else {
					if (length(v_group) == 1) {
						df_id_group$show[1] <- T
					}
					else {
						df_id_group$show[c(1, cumsum(lengths(l_name)[-length(v_group)]) + 1)] <- T
					}
				}
			}
			
			l_id_group[[length(l_id_group) + 1]] <- df_id_group
			names(l_id_group)[length(l_id_group)] <- "no_flag"
			
			# (b) With qc = 1 flags  
			
			if (inherits(df_qc1, "data.frame")) {
				df_qc1_tr <- as.data.frame(t(df_qc1[, as.vector(df_code_freq$Code)]))
				v_name <- paste0("col", 1:dim(df_qc1)[1])
				df_qc1_tr <- cbind(df_code_freq, df_qc1_tr)
				names(df_qc1_tr)[3:(length(v_name) + 2)] <- v_name
				df_id_group <- data.frame("id" = rep(NA, length(v_name)), "group" = rep(NA, length(v_name)), "show" = rep(F, length(v_name))) 
				eval(parse(text = paste(paste0("df_id_group[, ", 1:dim(df_id_group)[2], "] <- as.vector(df_id_group[, ", 1:dim(df_id_group)[2], "])"), collapse = "; "))) 
				
				if (is.na(isolate(o_parameter$group))) {
					df_id_group$id <- l_id[[2]]
					df_id_group$group <- rep("all qc = 1", length(v_name))
					
					if (length(v_name) == 1) {
						df_id_group$show <- T
					}
					else {
						df_id_group$show <- c(T, rep(F, length(v_name) - 1))
					}
				}
				else {
					v_group <- as.vector(unique(df_qc1[, isolate(o_parameter$group)]))
					v_group <- v_group[order(v_group)]
					
					l_name <- lapply(v_group, function(x) {
						return(which(as.vector(df_qc1[, isolate(o_parameter$group)]) == x))
					})
					
					df_id_group$id <- l_id[[2]][as.vector(unlist(l_name))]
					df_qc1_tr <- df_qc1_tr[, c("Code", "Frequency", v_name[as.vector(unlist(l_name))])]
					eval(parse(text = paste0("df_id_group$group <- c(", paste(paste0("rep(\"", v_group, " qc = 1\", ", lengths(l_name), ")"), collapse = ", "), ")")))
					
					if (length(v_name) == 1) {
						df_id_group$show <- T
					}
					else {
						if (length(v_group) == 1) {
							df_id_group$show[1] <- T
						}
						else {
							df_id_group$show[c(1, cumsum(lengths(l_name)[-length(v_group)]) + 1)] <- T
						}
					}
				}
				
				l_id_group[[length(l_id_group) + 1]] <- df_id_group
				names(l_id_group)[length(l_id_group)] <- "qc1_flag"
			}
			else {
				df_qc1_tr <- NA
			}
			
			# (c) With qc = 2 flags 
			
			if (inherits(df_qc2, "data.frame")) {
				df_qc2_tr <- as.data.frame(t(df_qc2[, as.vector(df_code_freq$Code)]))
				v_name <- paste0("col", 1:dim(df_qc2)[1])
				df_qc2_tr <- cbind(df_code_freq, df_qc2_tr)
				names(df_qc2_tr)[3:(length(v_name) + 2)] <- v_name
				df_id_group <- data.frame("id" = rep(NA, length(v_name)), "group" = rep(NA, length(v_name)), "show" = rep(F, length(v_name))) 
				eval(parse(text = paste(paste0("df_id_group[, ", 1:dim(df_id_group)[2], "] <- as.vector(df_id_group[, ", 1:dim(df_id_group)[2], "])"), collapse = "; "))) 
				
				if (is.na(isolate(o_parameter$group))) {
					df_id_group$id <- l_id[[3]]
					df_id_group$group <- rep("all qc = 2", length(v_name))
					
					if (length(v_name) == 1) {
						df_id_group$show <- T
					}
					else {
						df_id_group$show <- c(T, rep(F, length(v_name) - 1))
					}
				}
				else {
					v_group <- as.vector(unique(df_qc2[, isolate(o_parameter$group)]))
					v_group <- v_group[order(v_group)]
					
					l_name <- lapply(v_group, function(x) {
						return(which(as.vector(df_qc2[, isolate(o_parameter$group)]) == x))
					})
					
					df_id_group$id <- l_id[[3]][as.vector(unlist(l_name))]
					df_qc2_tr <- df_qc2_tr[, c("Code", "Frequency", v_name[as.vector(unlist(l_name))])]
					eval(parse(text = paste0("df_id_group$group <- c(", paste(paste0("rep(\"", v_group, " qc = 2\", ", lengths(l_name), ")"), collapse = ", "), ")")))
					
					if (length(v_name) == 1) {
						df_id_group$show <- T
					}
					else {
						if (length(v_group) == 1) {
							df_id_group$show[1] <- T
						}
						else {
							df_id_group$show[c(1, cumsum(lengths(l_name)[-length(v_group)]) + 1)] <- T
						}
					}
				}
				
				l_id_group[[length(l_id_group) + 1]] <- df_id_group
				names(l_id_group)[length(l_id_group)] <- "qc2_flag"
			}
			else {
				df_qc2_tr <- NA
			}
			
			return(list(df_all_tr, df_qc1_tr, df_qc2_tr, l_id_group))
		}
	}
}

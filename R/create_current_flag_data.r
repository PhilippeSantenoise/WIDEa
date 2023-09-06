#' @importFrom shiny isolate
NULL

#' Creating a list including current flag data information

#' @description
#' `f_create_current_flag_data` is used to create a list including current flag data
#' and other ouputs. The number of outputs returned by the list depend on the data
#' type:
#' \cr(1) Flag number (normal/temporal);
#' \cr(2) Data coordinates (normal/ir: 1 dataset; temporal: 2 datasets); 
#' \cr(3) Outputs used to draw an interval (temporal: 2 successive click events
#' required);
#' \cr(4) Flag tab button name to be enabled (temporal: interval);  
#' \cr(5) An error message (normal: if the flag already exists; temporal: if the Y
#' variable is not the same for the 2 successive click events of the interval).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_action is the value of the "action" radio button in the Flag tab (2
#' values: "add_flag", "replace_qc"). This input is only used for the temporal data
#' type.
#' @param s_draw is the value of the "draw" radio button in the Flag tab (2 values:
#' "interval", "point"). This input is only used for the temporal data type.
#' @param s_x_var is the X variable name. This input is only used for the temporal
#' data type.
#' @param l_click_info is a list of data information associated to a click event and
#' created by the `f_create_click_info` function (only for normal/ir data type). For
#' the temporal data type, the list returns informations given by the click event
#' ("plotly_click" with the `event_data` function).
#' @param df_previous_flag are existing flag data given by environments created in
#' the "WIDEa_server" R script (normal: e_data$flag; temporal: 
#' e_previous_flag$data). This input is only used for the normal/temporal data type.
#' @param e_current_flag is the environment created in the "WIDEa_server" R script.
#' @param o_click_graph is a reactive value including previous plotly click data 
#' information used to draw an interval for the temporal data type.
#' @param o_plot is a reactive value including main plotly data information.

#' @encoding UTF-8

f_create_current_flag_data <- function (s_data_type = "normal", s_action = NULL, s_draw = NULL, s_x_var = NULL, l_click_info, df_previous_flag = NULL, e_current_flag, o_click_graph = NULL, o_plot = NULL) {
	if (s_data_type == "normal") {
		s_message <- character(0)
		v_axis <- l_click_info[[1]]
		v_name <- l_click_info[[2]]
		v_num <- l_click_info[[3]]
		v_pos_1 <- c()
		
		if (!is.null(df_previous_flag)) {
			v_pos_1 <- which(v_name %in% names(df_previous_flag))
		}
		
		if (length(v_pos_1) > 0) {
			l_num <- lapply(v_pos_1, function(x) {
				return(v_num[which(!v_num %in% as.vector(df_previous_flag[df_previous_flag[, v_name[x]] != 0, 1]))])
			})
			
			v_pos_2 <- which(lengths(l_num) == 0)
			
			if (length(v_pos_2) == 0) {
				names(l_num) <- v_name[v_pos_1]
				
				df_add <- as.data.frame(matrix(data = 0, ncol = length(v_name) + 2, nrow = length(v_num)))
				names(df_add) <- c("id", "num", v_name)
				df_add$id <- v_num
				
				if (length(v_pos_1) < length(v_name)) {
					df_add[, v_name[-v_pos_1]] <- 1
				}
				
				eval(parse(text = paste(paste0("df_add[which(df_add$id %in% l_num$", v_name[v_pos_1], "), \"" , v_name[v_pos_1], "\"] <- 1"), collapse = "; ")))
				
				if ("coord" %in% ls(e_current_flag)) {
					i_num <- e_current_flag$num + 1
					df_add$num <- i_num
					df_coord <- rbind(e_current_flag$coord, df_add)
				}
				else {
					i_num <- 1
					df_add$num <- i_num
					df_coord <- df_add
				}
				
				return (list(num = i_num, coord = df_coord))
			}
			else { # error message returned
				v_axis <- v_axis[v_pos_1[v_pos_2]]
				
				if (length(v_axis) == 1) {
					s_message <- paste0("Flag already exists for ", toupper(v_axis), " variable")
				}
				else {
					s_message <- paste0("Flag already exists for ", paste(toupper(v_axis[1:(length(v_axis) - 1)]), collapse = ", "), " and ", toupper(v_axis[length(v_axis)]), " variables")
				}
				
				return (list(message = s_message))
			}
		}
		else {
			df_add <- as.data.frame(matrix(data = 0, ncol = length(v_name) + 2, nrow = length(v_num)))
			names(df_add) <- c("id", "num", v_name)
			df_add$id <- v_num
			df_add[, v_name] <- 1
			
			if ("coord" %in% ls(e_current_flag)) {
				i_num <- e_current_flag$num + 1
				df_add$num <- i_num
				df_coord <- rbind(e_current_flag$coord, df_add)
			}
			else {
				i_num <- 1
				df_add$num <- i_num
				df_coord <- df_add
			}
			
			return (list(num = i_num, coord = df_coord))
		}
	}
	else if (s_data_type == "temporal") {
		s_message <- character(0)
		
		if (s_draw == "pt") {
			if ("coord" %in% ls(e_current_flag)) {
				i_num <- e_current_flag$num + 1
				df_add <- data.frame("x1" = l_click_info[[1]], "x2" = NA, "var_name" = l_click_info[[3]], "num" = i_num, "geom" = "pt")
				df_coord <- rbind(e_current_flag$coord, df_add)
				df_add <- data.frame("x" = l_click_info[[1]], "y" = l_click_info[[2]], "var_name" = l_click_info[[3]], "num" = i_num)
				df_all_coord <- rbind(e_current_flag$all_coord, df_add)
			}
			else {
				i_num <- 1
				df_coord <- data.frame("x1" = l_click_info[[1]], "x2" = NA, "var_name" = l_click_info[[3]], "num" = i_num, "geom" = "pt")
				df_all_coord <- data.frame("x" = l_click_info[[1]], "y" = l_click_info[[2]], "var_name" = l_click_info[[3]], "num" = i_num)
			}
			
			v_button_name <- c("clear1_button", "clear2_button", "save_button")
			return (list(num = i_num, coord = df_coord, all_coord = df_all_coord, button_name = v_button_name, message = s_message))
		}
		else { # interval
			if ("click_iter" %in% ls(e_current_flag)) { # second click event
				if (isolate(o_click_graph$prev_var) == l_click_info[[3]]) { 
					df_all <- isolate(o_plot$data)
					df_interv <- df_all[which(df_all[, s_x_var] == o_click_graph$prev_date):which(df_all[, s_x_var] == l_click_info[[1]]),]
					v_pos <- c()
					
					if (!is.null(df_previous_flag)) {
						if (s_action == "add_flag") {
							v_pos <- which(df_interv[, s_x_var] %in% as.vector(df_previous_flag[which(df_previous_flag$qc == 1 & df_previous_flag$var_name == isolate(o_click_graph$prev_var)), "x"]))
						}
						else {
							v_pos <- which(!df_interv[, s_x_var] %in% as.vector(df_previous_flag[which(df_previous_flag$qc == 1 & df_previous_flag$var_name == isolate(o_click_graph$prev_var)), "x"]))
						}
					}
					
					if ("coord" %in% ls(e_current_flag)) {
						v_pos <- c(v_pos, which(df_interv[, s_x_var] %in% as.vector(e_current_flag$all_coord$x)[-length(e_current_flag$all_coord$x)]))
					}
					
					if (length(v_pos) > 0) {
						df_interv[v_pos, isolate(o_click_graph$prev_var)] <- NA
					}
					
					i_pt_pos <- which(df_interv[, s_x_var] == l_click_info[[1]])
					v_mpt_pos_all <- 1:dim(df_interv)[1]
					v_x_all <- as.vector(df_interv[, s_x_var])
					v_y_all <- as.vector(df_interv[, isolate(o_click_graph$prev_var)])
					v_pos_1 <- which(is.na(v_y_all))
					v_x_pt <- c()
					v_y_pt <- c()
					
					if (length(v_pos_1) > 0) {
						v_pos_2 <- unique(c(v_pos_1 - 1, v_pos_1 + 1))
						v_pos_3 <- which(v_pos_2 %in% c(v_pos_1, 1, length(v_y_all)))
						
						if (length(v_pos_3) > 0) {
							v_pos_2 <- v_pos_2[-v_pos_3]
						}
						
						if (length(v_pos_2) > 0) {
							v_x_pt <- v_x_all[sort(v_pos_2)] 
							v_y_pt <- v_y_all[sort(v_pos_2)]
						}
						
						v_mpt_pos_all <- v_mpt_pos_all[-v_pos_1]
					}
					
					v_mpt_pos <- v_mpt_pos_all[-which(v_mpt_pos_all == which(df_interv[, s_x_var] == isolate(o_click_graph$prev_date)))]
					v_y <- df_interv[v_mpt_pos, isolate(o_click_graph$prev_var)]
					v_pos <- which(df_interv[, s_x_var] %in% c(o_click_graph$prev_date, l_click_info[[1]]))
					
					if (length(v_x_pt) > 0) {
						v_pos <- v_pos[order(v_pos)]
						v_y_1 <- df_interv[v_pos[1] + 1, l_click_info[[3]]]  
						v_y_2 <- df_interv[v_pos[2] - 1, l_click_info[[3]]]  
						
						v_x_pt_save <- c()
						v_x_mpt_save <- c()
						
						if (is.na(v_y_1)) {
							v_x_pt_save <- df_interv[v_pos[1], s_x_var]
						}
						else {
							v_x_mpt_save <- df_interv[v_pos[1], s_x_var]
						}
						
						v_y_3 <- df_interv[which(df_interv[, s_x_var] %in% v_x_pt) - 1, l_click_info[[3]]]  
						v_y_4 <- df_interv[which(df_interv[, s_x_var] %in% v_x_pt) + 1, l_click_info[[3]]]  
						v_cond_1 <- which(is.na(v_y_3))
						v_cond_2 <- which(is.na(v_y_4))
						v_cond_3 <- which(v_cond_1 %in% v_cond_2)
						
						if (length(v_cond_3) > 0) {
							v_x_pt_save <- c(v_x_pt_save, v_x_pt[v_cond_1[v_cond_3]])
							
							if (length(v_x_pt) > length(v_cond_3)) {
								v_x_mpt_save <- c(v_x_mpt_save, v_x_pt[-v_cond_1[v_cond_3]])
							}
						}
						else {
							v_x_mpt_save <- c(v_x_mpt_save, v_x_pt)
						}
						
						if (is.na(v_y_2)) {
							v_x_pt_save <- c(v_x_pt_save, df_interv[v_pos[2], s_x_var])
						}
						else {
							v_x_mpt_save <- c(v_x_mpt_save, df_interv[v_pos[2], s_x_var])
						}
						
						if (length(v_x_mpt_save) > 0) {
							v_x_start <- c(v_x_pt_save, v_x_mpt_save[seq(1, length(v_x_mpt_save), by = 2)])
							v_x_end <- c(rep(NA, length(v_x_pt_save)), v_x_mpt_save[seq(2, length(v_x_mpt_save), by = 2)])
						}
						else {
							v_x_start <- v_x_pt_save
							v_x_end <- rep(NA, length(v_x_pt_save))
						}
						
						df_add <- data.frame("x1" = v_x_start, "x2" = v_x_end, "var_name" = rep(l_click_info[[3]], length(v_x_start)), "num" = rep(e_current_flag$num, length(v_x_start)), "geom" = c(rep("pt", length(v_x_pt_save)), rep("mpt", length(v_x_mpt_save) / 2)))
						v_x_pt <- c(v_x_pt, df_interv[i_pt_pos, s_x_var])
					}
					else {
						if (abs(diff(v_pos)) == 1) {
							df_add <- data.frame("x1" = o_click_graph$prev_date, "x2" = l_click_info[[1]], "var_name" = l_click_info[[3]], "num" = e_current_flag$num, "geom" = "mpt")
						}
						else {
							v_pos <- v_pos[order(v_pos)]
							
							if (is.na(df_interv[v_pos[1] + 1, l_click_info[[3]]])) {
								df_add <- data.frame("x1" = c(o_click_graph$prev_date, l_click_info[[1]]), "x2" = rep(NA, 2), "var_name" = rep(l_click_info[[3]], 2), "num" = rep(e_current_flag$num, 2), "geom" = rep("pt", 2))
							}
							else {
								df_add <- data.frame("x1" = o_click_graph$prev_date, "x2" = l_click_info[[1]], "var_name" = l_click_info[[3]], "num" = e_current_flag$num, "geom" = "mpt")
							}
						}
						
						v_x_pt <- rep(df_interv[i_pt_pos, s_x_var], 2)
					}
					
					if ("coord" %in% ls(e_current_flag)) {
						df_coord <- rbind(e_current_flag$coord, df_add)
					}
					else {
						df_coord <- df_add
					}
					
					s_prev_date <- NULL
					s_prev_var <- NULL
					
					df_add <- data.frame("x" = df_interv[v_mpt_pos, s_x_var], "y" = v_y, "var_name" = rep(l_click_info[[3]], length(v_y)), "num" = rep(e_current_flag$num, length(v_y)))
					df_all_coord <- rbind(e_current_flag$all_coord, df_add)
					
					if (length(v_y_pt) > 0) {
						v_y_pt <- c(v_y_pt, df_interv[i_pt_pos, l_click_info[[3]]])
					}
					else {
						v_y_pt <- rep(df_interv[i_pt_pos, l_click_info[[3]]], 2)
					}
					
					df_marker <- data.frame("x" = v_x_pt, "y" = v_y_pt)
					df_line <- data.frame("x" = v_x_all, "y" = v_y_all)
					v_button_name <- "save_button"
					return (list(coord = df_coord, all_coord = df_all_coord, prev_date = s_prev_date, prev_var = s_prev_var, marker = df_marker, line = df_line, button_name = v_button_name, message = s_message)) 
				}
				else { # error message returned
					s_message <- paste0("Please click on the same variable (", isolate(o_click_graph$prev_var), ")")
					return (list(message = s_message))
				}
			}
			else { # first click event
				if ("coord" %in% ls(e_current_flag)) {
					i_num <- e_current_flag$num + 1
					df_add <- data.frame("x" = l_click_info[[1]], "y" = l_click_info[[2]], "var_name" = l_click_info[[3]], "num" = i_num)
					df_all_coord <- rbind(e_current_flag$all_coord, df_add)
				}
				else {
					i_num <- 1
					df_all_coord <- data.frame("x" = l_click_info[[1]], "y" = l_click_info[[2]], "var_name" = l_click_info[[3]], "num" = i_num)
				}
				
				s_prev_date <- l_click_info[[1]]
				s_prev_var <- l_click_info[[3]]
				i_click_iter <- 1
				v_button_name <- c("clear1_button", "clear2_button")
				return (list(num = i_num, all_coord = df_all_coord, click_iter = i_click_iter, prev_date = s_prev_date, prev_var = s_prev_var, button_name = v_button_name, message = s_message))
			}
		}
	}
	else { # ir
		if ("coord" %in% ls(e_current_flag)) {
			df_add <- data.frame("id" = l_click_info[[1]], "pt" = ifelse(length(which(!is.na(isolate(o_plot$pt_pos)))) > 0, 1, 0)) 
			df_coord <- rbind(e_current_flag$coord, df_add)
		}
		else {
			df_coord <- data.frame("id" = l_click_info[[1]], "pt" = ifelse(length(which(!is.na(isolate(o_plot$pt_pos)))) > 0, 1, 0)) 
		}
		
		return(df_coord)
	}
}

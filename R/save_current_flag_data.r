#' Saving the flag data

#' @description
#' `f_save_current_flag_data` is used to create/update the flag data. If the
#' "with flags" box (left panel) is not checked, then a new flag data is created,
#' else the current flag data (e_current_flag$coord created from the
#' `f_create_current_flag_data` function) is added to the previous (existing) flag
#' data (e_data$flag).

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_action is the value of "action" radio button in the Flag tab (2 values:
#' "add_flag", "replace_qc").
#' @param df_previous_flag are flag data saved in the e_data environment
#' (e_data$flag). This input is NULL if the "with flags" box is not checked (left 
#' panel).
#' @param df_current_flag are flag data saved in the e_current_flag environment
#' (e_current_flag$coord).
#' @param df_all are data saved in the o_plot reactive value (o_plot$data). This
#' input is only used for the temporal data type (default = NULL).
#' @param s_id_var is the ID variable name (input only used for the normal/IR data
#' type, default = NA).
#' @param s_x_var is the X variable name (input only used for the temporal data
#' type, default = NA).
#' @param i_qc is the value of the "qc" radio button in the Flag tab (input only
#' used for the temporal/IR data type, default = NA).
#' @param s_comment is the comment added from the corresponding field in the Flag
#' tab (input only used for the temporal/IR data type, default = "").

#' @encoding UTF-8

f_save_current_flag_data <- function (s_data_type = "normal", s_action = "add_flag", df_previous_flag, df_current_flag, df_all = NULL, s_id_var = NA, s_x_var = NA, i_qc = NA, s_comment = "") {
	if (s_data_type == "normal") {
		df_flag_add <- df_current_flag[, -which(names(df_current_flag) == "num")]
		names(df_flag_add)[1] <- ifelse(!is.na(s_id_var), s_id_var, ".row_num.")
		
		if (!is.null(df_previous_flag)) {
			df_flag_add <- df_flag_add[order(df_flag_add[, 1]),]
			df_previous_flag <- df_previous_flag[order(df_previous_flag[, 1]),]
			v_var_in <- names(df_flag_add)[-1][which(names(df_flag_add)[-1] %in% names(df_previous_flag))] 
			v_var_out <- names(df_flag_add)[-1][which(!names(df_flag_add)[-1] %in% names(df_previous_flag))]
			v_id_in <- as.vector(df_flag_add[, 1])[which(df_flag_add[, 1] %in% as.vector(df_previous_flag[, 1]))]
			v_id_out <- as.vector(df_flag_add[, 1])[which(!df_flag_add[, 1] %in% as.vector(df_previous_flag[, 1]))]
			
			if (length(v_var_out) > 0) {
				df_var_out <- as.data.frame(matrix(data = 0, ncol = length(v_var_out), nrow = dim(df_previous_flag)[1]))
				names(df_var_out) <- v_var_out
				df_previous_flag <- cbind(df_previous_flag, df_var_out)
				
				if (length(v_var_in) > 0) {
					v_var <- c(v_var_in, v_var_out)
				}
				else {
					v_var <- v_var_out
				}
				
				if (length(v_id_in) > 0) {
					df_previous_flag[df_previous_flag[, 1] %in% v_id_in, v_var] <- df_previous_flag[df_previous_flag[, 1] %in% v_id_in, v_var] + df_flag_add[df_flag_add[, 1] %in% v_id_in, v_var]
				}
				
				if (length(v_id_out) > 0) {
					df_id_out <- as.data.frame(matrix(data = 0, ncol = length(names(df_previous_flag)), nrow = length(v_id_out)))
					names(df_id_out) <- names(df_previous_flag)
					df_id_out[, 1] <- df_flag_add[df_flag_add[, 1] %in% v_id_out, 1]
					df_previous_flag <- rbind(df_previous_flag, df_id_out)
					df_previous_flag[df_previous_flag[, 1] %in% v_id_out, v_var] <- df_flag_add[df_flag_add[, 1] %in% v_id_out, v_var]
				}
			}
			else {
				if (length(v_id_in) > 0) {
					df_previous_flag[df_previous_flag[, 1] %in% v_id_in, v_var_in] <- df_previous_flag[df_previous_flag[, 1] %in% v_id_in, v_var_in] + df_flag_add[df_flag_add[, 1] %in% v_id_in, v_var_in]
				}
				
				if (length(v_id_out) > 0) {
					df_id_out <- as.data.frame(matrix(data = 0, ncol = length(names(df_previous_flag)), nrow = length(v_id_out)))
					names(df_id_out) <- names(df_previous_flag)
					df_id_out[, 1] <- df_flag_add[df_flag_add[, 1] %in% v_id_out, 1]
					df_previous_flag <- rbind(df_previous_flag, df_id_out)
					df_previous_flag[df_previous_flag[, 1] %in% v_id_out, v_var_in] <- df_flag_add[df_flag_add[, 1] %in% v_id_out, v_var_in]
				}
			}
		}
		else {
			df_previous_flag <- df_flag_add
		}
	}
	else if (s_data_type == "temporal") {
		df_flag_add <- cbind(df_current_flag, rep(i_qc, dim(df_current_flag)[1]), rep(ifelse(s_comment == "", NA, s_comment), dim(df_current_flag)[1]))
		df_flag_add <- df_flag_add[, c(1:3, 6, 7)]
		names(df_flag_add) <- c("date_start", "date_end", "var_name", "qc", "comment")
		df_flag_add$date_start <- as.vector(df_flag_add$date_start)
		df_flag_add$date_end <- as.vector(df_flag_add$date_end)
		df_flag_add$date_end <- ifelse(is.na(df_flag_add$date_end), df_flag_add$date_start, df_flag_add$date_end)
		v_pos <- which(as.numeric(as.POSIXct(df_flag_add$date_start, tz = "GMT")) > as.numeric(as.POSIXct(df_flag_add$date_end, tz = "GMT")))
		
		if (length(v_pos) > 0) {
			v_col_1 <- df_flag_add[v_pos, "date_start"]
			v_col_2 <- df_flag_add[v_pos, "date_end"]
			df_flag_add[v_pos, "date_start"] <- v_col_2
			df_flag_add[v_pos, "date_end"] <- v_col_1
		}
		
		if (s_action == "add_flag") { 
			if (!is.null(df_previous_flag)) {
				df_previous_flag <- rbind(df_previous_flag, df_flag_add)
			}
			else {
				df_previous_flag <- df_flag_add
			}
		} # replace_qc
		else {
			df_flag_save <- df_previous_flag
			df_flag_save$date_start <- as.vector(df_flag_save$date_start)
			df_flag_save$date_end <- as.vector(df_flag_save$date_end)
		
			v_var_name <- as.vector(unique(df_flag_add$var_name))
			
			for (i in 1:length(v_var_name)) {
				df_flag_var <- df_flag_add[df_flag_add$var_name == v_var_name[i],]
				df_flag_var$date_start_trf <- as.numeric(as.POSIXct(df_flag_var$date_start, tz = "GMT"))
				df_flag_var$date_end_trf <- as.numeric(as.POSIXct(df_flag_var$date_end, tz = "GMT"))
				
				for (j in 1:dim(df_flag_var)[1]) {
					df_flag_save$qc <- as.vector(df_flag_save$qc)
					df_flag_save$comment <- as.vector(df_flag_save$comment)
					df_flag_save$date_start_trf <- as.numeric(as.POSIXct(df_flag_save$date_start, tz = "GMT"))
					df_flag_save$date_end_trf <- as.numeric(as.POSIXct(df_flag_save$date_end, tz = "GMT"))
					v_pos_1 <- which(df_flag_save$var_name == v_var_name[i] & df_flag_save$qc == 1 & ((df_flag_save$date_start_trf >= df_flag_var[j, "date_start_trf"] & df_flag_save$date_end_trf <= df_flag_var[j, "date_end_trf"]) | (df_flag_var[j, "date_start_trf"] > df_flag_save$date_start_trf & df_flag_var[j, "date_end_trf"] < df_flag_save$date_end_trf) | (df_flag_var[j, "date_start_trf"] >= df_flag_save$date_start_trf & df_flag_var[j, "date_end_trf"] < df_flag_save$date_end_trf) | (df_flag_var[j, "date_start_trf"] > df_flag_save$date_start_trf & df_flag_var[j, "date_end_trf"] <= df_flag_save$date_end_trf)))
					v_pos_2 <- which(df_flag_save$var_name == v_var_name[i] & df_flag_save$qc == 1 & df_flag_save$date_end_trf > df_flag_var[j, "date_start_trf"] & df_flag_save$date_end_trf < df_flag_var[j, "date_end_trf"] & df_flag_save$date_start_trf < df_flag_var[j, "date_start_trf"])
					v_pos_3 <- which(df_flag_save$var_name == v_var_name[i] & df_flag_save$qc == 1 & df_flag_save$date_start_trf < df_flag_var[j, "date_end_trf"] & df_flag_save$date_start_trf > df_flag_var[j, "date_start_trf"] & df_flag_save$date_end_trf > df_flag_var[j, "date_end_trf"])
					
					if (s_comment == "") {
						if (length(v_pos_1) > 0) {
							if (length(v_pos_2) > 0 | length(v_pos_3) > 0) {
								df_flag_save[v_pos_1, "qc"] <- 2
							}
							else {
								if (length(v_pos_1) == 1) {
									if (df_flag_save[v_pos_1, "date_start_trf"] == df_flag_var[j, "date_start_trf"] & df_flag_save[v_pos_1, "date_end_trf"] == df_flag_var[j, "date_end_trf"]) {
										df_flag_save[v_pos_1, "qc"] <- 2
									}
									else {
										if (df_flag_save[v_pos_1, "date_start_trf"] != df_flag_var[j, "date_start_trf"] & df_flag_save[v_pos_1, "date_end_trf"] != df_flag_var[j, "date_end_trf"]) {
											df_rows <- data.frame("var_name" = rep(v_var_name[i], 3), "date_start" = c(df_flag_save[v_pos_1, "date_start"], df_flag_var[j, "date_start"], as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var])), "date_end" = c(as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), df_flag_var[j, "date_end"], df_flag_save[v_pos_1, "date_end"]), "qc" = c(1, 2, 1), "comment" = rep(as.vector(df_flag_save[v_pos_1, "comment"]), 3), "date_start_trf" = rep(NA, 3), "date_end_trf" = rep(NA, 3))
										}
										else if (df_flag_save[v_pos_1, "date_start_trf"] == df_flag_var[j, "date_start_trf"] & df_flag_save[v_pos_1, "date_end_trf"] != df_flag_var[j, "date_end_trf"]) {
											df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_var[j, "date_start"], as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var])), "date_end" = c(df_flag_var[j, "date_end"], df_flag_save[v_pos_1, "date_end"]), "qc" = c(2, 1), "comment" = rep(as.vector(df_flag_save[v_pos_1, "comment"]), 2), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
										}
										else {
											df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_save[v_pos_1, "date_start"], df_flag_var[j, "date_start"]), "date_end" = c(as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), df_flag_var[j, "date_end"]), "qc" = c(1, 2), "comment" = rep(as.vector(df_flag_save[v_pos_1, "comment"]), 2), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
										}
										
										df_flag_save <- df_flag_save[-v_pos_1,]
										df_flag_save <- rbind(df_flag_save, df_rows)
									}
								}
								else {
									df_flag_save[v_pos_1, "qc"] <- 2
								}
							}
						}
						
						if (length(v_pos_2) + length(v_pos_3) > 0) {
							if (length(v_pos_2) > 0) {
								df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_save[v_pos_2, "date_start"], df_flag_var[j, "date_start"]), "date_end" = c(as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), df_flag_save[v_pos_2, "date_end"]), "qc" = c(1, 2), "comment" = rep(as.vector(df_flag_save[v_pos_2, "comment"]), 2), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
								df_flag_save <- rbind(df_flag_save, df_rows)
							}
							
							if (length(v_pos_3) > 0) {
								df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_save[v_pos_3, "date_start"], as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var])), "date_end" = c(df_flag_var[j, "date_end"], df_flag_save[v_pos_3, "date_end"]), "qc" = c(2, 1), "comment" = rep(as.vector(df_flag_save[v_pos_3, "comment"]), 2), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
								df_flag_save <- rbind(df_flag_save, df_rows)
							}
							
							df_flag_save <- df_flag_save[-c(v_pos_2, v_pos_3),]
						}
					}
					else {
						if (length(v_pos_1) == 1) {
							if (df_flag_save[v_pos_1, "date_start_trf"] != df_flag_var[j, "date_start_trf"] | df_flag_save[v_pos_1, "date_end_trf"] != df_flag_var[j, "date_end_trf"]) {
								if (df_flag_save[v_pos_1, "date_start_trf"] != df_flag_var[j, "date_start_trf"] & df_flag_save[v_pos_1, "date_end_trf"] != df_flag_var[j, "date_end_trf"]) {
									df_rows <- data.frame("var_name" = rep(v_var_name[i], 3), "date_start" = c(df_flag_save[v_pos_1, "date_start"], df_flag_var[j, "date_start"], as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var])), "date_end" = c(as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), df_flag_var[j, "date_end"], df_flag_save[v_pos_1, "date_end"]), "qc" = c(1, 2, 1), "comment" = c(as.vector(df_flag_save[v_pos_1, "comment"]), as.vector(df_flag_var[j, "comment"]), as.vector(df_flag_save[v_pos_1, "comment"])), "date_start_trf" = rep(NA, 3), "date_end_trf" = rep(NA, 3))
								}
								else if (df_flag_save[v_pos_1, "date_start_trf"] == df_flag_var[j, "date_start_trf"] & df_flag_save[v_pos_1, "date_end_trf"] != df_flag_var[j, "date_end_trf"]) {
									df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_var[j, "date_start"], as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var])), "date_end" = c(df_flag_var[j, "date_end"], df_flag_save[v_pos_1, "date_end"]), "qc" = c(2, 1), "comment" = c(as.vector(df_flag_var[j, "comment"]), as.vector(df_flag_save[v_pos_1, "comment"])), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
								}
								else {
									df_rows <- data.frame("var_name" = rep(v_var_name[i], 2), "date_start" = c(df_flag_save[v_pos_1, "date_start"], df_flag_var[j, "date_start"]), "date_end" = c(as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), df_flag_var[j, "date_end"]), "qc" = c(1, 2), "comment" = c(as.vector(df_flag_save[v_pos_1, "comment"]), as.vector(df_flag_var[j, "comment"])), "date_start_trf" = rep(NA, 2), "date_end_trf" = rep(NA, 2))
								}
								
								df_flag_save <- rbind(df_flag_save, df_rows)
								df_flag_save <- df_flag_save[-v_pos_1,]
							}
							else {
								df_flag_save[v_pos_1, c("qc", "comment")] <- c(2, as.vector(df_flag_var[j, "comment"]))
							}
						}
						else {
							if (length(v_pos_2) > 0) {
								df_rows <- data.frame("var_name" = v_var_name[i], "date_start" = df_flag_save[v_pos_2, "date_start"], "date_end" = as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_start_trf"]) - 1, s_x_var]), "qc" = 1, "comment" = as.vector(df_flag_save[v_pos_2, "comment"]), "date_start_trf" = NA, "date_end_trf" = NA)
								df_flag_save <- rbind(df_flag_save, df_rows)
							}
							
							df_rows <- data.frame("var_name" = v_var_name[i], "date_start" = df_flag_var[j, "date_start"], "date_end" = df_flag_var[j, "date_end"], "qc" = 2, "comment" = as.vector(df_flag_var[j, "comment"]), "date_start_trf" = NA, "date_end_trf" = NA)
							df_flag_save <- rbind(df_flag_save, df_rows)
							
							if (length(v_pos_3) > 0) {
								df_rows <- data.frame("var_name" = v_var_name[i], "date_start" = as.vector(df_all[which(df_all[, paste0(s_x_var, "_trf")] == df_flag_var[j, "date_end_trf"]) + 1, s_x_var]), "date_end" = df_flag_save[v_pos_3, "date_end"], "qc" = 1, "comment" = as.vector(df_flag_save[v_pos_3, "comment"]), "date_start_trf" = NA, "date_end_trf" = NA)
								df_flag_save <- rbind(df_flag_save, df_rows)
							}
							
							df_flag_save <- df_flag_save[-c(v_pos_2, v_pos_3),]
						}
					}
				}
			}
			
			df_flag_save <- df_flag_save[, -which(names(df_flag_save) %in% c("date_start_trf", "date_end_trf"))]
			df_previous_flag <- df_flag_save
		}
		
		df_previous_flag$comment <- as.vector(df_previous_flag$comment)
		df_previous_flag$comment <- ifelse(df_previous_flag$comment == "" & !is.na(df_previous_flag$comment), NA, df_previous_flag$comment)
	}
	else { # ir
		if (s_action == "add_flag") {
			df_flag_add <- data.frame("id" = as.vector(df_current_flag[, 1]), "qc" = rep(i_qc, dim(df_current_flag)[1]), "comment" = rep(ifelse(s_comment == "", NA, s_comment), dim(df_current_flag)[1]))
			names(df_flag_add)[1] <- ifelse(!is.na(s_id_var), s_id_var, ".row_num.") 
			
			if (!is.null(df_previous_flag)) {
				df_previous_flag <- rbind(df_previous_flag, df_flag_add)
			}
			else {
				df_previous_flag <- df_flag_add
			}
		} # replace_qc
		else {
			v_pos <- which(df_previous_flag[, 1] %in% as.vector(df_current_flag[, 1]))
			df_previous_flag[v_pos, "qc"] <- 2
			
			if (s_comment != "") {
				df_previous_flag[v_pos, "comment"] <- s_comment
			}
		}
		
		df_previous_flag$comment <- as.vector(df_previous_flag$comment)
		df_previous_flag$comment <- ifelse(df_previous_flag$comment == "" & !is.na(df_previous_flag$comment), NA, df_previous_flag$comment)
	}
	
	return(df_previous_flag)
}

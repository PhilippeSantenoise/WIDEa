#' Loading all data

#' @description
#' `f_load_data` returns a list including (1) the loaded data, (2) data information
#' (related with the load1 button: flag data, flag filename, vector of the code
#' range for IR data type and a boolean value corresponding to a special enabling of
#' the "ID" selectize input) and (3) an error/warning message if problem occured
#' with loaded data.

#' @param s_id is the action button input ID's used to load data (3 values:
#' "load1_button", "load2_button", "load3_button").
#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param s_path is the data path.
#' @param o_data_opt is a reactive value includig data option: decimal separator,
#' encoding.
#' @param b_flag is a boolean value used to load flag data when the "load1_button"
#' action button is clicked.
#' @param s_model is the model type (3 values: "none", "calib", "valid"). This input
#' is only used when the "load3_button" action button is clicked.

#' @encoding UTF-8

f_load_data <- function(s_id, s_data_type, s_path, o_data_opt, b_flag = F, s_model = "none") {
	s_e_message <- NA
	
	if (s_id == "load1_button") {
		s_w_message <- NA
		df_1 <- tryCatch({suppressWarnings(data.table::fread(file = s_path, data.table = F, dec = o_data_opt$dec[1], encoding = o_data_opt$encoding[1]))}, error = function(e) F) 
		df_flag <- data.frame()
		v_code_range <- c()
		b_special <- F
		s_flag_name <- NA
		
		if (!is.data.frame(df_1)) {
			s_e_message <- "Path/file doesn't exist"
		}
		else {
			if (dim(df_1)[1] == 0) {
				s_e_message <- "Data is empty"
			}
			else {
				if (length(grep("[(]|[)]|[[]|[]]|-|:|[+]|/| ", names(df_1))) > 0) {
					s_e_message <- "Variable name should not contains any white-spaces or special characters, such as: parenthesis, square bracket, colon, minus, plus, forward slash."
				}
				else {
					v_split_path <- unlist(strsplit(s_path, split = "[.]"))
					
					if (s_data_type %in% c("normal", "ir")) {
						s_flag_path_1 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_", ifelse(s_data_type == "normal", "norm", "ir"), "_flag.", v_split_path[length(v_split_path)])
						s_flag_path_2 <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_", ifelse(s_data_type == "normal", "norm", "ir"), "_flag_withID.", v_split_path[length(v_split_path)])
						v_split_path_1 <- unlist(strsplit(s_flag_path_1, split = "/"))
						v_split_path_2 <- unlist(strsplit(s_flag_path_2, split = "/"))
						b_cond_1 <- v_split_path_1[length(v_split_path_1)] %in% list.files(paste0(paste(v_split_path_1[1:(length(v_split_path_1) - 1)], collapse = "/"), "/"))
						b_cond_2 <- v_split_path_2[length(v_split_path_2)] %in% list.files(paste0(paste(v_split_path_2[1:(length(v_split_path_2) - 1)], collapse = "/"), "/"))
						b_cond <- b_cond_1 | b_cond_2
					}
					else { # temporal
						s_flag_path <- paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "."), "_temp_flag.", v_split_path[length(v_split_path)])
						v_split_path <- unlist(strsplit(s_flag_path, split = "/"))
						b_cond <- v_split_path[length(v_split_path)] %in% list.files(paste0(paste(v_split_path[1:(length(v_split_path) - 1)], collapse = "/"), "/"))
						s_flag_name <- ifelse(b_cond, v_split_path[length(v_split_path)], NA)
					}
					
					if (s_data_type == "normal") { 
						v_pos <- which(c(".concat1.", ".concat2.") %in% names(df_1))
						
						if (length(v_pos) > 0) {
							if (length(v_pos) == 2) {
								v_concat <- rep(1, 2)
								s_w_message <- "The concatenation option will be disabled for X and Group variables, because variables named .concat1. and .concat2. already exists in loaded data"
							}
							else {
								if (v_pos == 1) {
									v_concat <- c(1, 0)
									s_w_message <- "The concatenation option will be disabled for X variable, because the variable named .concat1. already exists in loaded data"
								}
								else {
									v_concat <- c(0, 1)
									s_w_message <- "The concatenation option will be disabled for Group variable, because the variable named .concat2. already exists in loaded data"
								}
							}
						}
					}
					
					if (b_flag) {
						if (b_cond) {
							if (s_data_type == "normal") {
								if (b_cond_1 & b_cond_2) {
									s_e_message <- paste0("The flag file is not unique. Please keep one file (", v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)], ").")
								}
								else {
									if (b_cond_2) {
										s_flag_name <- v_split_path_2[length(v_split_path_2)]
										df_flag <- data.table::fread(file = s_flag_path_2, data.table = F, encoding = o_data_opt$encoding[1])
										
										if (dim(df_flag)[1] == 0) {
											s_e_message <- "Flag data is empty"
										}
										else {
											if (names(df_flag)[1] %in% names(df_1)) {
												b_special <- T
											}
											else {
												s_e_message <- paste0("ID flag variable (", names(df_flag)[1], ") is missing in data")
											}
										}
									}
									else {
										s_flag_name <- v_split_path_1[length(v_split_path_1)]
										df_flag <- data.table::fread(file = s_flag_path_1, data.table = F, encoding = o_data_opt$encoding[1])
										if (dim(df_flag)[1] == 0) {s_e_message <- "Flag data is empty"}
									}
								}
							}
							else if (s_data_type == "temporal") {
								df_flag <- data.table::fread(file = s_flag_path, data.table = F, encoding = o_data_opt$encoding[1])
								if (dim(df_flag)[1] == 0) {s_e_message <- "Flag data is empty"}
							}
							else {
								if (b_cond_1 & b_cond_2) {
									s_e_message <- paste0("The flag file is not unique. Please keep one file (", v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)], ").")
								}
								else {
									if (b_cond_2) {
										s_flag_name <- v_split_path_2[length(v_split_path_2)]
										df_flag <- data.table::fread(file = s_flag_path_2, data.table = F, encoding = o_data_opt$encoding[1])
									}
									else {
										s_flag_name <- v_split_path_1[length(v_split_path_1)]
										df_flag <- data.table::fread(file = s_flag_path_1, data.table = F, encoding = o_data_opt$encoding[1])
									}
									
									if (dim(df_flag)[1] == 0) {
										s_e_message <- "Flag data is empty"
									}
									else {
										if (b_cond_2 & !names(df_flag)[1] %in% names(df_1)) {
											s_e_message <- paste0("ID flag variable (", names(df_flag)[1], ")")
										}
										
										v_pos_1 <- which(substr(names(df_1), 1, 1) %in% c("M", "N"))
										
										if (length(v_pos_1) > 0) {
											v_pos_2 <- which(!is.na(suppressWarnings(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))) 
											
											if (length(v_pos_2) > 0) {
												v_pos_1 <- v_pos_1[v_pos_2]
												b_cond_3 <- T
											}
											else {
												b_cond_3 <- F
											}
										}
										else {
											b_cond_3 <- F
										}
										
										if (b_cond_3) {
											eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_1$", names(df_1)[v_pos_1], ") | length(which(!is.na(df_1$", names(df_1)[v_pos_1], "))) == 0"), collapse = ", "), ")")))
											
											if (length(which(!v_cond)) > 0) {
												if (!is.na(s_e_message)) {
													s_e_message <- paste0(s_e_message, "is missing in data<br/>", paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(!v_cond)], collapse = ", "))
												}
												else {
													s_e_message <- paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(!v_cond)], collapse = ", ")
												}
											}
											else {
												if (!is.na(s_e_message)) {
													s_e_message <- paste0(s_e_message, "is missing in data")
												}
												else {
													v_code_range <- paste0(substr(names(df_1)[v_pos_1[1]], 1, 1), range(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))
													if (b_cond_2) {b_special <- T}
												}
											}
										}
										else {
											if (!is.na(s_e_message)) {
												s_e_message <- paste0(s_e_message, "and all code variables are missing in data")
											}
											else {
												s_e_message <- "All code variables are missing in data"
											}
										}
									}
								}
							}
						}
						else {
							s_e_message <- paste0("Flag data doesn't exist (", ifelse(s_data_type == "temporal", v_split_path[length(v_split_path)], paste0(v_split_path_1[length(v_split_path_1)], " or ", v_split_path_2[length(v_split_path_2)])), "). Please uncheck the flag box.")
						}
					}
					else {
						if (b_cond) {
							if (s_data_type == "temporal") {
								s_flag_name_msg <- v_split_path[length(v_split_path)]
							}
							else {
								if (b_cond_1) {
									s_flag_name_msg <- v_split_path_1[length(v_split_path_1)]
								}
								else {
									s_flag_name_msg <- v_split_path_2[length(v_split_path_2)]
								}
							}
							
							s_w_message <- paste0(ifelse(!is.na(s_w_message), paste0(s_w_message, ".<br/>"), ""), "A flag file (", s_flag_name_msg, ") exists but the corresponding box is not checked. If you save new flags, the previous flag(s) will be removed. Please check the box, if you want to keep previous flag(s).")
						}
						
						if (s_data_type == "ir") {
							v_pos_1 <- which(substr(names(df_1), 1, 1) %in% c("M", "N"))
							
							if (length(v_pos_1) > 0) {
								v_pos_2 <- which(!is.na(suppressWarnings(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))) 
								
								if (length(v_pos_2) > 0) {
									v_pos_1 <- v_pos_1[v_pos_2]
									b_cond_3 <- T
								}
								else {
									b_cond_3 <- F
								}
							}
							else {
								b_cond_3 <- F
							}
							
							if (b_cond_3) {
								eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_1$", names(df_1)[v_pos_1], ") | length(which(!is.na(df_1$", names(df_1)[v_pos_1], "))) == 0"), collapse = ", "), ")")))
								
								if (length(which(!v_cond)) > 0) {
									s_e_message <- paste("No numerical values for the following code variables: ", names(df_1)[v_pos_1][which(!v_cond)], collapse = ", ")
								}
								else {
									v_code_range <- paste0(substr(names(df_1)[v_pos_1[1]], 1, 1), range(as.numeric(substr(names(df_1)[v_pos_1], 2, nchar(names(df_1)[v_pos_1])))))
								}
							}
							else {
								s_e_message <- "All code variables are missing in data"
							}
						}
					}
				}
			}
		}
		
		return(list(df_1, df_flag, s_flag_name, v_code_range, s_e_message, s_w_message, b_special))
	}
	else if (s_id == "load2_button") {
		df_1 <- tryCatch({suppressWarnings(data.table::fread(file = s_path, data.table = F, dec = o_data_opt$dec[2], encoding = o_data_opt$encoding[2]))}, error = function(e) F) 
		
		if (!is.data.frame(df_1)) {
			s_e_message <- "Path/file doesn't exist"
		}
		else {
			if (dim(df_1)[1] == 0) {
				s_e_message <- "Code/frequency data is empty"
			}
			else {
				v_cond <- c("Code", "Frequency") %in% names(df_1)
				v_pos <- which(!v_cond)
				
				if (length(v_pos) > 0) {
					s_e_message <- paste0(paste(c("Code", "Frequency")[v_pos], sep = " and "), " variable")
					if (length(v_pos) == 2) {s_e_message <- paste0(s_e_message, "s")}
					s_e_message <- paste0(s_e_message, " missing in code-frequency data")
				}
			}
		}
		
		return(list(df_1, s_e_message))
	}
	else { # load3_button
		s_w_message <- NA
		df_1 <- tryCatch({suppressWarnings(data.table::fread(file = s_path, data.table = F, dec = o_data_opt$dec[3], encoding = o_data_opt$encoding[3]))}, error = function(e) F) 
		
		if (!is.data.frame(df_1)) {
			s_e_message <- "Path/file doesn't exist"
		}
		else {
			if (dim(df_1)[1] == 0) {
				s_e_message <- "Data is empty"
			}
			else {
				v_name <- c("parameter", "value")
				v_pos_1 <- which(v_name %in% names(df_1))
				
				if (length(v_pos_1) < 2) {
					if (length(v_pos_1) == 1) {
						s_e_message <- paste0("The following variable is missing in model parameter data: ", v_name[-v_pos_1])
					}
					else {
						s_e_message <- "The following variables are missing in model parameter data: parameter, value"
					}
				}
				else {
					if (!is.numeric(df_1$value)) {
						s_e_message <- "The value variable is not numeric (decimal is a point)"
					}
					else {
						v_param <- as.vector(unique(df_1$parameter)) 
						v_pos_1 <- which(v_param == "sigma")
						
						if (length(v_pos_1) > 0) {
							v_num <- gsub("a|d|re|gr|[|]|:", "", v_param[-v_pos_1])
						}
						else {
							v_num <- gsub("a|d|re|gr|[|]|:", "", v_param)
						}
						
						v_num <- suppressWarnings(as.numeric(v_num))
						
						if (length(which(is.na(v_num))) == 0) {
							v_pos_2 <- grep("a", v_param)
							v_pos_3 <- grep("d", v_param)
							v_pos_4 <- grep("re", v_param)
							v_pos_5 <- grep("gr", v_param)
							
							if (length(v_pos_2) > 0) {
								if (length(which(v_pos_2 %in% v_pos_3)) == 0 & length(which(v_pos_2 %in% v_pos_5)) == 0 & length(which(v_pos_3 %in% v_pos_4)) == 0 & length(which(v_pos_4 %in% v_pos_5)) == 0) {
									v_pos_6 <- grep("[|]", v_param)
									v_pos_7 <- grep(":", v_param)
								
									if (length(v_pos_6) > 0) {
										v_name <- substr(v_param[v_pos_6], unlist(gregexpr("[|]", v_param[v_pos_6])) + 1, nchar(v_param[v_pos_6]))
										if (length(v_pos_7) > 0) {v_name <- c(v_name[-grep(":", v_name)], substr(v_name[grep(":", v_name)], 1, unlist(gregexpr(":", v_name[grep(":", v_name)])) - 1), substr(v_name[grep(":", v_name)], unlist(gregexpr(":", v_name[grep(":", v_name)])) + 1, nchar(v_name[grep(":", v_name)])))}
										v_name <- unique(v_name)
										
										if (length(which(v_name %in% names(df_1))) < length(v_name)) {
											s_e_message <- paste0("The following variable is missing in model parameter data: ", paste(v_name[which(!v_name %in% names(df_1))], collapse = ", "))
											b_cond <- F
										}
										else {
											b_cond <- T
										}
									}
									else {
										b_cond <- T
									}
									
									if (b_cond & length(v_pos_1) == 0 & s_model == "calib") {s_w_message <- "As the residual standard deviation (sigma) is missing in model parameters, then:<br/>(1) The standardized residuals are not calculated ;<br/>(2) Fields corresponding to the weighted residuals are disabled ;<br/>(3) The qqplot will be not displayed."}
								}
								else {
									s_e_message <- "Invalid association between model parameters"
								}
							}
							else {
								s_e_message <- "Model with no coefficient (parameter beginning with a)" 
							}
						}
						else {
							s_e_message <- "Invalid model parameters"
						}
					}
				}
			}
		}
		
		return(list(df_1, s_e_message, s_w_message))
	}
}
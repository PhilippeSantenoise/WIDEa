#' @importFrom shiny isolate
NULL

#' Checking process: fields used to apply (f, g, h) functions on (X, Y, Z) variables
#' are filled in without error ? 

#' @description
#' `f_check_trsf_variables` execute a checking process in several steps: 
#' \cr(1) Checking (X, Y, Z) variables codes filled in the (f, g, h) text fields
#' (variable code specified by the expand buttons);  
#' \cr(2) Checking the calculation of (f, g, h) functions. 
#' \cr\cr The output is a list including all transformed variables or an error
#' message if a problem occured with the checking process.

#' @param s_data_type is the data type (2 values: "normal", "temporal").
#' @param df_all are data created by the `f_prepare_data` function (process 1).
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.

#' @encoding UTF-8

f_check_trsf_variables <- function (s_data_type = "normal", df_all, o_parameter) {
	if (s_data_type == "normal") {
		v_f_val <- NULL
		v_g_val <- NULL
		v_h_val <- NULL
		v_message_out <- c()
		v_num <- which(c(!is.na(isolate(o_parameter$f)), !is.na(isolate(o_parameter$g)), !is.na(isolate(o_parameter$h))))
		v_fun <- c("f", "g", "h")
		v_var_letter <- c("x", "y", "z")
		
		v_message <- unlist(lapply(v_num, function(i) {
			s_out <- "-"
			s_formula <- eval(parse(text = paste0("isolate(o_parameter$", v_fun[i], ")")))
			i_num <- eval(parse(text = paste0("length(isolate(o_parameter$", v_var_letter[i], "))"))) 
			v_var_letter_i <- paste0(v_var_letter[i], 1:i_num)
			
			v_var <- unlist(lapply(v_var_letter_i, function(j) {
				s_var <- "-"
				v_split <- unlist(strsplit(s_formula, j))
				
				if (length(v_split) > 1) {
					if (substr(s_formula, nchar(s_formula) - nchar(j) - 1, nchar(s_formula)) == j) {
						v_split <- c(v_split, "")
					}
					
					v_split <- ifelse(v_split == "", NA, v_split)
					df_split <- data.frame("col1" = substr(v_split[1:(length(v_split) - 1)], nchar(v_split[1:(length(v_split) - 1)]), nchar(v_split[1:(length(v_split) - 1)])), "col2" = substr(v_split[2:length(v_split)], 1, 1))
					v_pos_1 <- which(as.vector(df_split$col1) %in% c(NA, "^", "(", " ", "+", "*", "-", "/")) 
					v_pos_2 <- which(as.vector(df_split$col2) %in% c(NA, "^", ")", " ", "+", "*", "-", "/"))
					
					if (length(v_pos_1) > 0 & length(v_pos_2) > 0) {
						v_pos <- v_pos_1 %in% v_pos_2
					}
					else {
						v_pos <- c()
					}
					
					if (length(v_pos) == 0) {
						s_var <- j
					}
				}
				else {
					if (nchar(v_split) == nchar(s_formula)) {
						s_var <- j
					}
				}
				
				return(s_var)
			}))
			
			v_pos <- which(v_var != "-")
			
			if (length(v_pos) > 0) {
				s_out <- paste0("Error with ", v_fun[i], "(", v_var_letter[i], "): ", ifelse(length(v_pos) == 1, paste0(v_var[v_pos]," is"), paste0("(", paste(v_var[v_pos], collapse = ", "), ") are")), " missing in the function")
			}
			
			return(s_out)
		}))
		
		if (length(which(v_message != "-")) > 0) {
			v_message_out <- c(v_message_out, v_message[which(v_message != "-")])
		}
		
		if (length(v_message_out) == 0) {
			v_pos <- c()
			
			if (!is.na(isolate(o_parameter$f))) {
				v_name <- isolate(o_parameter$x)
				x1 <- as.vector(df_all[, v_name[1]]) # x1 : code designated the first X variable
				if (length(v_name) > 1) {eval(parse(text = paste0("x", 2:length(v_name), " <- as.vector(df_all[, \"", v_name[-1], "\"])")))} # x2, ... : code designated the other X variable(s)
				v_f_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$f))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_f_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_f_val))) > 0 | length(v_f_val) != length(x1)) {
					v_pos <- c(v_pos, 1)
				}
			}
			
			if (!is.na(isolate(o_parameter$g))) {
				v_name <- isolate(o_parameter$y)
				y1 <- as.vector(df_all[, v_name[1]]) # y1 : code designated the first Y variable
				if (length(v_name) > 1) {eval(parse(text = paste0("y", 2:length(v_name), " <- as.vector(df_all[, \"", v_name[-1], "\"])")))} # y2, ... : code designated the other Y variable(s)
				v_g_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$g))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_g_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_g_val))) > 0 | length(v_g_val) != length(y1)) {
					v_pos <- c(v_pos, 2)
				}
			}
			
			if (!is.na(isolate(o_parameter$h))) {
				v_name <- isolate(o_parameter$z)
				z1 <- as.vector(df_all[, v_name[1]]) # z1 : code designated the first Z variable
				if (length(v_name) > 1) {eval(parse(text = paste0("z", 2:length(v_name), " <- as.vector(df_all[, \"", v_name[-1], "\"])")))} # z2, ... : code designated the other Z variable(s)
				v_h_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$h))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_h_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_h_val))) > 0 | length(v_h_val) != length(z1)) {
					v_pos <- c(v_pos, 3)
				}
			}
			
			if (length(v_pos) > 0) {
				v_message_out <- paste0("Error with the following function(s): ", paste(v_fun[v_pos], collapse = ", "))
			}
		}
		
		l_fun_val <- list("f" = v_f_val, "g" = v_g_val, "h" = v_h_val)
		
		if (length(v_message_out) > 0) {
			s_message_out <- paste(v_message_out, collapse = "<br/>")
		}
		else {
			s_message_out <- character(0)
		}
		
		return (list(l_fun_val[which(lengths(l_fun_val) > 0)], s_message_out))
	}
	else { # temporal
		if (is.null(df_all)) {
			s_message <- character(0)
			v_split <- unlist(strsplit(isolate(o_parameter$g), "y"))
			
			if (length(v_split) > 1) {
				if (substr(isolate(o_parameter$g), nchar(isolate(o_parameter$g)), nchar(isolate(o_parameter$g))) == "y") {
					v_split <- c(v_split, "")
				}
				
				v_split <- ifelse(v_split == "", NA, v_split)
				df_split <- data.frame("col1" = substr(v_split[1:(length(v_split) - 1)], nchar(v_split[1:(length(v_split) - 1)]), nchar(v_split[1:(length(v_split) - 1)])), "col2" = substr(v_split[2:length(v_split)], 1, 1))
				v_pos_1 <- which(as.vector(df_split$col1) %in% c(NA, "^", "(", " ", "+", "*", "-", "/")) 
				v_pos_2 <- which(as.vector(df_split$col2) %in% c(NA, "^", ")", " ", "+", "*", "-", "/"))
				
				if (length(v_pos_1) > 0 & length(v_pos_2) > 0) {
					v_pos_1 <- v_pos_1 %in% v_pos_2
				}
				else {
					v_pos_1 <- c()
				}
				
				if (length(v_pos_1) == 0) {
					s_message <- "Error with g(y): y is missing in the function"
				}
			}
			else {
				s_message <- "Error with g(y): y is missing in the function"
			}
			
			return (s_message)
		}
		else {
			s_message <- character(0)
			v_cond <- rep(T, length(isolate(o_parameter$y)))
			
			l_g_val <- lapply(isolate(o_parameter$y), function(i) {
				y <- as.vector(df_all[, i])
				v_out <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$g))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_out) | length(which(c(-Inf, Inf) %in% unique(v_out))) > 0 | length(which(is.na(y))) < length(which(is.na(v_out))) | length(v_out) != length(y)) {
					v_out <- F
				}
				
				return(v_out)
			})
			
			eval(parse(text = paste(paste0("v_cond[", 1:length(l_g_val), "] <- ifelse(!is.numeric(l_g_val[[", 1:length(l_g_val), "]]), F, T)"), collapse = "; ")))
			
			if (length(which(v_cond == F)) > 0) {
				s_message <- paste0("Error with g function for the following Y variable(s): ", paste(isolate(o_parameter$y)[which(v_cond == F)], collapse = ", "))
			}
			
			return (list(l_g_val, s_message))
		}
	}
	
	
}

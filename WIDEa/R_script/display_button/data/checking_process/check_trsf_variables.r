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
# Description : function used to execute a checking process on functions filled in "f(x)", "g(y)" and 
#               "h(z)" fields ("Parameter selection" in left panel).
#               (1) "x", "y" and "z" character found in the corresponding fiedls ?
#               (2) problem returned with the function calculation
#               If no problem is occurred, then transformed variable values are returned. 
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (2 values: "normal", "temporal")
# df_all: data created by the f_prepare_data function (process = 1)  
# o_parameter: reactive value from the R script "WIDEa_launcher"

# Output:
# -------
# return a list with transformed variable values or an error message

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
			eval(parse(text = paste0("s_formula <- isolate(o_parameter$", v_fun[i], ")")))
			v_split <- unlist(strsplit(s_formula, v_var_letter[i]))
		
			if (length(v_split) > 1) {
				if (substr(s_formula, nchar(s_formula), nchar(s_formula)) == v_var_letter[i]) {
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
					s_out <- paste0("Error with ", v_fun[i], "(", v_var_letter[i], "): ", v_var_letter[i], " is missing in the function")
				}
			}
			else {
				s_out <- paste0("Error with ", v_fun[i], "(", v_var_letter[i], "): ", v_var_letter[i], " is missing in the function")
			}
			
			return(s_out)
		}))
		
		if (length(which(v_message != "-")) > 0) {
			v_message_out <- c(v_message_out, v_message[which(v_message != "-")])
		}
		
		if (length(v_message_out) == 0) {
			v_pos <- c()
			
			if (!is.na(isolate(o_parameter$f))) {
				x <- as.vector(df_all[, isolate(o_parameter$x)])
				v_f_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$f))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_f_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_f_val))) > 0 | length(v_f_val) != length(x)) {
					v_pos <- c(v_pos, 1)
				}
			}
			
			if (!is.na(isolate(o_parameter$g))) {
				y <- as.vector(df_all[, isolate(o_parameter$y)])
				v_g_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$g))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_g_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_g_val))) > 0 | length(v_g_val) != length(y)) {
					v_pos <- c(v_pos, 2)
				}
			}
			
			if (!is.na(isolate(o_parameter$h))) {
				z <- as.vector(df_all[, isolate(o_parameter$z)])
				v_h_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$h))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_h_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_h_val))) > 0 | length(v_h_val) != length(z)) {
					v_pos <- c(v_pos, 3)
				}
			}
			
			if (length(v_pos) > 0) {
				v_message_out <- paste0("Error with the following function(s): ", paste(v_fun[v_pos], collapse = ", "))
			}
		}
		
		if (length(v_message_out) > 1) {
			s_message_out <- paste(v_message_out, collapse = "<br/>")
		}
		else {
			s_message_out <- character(0)
		}
						    
		return (list(list("f" = v_f_val, "g" = v_g_val, "h" = v_h_val), s_message_out))
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

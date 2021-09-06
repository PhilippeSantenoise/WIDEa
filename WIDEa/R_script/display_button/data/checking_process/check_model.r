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
# Description : function used to execute a checking process on dedicated fields (in "Variable selection" 
#               section) for model building (Random, X, f(x), Y, g(y), Weigthed residuals), 
#               (step 1) Data matching process applied between informations filled in four of dedicated 
#                        fields (Random, f(x), Weighted residuals: with groups, variance function) and 
#                        model parameters data (loaded from the "Model parameter loading" section);
#               (step 2) checking of variable code in f(x) and variance function fields;
#               (step 3) checking of fitted values (vector size, missing/infinite values). 
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# df_all: data created by the f_prepare_data function (normal data type, process 1)
# df_mod_param: data saved in the e_data environment (e_data$m_param)
# o_parameter: reactive value of the R script "WIDEa_launcher"

# Output:
# -------
# return a list including (1) fitted values;
#                         (2) transformed Y variable values (NULL if g(y) not informed); 
#                         (3) residual variance (NULL if "sigma" not informed in model paramaters data);
#                         (4) error message. 
# The three first list elements are NULL if a problem is occurred to one of checking process steps and an error 
# message is informed.

f_check_model <- function (df_all, df_mod_param, o_parameter) {
	v_f_val <- c()
	v_g_val <- c()
	v_vfun_val <- c()
	v_message_out <- c()
	
	v_param <- as.vector(unique(df_mod_param$parameter))
	i_num <- length(which(!is.na(isolate(o_parameter$ref))))
	
	if (i_num > 0) {
		v_pos <- grep("re", names(df_mod_param))
		
		if (length(v_pos) > 0) {
			v_name <- names(df_mod_param)[v_pos]
			
			v_num <- as.vector(unlist(lapply(v_name, function(x) {
				if (length(grep(x, isolate(o_parameter$f))) > 0) {
					return(1)
				}
				else {
					return(0)
				}
			})))
			
			if (i_num < length(which(v_num == 1))) {
				v_message_out <- c(v_message_out, "The number of variables used as random effects is incorrect")
			}
			else {
				if (length(which(paste0("re", 1:i_num) %in% v_name)) < i_num) {
					v_message_out <- c(v_message_out, paste0("The following variable", ifelse(length(which(!paste0("re", 1:i_num) %in% v_name)) == 1, " is", "s are"), " missing in model parameter data: ", paste(paste0("re", 1:i_num)[which(!paste0("re", 1:i_num) %in% v_name)], collapse = ", ")))
				}
				else {
					v_message <- unlist(lapply(1:i_num, function(x) {
						v_group_1 <- as.vector(unique(df_all[, isolate(o_parameter$ref)[x]])) 
						v_group_2 <- as.vector(unique(df_mod_param[, paste0("re", x)]))
						v_group_2 <- v_group_2[!is.na(v_group_2)]
						v_pos <- which(v_group_1 %in% v_group_2)
						
						if (length(v_pos) < length(v_group_1)) {
							s_out <- paste0("The following value", ifelse(length(which(!v_group_1 %in% v_group_2)) == 1, " is", "s are"), " missing in re", x, " variable (model parameter data): ", paste(v_group_1[which(!v_group_1 %in% v_group_2)], collapse = ", "))
						}
						else {
							s_out <- "-"
						}
						
						return(s_out)
					}))
					
					if (length(which(v_message != "-")) > 0) {
						v_message_out <- c(v_message_out, v_message[which(v_message != "-")])
					}
				}
			}
		}
		else {
			v_message_out <- c(v_message_out, paste0("The following variable", ifelse(i_num == 1, " is", "s are"), " missing in model parameter data: ", paste(paste0("re", 1:i_num), collapse = ", ")))
		}
	}
	else {
		if (length(grep("[|]re", isolate(o_parameter$f))) > 0) {
			v_message_out <- c(v_message_out, "Please add random effect(s)") 
		}
		else {
			v_pos <- grep("[|]re", v_param)
			
			if (length(v_pos) > 0) {
				v_param <- v_param[-v_pos]
			}
		}
	}
	
	if (length(v_message_out) == 0) {
		v_pos <- grep("a", v_param)
		
		if (length(v_pos) > 0) {
			s_param <- paste(gsub("[|]", "[|]", v_param[v_pos]), collapse = "|")
			s_f_fun <- gsub(s_param, "_", isolate(o_parameter$f))
			v_split <- unlist(strsplit(s_f_fun, "a"))
			
			if (length(v_split) > 1) {
				v_split <- substr(v_split[-1], 1, 1)
				v_split <- suppressWarnings(as.numeric(v_split))
				v_pos <- which(!is.na(v_split))
				
				if (length(v_pos) > 0) {
					v_message_out <- c(v_message_out, paste0("Unknown parameter", ifelse(length(v_pos) > 1, "s", ""), " (beginning with a) in f function. Please add ", ifelse(length(v_pos) > 1, "these parameters", "this parameter"), " in model parameter data.")) 
				}
			}
		}
		else {
			v_message_out <- c(v_message_out, "A fixed effet(s) model (f function) is used, but no corresponding parameter(s) (example: a1, a2, ...) found in model parameter data")
		}
	}
	
	i_num <- length(which(!is.na(isolate(o_parameter$wres_group))))
	i_size <- length(v_message_out)
	
	if (i_num > 0) {
		v_pos <- grep("gr", names(df_mod_param))
		
		if (length(v_pos) > 0) {
			v_name <- names(df_mod_param)[v_pos]
			
			v_num <- as.vector(unlist(lapply(v_name, function(x) {
				if (length(grep(x, isolate(o_parameter$wres_vfun))) > 0) {
					return(1)
				}
				else {
					return(0)
				}
			})))
			
			if (i_num < length(which(v_num == 1))) {
				v_message_out <- c(v_message_out, "The number of variables used as weighted residuals groups is incorrect")
			}
			else {
				if (length(which(paste0("gr", 1:i_num) %in% v_name)) < i_num) {
					v_message_out <- c(v_message_out, paste0("The following variable", ifelse(length(which(!paste0("gr", 1:i_num) %in% v_name)) == 1, " is", "s are"), " missing in model parameter data: ", paste(paste0("gr", 1:i_num)[which(!paste0("gr", 1:i_num) %in% v_name)], collapse = ", ")))
				}
				else {
					v_message <- unlist(lapply(1:i_num, function(x) {
						v_group_1 <- as.vector(unique(df_all[, isolate(o_parameter$wres_group)[x]])) 
						v_group_2 <- as.vector(unique(df_mod_param[, paste0("gr", x)]))
						v_group_2 <- v_group_2[!is.na(v_group_2)]
						v_pos <- which(v_group_1 %in% v_group_2)
						
						if (length(v_pos) < length(v_group_1)) {
							s_out <- paste0("The following value", ifelse(length(which(!v_group_1 %in% v_group_2)) == 1, " is", "s are"), " missing in gr", x, " variable (model parameter data): ", paste(v_group_1[which(!v_group_1 %in% v_group_2)], collapse = ", "))
						}
						else {
							s_out <- "-"
						}
						
						return(s_out)
					}))
					
					if (length(which(v_message != "-")) > 0) {
						v_message_out <- c(v_message_out, v_message[which(v_message != "-")])
					}
				}
			}
		}
		else {
			v_message_out <- c(v_message_out, paste0("The following variable", ifelse(i_num == 1, " is", "s are"), " missing in model parameter data: ", paste(paste0("gr", 1:i_num), collapse = ", ")))
		}
	}
	else {
		if (!is.na(isolate(o_parameter$wres_vfun))) {
			if (length(grep("[|]gr", isolate(o_parameter$wres_vfun))) > 0) {
				v_message_out <- c(v_message_out, "Please add weighted residuals group(s)") 
			}
			else {
				v_pos <- grep("[|]gr", v_param)
				
				if (length(v_pos) > 0) {
					v_param <- v_param[-v_pos]
				}
			}
			
			
		}
		else {
			v_pos <- grep("[|]gr", v_param)
			
			if (length(v_pos) > 0) {
				v_param <- v_param[-v_pos]
			}
		}
	}
	
	if (i_size == length(v_message_out) & !is.na(isolate(o_parameter$wres_vfun))) {
		v_pos <- grep("d", v_param)
		
		if (length(v_pos) > 0) {
			s_param <- paste(gsub("[|]", "[|]", v_param[v_pos]), collapse = "|")
			s_f_fun <- gsub(s_param, "_", isolate(o_parameter$f))
			v_split <- unlist(strsplit(s_f_fun, "d"))
			
			if (length(v_split) > 1) {
				v_split <- substr(v_split[-1], 1, 1)
				v_split <- suppressWarnings(as.numeric(v_split))
				v_pos <- which(!is.na(v_split))
				
				if (length(v_pos) > 0) {
					v_message_out <- c(v_message_out, paste0("Unknown parameter", ifelse(length(v_pos) > 1, "s", ""), " (beginning with d) in the weighted residuals function. Please add ", ifelse(length(v_pos) > 1, "these parameters", "this parameter"), " in model parameter data.")) 
				}
			}
		}
		else {
			v_message_out <- c(v_message_out, "No parameter beginning with d found in model parameter data")
		}
	}
	
	v_cond <- rep(F, 2)
	v_split <- unlist(strsplit(isolate(o_parameter$f), "d"))
	
	if (length(v_split) > 1) {
		v_split <- substr(v_split[-1], 1, 1)
		v_split <- suppressWarnings(as.numeric(v_split))
		
		if (length(which(!is.na(v_split))) > 0) {
			v_cond[1] <- T
		}
	}
	
	v_split <- unlist(strsplit(isolate(o_parameter$f), "gr"))
	
	if (length(v_split) > 1) {
		v_split <- substr(v_split[-1], 1, 1)
		v_split <- suppressWarnings(as.numeric(v_split))
		
		if (length(which(!is.na(v_split))) > 0) {
			v_cond[2] <- T
		}
	}
	
	if (length(which(v_cond == T)) > 0) {
		v_message_out <- c(v_message_out, paste0("Error with f(x): parameters beginning with ", paste(c("d", "gr")[which(v_cond == T)], collapse = " or "), " found in the function"))
	}
	
	v_split <- unlist(strsplit(isolate(o_parameter$f), "x"))
	
	if (length(v_split) > 1) {
		v_num <- suppressWarnings(as.numeric(substr(v_split[-1], 1, 1)))
		
		if (substr(isolate(o_parameter$f), nchar(isolate(o_parameter$f)), nchar(isolate(o_parameter$f))) == "x") {
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
		
		if (length(v_pos) > 0) {
			v_message_out <- c(v_message_out, "Error with f(x): x is an incorrect variable code (replace by : x1, x2, ...)")
		}
		else {
			if (length(which(!is.na(v_num))) == 0) {
				v_message_out <- c(v_message_out, "Error with f(x): variable code (x1, x2, ...) is missing in the function")
			}
		}
	}
	else {
		v_message_out <- c(v_message_out, paste0("Error with f(x): variable code (x1, x2, ...) is missing in the function"))
	}
	
	if (!is.na(isolate(o_parameter$g))) {
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
				v_pos <- v_pos_1 %in% v_pos_2
			}
			else {
				v_pos <- c()
			}
			
			if (length(v_pos) == 0) {
				v_message_out <- c(v_message_out, "Error with g(y): y is missing in the function")
			}
		}
		else {
			v_message_out <- c(v_message_out, "Error with g(y): y is missing in the function")
		}
	}
	
	if (!is.na(isolate(o_parameter$wres_vfun))) {
		if (length(grep("sigma", isolate(o_parameter$wres_vfun))) == 0) {
			v_message_out <- c(v_message_out, "Error with sigma: term missing in the weighted residuals variance function")
		}
		
		v_cond <- rep(F, 2)
		v_split <- unlist(strsplit(isolate(o_parameter$wres_vfun), "a"))
		
		if (length(v_split) > 1) {
			v_split <- substr(v_split[-1], 1, 1)
			v_split <- suppressWarnings(as.numeric(v_split))
			
			if (length(which(!is.na(v_split))) > 0) {
				v_cond[1] <- T
			}
		}
		
		v_split <- unlist(strsplit(isolate(o_parameter$wres_vfun), "re"))
		
		if (length(v_split) > 1) {
			v_split <- substr(v_split[-1], 1, 1)
			v_split <- suppressWarnings(as.numeric(v_split))
			
			if (length(which(!is.na(v_split))) > 0) {
				v_cond[2] <- T
			}
		}
		
		if (length(which(v_cond == T)) > 0) {
			v_message_out <- c(v_message_out, paste0("Error with the residual variance function: parameters beginning with ", paste(c("a", "re")[which(v_cond == T)], collapse = " or "), " found in the function"))
		}
		
		v_split <- unlist(strsplit(isolate(o_parameter$wres_vfun), "x"))
		
		if (length(v_split) > 1) {
			if (substr(isolate(o_parameter$wres_vfun), nchar(isolate(o_parameter$wres_vfun)), nchar(isolate(o_parameter$wres_vfun))) == "x") {
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
			
			if (length(v_pos) > 0) {
				v_message_out <- c(v_message_out, "Error with the residual variance function: x is an incorrect variable code (replace by : x1, x2, ...)")
			}
		}
	}
	
	if (length(v_message_out) == 0) {
		df_param <- as.data.frame(matrix(unlist(lapply(v_param, function(x) {
			if (length(grep("[|]", x)) == 0) {
				v_out <- rep(df_mod_param[which(df_mod_param$parameter == x), "value"], dim(df_all)[1])
			}
			else {
				s_name <- substr(x, unlist(gregexpr("[|]", x)) + 1, nchar(x))
				
				if (length(grep(":", x)) == 0) {
					i_num <- as.numeric(substr(s_name, 3, nchar(s_name)))
					
					if (length(grep("re", s_name)) > 0) {
						df_merge <- as.data.frame(cbind(c(1:dim(df_all)[1]), as.vector(df_all[, isolate(o_parameter$ref)[i_num]])))
					}
					else {
						df_merge <- as.data.frame(cbind(c(1:dim(df_all)[1]), as.vector(df_all[, isolate(o_parameter$wres_group)[i_num]])))
					}
					
					names(df_merge) <- c("id", s_name)
					df_merge <- merge(df_merge, df_mod_param[which(df_mod_param$parameter == x), c("value", s_name)], by = s_name, all.x = T)
				}
				else {
					v_name <- unlist(strsplit(s_name, ":"))
					v_num <- as.numeric(substr(v_name, 3, nchar(v_name)))
					
					if (length(grep("re", s_name)) > 0) {
						eval(parse(text = paste0("v_comb <- paste(", paste(paste0("as.vector(df_all[, isolate(o_parameter$ref)[", v_num, "]])"), collapse = ", "), ", sep = \" \")"))) 
					}
					else {
						eval(parse(text = paste0("v_comb <- paste(", paste(paste0("as.vector(df_all[, isolate(o_parameter$wres_group)[", v_num, "]])"), collapse = ", "), ", sep = \" \")")))
					}
					
					df_merge_1 <- as.data.frame(cbind(c(1:dim(df_all)[1]), v_comb))
					names(df_merge_1) <- c("id", "comb")
					eval(parse(text = paste0("v_comb <- paste(", paste(paste0("as.vector(df_mod_param[which(df_mod_param$parameter == x), \"", v_name, "\"])"), collapse = ", "), ", sep = \" \")")))
					df_merge_2 <- as.data.frame(cbind(as.vector(df_mod_param[which(df_mod_param$parameter == x), "value"]), v_comb))
					names(df_merge_2) <- c("value", "comb")
					df_merge <- merge(df_merge_1, df_merge_2, by = "comb", all.x = T)
				}
				
				df_merge$id <- as.integer(as.vector(df_merge$id))
				df_merge <- df_merge[order(df_merge$id),]
				v_out <- as.numeric(as.vector(df_merge$value))
			}
			
			return(v_out)
		})), nrow = dim(df_all)[1], ncol = length(v_param)))
		
		if (length(which(is.na(df_param))) > 0) {
			v_message_out <- c(v_message_out, "Error with model parameter data: check values in (re, gr) columns")
		}
		else {
			v_pos <- grep("[|]", v_param)
			v_param_trsf <- v_param
			
			if (length(v_pos) > 0) {
				v_param_trsf[v_pos] <- paste0("b", 1:length(v_pos))
				v_param_1 <- gsub("[|]", "[|]", v_param[v_pos]) 
				v_param_2 <- paste0("b", 1:length(v_pos))
				v_num <- lengths(regmatches(v_param_1, gregexpr(":", v_param_1)))
				df_order_param <- data.frame("col1" = v_param_1, "col2" = v_param_2, "col3" = v_num)
				df_order_param <- df_order_param[order(df_order_param$col3, decreasing = T),]
				v_param_1 <- as.vector(df_order_param$col1)
				v_param_2 <- as.vector(df_order_param$col2)
				
				if (!is.na(isolate(o_parameter$wres_vfun))) {
					eval(parse(text = paste(paste0("s_vfun_text <- gsub(\"", v_param_1, "\", \"", v_param_2, "\", isolate(o_parameter$wres_vfun))"), collapse = "; ")))
				}
				
				s_f_text <- isolate(o_parameter$f)
				eval(parse(text = paste(paste0("s_f_text <- gsub(\"", v_param_1, "\", \"", v_param_2, "\", s_f_text)"), collapse = "; ")))
			}
			else {
				s_f_text <- isolate(o_parameter$f)
			}
			
			names(df_param) <- v_param_trsf
			eval(parse(text = paste(paste0(v_param_trsf, " <- as.vector(df_param[, \"", v_param_trsf, "\"])"), collapse = "; ")))
			v_cond <- rep(T, 3)
			names(v_cond) <- c("f", "g", "residual variance")
			eval(parse(text = paste(paste0("x", 1:length(isolate(o_parameter$x)), " <- as.vector(df_all[, isolate(o_parameter$x)[", 1:length(isolate(o_parameter$x)), "]])"), collapse = "; ")))
			v_f_val <- tryCatch({suppressWarnings(eval(parse(text = s_f_text)))}, error = function(e) FALSE)
			
			if (!is.numeric(v_f_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_f_val))) > 0) {
				v_cond[1] <- F
				v_f_val <- c()
			}
			
			if (!is.na(isolate(o_parameter$g))) {
				y <- as.vector(df_all[, isolate(o_parameter$y)])
				v_g_val <- tryCatch({suppressWarnings(eval(parse(text = isolate(o_parameter$g))))}, error = function(e) FALSE)
				
				if (!is.numeric(v_g_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_g_val))) > 0) {
					v_cond[2] <- F
					v_g_val <- c()
				}
			}
			
			if (!is.na(isolate(o_parameter$wres_vfun))) {
				v_vfun_val <- tryCatch({suppressWarnings(eval(parse(text = s_vfun_text)))}, error = function(e) FALSE)
				
				if (!is.numeric(v_vfun_val) | length(which(c(-Inf, Inf, NA) %in% unique(v_vfun_val))) > 0) {
					v_cond[3] <- F
					v_vfun_val <- c()
				}
			}
			
			if (length(which(v_cond == F)) > 0) {
				v_message_out <- paste0("Error with the following function(s): ", paste(names(v_cond)[which(v_cond == F)], collapse = ", "))
			}
			else {
				if (isolate(o_parameter$model) == "calib") {
					if (length(v_vfun_val) == 0 & "sigma" %in% names(df_param)) {
						v_vfun_val <- as.vector(df_param$sigma)^2
					}
				}
			}
		}
	}
	
	if (!is.null(v_message_out)) {
		s_message_out <- ifelse(length(v_message_out) > 1, paste(v_message_out, collapse = "<br/>"), v_message_out)
	}
	else {
		s_message_out <- character(0)
	}
	
	return (list(v_f_val, v_g_val, v_vfun_val, s_message_out))
}

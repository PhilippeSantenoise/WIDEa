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
# Description : function used to execute a checking process on selected variables step by step, 
#               (step 1) Are there any fields with same variables ?
#               (step 2) (1) Is the type/number of variables correct ?
#                        (2) Are there any fields where variable(s) have no value ? 
#               (step 3) Date variable has missing value(s) ? Date variable format is recognized ? Date 
#                        variable has unique values ?      
#               (step 4) Date variable has been transformed as an integer variable when loading data ?
#               (step *) Is ".row_num." variable in loaded data (df_all input) ? 
#
#               The steps 1 to 3 return an error message and the step 4 return a warning message.
#               The steps 3 and 4 only concern the temporal data type.
#               The step * only concerns the normal (2D/3D plot without model added) & ir data type when 
#               the ID variable is not specified in the corresponding text field (left panel). The 
#               ".row_num." variable allows to identify flags currently added from the Flag tab.   
# 
#               The steps (or number) may be different depending on the data type,
#               (normal) The step 1 is not applied to some fields when a calibration/validation model is 
#                        added (3 fields concerned: Random, Weighted residual group and Group).
#                        The step 1 also checks if the ID variable has unique values.
#                        The combination of Y variables of the correlation matrix (corplot) is also 
#                        checked for the step 2-2. 
#               (temporal) The type of Y variables is only checked for the step 2-1.
#               (ir) The type of selected variables (ID, Group) is only checked for the step 2-1.
#               
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# s_data_type: data type (3 values: "normal", "temporal", "ir")
# df_all: data saved in the e_data environment (e_data$all)
# o_parameter: reactive value from the R script "WIDEa_launcher"
# (v_var_x, v_var_group): variables selected from the X/Group field when the concatenation box is checked

# Output:
# -------
# return a message (empty value only if false is returned to all steps)

f_check_variables <- function (s_data_type = "normal", df_all, o_parameter, v_var_x = NULL, v_var_group = NULL) {
	if (s_data_type == "normal") {
		s_e_message <- character(0)
		
		# execute the step 1
		
		if (length(which(isolate(o_parameter$x) == ".concat1.")) > 0 & length(which(isolate(o_parameter$group) == ".concat2.")) > 0) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), v_var_x, isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), v_var_group)  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(v_var_x)), paste0("Y", 1:length(isolate(o_parameter$y))), "Z", paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(v_var_group)))
		}
		else if (length(which(isolate(o_parameter$x) == ".concat1.")) == 0 & length(which(isolate(o_parameter$group) == ".concat2.")) > 0) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), v_var_group)  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), "Z", paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(v_var_group)))
		}
		else if (length(which(isolate(o_parameter$x) == ".concat1.")) > 0 & length(which(isolate(o_parameter$group) == ".concat2.")) == 0) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), v_var_x, isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(v_var_x)), paste0("Y", 1:length(isolate(o_parameter$y))), "Z", paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$group))))
		}
		else {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), "Z", paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$group))))
		}
		
		v_var <- v_var[!is.na(v_var)]
		df_num <- as.data.frame(addmargins(table(v_var)))
		df_num <- df_num[-dim(df_num)[1],]
		v_pos <- which(df_num$Freq > 1)
		
		if (length(v_pos) > 0) {
			v_e_message <- as.vector(unlist(lapply(v_pos, function(i) {
				v_1 <- names(v_var)[which(v_var == df_num[i, 1])]
				
				if (length(v_1) == length(grep("Random", v_1)) + length(grep("Weighted", v_1)) + length(grep("Group", v_1))) {										
					return(NA)
				}
				else {
					v_2 <- unique(substr(v_1, 1, 1))
					v_2 <- c(ifelse("I" %in% v_2, "ID", NA), ifelse("R" %in% v_2, "Random", NA), ifelse("X" %in% v_2, "X", NA), ifelse("Y" %in% v_2, "Y", NA), ifelse("Z" %in% v_2, "Z", NA), ifelse("W" %in% v_2, "Weighted residual group", NA), ifelse("G" %in% v_2, "Group", NA))
					v_2 <- v_2[!is.na(v_2)]
					return(paste0("(", paste(v_2, collapse = ", "), ") ", df_num[i, 1])) 
				}
			})))
			
			v_e_message <- v_e_message[!is.na(v_e_message)]
			
			if (length(v_e_message) > 0) {
				s_e_message <- paste0("The following fields contain same variable(s) (only autorized for calibration/validation model between Random, Weighted residual group and Group fields):<br/>", paste(v_e_message, collapse = "<br/>"))
			}
		}
		
		if (length(s_e_message) == 0) { # No error message returned to the step 1
			v_cond <- T
			b_comb <- F
			v_var <- c()
			
			# execute the step 2-1
			
			if (isolate(o_parameter$plot_type) == "plot") {
				if (isolate(o_parameter$dim_num) == "3d") {
					v_cond <- c(ifelse(!is.na(isolate(o_parameter$id)), length(as.vector(unique(df_all[, isolate(o_parameter$id)]))) == dim(df_all)[1], T), is.numeric(as.vector(df_all[, isolate(o_parameter$x)])), is.numeric(as.vector(df_all[, isolate(o_parameter$y)])), is.numeric(as.vector(df_all[, isolate(o_parameter$z)])), ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
					v_name_1 <- c("ID", "X", "Y", "Z", "Group")
					names(v_cond) <- v_name_1
					v_type <- c("unique", "quantitative", "quantitative", "quantitative", "qualitative")
					v_name_2 <- c(isolate(o_parameter$id), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$group))
				}
				else {
					if (isolate(o_parameter$model) == "none") {
						v_cond <- c(ifelse(!is.na(isolate(o_parameter$id)), length(as.vector(unique(df_all[, isolate(o_parameter$id)]))) == dim(df_all)[1], T), is.numeric(as.vector(df_all[, isolate(o_parameter$x)])), is.numeric(as.vector(df_all[, isolate(o_parameter$y)])), ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
						v_name_1 <- c("ID", "X", "Y", "Group")
						names(v_cond) <- v_name_1
						v_type <- c("unique", "quantitative", "quantitative", "qualitative")
						v_name_2 <- c(ifelse(!is.na(isolate(o_parameter$id)), isolate(o_parameter$id), NA), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
					}
					else {
						if (length(which(!is.na(o_parameter$ref))) > 0 & length(which(!is.na(o_parameter$wres_group))) > 0) {
							eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.character(df_all$", isolate(o_parameter$ref), ") | (is.numeric(df_all$", isolate(o_parameter$ref), ") & length(grep(\"[.]\", df_all$", isolate(o_parameter$ref), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.character(df_all$", isolate(o_parameter$wres_group), ") | (is.numeric(df_all$", isolate(o_parameter$wres_group), ") & length(grep(\"[.]\", df_all$", isolate(o_parameter$wres_group), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(df_all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(df_all$", isolate(o_parameter$y), "))")))
							v_name_1 <- c(paste0("ref", 1:length(isolate(o_parameter$ref))), paste0("wres_group", 1:length(isolate(o_parameter$wres_group))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
							v_name_2 <- c(isolate(o_parameter$ref), isolate(o_parameter$wres_group), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
						}
						else if (length(which(!is.na(o_parameter$ref))) > 0 & length(which(!is.na(o_parameter$wres_group))) == 0) {
							eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.character(df_all$", isolate(o_parameter$ref), ") | (is.numeric(df_all$", isolate(o_parameter$ref), ") & length(grep(\"[.]\", df_all$", isolate(o_parameter$ref), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(df_all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(df_all$", isolate(o_parameter$y), "))")))
							v_name_1 <- c(paste0("ref", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
							v_name_2 <- c(isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
						}
						else if (length(which(!is.na(o_parameter$ref))) == 0 & length(which(!is.na(o_parameter$wres_group))) > 0) {
							eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.character(df_all$", isolate(o_parameter$wres_group), ") | (is.numeric(df_all$", isolate(o_parameter$wres_group), ") & length(grep(\"[.]\", df_all$", isolate(o_parameter$wres_group), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(df_all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(df_all$", isolate(o_parameter$y), "))")))
							v_name_1 <- c(paste0("wres_group", 1:length(isolate(o_parameter$wres_group))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
							v_name_2 <- c(isolate(o_parameter$wres_group), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
						}
						else {
							eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(df_all$", isolate(o_parameter$y), "))")))
							v_name_1 <- c(paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
							v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
						}
						
						v_cond <- c(v_cond, ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
						names(v_cond) <- v_name_1
						
					}
				}
			}
			else if (isolate(o_parameter$plot_type) == "boxplot") {
				v_cond <- c((is.numeric(as.vector(df_all[, isolate(o_parameter$x)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$x)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$x)])), is.numeric(as.vector(df_all[, isolate(o_parameter$y)])), ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
				v_name_1 <- c("X", "Y", "Group") 
				names(v_cond) <- v_name_1
				v_type <- c("qualitative", "quantitative", "qualitative")
				v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
			}
			else if (isolate(o_parameter$plot_type) == "histplot") {
				v_cond <- c(is.numeric(as.vector(df_all[, isolate(o_parameter$x)])), ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
				v_name_1 <- c("X", "Group")
				names(v_cond) <- v_name_1
				v_type <- c("quantitative", "qualitative")
				v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$group), NA)
			}
			else if (isolate(o_parameter$plot_type) == "barplot") {
				v_cond <- c((is.numeric(as.vector(df_all[, isolate(o_parameter$x)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$x)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$x)])), ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T)) 
				v_name_1 <- c("X", "Group")
				names(v_cond) <- v_name_1
				v_type <- c("qualitative", "qualitative")
				v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$group))
			}
			else { # corplot
				if (length(which(!is.na(isolate(o_parameter$y)))) < 2) { # return a message if the number of Y variables is inferior to 2
					s_e_message <- "The number of Y variables must be strictly superior to 1" 
				}
				else {
					eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_all$", isolate(o_parameter$y), ")"), collapse = ", "), ")")))
					v_name_1 <- c(paste0("Y", 1:length(v_cond)), "Group")
					v_cond <- c(v_cond, ifelse(!is.na(isolate(o_parameter$group)), (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0) | is.character(as.vector(df_all[, isolate(o_parameter$group)])), T))
					names(v_cond) <- v_name_1
					v_name_2 <- c(isolate(o_parameter$y), isolate(o_parameter$group))
				}
			}
			
			if (length(s_e_message) == 0) { # Stop the step 2 only if an error message is returned for the corplot 
				# execute the step 2-2
				
				eval(parse(text = paste0("v_dim <- c(", paste0("dim(df_all[!is.na(df_all[, \"", v_name_2[which(!is.na(v_name_2))], "\"]),])[1]", collapse = ", "), ")")))
				v_pos <- which(v_dim == 0)
				
				if (length(v_pos) > 0) {
					if (isolate(o_parameter$plot_type) != "corplot") {
						v_var <- v_name_1[which(!is.na(v_name_2))[v_pos]]
					}
					else {
						if (!is.na(isolate(o_parameter$group))) {
							if (length(v_pos) == length(v_name_1)) {
								v_var <- "all"
							}
							else {
								if (length(v_name_1) %in% v_pos) {
									v_var <- "group"
								}
							}
						}
						else {
							if (length(v_pos) == (length(v_name_1) - 1)) {
								v_var <- "y"
							}
						}
					}
					
					v_cond[which(!is.na(v_name_2))[v_pos]] <- T
				}
				else {
					if (isolate(o_parameter$plot_type) != "corplot") {
						eval(parse(text = paste0("i_dim <- dim(df_all[", paste0("!is.na(df_all[, \"", v_name_2[which(!is.na(v_name_2))], "\"])", collapse = " & "), ",])[1]")))  
						
						if (i_dim == 0) {
							v_var <- v_name_1[which(!is.na(v_name_2))]
							b_comb <- T
						}
					}
				}
				
				i_sum <- length(which(v_cond == F)) + length(v_var) + length(which(b_comb == T))
			
				if (i_sum > 0) { # invalid step 2
					v_pos <- which(v_cond == F)
					v_name <- names(v_cond)[v_pos]
					v_e_message <- c()
					
					if (isolate(o_parameter$plot_type) != "corplot") {
						if (length(v_var) > 0) {
							if (length(v_var) == 1) {
								v_e_message <- c(v_e_message, paste0("The following variable has no value: ", v_var))
							}
							else {
								v_e_message <- c(v_e_message, paste0("The following variables have no value: ", paste(v_var, collapse = ", ")))
							}
						}
						
						if (b_comb) {
							v_e_message <- c(v_e_message, paste0("The combination of variables (", paste(v_var, collapse = ", "), ") has no value"))
						}
						
						if (length(v_pos) > 0) {
							if (is.na(isolate(o_parameter$model)) | length(which(isolate(o_parameter$model) == "none")) > 0) {
								v_type <- v_type[v_pos]
								
								if (length(which(v_type == "unique")) > 0) {
									v_e_message <- c(v_e_message, "The ID variable doesn't have unique values")
								}
									
								if (length(which(v_type == "quantitative")) > 0) {
									v_e_message <- c(v_e_message, paste0("The following variable(s) must be quantitative: ", paste(paste0(v_name[which(v_type == "quantitative")]), collapse = ", "))) 
								}
								
								if (length(which(v_type == "qualitative")) > 0) {
									v_e_message <- c(v_e_message, paste0("The following variable(s) must be qualitative: ", paste(paste0(v_name[which(v_type == "qualitative")]), collapse = ", "))) 
								}
							}
							else {
								v_pos <- grep("ref", v_name)
								
								if (length(v_pos) > 0) {
									v_e_message <- c(v_e_message, paste0("The following random effect(s) must be qualitative: ", paste(isolate(o_parameter$ref)[as.numeric(substr(v_name[v_pos], 4, nchar(v_name[v_pos])))], collapse = ", ")))
								}
								
								if (isolate(o_parameter$model) == "calib") {
									v_pos <- grep("wres_group", v_name)
									
									if (length(v_pos) > 0) {
										v_e_message <- c(v_e_message, paste0("The following group variable(s) of the residual variance function must be qualitative: ", paste(isolate(o_parameter$wres_group)[as.numeric(substr(v_name[v_pos], 11, nchar(v_name[v_pos])))], collapse = ", ")))
									}
								}
								
								v_pos <- grep("X", v_name)
								
								if (length(v_pos) > 0) {
									v_e_message <- c(v_e_message, paste0("The following X variable(s) must be quantitative: ", paste(isolate(o_parameter$x)[as.numeric(substr(v_name[v_pos], 2, nchar(v_name[v_pos])))], collapse = ", ")))
								}
								
								v_pos <- grep("Y", v_name)
								
								if (length(v_pos) > 0) {
									v_e_message <- c(v_e_message, "The Y variable must be quantitative")
								}
								
								v_pos <- grep("Group", v_name)
								
								if (length(v_pos) > 0) {
									v_e_message <- c(v_e_message, "The Group variable must be qualitative")
								}
							}
						}
					}
					else {
						if (length(v_var) > 0) {
							if (v_var == "all") {
								v_e_message <- c(v_e_message, "All Y variables and Group variable have no value")
							}
							else if (v_var == "group") {
								v_e_message <- c(v_e_message, "The Group variable has no value")
							}
							else {
								v_e_message <- c(v_e_message, "All Y variables have no value")
							}
						}
						
						if (length(v_pos) > 0) {
							if ("Group" %in% v_name) {
								v_e_message <- c(paste0("The following Y variable(s) must be quantitative: ", paste(isolate(o_parameter$y)[v_pos[-length(v_pos)]], collapse = ", ")), "The Group variable must be qualitative")
							}
							else {
								v_e_message <- paste0("The following Y variable(s) must be quantitative: ", paste(isolate(o_parameter$y)[v_pos], collapse = ", "))
							}
						}
					}
					
					# if (length(v_e_message) > 0) {
						s_e_message <- paste(v_e_message, collapse = "<br/>")
					# }
				} 
				else { # exexcute the step *
					if (isolate(o_parameter$plot_type) == "plot") {
						if (isolate(o_parameter$model) == "none" & is.na(isolate(o_parameter$id))) {
							if (".row_num." %in% names(df_all)) {
								s_e_message <- "A variable named \".row_num.\" already exists in the loaded data. This variable is used to identify flags added from the Flag tab when an ID variable is not specified." 
							}
						}
					}
				}
			} 
		}
		
		return (s_e_message)
	}
	else if (s_data_type == "temporal") {
		s_e_message <- character(0)
		s_w_message <- character(0)
		v_date <- c()
		
		# execute the step 1
		
		v_var <- c(isolate(o_parameter$x), isolate(o_parameter$y))  
		df_num <- as.data.frame(addmargins(table(v_var)))
		df_num <- df_num[-dim(df_num)[1],]
		v_pos <- which(df_num$Freq > 1)
		
		if (length(v_pos) > 0) {
			s_e_message <- paste0("The (X, Y) fields contain same variable(s): ", df_num[v_pos, 1])
		}
		
		if (length(s_e_message) == 0) { # No error message returned to the step 1
			# execute the step 2
			
			v_var <- c()
			eval(parse(text = paste0("v_cond <- c(", paste(paste0("is.numeric(df_all$", isolate(o_parameter$y), ")"), collapse = ", "), ")")))
			eval(parse(text = paste0("v_dim <- c(", paste0("dim(df_all[!is.na(df_all$", c(isolate(o_parameter$x), isolate(o_parameter$y)), "),])[1]", collapse = ", "), ")")))
			
			if (length(which(v_dim == 0)) > 0) {
				v_var <- c("X", paste0("Y", c(1:length(isolate(o_parameter$y)))))[which(v_dim == 0)]
				
				if ("X" %in% v_var) {
					if (length(v_var) > 1) {
						v_cond[which(v_dim == 0) - 1] <- T
					}
				}
				else {
					v_cond[which(v_dim == 0)] <- T
				} 
			}
			
			i_sum <- length(which(v_cond == F)) + length(v_var)
		
			if (i_sum > 0) {
				v_e_message <- c()
				
				if (length(v_var) > 0) {
					if ("X" %in% v_var) {
						v_var <- v_var[-which(v_var == "X")]
						v_e_message <- c(v_e_message, "X variable has no value")
						
						if (length(v_var) > 0) {
							if (length(v_var) == 1) {
								v_e_message <- c(v_e_message, paste0("The following Y variable has no value: ", paste(isolate(o_parameter$y)[as.numeric(substr(v_var, 2, nchar(v_var)))], collapse = ", ")))
							}
							else {
								v_e_message <- c(v_e_message, paste0("The following Y variables have no value: ", paste(isolate(o_parameter$y)[as.numeric(substr(v_var, 2, nchar(v_var)))], collapse = ", ")))
							}
						}
					}
					else {
						if (length(v_var) == 1) {
							v_e_message <- c(v_e_message, paste0("The following Y variable has no value: ", paste(isolate(o_parameter$y)[as.numeric(substr(v_var, 2, nchar(v_var)))], collapse = ", ")))
						}
						else {
							v_e_message <- c(v_e_message, paste0("The following Y variables have no value: ", paste(isolate(o_parameter$y)[as.numeric(substr(v_var, 2, nchar(v_var)))], collapse = ", ")))
						}
					}
				}
				
				v_pos <- which(v_cond == F)
				
				if (length(v_pos) > 0) {
					v_e_message <- c(v_e_message, paste0("The following Y variable(s) must be quantitative: ", paste(isolate(o_parameter$y)[v_pos], collapse = ", ")))
				}
				
				if (length(v_e_message) > 0) {
					s_e_message <- paste(v_e_message, collapse = "<br/>")
				}
			}
			
			if (length(s_e_message) == 0) { # No error message returned to the step 2
				v_pos <- which(is.na(df_all[, isolate(o_parameter$x)]))
				
				if (length(v_pos) > 0) {
					s_e_message <- "X variable has missing values"
				}
				else {
					df_all[, isolate(o_parameter$x)] <- as.vector(df_all[, isolate(o_parameter$x)])
					v_range <- c()
					
					if (is.numeric(df_all[, isolate(o_parameter$x)]) & length(grep("[.]", df_all[, isolate(o_parameter$x)])) == 0) {
						v_range <- range(df_all[, isolate(o_parameter$x)])
					}
					else {
						df_all[, isolate(o_parameter$x)] <- gsub("/| |:|-|[.]|h", "", df_all[, isolate(o_parameter$x)])
					}
					
					v_date <- as.character(strptime(df_all[, isolate(o_parameter$x)], format = isolate(o_parameter$date_format)))
					i_num <- length(which(is.na(v_date)))
					
					if (length(v_range) > 0) {
						if (v_range[1] > 0 & v_range[2] < unclass(as.Date(Sys.time()) + 1)) {
							if (i_num > 0) {
								v_date <- as.character(as.Date(df_all[, isolate(o_parameter$x)], origin = "1970-01-01", tz = "GMT"))
								i_num <- length(which(is.na(v_date)))
								
								if (i_num > 0) {
									s_e_message <- "X variable is not recognized as a date variable. The problem can occur if the date format is incorrect.<br/>The date format must be the following:<br/>(1) four-digit year numbers and two-digits numbers for the other units of time ;<br/>(2) the authorized separator between units of time are \" \", \"/\", \"-\", \":\", \".\" and \"h\"."
								}
								else {
									if (length(unique(v_date)) != length(v_date)) { # return 2 messages (error and warning)
										s_e_message <- "X variable doesn't have unique values"
										s_w_message <- "X variable is recognized as an integer variable. The variable could have been transformed as an integer variable when loading data. Therefore, the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times."
									}
								}
							}
							else {
								if (length(unique(v_date)) != length(v_date)) { # return 2 messages (error and warning)
									s_e_message <- "X variable doesn't have unique values"
									s_w_message <- "X variable is recognized as an integer variable. The variable could have been transformed as an integer variable when loading data. Therefore, the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times."
								}
							}
						}
						else {
							if (i_num > 0) {
								s_e_message <- "X variable is not recognized as a date variable. The problem can occur if the date format is incorrect.<br/>The date format must be the following:<br/>(1) four-digit year numbers and two-digits numbers for the other units of time ;<br/>(2) the authorized separator between units of time are \" \", \"/\", \"-\", \":\", \".\" and \"h\"."
							}
							else {
								if (length(unique(v_date)) != length(v_date)) {
									s_e_message <- "X variable doesn't have unique values"
								}
							}
						}
					}
					else {
						if (i_num > 0) {
							s_e_message <- "X variable is not recognized as a date variable. The problem can occur if the date format is incorrect.<br/>The date format must be the following:<br/>(1) four-digit year numbers and two-digits numbers for the other units of time ;<br/>(2) the authorized separator between units of time are \" \", \"/\", \"-\", \":\", \".\" and \"h\"."
						}
						else {
							if (length(unique(v_date)) != length(v_date)) {
								s_e_message <- "X variable doesn't have unique values"
							}
						}
					}
				}
				
				if (length(s_e_message) == 0) { # No error message returned to the step 3
					# execute the step 4
					
					if (length(v_range) > 0) {
						if (v_range[1] > 0 & v_range[2] < unclass(as.Date(Sys.time()) + 1)) {
							s_w_message <- "The X variable could have been transformed as an integer variable when loading data. Please, check the conformity of dates on the graph. If dates are incorrect, then the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times." 
						}
					}
				} # end step 4
			}
		}
		
		return (list(s_e_message, s_w_message, v_date))
	}
	else { # ir
		s_e_message <- character(0)
		
		# execute the step 1
		
		if (!is.na(isolate(o_parameter$id)) & !is.na(isolate(o_parameter$group))) {
			if (isolate(o_parameter$concat2)) {
				v_var <- c(isolate(o_parameter$id), isolate(o_parameter$concat2_group))
			}
			else {
				v_var <- c(isolate(o_parameter$id), isolate(o_parameter$group))
			}
			
			df_num <- as.data.frame(addmargins(table(v_var)))
			df_num <- df_num[-dim(df_num)[1],]
			v_pos <- which(df_num$Freq > 1)
			
			if (length(v_pos) > 0) {
				s_e_message <- paste0("The (ID, Group) fields contain same variable: ", df_num[v_pos, 1])
			}
		}
		
		if (length(s_e_message) == 0) { # No error message returned to the step 1
			# execute the step 2
			
			v_e_message <- c()
			
			if (!is.na(isolate(o_parameter$id))) {
				v_id <- as.vector(unique(df_all[, isolate(o_parameter$id)]))
				
				if (length(v_id) == 1) {
					if (is.na(v_id)) {
						v_e_message <- c(v_e_message, "The ID variable has no value")
					}
				}
				else {
					if (length(v_id) < dim(df_all)[1]) { 
						v_e_message <- c(v_e_message, "The ID variable doesn't have unique values")
					}
				}
			}
			
			if (!is.na(isolate(o_parameter$group))) {
				if (length(which(is.na(as.vector(df_all[,isolate(o_parameter$group)])))) == dim(df_all) [1]) {
					v_e_message <- c(v_e_message, "The Group variable has no value")
				}
				else {
					b_cond <- is.character(as.vector(df_all[, isolate(o_parameter$group)])) | (is.numeric(as.vector(df_all[, isolate(o_parameter$group)])) & length(grep("[.]", as.vector(df_all[, isolate(o_parameter$group)]))) == 0)
					
					if (b_cond == F) { 
						v_e_message <- c(v_e_message, "The Group variable must be qualitative")
					}
				}
			}
			
			if (!is.na(isolate(o_parameter$id)) & !is.na(isolate(o_parameter$group))) {
				if (length(which(c("The ID variable has no value", "The Group variable has no value") %in% v_e_message)) == 0) {
					if (dim(df_all[!is.na(df_all[, isolate(o_parameter$id)]) & !is.na(df_all[,isolate(o_parameter$group)]),])[1] == 0) {
						v_e_message <- c(v_e_message, "The combination of ID and Group variables has no value")
					}
				}
			}
			
			if (length(v_e_message) > 0) {
				s_e_message <- paste(v_e_message, collapse = "<br/>")
			}
			else { # execute the step *
				if (is.na(isolate(o_parameter$id))) {
					if (".row_num." %in% names(df_all)) {
						s_e_message <- "A variable named \".row_num.\" already exists in the loaded data. This variable is used to identify flags added from the Flag tab when an ID variable is not specified." 
					}
				}
			}
		}
		
		return (s_e_message)
	}
}

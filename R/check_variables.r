#' @importFrom shiny isolate
#' @importFrom stats addmargins
NULL

#' Checking process: the selected variables are compatible with the data/plot type
#' requested ? 

#' @description
#' `f_check_variables` execute a checking process in several steps: 
#' \cr(1) Checking fields with the same variables. At least one variable must be
#' different between fields corresponding to X, Y and Z variables. This process is
#' not applied for fields corresponding to Random, Weighted residual group and Group
#' variables when a calibration/validation model is selected;  
#' \cr(2) Check if the ID variable has missing value(s); 
#' \cr(3) Checking the type/number of the selected variables;
#' \cr(4) Check if the selected variables have no value (combination of all Y 
#' variables for the correlation matrix);
#' \cr(5) Checking the date variable for the temporal data type (missing values,
#' date format recognized, unique values, date variable transformed into an integer
#' variable when loading data ?);
#' \cr(6) Check if (".row_num.", ".f.", ".g.", ".h.") variables already exist in
#' loaded data.
#' \cr\cr An error message is returned if a problem occured to one of these steps.

#' @param s_data_type is the data type (3 values: "normal", "temporal", "ir").
#' @param b_check_plus is a boolean value such that `b_check_plus` is true if the
#' data type is "normal", the plot type is "plot" and the model process is "none",
#' false else.
#' @param e_data is the environment including loaded data and sub-data (if created).
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.

#' @encoding UTF-8

f_check_variables <- function (s_data_type = "normal", b_check_plus = F, e_data, o_parameter) {
	df_all <- e_data[[isolate(o_parameter$data_name)]]
	
	if (s_data_type == "normal") {
		s_e_message <- character(0)
		s_w_message <- character(0)
		
		# execute the step 1
		
		if (isolate(o_parameter$concat1) & isolate(o_parameter$concat2)) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$concat1_group), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$concat2_group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$concat1_group))), paste0("Y", 1:length(isolate(o_parameter$y))), paste0("Z", 1:length(isolate(o_parameter$z))), paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$concat2_group))))
		}
		else if (!isolate(o_parameter$concat1) & isolate(o_parameter$concat2)) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$concat2_group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), paste0("Z", 1:length(isolate(o_parameter$z))), paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$concat2_group))))
		}
		else if (isolate(o_parameter$concat1) & !isolate(o_parameter$concat2)) {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$concat1_group), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$concat1_group))), paste0("Y", 1:length(isolate(o_parameter$y))), paste0("Z", 1:length(isolate(o_parameter$z))), paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$group))))
		}
		else {
			v_var <- c(isolate(o_parameter$id), isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$wres_group), isolate(o_parameter$group))  
			names(v_var) <- c("ID", paste0("Random", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), paste0("Z", 1:length(isolate(o_parameter$z))), paste0("Weighted residual group", 1:length(isolate(o_parameter$wres_group))), paste0("Group", 1:length(isolate(o_parameter$group))))
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
			
			if (b_check_plus) {
				v_pos_1 <- grep("(X, Y)", v_e_message)
				v_pos_2 <- grep("(X, Z)", v_e_message)
				v_pos_3 <- grep("(Y, Z)", v_e_message)
				v_pos <- c()
				if (length(v_pos_1) > 0) {v_pos <- eval(parse(text = paste0("c(v_pos, ", ifelse(length(v_pos_1) == length(o_parameter$x) & length(v_pos_1) == length(o_parameter$y), "NA", "v_pos_1"), ")")))}
				if (length(v_pos_2) > 0) {v_pos <- eval(parse(text = paste0("c(v_pos, ", ifelse(length(v_pos_2) == length(o_parameter$x) & length(v_pos_2) == length(o_parameter$z), "NA", "v_pos_2"), ")")))}
				if (length(v_pos_3) > 0) {v_pos <- eval(parse(text = paste0("c(v_pos, ", ifelse(length(v_pos_3) == length(o_parameter$y) & length(v_pos_3) == length(o_parameter$z), "NA", "v_pos_3"), ")")))}
				v_pos <- v_pos[!is.na(v_pos)]
				if (length(v_pos) > 0) {v_e_message <- v_e_message[-v_pos]}
			}
			
			if (length(v_e_message) > 0) {
				s_e_message <- paste0("The following fields contain same variable(s):<br/>", paste(v_e_message, collapse = "<br/>"), "<br/>Only authorized for the following two cases:<br/>(1) 2D/3D plot without model between X, Y, Z variables, if at least one variable differs between the fields concerned;<br/>(2) Calibration/validation model between Random, Weighted residual group and Group fields.")
			}
		}
		
		if (length(s_e_message) == 0) { # No error message returned to the step 1
			# execute the step 2-1
			
			if (!is.na(isolate(o_parameter$id))) { 
				if (length(which(is.na(e_data$all[, isolate(o_parameter$id)]))) > 0) {
					s_e_message <- "ID variable has missing value(s)"
				}
			}
			
			if (length(s_e_message) == 0) {
				v_cond <- T
				b_comb <- F
				v_var <- c()
				
				# execute the step 2-2
				
				if (isolate(o_parameter$plot_type) == "plot") {
					if (isolate(o_parameter$dim_num) == "3d") {
						v_cond <- c(ifelse(!is.na(isolate(o_parameter$id)), length(as.vector(unique(e_data$all[, isolate(o_parameter$id)]))) == dim(e_data$all)[1], T), eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z)), ")"), collapse = ", "), ")"))), ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
						v_name_1 <- c("ID", paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), paste0("Z", 1:length(isolate(o_parameter$z))), "Group")
						names(v_cond) <- v_name_1
						v_type <- c("unique", rep("quantitative", length(c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z)))), "qualitative")
						v_name_2 <- c(isolate(o_parameter$id), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$z), isolate(o_parameter$group))
					}
					else {
						if (isolate(o_parameter$model) == "none") {
							v_cond <- c(ifelse(!is.na(isolate(o_parameter$id)), length(as.vector(unique(e_data$all[, isolate(o_parameter$id)]))) == dim(e_data$all)[1], T), eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", c(isolate(o_parameter$x), isolate(o_parameter$y)), ")"), collapse = ", "), ")"))), ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
							v_name_1 <- c("ID", paste0("X", 1:length(isolate(o_parameter$x))), paste0("Y", 1:length(isolate(o_parameter$y))), "Group")
							names(v_cond) <- v_name_1
							v_type <- c("unique", rep("quantitative", length(c(isolate(o_parameter$x), isolate(o_parameter$y)))), "qualitative")
							v_name_2 <- c(ifelse(!is.na(isolate(o_parameter$id)), isolate(o_parameter$id), NA), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
						}
						else {
							if (length(which(!is.na(o_parameter$ref))) > 0 & length(which(!is.na(o_parameter$wres_group))) > 0) {
								v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.character(e_data$all$", isolate(o_parameter$ref), ") | (is.numeric(e_data$all$", isolate(o_parameter$ref), ") & length(grep(\"[.]\", e_data$all$", isolate(o_parameter$ref), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.character(e_data$all$", isolate(o_parameter$wres_group), ") | (is.numeric(e_data$all$", isolate(o_parameter$wres_group), ") & length(grep(\"[.]\", e_data$all$", isolate(o_parameter$wres_group), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(e_data$all$", isolate(o_parameter$y), "))")))
								v_name_1 <- c(paste0("ref", 1:length(isolate(o_parameter$ref))), paste0("wres_group", 1:length(isolate(o_parameter$wres_group))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
								v_name_2 <- c(isolate(o_parameter$ref), isolate(o_parameter$wres_group), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
							}
							else if (length(which(!is.na(o_parameter$ref))) > 0 & length(which(!is.na(o_parameter$wres_group))) == 0) {
								v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.character(e_data$all$", isolate(o_parameter$ref), ") | (is.numeric(e_data$all$", isolate(o_parameter$ref), ") & length(grep(\"[.]\", e_data$all$", isolate(o_parameter$ref), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(e_data$all$", isolate(o_parameter$y), "))")))
								v_name_1 <- c(paste0("ref", 1:length(isolate(o_parameter$ref))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
								v_name_2 <- c(isolate(o_parameter$ref), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
							}
							else if (length(which(!is.na(o_parameter$ref))) == 0 & length(which(!is.na(o_parameter$wres_group))) > 0) {
								v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.character(e_data$all$", isolate(o_parameter$wres_group), ") | (is.numeric(e_data$all$", isolate(o_parameter$wres_group), ") & length(grep(\"[.]\", e_data$all$", isolate(o_parameter$wres_group), ")) == 0)"), collapse = ", "), ", ", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(e_data$all$", isolate(o_parameter$y), "))")))
								v_name_1 <- c(paste0("wres_group", 1:length(isolate(o_parameter$wres_group))), paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
								v_name_2 <- c(isolate(o_parameter$wres_group), isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
							}
							else {
								v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$x), ")"), collapse = ", "), ", is.numeric(e_data$all$", isolate(o_parameter$y), "))")))
								v_name_1 <- c(paste0("X", 1:length(isolate(o_parameter$x))), "Y", "Group")
								v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
							}
							
							v_cond <- c(v_cond, ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
							names(v_cond) <- v_name_1
						}
					}
				}
				else if (isolate(o_parameter$plot_type) == "boxplot") {
					v_cond <- c(ifelse(length(which(!is.na(isolate(o_parameter$concat1_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$x)])), T), eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$y), ")"), collapse = ", "), ")"))), ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
					v_name_1 <- c("X", paste0("Y", 1:length(isolate(o_parameter$y))), "Group") 
					names(v_cond) <- v_name_1
					v_type <- c("qualitative", rep("quantitative", length(isolate(o_parameter$y))), "qualitative")
					v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$y), isolate(o_parameter$group))
				}
				else if (isolate(o_parameter$plot_type) == "histplot") {
					v_cond <- c(eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$x), ")"), collapse = ", "), ")"))), ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
					v_name_1 <- c(paste0("X", 1:length(isolate(o_parameter$x))), "Group")
					names(v_cond) <- v_name_1
					v_type <- c(rep("quantitative", length(isolate(o_parameter$x))), "qualitative")
					v_name_2 <- c(isolate(o_parameter$x), isolate(o_parameter$group))
				}
				else if (isolate(o_parameter$plot_type) == "barplot") {
					v_cond <- c(ifelse(length(which(!is.na(isolate(o_parameter$concat1_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$x)])), T), ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T)) 
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
						v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.numeric(e_data$all$", isolate(o_parameter$y), ")"), collapse = ", "), ")")))
						v_name_1 <- c(paste0("Y", 1:length(v_cond)), "Group")
						v_cond <- c(v_cond, ifelse(!is.na(isolate(o_parameter$group)) & length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T))
						names(v_cond) <- v_name_1
						v_name_2 <- c(isolate(o_parameter$y), isolate(o_parameter$group))
					}
				}
				
				if (length(s_e_message) == 0) { # Stop the step 2 only if an error message is returned for the corplot 
					# execute the step 2-3
					
					v_dim <- eval(parse(text = paste0("c(", paste0("dim(df_all[!is.na(df_all[, \"", v_name_2[which(!is.na(v_name_2))], "\"]),])[1]", collapse = ", "), ")")))
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
							i_dim <- eval(parse(text = paste0("dim(df_all[", paste0("!is.na(df_all[, \"", v_name_2[which(!is.na(v_name_2))], "\"])", collapse = " & "), ",])[1]")))  
							
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
										if (o_parameter$plot_type == "plot" & "Group" %in% v_name[which(v_type == "qualitative")]) { # quantitative Group variable accepted for the plot type: plot (normal/ir data type)
											# add a message to help users to transform the Group variable as a qualitative one (warning)
											s_w_message <- "The Group variable is a quantitative variable.<br/>A quantitative variable can be transformed into a qualitative one by checking the concatenation box and re-selecting this variable from the input list"
											v_type <- v_type[-which(v_name == "Group")]
											v_name <- v_name[-which(v_name == "Group")]
										}
										
										if (length(which(v_type == "qualitative")) > 0) {
											v_e_message <- c(v_e_message, paste0("The following variable(s) must be qualitative: ", paste(paste0(v_name[which(v_type == "qualitative")]), collapse = ", ")))
											v_warning <- c(ifelse("X" %in% v_name[which(v_type == "qualitative")] & o_parameter$plot_type %in% c("boxplot", "barplot"), T, F), ifelse("Group" %in% v_name[which(v_type == "qualitative")] & o_parameter$plot_type != "plot", T, F))
											
											if (length(which(v_warning)) > 0) { # add a message to help users to transform (X, Group) variables as a qualitative variable (error)
												if (length(which(v_warning)) > 1) {
													v_e_message <- c(v_e_message, paste0(paste(c("X", "Group"), collapse = " and "), " variables can be transformed into a qualitative variable by checking the concatenation box and re-selecting the variable from the input list"))
												}
												else {
													v_e_message <- c(v_e_message, paste0("The ", c("X", "Group")[which(v_warning)], " variable can be transformed into a qualitative variable by checking the concatenation box and re-selecting the variable from the input list"))
												}
											}
										}
									}
								}
								else {
									v_pos <- grep("ref", v_name)
									
									if (length(v_pos) > 0) {
										v_e_message <- c(v_e_message, paste0("The following random effect(s) must be qualitative: ", paste(isolate(o_parameter$ref)[as.numeric(substr(v_name[v_pos], 4, nchar(v_name[v_pos])))], collapse = ", "), ". Only character/integer variables are detected as a qualitative variable"))
									}
									
									if (isolate(o_parameter$model) == "calib") {
										v_pos <- grep("wres_group", v_name)
										
										if (length(v_pos) > 0) {
											v_e_message <- c(v_e_message, paste0("The following group variable(s) of the residual variance function must be qualitative: ", paste(isolate(o_parameter$wres_group)[as.numeric(substr(v_name[v_pos], 11, nchar(v_name[v_pos])))], collapse = ", "), ". Only character/integer variables are detected as a qualitative variable"))
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
									
									if (length(v_pos) > 0) { # quantitative Group variable accepted for the plot type: plot
										# add a message to help users to transform the Group variable as a qualitative one (warning)
										s_w_message <- "The Group variable is a quantitative variable.<br/>A quantitative variable can be transformed into a qualitative one by checking the concatenation box and re-selecting this variable from the input list"
									}
								}
							}
						}
						else { # corplot
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
									if (length(v_pos) > 1) {
										v_e_message <- c(v_e_message, paste0("The following Y variable(s) must be quantitative: ", paste(isolate(o_parameter$y)[v_pos[-length(v_pos)]], collapse = ", ")))
									}
									
									v_e_message <- c(v_e_message, "The Group variable must be qualitative")
									
									# add a message to help users to transform the Group variable as a qualitative one (error)
									v_e_message <- c(v_e_message, "The Group variable can be transformed into a qualitative variable by checking the concatenation box and re-selecting the variable from the input list")
								}
								else {
									v_e_message <- c(v_e_message, paste0("The following Y variable(s) must be quantitative: ", paste(isolate(o_parameter$y)[v_pos], collapse = ", ")))
								}
							}
						}
						
						if (length(v_e_message) > 0) {s_e_message <- paste0(paste(v_e_message, collapse = ".<br/>"), ifelse(length(v_e_message) > 1, ".", ""))}
					} 
					else { # exexcute the step 5
						v_var_name <- c()
						
						if (isolate(o_parameter$plot_type) == "plot") {
							if (isolate(o_parameter$model) == "none" & is.na(isolate(o_parameter$id))) {
								if (".row_num." %in% names(df_all)) {
									v_var_name <- ".row_num."
								}
							}
						}
						
						if (!isolate(o_parameter$model) %in% c("calib", "valid") & !is.na(o_parameter$f)) {
							if (".f." %in% names(df_all)) {
								v_var_name <- c(v_var_name, ".f.")
							}
						}
						
						if (!is.na(o_parameter$g)) {
							if (".g." %in% names(df_all)) {
								v_var_name <- c(v_var_name, ".g.")
							}
						}
						
						if (!is.na(o_parameter$h)) {
							if (".h." %in% names(df_all)) {
								v_var_name <- c(v_var_name, ".h.")
							}
						}
						
						if (length(v_var_name) > 0) {
							s_e_message <- paste0(ifelse(length(v_var_name) == 1, "A variable", "Variables"), " named ", paste(paste0("\"", v_var_name, "\""), collapse = ", "), " already exist", ifelse(length(v_var_name) == 1, "s", ""), " in the loaded data.")
							
							if (".row_num." %in% v_var_name) {
								s_e_message <- paste0(s_e_message, "<br/>The \".row_num.\" variable is used to identify flags added from the Flag tab when an ID variable is not specified.")
							}
							
							v_pos <- which(v_var_name %in% c(".f.", ".g.", ".h."))
							
							if (length(v_pos) > 0) {
								s_e_message <- paste0(s_e_message, "<br/>", paste(paste0("\"", v_var_name[v_pos], "\""), collapse = ", "), " variable", ifelse(length(v_pos) == 1, " is", "s are"), " used as ", paste(substr(v_var_name[v_pos], 2, 2), collapse = ", "), " function", ifelse(length(v_pos) == 1, "", "s"), ".") 
							}
						}
					}
				} 
			}
		}
		
		return (list(s_e_message, s_w_message))
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
			v_cond <- eval(parse(text = paste0("c(", paste(paste0("is.numeric(df_all$", isolate(o_parameter$y), ")"), collapse = ", "), ")")))
			v_dim <- eval(parse(text = paste0("c(", paste0("dim(df_all[!is.na(df_all$", c(isolate(o_parameter$x), isolate(o_parameter$y)), "),])[1]", collapse = ", "), ")")))
			
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
					
					v_date <- as.character(format(strptime(df_all[, isolate(o_parameter$x)], format = isolate(o_parameter$date_format))))
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
										s_w_message <- "X variable is recognized as an integer variable. The variable could have been transformed into an integer variable when loading data. Therefore, the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times."
									}
								}
							}
							else {
								if (length(unique(v_date)) != length(v_date)) { # return 2 messages (error and warning)
									s_e_message <- "X variable doesn't have unique values"
									s_w_message <- "X variable is recognized as an integer variable. The variable could have been transformed into an integer variable when loading data. Therefore, the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times."
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
							s_w_message <- "The X variable could have been transformed into an integer variable when loading data. Please, check the conformity of dates on the graph. If dates are incorrect, then the problem can be solved by modifying either the data format (txt, csv) or the separator between unit of times."
						}
					}
				} # end step 4
			}
		}
		
		return (list(s_e_message, s_w_message, v_date))
	}
	else { # ir
		s_e_message <- character(0)
		s_w_message <- character(0)
		
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
			
			if (!is.na(isolate(o_parameter$id))) {
				if (length(which(is.na(e_data$all[, isolate(o_parameter$id)]))) > 0) {
					s_e_message <- "ID variable has missing value(s)"
				}
			}
			
			if (length(s_e_message) == 0) {
				v_e_message <- c()
				
				if (!is.na(isolate(o_parameter$id))) {
					v_id <- as.vector(unique(e_data$all[, isolate(o_parameter$id)]))
					
					if (length(v_id) < dim(e_data$all)[1]) { 
						v_e_message <- c(v_e_message, "The ID variable doesn't have unique values")
					}
				}
				
				if (!is.na(isolate(o_parameter$group))) {
					if (length(which(is.na(as.vector(df_all[, isolate(o_parameter$group)])))) == dim(df_all) [1]) {
						v_e_message <- c(v_e_message, "The Group variable has no value")
					}
					else {
						b_cond <- ifelse(length(which(!is.na(isolate(o_parameter$concat2_group)))) == 0, is.character(as.vector(e_data$all[, isolate(o_parameter$group)])), T)
						
						if (b_cond == F) { # quantitative Group variable accepted
							# add a message to help users to transform the Group variable as a qualitative one (warning)
							s_w_message <- "The Group variable is a quantitative variable.<br/>A quantitative variable can be transformed into a qualitative one by checking the concatenation box and re-selecting this variable from the input list"
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
					s_e_message <- paste0(paste(v_e_message, collapse = ".<br/>"), ifelse(length(v_e_message) > 1, ".", ""))
				}
				else { # execute the step 5
					if (is.na(isolate(o_parameter$id))) {
						if (".row_num." %in% names(df_all)) {
							s_e_message <- "A variable named \".row_num.\" already exists in the loaded data. This variable is used to identify flags added from the Flag tab when an ID variable is not specified." 
						}
					}
				}
			}
		}
		
		return (list(s_e_message, s_w_message))
	}
}

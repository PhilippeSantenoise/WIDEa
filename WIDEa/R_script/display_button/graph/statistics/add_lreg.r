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
# Description : function used to execute a linear regression method and add results on the (plotly) 
#               graph. This function only concerns 2D plot of the normal data type (disabled if a 
#               calibration model is applied).
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# ply_1: plotly object created with the R script "Build_graph"
# v_group: (Group) variable modalities retained to execute the corresponding method. If no Group variable 
#          is selected, then v_group = "all". 
# df_click_legend: legend item informations (name and status)
# (o_plot, o_parameter, o_lreg_info): reactive values from the R script "WIDEa_launcher"
# i_w_message: binary value used to display (= 0) or not (= 1) the warning message returned by the function        

# Output:
# ------
# return a list containing a plotly object and warning/error messages

f_add_lreg <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, o_lreg_info, i_w_message) {
	s_w_message <- NULL
	s_e_message <- NULL
	
	if (length(v_group) > 0) {
		if (isolate(o_parameter$model) == "none") {
			eval(parse(text = paste0("df_all <- isolate(o_plot$data)[, c(isolate(o_parameter$x), isolate(o_parameter$y)", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ")]")))
			v_x <- seq(min(df_all[, isolate(o_parameter$x)]), max(df_all[, isolate(o_parameter$x)]), length.out = 1000) # sequence generation (size = 1000) from X variable 
		}
		else { # validation model
			eval(parse(text = paste0("df_all <- as.data.frame(cbind(isolate(o_plot$data)[, c(isolate(o_parameter$y)", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ")], isolate(o_plot$model)[, \"fit\"]))")))
			eval(parse(text = paste0("names(df_all) <- c(isolate(o_parameter$y), ", ifelse(!is.na(isolate(o_parameter$group)), ", isolate(o_parameter$group)", ""), ", \".fit.\")")))
			v_x <- seq(min(df_all[, ".fit."]), max(df_all[, ".fit."]), length.out = 1000) # sequence generation (size = 1000) from fitted values
		}
		
		if (!is.na(isolate(o_parameter$group))) {
			v_group_all <- as.vector(unique(df_all[, isolate(o_parameter$group)]))
			v_group_all <- v_group_all[order(v_group_all)]
			
			l_val <- lapply(v_group, function(x) {
				df_group <- df_all[df_all[, isolate(o_parameter$group)] == x,]
				eval(parse(text = paste0("l_lreg <- lm(", isolate(o_parameter$y), " ~ ", ifelse(isolate(o_parameter$model) == "none", isolate(o_parameter$x), ".fit."), ", data = df_group)")))
				n_intercept <- coef(l_lreg)[[1]]
				s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
				n_slope <- coef(l_lreg)[[2]]
				s_slope <- f_numeric_trsf(n_slope, b_pval = F)
				v_y <- n_intercept + n_slope * v_x
				s_text <- paste0(ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), " = a + b.", ifelse(isolate(o_parameter$model) != "none", paste0("fit(", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ")"), ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), "<br>a = ", s_intercept, "<br>b = ", s_slope)
				n_r2 <- round(1 - sum((df_group[, isolate(o_parameter$y)] - fitted(l_lreg))^2)/sum((df_group[, isolate(o_parameter$y)] - mean(df_group[, isolate(o_parameter$y)]))^2), digits = 2)
				
				if (isolate(o_parameter$model) == "none") {
					n_rmse <- sqrt(sum((df_group[, isolate(o_parameter$y)] - fitted(l_lreg))^2) / length(df_group[, isolate(o_parameter$y)]))
					s_rmse <- f_numeric_trsf(n_rmse, b_pval = F)
				}
				else {
					n_test1 <- f_ttest(l_lreg, 1, 0) # intercept = 0 t-test
					s_test1 <- f_numeric_trsf(n_test1, b_pval = T)
					n_test2 <- f_ttest(l_lreg, 2, 1) # slope = 1 t-test
					s_test2 <- f_numeric_trsf(n_test2, b_pval = T)	
				}
				
				s_text <- paste0(s_text, "<br>R2 = ", n_r2, "<br>", ifelse(isolate(o_parameter$model) == "none", paste0("RMSE = ", s_rmse), paste0("a = 0 test: p-value ", s_test1, "<br>b = 1 test: p-value ", s_test2)))
				
				if (length(which(!is.na(isolate(o_lreg_info$elt)))) > 0) {
					v_info <- as.vector(unlist(lapply(isolate(o_lreg_info$elt), function(x) {
						if (x == "lreg parameters") {
							s_out <- paste0("intercept = ", s_intercept, "<br>slope = ", s_slope)  
						}
						else if (x == "R2") {
							s_out <- paste0("R2 = ", n_r2) 
						}
						else {
							if (isolate(o_parameter$model) == "none") {
								s_out <- paste0("RMSE = ", s_rmse)
							}
							else {
								s_out <- paste0("intercept = 0 test: p-value ", s_test1, "<br>slope = 1 test: p-value ", s_test2)
							}
						}
						
						return(s_out)
					})))
				
					return(list(v_y, s_text, paste(v_info, collapse = "<br>")))
				}
				else {
					return(list(v_y, s_text))
				}
			})
			
			v_pos_1 <- which(v_group_all %in% v_group) 
			v_pos_2 <- which(!v_group_all %in% v_group) 
			v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (lreg)")), "statut"]
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_x, y = l_val[[", 1:length(l_val), "]][[1]], name = \"", v_group, " (lreg)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = adjustcolor(\"", isolate(o_plot$color)[v_pos_1], "\", alpha.f = 0.95), width = 3, dash = \"dash\"), hoverinfo = \"text+name\", text = l_val[[", 1:length(l_val), "]][[2]], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
			
			if (length(which(!is.na(isolate(o_lreg_info$elt)))) > 0) {
				i_num <- length(isolate(o_lreg_info$elt)) + ifelse("lreg parameters" %in% isolate(o_lreg_info$elt), 1, 0) + ifelse("t-test on lreg parameters" %in% isolate(o_lreg_info$elt), 1, 0)
				
				if (length(v_pos_1) > 1) { 
					if (isolate(o_lreg_info$ypos) == 1) {
						eval(parse(text = paste0("v_br <- c(\"\", ", paste(paste0("paste(rep(\" <br>\", ", i_num * (1:(length(v_pos_1) - 1)), "), collapse = \"\")"), collapse = ", "), ")")))
					}
					else {
						eval(parse(text = paste0("v_br <- c(", paste(paste0("paste(rep(\"<br> \", ", i_num * ((length(v_pos_1) - 1):1), "), collapse = \"\")"), collapse = ", "), ", \"\")")))
					}
				}
				else {
					v_br <- ""
				}
				
				if (isolate(o_lreg_info$ypos) == 1) {
					eval(parse(text = paste(paste0("ply_1 <- add_annotations(p = ply_1, x = isolate(o_lreg_info$xpos), y = isolate(o_lreg_info$ypos), xref = \"paper\", yref = \"paper\", align = \"", ifelse(isolate(o_lreg_info$xpos) == 0, "left", "right"), "\", text = paste0(\"", v_br, "\", l_val[[", 1:length(l_val), "]][[3]]), showarrow = F, font = list(color = isolate(o_plot$color)[", v_pos_1, "]))"), collapse = "; ")))
				}
				else {
					eval(parse(text = paste(paste0("ply_1 <- add_annotations(p = ply_1, x = isolate(o_lreg_info$xpos), y = isolate(o_lreg_info$ypos), xref = \"paper\", yref = \"paper\", align = \"", ifelse(isolate(o_lreg_info$xpos) == 0, "left", "right"), "\", text = paste0(l_val[[", 1:length(l_val), "]][[3]], \"", v_br, "\"), showarrow = F, font = list(color = isolate(o_plot$color)[", v_pos_1, "]))"), collapse = "; ")))
				}
			}
			
			if (length(v_pos_2) > 0) {
				if (i_w_message == 0) {
					s_w_message <- paste0("Error with linear regression modeling: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero for one or more groups (", paste(v_group_all[v_pos_2], collapse = ", "), ")")
				}
			}	
		}
		else {
			eval(parse(text = paste0("l_lreg <- lm(", isolate(o_parameter$y), " ~ ", ifelse(isolate(o_parameter$model) == "none", isolate(o_parameter$x), ".fit."), ", data = df_all)")))
			n_intercept <- coef(l_lreg)[[1]]
			s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
			n_slope <- coef(l_lreg)[[2]]
			s_slope <- f_numeric_trsf(n_slope, b_pval = F)
			v_y <- n_intercept + n_slope * v_x
			s_text <- paste0(ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), " = a + b.", ifelse(isolate(o_parameter$model) != "none", paste0("fit(", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ")"), ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), "<br>a = ", s_intercept, "<br>b = ", s_slope)
			n_r2 <- round(1 - sum((df_all[, isolate(o_parameter$y)] - fitted(l_lreg))^2)/sum((df_all[, isolate(o_parameter$y)] - mean(df_all[, isolate(o_parameter$y)]))^2), digits = 2)
			
			if (isolate(o_parameter$model) == "none") {
				n_rmse <- sqrt(sum((df_all[, isolate(o_parameter$y)] - fitted(l_lreg))^2) / length(df_all[, isolate(o_parameter$y)]))
				s_rmse <- f_numeric_trsf(n_rmse, b_pval = F)
			}
			else {
				n_test1 <- f_ttest(l_lreg, 1, 0) # intercept = 0 t-test
				s_test1 <- f_numeric_trsf(n_test1, b_pval = T)
				n_test2 <- f_ttest(l_lreg, 2, 1) # slope = 1 t-test
				s_test2 <- f_numeric_trsf(n_test2, b_pval = T)
			}
			
			s_text <- paste0(s_text, "<br>R2 = ", n_r2, "<br>", ifelse(isolate(o_parameter$model) == "none", paste0("RMSE = ", s_rmse), paste0("a = 0 test: p-value ", s_test1, "<br>b = 1 test: p-value ", s_test2)))
			eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (lreg)"), "statut"])))
			ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, name = "all (lreg)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "lines", line = list(color = adjustcolor(isolate(o_plot$color), alpha.f = 0.95), width = 3, dash = "dash"), hoverinfo = 'text+name', text = s_text, showlegend = T, visible = v_visible)
			
			if (length(which(!is.na(isolate(o_lreg_info$elt)))) > 0) {
				v_info <- as.vector(unlist(lapply(isolate(o_lreg_info$elt), function(x) {
					if (x == "lreg parameters") {
						s_out <- paste0("intercept = ", s_intercept, "<br>slope = ", s_slope)  
					}
					else if (x == "R2") {
						s_out <- paste0("R2 = ", n_r2) 
					}
					else {
						if (isolate(o_parameter$model) == "none") {
							s_out <- paste0("RMSE = ", s_rmse)
						}
						else {
							s_out <- paste0("intercept = 0 test: p-value ", s_test1, "<br>slope = 1 test: p-value ", s_test2)
						}
					}
					
					return(s_out)
				})))
				
				ply_1 <- add_annotations(p = ply_1, x = isolate(o_lreg_info$xpos), y = isolate(o_lreg_info$ypos), xref = "paper", yref = "paper", align = ifelse(isolate(o_lreg_info$xpos) == 0, "left", "right"), text = paste(v_info, collapse = "<br>"), showarrow = F, font = list(color = "#1C86EE"))
			}
		}
	}
	else {
		if (!is.na(isolate(o_parameter$group))) {
			s_e_message <- paste0("Error with linear regression modeling: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero for each group")
		}
		else {
			s_e_message <- paste0("Error with linear regression modeling: insufficient size of data (< 3) or the standard deviation of ", ifelse(isolate(o_parameter$model) == "none", "X" , "fit"), " and/or Y variable(s) is equal to zero")
		}
	}
	
	return (list(ply_1, s_w_message, s_e_message))
}

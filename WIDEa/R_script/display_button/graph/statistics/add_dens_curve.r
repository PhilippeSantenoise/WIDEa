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
# Description : function used to compute a density curve and add results on the (plotly) graph. This
#               function only concerns histplot of the normal data type.
#
# Creation date : February 2021
#########################################################################################################


# Input:
# ------
# ply_1: plotly object created with the R script "Build_graph"
# v_group: (Group) variable modalities retained to execute the corresponding method. If no Group variable 
#          is selected, then v_group = "all". 
# df_click_legend: legend item informations (name and status)
# (o_plot, o_parameter): reactive values from the R script "WIDEa_launcher"
# i_w_message: binary value used to display (= 0) or not (= 1) the warning message returned by the function 
# v_color: graph color vector

# Output:
# -------
# return a list containing a plotly object and warning/error messages

f_add_dens_curve <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, i_w_message, v_color) {
	s_w_message <- NULL
	s_e_message <- NULL
	
	if (length(v_group) > 0) {
		df_all <- isolate(o_plot$data)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "group")[which(!is.na(v_pos))]
		n_op <- ifelse(isolate(o_parameter$autoop) == F, isolate(o_parameter$op), 0.5)
		
		if (!is.na(isolate(o_parameter$group))) {
			v_group_all <- as.vector(unique(df_all$group))
			v_group_all <- v_group_all[order(v_group_all)]
			
			if (length(v_group) < length(v_group_all)) {
				if (i_w_message == 0) {
					s_w_message <- paste0("Error with density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to 0 for one or more groups (", paste(v_group_all[which(!v_group_all %in% v_group)], collapse = ", "), ")")
				}
			}
			
			v_subcolor <- v_color[which(v_group_all %in% v_group)]
			eval(parse(text = paste(paste0("l_density_", 1:length(v_group), " <- density(df_all[which(df_all$group == \"", v_group, "\"), \"x\"])"), collapse = "; ")))
			
			l_val <- lapply(v_group, function(x) {
				v_x <- df_all[which(df_all$group == x), "x"]
				i_size <- length(v_x)
				n_mean <- mean(v_x)
				s_mean <- f_numeric_trsf(n_mean, b_pval = F)
				n_median <- median(v_x)
				s_median <- f_numeric_trsf(n_median, b_pval = F)
				n_sd <- sd(v_x)
				s_sd <- f_numeric_trsf(n_sd, b_pval = F)
				n_sw_pval <- tryCatch({suppressWarnings(shapiro.test(v_x)$p.value)}, error = function(e) FALSE) # Shapiro-Wilk test
				s_test1 <- ifelse(is.numeric(n_sw_pval), paste0("<br>Shapiro-Wilk: p-value ", f_numeric_trsf(n_sw_pval, b_pval = T)), "")
				n_ks_pval <- tryCatch({suppressWarnings(ks.test(v_x, "pnorm", mean = n_mean, sd = n_sd)$p.value)}, error = function(e) FALSE) # Kolmogorov-Smirnov test 
				s_test2 <- ifelse(is.numeric(n_ks_pval), paste0("<br>Kolmogorov-Smirnov: p-value ", f_numeric_trsf(n_ks_pval, b_pval = T)), "")
				s_test2 <- paste0(s_test2, ifelse(length(unique(v_x)) < length(v_x), " (presence of ties)", ""))
				s_test <- paste0(s_test1, s_test2)
				return(list(c(n_mean, n_median), paste0("size = ", i_size, "<br>mean = ", s_mean, "<br>median = ", s_median, "<br>sd = ", s_sd, ifelse(s_test != "", paste0("<br><br>Normality test:", s_test), ""))))
			})
			
			eval(parse(text = paste0("v_text <- c(", paste(paste0("l_val[[", 1:length(v_group), "]][[2]]"), collapse = ", "), ")")))
			df_group <- df_all[which(df_all$group %in% v_group),]
			n_bartlett_pval <- tryCatch({suppressWarnings(bartlett.test(x ~ group, data = df_group)$p.value)}, error = function(e) FALSE) # Bartlett test
			s_test1 <- ifelse(is.numeric(n_bartlett_pval), paste0("<br>Bartlett: p-value ", f_numeric_trsf(n_bartlett_pval, b_pval = T)), "")
			n_levene1_pval <- tryCatch({suppressWarnings(leveneTest(x ~ group, data = df_group, center = mean)$"Pr(>F)"[1])}, error = function(e) FALSE) # Levene test (center mean)
			s_test2 <- ifelse(is.numeric(n_levene1_pval), paste0("<br>Levene (center = mean): p-value ", f_numeric_trsf(n_levene1_pval, b_pval = T)), "")
			n_levene2_pval <- tryCatch({suppressWarnings(leveneTest(x ~ group, data = df_group, center = median)$"Pr(>F)"[1])}, error = function(e) FALSE) # Levene test (center median)
			s_test3 <- ifelse(is.numeric(n_levene2_pval), paste0("<br>Levene (center = median): p-value ", f_numeric_trsf(n_levene2_pval, b_pval = T)), "")
			s_test <- paste0(s_test1, s_test2, s_test3)
			v_text <- paste0(v_text, ifelse(s_test != "", paste0("<br><br>Homogeneity of variances test:", s_test), ""))
			v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (curve)")), "statut"]
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = l_density_", 1:length(v_group), "$x, y = l_density_", 1:length(v_group), "$y, name = \"", v_group, " (curve)\", type = \"scatter\", mode = \"lines\", line = list(color = adjustcolor(\"", v_subcolor, "\", alpha.f = 1)), fill = \"tozeroy\", fillcolor = adjustcolor(\"", v_subcolor, "\", alpha.f = 0.3), legendgroup = \"", v_group, " (curve)\", hoverinfo = 'text+name', hoverlabel = list(bgcolor = adjustcolor(\"", v_subcolor, "\", alpha.f = 1)), text = \"", v_text, "\", showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
			
			# add mean/median:
			
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = rep(l_val[[", 1:length(v_group), "]][[1]][1], 2), y = c(0, approx(l_density_", 1:length(v_group), "$x, l_density_", 1:length(v_group), "$y, xout = l_val[[", 1:length(v_group), "]][[1]][1])$y), type = \"scatter\", mode = \"lines\", line = list(color = adjustcolor(\"", v_subcolor, "\", alpha.f = 1), dash = \"dash\"), legendgroup = \"", v_group, " (curve)\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
			eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = rep(l_val[[", 1:length(v_group), "]][[1]][2], 2), y = c(0, approx(l_density_", 1:length(v_group), "$x, l_density_", 1:length(v_group), "$y, xout = l_val[[", 1:length(v_group), "]][[1]][2])$y), type = \"scatter\", mode = \"lines\", line = list(color = adjustcolor(\"", v_subcolor, "\", alpha.f = 1), dash = \"dot\"), legendgroup = \"", v_group, " (curve)\", showlegend = F, visible = ", v_visible, ")"), collapse = "; ")))
		}
		else {
			v_x <- as.vector(df_all$x)
			n_mean <- mean(v_x)
			n_sd <- sd(v_x)
			l_density <- density(v_x)
			i_size <- length(v_x)
			s_mean <- f_numeric_trsf(n_mean, b_pval = F)
			n_median <- median(v_x)
			s_median <- f_numeric_trsf(n_median, b_pval = F)
			s_sd <- f_numeric_trsf(n_sd, b_pval = F)
			n_sw_pval <- tryCatch({suppressWarnings(shapiro.test(v_x)$p.value)}, error = function(e) FALSE)
			s_test1 <- ifelse(is.numeric(n_sw_pval), paste0("<br>Shapiro-Wilk: p-value ", f_numeric_trsf(n_sw_pval, b_pval = T)), "") # Shapiro-Wilk test
			n_ks_pval <- tryCatch({suppressWarnings(ks.test(v_x, "pnorm", mean = n_mean, sd = n_sd)$p.value)}, error = function(e) FALSE) 
			s_test2 <- ifelse(is.numeric(n_ks_pval), paste0("<br>Kolmogorov-Smirnov: p-value ", f_numeric_trsf(n_ks_pval, b_pval = T)), "") # Kolmogorov-Smirnov test
			s_test2 <- paste0(s_test2, ifelse(length(unique(v_x)) < length(v_x), " (presence of ties)", ""))
			s_test <- paste0(s_test1, s_test2)
			s_text <- paste0("size = ", i_size, "<br>mean = ", s_mean, "<br>median = ", s_median, "<br>sd = ", s_sd, ifelse(s_test != "", paste0("<br><br>Normality test:", s_test), ""))
			v_visible <- df_click_legend[which(df_click_legend$name == "all (curve)"), "statut"]
			ply_1 <- add_trace(p = ply_1, x = l_density$x, y = l_density$y, name = "all (curve)", type = "scatter", mode = "lines", line = list(color = adjustcolor(v_color, alpha.f = 1)), fill = "tozeroy", fillcolor = adjustcolor(v_color, alpha.f = 0.3), legendgroup = "all (curve)", hoverinfo = 'text+name', hoverlabel = list(bgcolor = adjustcolor(v_color, alpha.f = 1)), text = s_text, showlegend = T, visible = eval(parse(text = v_visible)))
			
			# add mean/median:
			
			ply_1 <- add_trace(p = ply_1, x = rep(n_mean, 2), y = c(0, approx(l_density$x, l_density$y, xout = n_mean)$y), type = "scatter", mode = "lines", line = list(color = adjustcolor(v_color, alpha.f = 1), dash = "dash"), legendgroup = "all (curve)", showlegend = F, visible = eval(parse(text = v_visible)))
			ply_1 <- add_trace(p = ply_1, x = rep(n_median, 2), y = c(0, approx(l_density$x, l_density$y, xout = n_median)$y), type = "scatter", mode = "lines", line = list(color = adjustcolor(v_color, alpha.f = 1), dash = "dot"), legendgroup = "all (curve)", showlegend = F, visible = eval(parse(text = v_visible)))
		}
	}
	else {
		if (!is.na(isolate(o_parameter$group))) {
			s_e_message <- "Error with density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to 0 for all groups"
		}
		else {
			s_e_message <- "Error with density calculation: insufficient size of data (< 2) or the standard deviation of X variable is equal to 0"
		}
	}
	
	return (list(ply_1, s_w_message, s_e_message))
}

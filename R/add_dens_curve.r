#' @importFrom shiny isolate
#' @importFrom plotly add_trace
#' @importFrom grDevices adjustcolor
#' @importFrom stats median sd shapiro.test ks.test bartlett.test density approx
NULL

#' Adding density curves in the main plotly graph

#' @description
#' `f_add_dens_curve` returns a plotly object including density curves. This 
#' function only concerns histplot of the normal data type. Several hypothesis tests
#' are added in the hover text of the density curve: normality (`shapiro.test`,
#' `ks.test`), variance homogeneity (`bartlett.test`, `leveneTest`) if a Group
#' variable is specified (`v_group`).

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param v_group is the Group variable levels used to applied the corresponding
#' method. If no Group variable is selected, then `v_group` = "all".
#' @param df_click_legend are legend item data information (name and status).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param v_color is the graph color vector.

#' @encoding UTF-8

f_add_dens_curve <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, v_color) {
	df_all <- isolate(o_plot$data)
	v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), isolate(o_parameter$group)), names(df_all))
	names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "group")[which(!is.na(v_pos))]
	n_op <- ifelse(isolate(o_parameter$autoop) == F, isolate(o_parameter$op), 0.5)
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group_all <- as.vector(unique(df_all$group))
		v_group_all <- v_group_all[order(v_group_all)]
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
		n_levene1_pval <- tryCatch({suppressWarnings(car::leveneTest(x ~ group, data = df_group, center = mean)$"Pr(>F)"[1])}, error = function(e) FALSE) # Levene test (center mean)
		s_test2 <- ifelse(is.numeric(n_levene1_pval), paste0("<br>Levene (center = mean): p-value ", f_numeric_trsf(n_levene1_pval, b_pval = T)), "")
		n_levene2_pval <- tryCatch({suppressWarnings(car::leveneTest(x ~ group, data = df_group, center = median)$"Pr(>F)"[1])}, error = function(e) FALSE) # Levene test (center median)
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
	
	return (ply_1)
}

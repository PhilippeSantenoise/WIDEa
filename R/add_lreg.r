#' @importFrom shiny isolate
#' @importFrom plotly add_trace add_annotations
NULL

#' Adding a linear regression in the main plotly graph

#' @description
#' `f_add_lreg` returns a plotly object including a linear regression
#' (`lm`). Several quality criteria and hypothesis tests are added in the hover text
#' of the linear regression: R2, RMSE, equality t-test (`f_ttest`: intercept = 0,
#' slope = 1). This function only concerns 2D plot of the normal data type (disabled
#' if a calibration model is applied).

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param v_group is the Group variable levels used to applied the corresponding
#' method. If no Group variable is selected, then `v_group` = "all".
#' @param df_click_legend are legend item data information (name and status).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param v_color is the graph color vector.

#' @encoding UTF-8

f_add_lreg <- function (ply_1, v_group = "all", df_click_legend, o_plot, o_parameter, v_color) {
	df_all <- isolate(o_plot$data)
	
	if (isolate(o_parameter$model) == "valid") { # validation model
		df_model <- isolate(o_plot$model)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		df_all <- as.data.frame(df_all[, v_pos[!is.na(v_pos)]])
		names(df_all) <- c("y", "group")[which(!is.na(v_pos))]
		df_all$x <- as.vector(df_model$fit)
		rm(list = "df_model")
	}
	else { # no model
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "y", "group")[which(!is.na(v_pos))]
	}
	
	v_x <- seq(min(df_all$x), max(df_all$x), length.out = 1000) # sequence generation (size = 1000) from X variable
	
	if (!is.na(isolate(o_parameter$group))) {
		v_group_all <- as.vector(unique(df_all$group))
		v_group_all <- v_group_all[order(v_group_all)]
		
		l_val <- lapply(v_group, function(x) {
			df_group <- df_all[which(df_all$group == x),]
			l_lreg <- stats::lm(y ~ x, data = df_group)
			n_intercept <- stats::coef(l_lreg)[[1]]
			s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
			n_slope <- stats::coef(l_lreg)[[2]]
			s_slope <- f_numeric_trsf(n_slope, b_pval = F)
			v_y <- n_intercept + n_slope * v_x
			s_text <- paste0(ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), " = a + b.", ifelse(isolate(o_parameter$model) != "none", paste0("fit(", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ")"), ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), "<br>a = ", s_intercept, "<br>b = ", s_slope)
			n_r2 <- round(1 - sum((df_group$y - stats::fitted(l_lreg))^2)/sum((df_group$y - mean(df_group$y))^2), digits = 2)
			
			if (isolate(o_parameter$model) == "none") {
				n_rmse <- sqrt(sum((df_group$y - stats::fitted(l_lreg))^2) / length(df_group$y))
				s_rmse <- f_numeric_trsf(n_rmse, b_pval = F)
			}
			else {
				n_test1 <- f_ttest(l_lreg, 1, 0) # intercept = 0 t-test
				s_test1 <- f_numeric_trsf(n_test1, b_pval = T)
				n_test2 <- f_ttest(l_lreg, 2, 1) # slope = 1 t-test
				s_test2 <- f_numeric_trsf(n_test2, b_pval = T)	
			}
			
			s_text <- paste0(s_text, "<br>R2 = ", n_r2, "<br>", ifelse(isolate(o_parameter$model) == "none", paste0("RMSE = ", s_rmse), paste0("a = 0 test: p-value ", s_test1, "<br>b = 1 test: p-value ", s_test2)))
			return(list(v_y, s_text))
		})
		
		v_pos_1 <- which(v_group_all %in% v_group) 
		v_pos_2 <- which(!v_group_all %in% v_group) 
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (lreg)")), "statut"]
		eval(parse(text = paste(paste0("ply_1 <- add_trace(p = ply_1, x = v_x, y = l_val[[", 1:length(l_val), "]][[1]], name = \"", v_group, " (lreg)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"lines\", line = list(color = grDevices::adjustcolor(\"", v_color[v_pos_1], "\", alpha.f = 1), width = 3, dash = \"dash\"), hoverinfo = \"text+name\", text = l_val[[", 1:length(l_val), "]][[2]], showlegend = T, visible = ", v_visible, ")"), collapse = "; ")))
	}
	else {
		l_lreg <- stats::lm(y ~ x, data = df_all)
		n_intercept <- stats::coef(l_lreg)[[1]]
		s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
		n_slope <- stats::coef(l_lreg)[[2]]
		s_slope <- f_numeric_trsf(n_slope, b_pval = F)
		v_y <- n_intercept + n_slope * v_x
		s_text <- paste0(ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), " = a + b.", ifelse(isolate(o_parameter$model) != "none", paste0("fit(", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ")"), ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), "<br>a = ", s_intercept, "<br>b = ", s_slope)
		n_r2 <- round(1 - sum((df_all$y - stats::fitted(l_lreg))^2)/sum((df_all$y - mean(df_all$y))^2), digits = 2)
		
		if (isolate(o_parameter$model) == "none") {
			n_rmse <- sqrt(sum((df_all$y - stats::fitted(l_lreg))^2) / length(df_all$y))
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
		ply_1 <- add_trace(p = ply_1, x = v_x, y = v_y, name = "all (lreg)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "lines", line = list(color = grDevices::adjustcolor(v_color, alpha.f = 1), width = 3, dash = "dash"), hoverinfo = 'text+name', text = s_text, showlegend = T, visible = v_visible)
	}
	
	return (ply_1)
}

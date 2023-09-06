#' @importFrom grDevices adjustcolor 
#' @importFrom stats sd coef lm
NULL

#' Modal dialog including a plotly graph used to describe the relationship between
#' X, Y variables of a correlation matrix.

#' @description
#' `f_build_modal_graph` allows to create a new graph in a modal dialog when a click
#' event is executed on a XY cell of the correlation matrix (corplot, normal data 
#' type).

#' @param df_all are data created by the `f_create_modal_data` function.
#' @param i_num is an integer used to build a plotly graph (0: XY scatter-plot + X 
#' histplot + Y histplot; 1: XY scatter-plot; 2: X histplot; 3: Y histplot).

#' @encoding UTF-8

f_build_modal_graph <- function (df_all,  i_num) {
	s_x_var <- names(df_all)[1]
	s_y_var <- names(df_all)[2]
	b_cond <- i_num %in% c(0, 1) & sd(df_all[, s_x_var]) > 0 & sd(df_all[, s_y_var]) > 0
	
	if (b_cond) { # add linear regression to XY scatter-plot
		l_lreg <- eval(parse(text = paste0("lm(", s_y_var, " ~ ", s_x_var, ", data = df_all)")))
		v_x <- seq(min(df_all[, s_x_var]), max(df_all[, s_x_var]), length.out = 1000) # sequence generation (size = 1000) from X variable
		n_intercept <- coef(l_lreg)[[1]]
		s_intercept <- f_numeric_trsf(n_intercept, b_pval = F)
		n_slope <- coef(l_lreg)[[2]]
		s_slope <- f_numeric_trsf(n_slope, b_pval = F)
		v_y <- n_intercept + n_slope * v_x
		n_test <- f_ttest(l_lreg, 2, 0) # slope = 0 t-test
		s_test <- f_numeric_trsf(n_test, b_pval = T)
		s_text <- paste0(s_y_var, " = a + b . ", s_x_var, "<br>a = ", s_intercept, "<br>b = ", s_slope, "<br>b = 0 test: p-value ", s_test)
	}
	
	if (i_num == 0) {
		v_text <- paste0(s_x_var, ": ", df_all[, 1], "<br>", s_y_var, ": ", df_all[, 2])
		ply_1_1 <- plotly::plot_ly()
		ply_1_1 <- plotly::add_trace(p = ply_1_1, x = df_all[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_x_var)
		ply_1_1 <- plotly::layout(p = ply_1_1, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1_2 <- plotly::plot_ly()
		ply_1_2 <- plotly::add_trace(p = ply_1_2, x = df_all[, 1], y = df_all[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverinfo = 'text', text = v_text)
		
		if (b_cond) {
			ply_1_2 <- plotly::add_trace(p = ply_1_2, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.95), width = 3, dash = "dash"), hoverinfo = 'text', text = s_text, showlegend = F)
		}
		
		ply_1_2 <- plotly::layout(p = ply_1_2, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		
		ply_1_3 <- plotly::plot_ly()
		ply_1_3 <- plotly::add_trace(p = ply_1_3, y = df_all[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_y_var)
		ply_1_3 <- plotly::layout(p = ply_1_3, xaxis = list(fixedrange = T), yaxis = list(fixedrange = T))
		
		ply_1 <- subplot(list(ply_1_1, plotly::plotly_empty(), ply_1_2, ply_1_3), nrows = 2, heights = c(0.3, 0.7), widths = c(0.7, 0.3), margin = 0, shareX = T, shareY = T, titleX = T, titleY = T) %>% plotly::config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		ply_1 <- plotly::layout(p = ply_1, showlegend = F)
	}
	else {
		ply_1 <- plotly::plot_ly() %>% plotly::config(displaylogo = F, doubleClick = F, displayModeBar = T, modeBarButtonsToRemove = c('sendDataToCloud', 'zoom2d', 'autoScale2d', 'resetScale2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toImage'))
		
		if (i_num == 1) {
			v_text <- paste0(s_x_var, ": ", df_all[, 1], "<br>", s_y_var, ": ", df_all[, 2])
			ply_1 <- plotly::add_trace(p = ply_1, x = df_all[, 1], y = df_all[, 2], type = "scattergl", mode = "markers", marker = list(size = 6, color = adjustcolor("dodgerblue2", alpha.f = 0.7)), hoverinfo = 'text', text = v_text)
			
			if (b_cond) {
				ply_1 <- plotly::add_trace(p = ply_1, x = v_x, y = v_y, name = "all (lreg)", type = "scattergl", mode = "lines", line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.95), width = 3, dash = "dash"), hoverinfo = 'text', text = s_text, showlegend = F)
			}
			
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = s_y_var, fixedrange = T))
		}
		else if (i_num == 2) {
			ply_1 <- plotly::add_trace(p = ply_1, x = df_all[, 1], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_x_var)
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_x_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
		else {
			ply_1 <- plotly::add_trace(p = ply_1, x = df_all[, 2], type = "histogram", histnorm = "probability density", marker = list(color = adjustcolor("dodgerblue2", alpha.f = 0.5), line = list(color = adjustcolor("dodgerblue2", alpha.f = 0.8), width = 1)), name = s_y_var)
			ply_1 <- plotly::layout(p = ply_1, xaxis = list(title = s_y_var, fixedrange = T), yaxis = list(title = "Density", fixedrange = T))
		}
	}
	
	return(ply_1)
}

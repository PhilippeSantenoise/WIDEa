#' @importFrom shiny isolate
#' @importFrom plotly add_trace
#' @importFrom grDevices adjustcolor 
NULL

#' Adding centroids in the main plotly graph

#' @description
#' `f_add_centroid` returns a plotly object including centroids. This function only
#' concerns 2D/3D plot of the normal data type (disabled if a calibration model is
#' applied).

#' @param ply_1 is the plotly object created with the "build_graph" R script.
#' @param df_click_legend are legend item data information (name and status).
#' @param o_plot is a reactive value including `ply_1` data information.
#' @param o_parameter is a reactive value including parameters associated to the
#' left panel (sections after data loading) and top panels.
#' @param l_graph_opt is the list of graph option (color, opacity, point type/size,
#' sorting) created by the `f_create_graph_opt_vector` function.

#' @encoding UTF-8

f_add_centroid <- function (ply_1, df_click_legend, o_plot, o_parameter, l_graph_opt) {
	df_all <- isolate(o_plot$data)
	i_dec_num <- ifelse(isolate(o_parameter$autodec_num) == F, isolate(o_parameter$dec_num), 2)
	
	if (length(which(o_parameter$dim_num == "3d")) > 0) {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
	}
	else {
		v_symbol <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "triangle-up", "triangle-up-open", "triangle-down", "triangle-down-open", "star", "star-open", "hourglass-open", "bowtie-open", "circle-cross-open", "circle-x-open", "square-cross-open", "square-x-open", "diamond-cross-open", "diamond-x-open", "cross-thin-open", "x-thin-open", "asterisk-open", "hash-open")
	}
	
	v_min_max <- c()
	
	if (isolate(o_parameter$model) == "valid") { # validation model
		df_model <- isolate(o_plot$model)
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), isolate(o_parameter$group)), names(df_all))
		df_all <- as.data.frame(df_all[, v_pos[!is.na(v_pos)]])
		names(df_all) <- c("y", "group")[which(!is.na(v_pos))]
		df_all$x <- as.vector(df_model$fit)
		rm(list = "df_model")
	}
	else { # no model
		v_pos <- match(c(ifelse(!is.na(isolate(o_parameter$f)), ".f.", isolate(o_parameter$x)), ifelse(!is.na(isolate(o_parameter$g)), ".g.", isolate(o_parameter$y)), ifelse(!is.na(isolate(o_parameter$h)), ".h.", isolate(o_parameter$z)), isolate(o_parameter$group)), names(df_all))
		names(df_all)[v_pos[!is.na(v_pos)]] <- c("x", "y", "z", "group")[which(!is.na(v_pos))]
	}
	
	if (!is.na(isolate(o_parameter$group)) & !isolate(o_parameter$quant_group)) {
		v_order <- order(l_graph_opt$sorting)
		v_group <- as.vector(unique(df_all$group))
		v_group <- v_group[order(v_group)]
		v_visible <- df_click_legend[which(df_click_legend$name %in% paste0(v_group, " (centroid)")), "statut"]
		
		if (isolate(o_parameter$dim_num) == "3d") {
			df_mean <- stats::aggregate(cbind(x, y, z) ~ group, data = df_all, mean)
			v_text <- paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(df_mean$x, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean$y, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "z", "h(z)"), ": ", round(df_mean$z, digits = i_dec_num)) 
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[which(df_mean$group == \"", v_group[v_order], "\"), \"x\"], y = df_mean[which(df_mean$group == \"", v_group[v_order], "\"), \"y\"], z = df_mean[which(df_mean$group == \"", v_group[v_order], "\"), \"z\"], name = \"", v_group[v_order], " (centroid)\", type = \"scatter3d\", mode = \"markers\", marker = list(symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1), size = ", 2 * l_graph_opt$point_size[v_order], ", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text\", text = \"", v_text[v_order], "\", showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; "))) 
		}
		else { # plot 2d or validation model
			df_mean <- stats::aggregate(cbind(x, y) ~ group, data = df_all, mean)
			v_text <- paste0(ifelse(isolate(o_parameter$model) == "valid", "fit", ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), ": ", round(df_mean$x, digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(df_mean$y, digits = i_dec_num))
			eval(parse(text = paste(paste0("ply_1  <- add_trace(p = ply_1, x = df_mean[which(df_mean$group == \"", v_group[v_order], "\"), \"x\"], y = df_mean[which(df_mean$group == \"", v_group[v_order], "\"), \"y\"], name = \"", v_group[v_order], " (centroid)\", type = \"", ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), "\", mode = \"markers\", marker = list(symbol = \"", v_symbol[l_graph_opt$point_type[v_order]], "\", color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1), size = ", 2 * l_graph_opt$point_size[v_order], ", line = list(color = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(\"", l_graph_opt$color[v_order], "\", alpha.f = 1)), hoverinfo = \"text\", text = \"", v_text[v_order], "\", showlegend = T, visible = ", v_visible[v_order], ")"), collapse = "; ")))
		}
	}
	else {
		eval(parse(text = paste0("v_visible <- ", df_click_legend[which(df_click_legend$name == "all (centroid)"), "statut"])))
		s_color <- ifelse(isolate(o_parameter$quant_group), "black", l_graph_opt$color)
		s_symbol <- eval(parse(text = paste0("v_symbol[l_graph_opt$", ifelse(isolate(o_parameter$quant_group), "pal_point$type", "point_type"), "]")))
		n_size <- eval(parse(text = paste0("l_graph_opt$", ifelse(isolate(o_parameter$quant_group), "pal_point$size", "point_size"))))
		
		if (isolate(o_parameter$dim_num) == "3d") {
			ply_1  <- add_trace(p = ply_1, x = mean(df_all$x), y = mean(df_all$y), z = mean(df_all$z), name = "all (centroid)", type = "scatter3d", mode = "markers", marker = list(symbol = s_symbol, color = adjustcolor(s_color, alpha.f = 1), size = 2 * n_size, line = list(color = adjustcolor(s_color, alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(s_color, alpha.f = 1)), hoverinfo = "text", text = paste0(ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)"), ": ", round(mean(df_all$x), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all$y), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$h)), "z", "h(z)"), ": ", round(mean(df_all$z), digits = i_dec_num)), showlegend = T, visible = v_visible)
			if (isolate(o_parameter$quant_group)) {v_min_max <- c(rep(mean(df_all$x), 2), rep(mean(df_all$y), 2), rep(mean(df_all$z), 2))}
		}
		else { # plot 2d or validation model
			ply_1  <- add_trace(p = ply_1, x = mean(df_all$x), y = mean(df_all$y), name = "all (centroid)", type = ifelse(isolate(o_parameter$webgl) == "yes", "scattergl", "scatter"), mode = "markers", marker = list(symbol = s_symbol, color = adjustcolor(s_color, alpha.f = 1), size = 2 * n_size, line = list(color = adjustcolor(s_color, alpha.f = 1), width = 1.5)), hoverlabel = list(bgcolor = adjustcolor(s_color, alpha.f = 1)), hoverinfo = "text", text = paste0(ifelse(isolate(o_parameter$model) == "valid", "fit", ifelse(is.na(isolate(o_parameter$f)), "x", "f(x)")), ": ", round(mean(df_all$x), digits = i_dec_num), "<br>", ifelse(is.na(isolate(o_parameter$g)), "y", "g(y)"), ": ", round(mean(df_all$y), digits = i_dec_num)), showlegend = T, visible = v_visible)
			if (nrow(isolate(o_parameter$min_max)) > 0) {v_min_max <- c(rep(mean(df_all$x), 2), rep(mean(df_all$y), 2))}
		}
	}
	
	if (length(v_min_max) > 0) {
		return(list(ply_1, v_min_max))
	}
	else {
		return (ply_1)
	}
}

###########################################################################
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
# Description : list of funtions used by the WIDEa server 
#
# Creation date : May 2021
###########################################################################


# Function used to show a notification message in the web-interface

f_showNotification <- function (ui, action = NULL, duration = 15, closeButton = TRUE, id = NULL, type = c("default", "message", "warning", "error"), session = shiny:::getDefaultReactiveDomain()) {
    if (is.null(id)) id <- shiny:::createUniqueId(8)
	res <- shiny:::processDeps(HTML(ui), session)
    actionRes <- shiny:::processDeps(action, session)
    session$sendNotification("show", list(html = res$html, action = actionRes$html, deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 1000, closeButton = closeButton, id = id, type = match.arg(type)))
    id
}

# Function used to calculate the p-value of coefficient equality test for linear regression
# l_lreg: linear regression model (type: lm())
# i_coef_num: coefficient number (type: integer). By example, if the model is a simple linear regression, then 1 and 2 corespond to intercept and slope
# n_val: value of equality test (type: numeric) 
 
f_ttest <- function(l_lreg, i_coef_num, n_val){
	df_coef <- coef(summary(l_lreg))
	n_tstat <- (df_coef[i_coef_num, 1] - n_val) / df_coef[i_coef_num, 2]
	return(2 * pt(abs(n_tstat), l_lreg$df.residual, lower.tail = F))
}

# Function used to write flag file with checking if the file is not opened
# df_flag: flag file (type: data.frame)
# s_flag_path : path where write the flag file (type: character)
# The function return a boolean value (FALSE) if the file is opened

f_file_write <- function(df_flag, s_flag_path) {
	tryCatch({suppressWarnings(fwrite(df_flag, file = s_flag_path, sep = ","))}, error = function(e) FALSE)
}

# Function used to round a numeric value (corresponding to a p-value or not) and transform it into a string value
# n_val: numeric value
# b_pval: the numeric value is a p-value ? TRUE or FALSE

f_numeric_trsf <- function (n_val, b_pval = F) { 
	v_pos_1 <- as.vector(gregexpr("0", as.character(n_val))[[1]])
	v_pos_2 <- as.vector(gregexpr("e-", as.character(n_val))[[1]])
	
	if (b_pval == F) {
		if (v_pos_1[1] == 1 | length(which(v_pos_2 == -1)) == 0) {
			if (v_pos_1[1] == 1) {
				i_exp <- min(as.vector(gregexpr("(1+|2+|3+|4+|5+|6+|7+|8+|9+)", as.character(n_val))[[1]]) - 2)
				
				if (i_exp > 2) {
					s_val <- paste0(round(n_val * 10^(i_exp), digits = 2), "e-", ifelse(i_exp < 10, paste0("0", i_exp), i_exp))
				}
				else {
					s_val <- as.character(round(n_val, digits = 2))
				}
			}
			else {
				i_exp <- as.numeric(substr(as.character(n_val), v_pos_2 + 2, nchar(as.character(n_val))))
				s_val <- paste0(round(n_val * 10^(i_exp), digits = 2), "e-", ifelse(i_exp < 10, paste0("0", i_exp), i_exp))
			}
		}
		else {
			s_val <- as.character(round(n_val, digits = 2))
		} 
	}
	else {
		if (v_pos_1[1] == 1 | length(which(v_pos_2 == -1)) == 0) {
			if (v_pos_1[1] == 1) {
				i_exp <- min(as.vector(gregexpr("(1+|2+|3+|4+|5+|6+|7+|8+|9+)", as.character(n_val))[[1]]) - 2)
				
				if (i_exp > 3 | n_val == 0) {
					s_val <- "< 1e-03 "
				}
				else {
					if (i_exp > 2) {
						s_val <- paste0("= ", round(n_val * 10^(i_exp), digits = 2), "e-", ifelse(i_exp < 10, paste0("0", i_exp), i_exp))
					}
					else {
						s_val <- paste0("= ", round(n_val, digits = 2))
					}
				}
			}
			else {
				i_exp <- as.numeric(substr(as.character(n_val), v_pos_2 + 2, nchar(as.character(n_val))))
				
				if (i_exp > 3) {
					s_val <- "< 1e-03 "
				}
				else {
					s_val <- paste0("= ", round(n_val * 10^(i_exp), digits = 2), "e-", ifelse(i_exp < 10, paste0("0", i_exp), i_exp))
				}
			}
		}
		else {
			s_val <- "= 1"
		}
	}
	
	return(s_val)
}

# Function used to add lines in plotly graph
# example : ply_1 <- layout(p = ply_1, shapes = list(f_hline(0)))

f_vline <- function(x = 0, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		y0 = 0, 
		y1 = 1, 
		yref = "paper",
		x0 = x, 
		x1 = x, 
		line = list(color = color, width = width, dash = dash)
	)
}

f_hline <- function(y = 0, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		x0 = 0, 
		x1 = 1, 
		xref = "paper",
		y0 = y, 
		y1 = y, 
		line = list(color = color, width = width, dash = dash)
	)
}

f_abline <- function(xmin = 0, xmax = 1, a = 0, b = 1, color = "black", width = 1, dash = NULL) {
	list(type = "line", 
		x0 = xmin, 
		x1 = xmax, 
		y0 = a + b * xmin, 
		y1 = a + b * xmax, 
		line = list(color = color, width = width, dash = dash)
	)
}

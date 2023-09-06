#' List of minor functions used by the WIDEa server 

#' @description
#' `f_file_write` is used to check if it is possible to write flag data. A boolean
#' value (FALSE) is returned if data are opened.
#' \cr`f_numeric_trsf` is used to round a numeric value and transform it into a
#' string value.

#' @param df_flag is the flag data frame.
#' @param s_flag_path is the location (path as string value) used to write the flag
#' data.
#' @param n_val is a numeric value.
#' @param b_pval is a boolean value equals to TRUE if `n_val` corresponds to a
#' p-value returned by an hypothesis test (FALSE as default value). 

f_file_write <- function(df_flag, s_flag_path) {
	tryCatch({suppressWarnings(data.table::fwrite(df_flag, file = s_flag_path, sep = ","))}, error = function(e) FALSE)
}

#' @rdname f_file_write
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

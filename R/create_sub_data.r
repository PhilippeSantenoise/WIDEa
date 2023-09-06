#' Creating sub-data

#' @description
#' `f_create_sub_data` returns a list including (1) row numbers corresponding to the
#' combination of conditions, (2) the sud-dataset and (3) an error message if a 
#' problem is occured.

#' @param df_all are data saved in the e_data environment (e_data$all).
#' @param o_sdata_cond is a reactive value including all conditions added. 
#' @param s_formula is the formula corresponding to the combination of conditions.

#' @encoding UTF-8

f_create_sub_data <- function(df_all, o_sdata_cond, s_formula) {
	v_row <- c()
	df_sub <- data.frame()
	s_e_message <- character(0)
	
	l_pos <- lapply(1:length(o_sdata_cond$var_name), function(x) {
		s_char <- paste0("c", x)
		v_pos_1 <- as.vector(gregexpr(s_char, s_formula)[[1]])
		
		if (sum(v_pos_1) < 0) {
			return(NULL)
		}
		else {
			v_pos_2 <- v_pos_1 + nchar(s_char)
			
			if (v_pos_2[length(v_pos_2)] > nchar(s_formula)) {
				if (length(v_pos_2) == 1) {
					return(v_pos_1)
				}
				else {
					v_pos_2 <- v_pos_2[-length(v_pos_2)]
					v_char <- eval(parse(text = paste0("c(", paste(paste0("substr(s_formula, ", v_pos_2, ", ", v_pos_2, ")"), collapse = ", "), ")")))
					v_pos_3 <- which(!v_char %in% 0:9)					
					return(v_pos_1[c(v_pos_3, length(v_pos_1))])
				}
			}
			else {
				v_char <- eval(parse(text = paste0("c(", paste(paste0("substr(s_formula, ", v_pos_2, ", ", v_pos_2, ")"), collapse = ", "), ")")))
				v_pos_3 <- which(!v_char %in% 0:9)
				
				if (length(v_pos_3) == 0) {
					return(NULL)
				}
				else {
					return(v_pos_1[v_pos_3])
				}
			}
		} 
	})
	
	v_pos <- which(lengths(l_pos) == 0)
	
	if (length(v_pos) > 0) {
		s_e_message <- paste0("Formula with missing condition", ifelse(length(v_pos) > 1, "s", ""), ": ", paste(paste0("c", v_pos), collapse = ", "))  
	}
	else {
		s_formula_modif <- gsub(paste(paste0("c", 1:length(o_sdata_cond$var_name)), collapse = "|"), "_", s_formula)
		v_char <- paste0("_", 0:9)
		
		if (length(o_sdata_cond$var_name) < 9) {
			v_char <- c(v_char, paste0("c", (length(o_sdata_cond$var_name) + 1):9))
		}
		
		if (length(grep(paste(v_char, collapse = "|"), s_formula_modif)) > 0) {
			s_e_message <- "Formula with unknown condition(s)"
		}
		else {
			v_value <- as.vector(unlist(lapply(1:length(o_sdata_cond$value), function(i) {
				if (o_sdata_cond$var_type[i] == "quant") {
					return(o_sdata_cond$value[[i]])
				}
				else {
					return(paste0("c(", paste(paste0("\"", o_sdata_cond$value[[i]], "\""), collapse = ", "), ")"))
				}
			})))
			
			v_start <- ifelse(o_sdata_cond$var_rel == "!%in%", "!", "")
			v_rel <- ifelse(o_sdata_cond$var_rel == "!%in%", "%in%", ifelse(o_sdata_cond$var_rel == "=", "==", o_sdata_cond$var_rel))
			v_char <- paste0(v_start, "df_all$", o_sdata_cond$var_name, " ", v_rel, " ", v_value) 
			
			if (length(v_char) == 1) {
				s_char <- paste0("which(", v_char, ")")
			}
			else {
				v_cond <- eval(parse(text = paste0("c(", paste(paste0("rep(\"", paste0("c", 1:length(o_sdata_cond$var_name)), "\", ", lengths(l_pos), ")"), collapse = ", "), ")")))
				df_pos <- data.frame("pos" = as.vector(unlist(l_pos)), "cond" = v_cond)
				df_add <- data.frame("cond" = paste0("c", 1:length(o_sdata_cond$var_name)), "replace" = v_char)
				df_pos <- merge(df_pos, df_add, all.x = T, by = "cond")
				df_pos <- df_pos[order(df_pos$pos),]
				v_split <- as.vector(unlist(strsplit(s_formula_modif, split = "_")))
				v_split[1:nrow(df_pos)] <- paste0(v_split[1:nrow(df_pos)], as.vector(df_pos$replace))
				s_char <- paste0("which(", paste(v_split, collapse = ""), ")")
			}
			
			v_row <- tryCatch({suppressWarnings(eval(parse(text = s_char)))}, error = function(e) FALSE)
			
			if (!is.numeric(v_row) | length(which(c(-Inf, Inf, NA) %in% unique(v_row))) > 0) {
				s_e_message <- "Error with the formula used to combine conditions"
				v_row <- c()
			}
			else {
				if (length(v_row) == 0) {
					s_e_message <- "No rows match with this combination of conditions"
					v_row <- c()
				}
				else {
					df_sub <- df_all[v_row,]
				}
			}
		}
	}
	
	return(list(v_row, df_sub, s_e_message))
}


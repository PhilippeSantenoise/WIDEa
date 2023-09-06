#' Concatenation of variables

#' @description
#' `f_create_concat_variable` is used to create the concatenated variable (only used
#' for the normal/ir data type).

#' @param df_1 are data saved in the e_data environment (e_data$all or e_data$sub).
#' @param v_var is the vector of variables.

#' @encoding UTF-8

f_create_concat_variable <- function (df_1, v_var) {
	v_row <- eval(parse(text = paste0("unique(c(", paste(paste0("which(is.na(as.vector(df_1$", v_var, ")))"), collapse = ", "), "))")))
	v_out <- eval(parse(text = paste0("as.vector(paste(", paste(paste0("as.vector(df_1$", v_var, ")"), collapse = ", "), ", sep = \" \"))")))
	if (length(v_row) > 0) {v_out[v_row] <- NA}
	return(v_out)
}

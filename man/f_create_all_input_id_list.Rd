% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_inventories.r
\encoding{UTF-8}
\name{f_create_all_input_id_list}
\alias{f_create_all_input_id_list}
\alias{f_create_match_id_data}
\alias{f_create_input_value_list}
\alias{f_create_input_sp_num_list}
\alias{f_create_input_maxitem_list}
\alias{f_update_input_maxitem_list}
\alias{f_create_varsel_input_id_vector}
\alias{f_create_t3_input_id_vector}
\alias{f_create_input_id_vector}
\title{List of functions associated to UI's inputs}
\usage{
f_create_all_input_id_list(s_by = "section")

f_create_match_id_data(b_concat1 = F, b_concat2 = F)

f_create_input_value_list(
  s_type = "selectize",
  df_all = NULL,
  v_concat = rep(0, 2),
  i_sub_data = 0,
  i_only_value = 0,
  s_sub_var_name = NULL,
  s_sub_var_type = "qualit"
)

f_create_input_sp_num_list()

f_create_input_maxitem_list(
  s_data = "normal",
  s_graph = "plot_2d",
  s_model = "none",
  b_concat1 = F
)

f_update_input_maxitem_list(
  o_selectize_input,
  l_id_status = NULL,
  df_id_status = NULL
)

f_create_varsel_input_id_vector(
  s_data = "normal",
  s_graph = "plot_2d",
  s_model = "none",
  b_flag = F
)

f_create_t3_input_id_vector(
  s_data = "normal",
  s_graph = "plot_2d",
  s_model = "none"
)

f_create_input_id_vector(
  s_data = "normal",
  s_graph = "plot_2d",
  s_model = "none",
  b_load2 = F,
  b_load3 = F,
  b_display = F,
  i_concat = 0
)
}
\arguments{
\item{s_by}{is a string value used to order UI's input inventory (two values:
"section" as default, "type").}

\item{b_concat1}{is a boolean value associated to concat1 check box.}

\item{b_concat2}{is a boolean value associated to concat2 check box.}

\item{s_type}{is a string value associated to the input type (four values:
"selectize" as default, "radio_button", "check_box_group", "numeric").}

\item{df_all}{is a data frame saved in e_data environment (e_data$all).}

\item{v_concat}{is a vector of two binary values (given by o_cond reactive
value: concat1, concat2) used to remove variables named ".concat1." and
".concat2." (respectively) from e_data$all.}

\item{i_sub_data}{is a binary value associated to sub-data (e_data$sub). The
value is equals to 1 if sub-data are created.}

\item{i_only_value}{is an integer value used to return specific values of
selectize inputs (values: 0, 1 or 2). O returns a list of all selectize input
choices. 1 only returns the vector of variable names available from df_all
(e_data environment). 2 only returns the vector of unique ordered values
associated to the variable selected from the "vname" selectize input (subdata
creation section).}

\item{s_sub_var_name}{is a string value returned by the "vname" selectize input
(NULL as default value).}

\item{s_sub_var_type}{is a string value returned by the "vtype" radio button
input (2 values: "qualit", "quanti").}

\item{s_data}{is the data type (three values: "normal", "temporal", "ir").}

\item{s_graph}{is the graph type (six values: "plot_2d", "plot_3d", "histplot",
"corplot", "boxplot", "barplot").}

\item{s_model}{is the model procedure (three values: "none" as default, "calib",
"valid"). This option is only available if s_data is "normal" and s_graph is
"plot_2d".}

\item{o_selectize_input}{is a reactive value with max item information for
selectize inputs (ID: "var_x", "var_y").}

\item{l_id_status}{is a list of input IDs with status information obtained by the
reactive value o_on_off.}

\item{df_id_status}{is a data frame of input IDs with status information obtained
by the reactive value o_input_status.}

\item{b_flag}{is a boolean value associated to e_data$flag (T if flag data exist
in e_data environment, F else).}

\item{b_load2}{is a boolean value associated to the load2 button (clicked = T;
not clicked = F).}

\item{b_load3}{is a boolean value associated to the load3 button (clicked = T;
not clicked = F).}

\item{b_display}{is a boolean value associated to the display button (clicked =
T; not clicked = F).}

\item{i_concat}{is an integer associated to o_cond$concat1 reactive value.}
}
\description{
\code{f_create_all_input_id_list} is used to create a list with all UI's input IDs
ordered by panel section or type (selectize, numeric, text, action button, radio
button, check box).
\cr\code{f_create_match_id_data} is used to create a data frame with matching IDs
between UI's input and o_parameter reactive value. Data are used to update inputs
in the variable selection section (left panel).
\cr\code{f_create_input_value_list} allows to create a list with (selectize, radio
button, check box group) input default values.
\cr\code{f_create_input_sp_num_list} allows to create a list for radio button inputs
with number used as a special enabling/desabling.
\cr\code{f_create_input_maxitem_list} is used to create a list for selectize input IDs
("var_x", "var_y") with new maximum item values to be replaced.
\cr\code{f_update_input_maxitem_list} is used to update lists of selectize input ID
with the maximum item value (saved in o_selectize_input reactive value).
\cr\code{f_create_varsel_input_id_vector} is used to create a vector of input IDs
associated to the variable selection section (left panel). The input IDs are used
to save the status (enabled/disabled) once the display button is clicked.
\cr\code{f_create_t3_input_id_vector} is used to create a vector of input IDs
associated to the Statistics tab.
\cr\code{f_create_input_id_vector} allows to create a vector of input IDs enabled when
a data/graph type and/or model procedure is selected.
}

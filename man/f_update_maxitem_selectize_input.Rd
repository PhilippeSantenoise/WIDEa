% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_ui_inputs.r
\encoding{UTF-8}
\name{f_update_maxitem_selectize_input}
\alias{f_update_maxitem_selectize_input}
\alias{f_update_selectize_input}
\alias{f_update_text_field_input}
\alias{f_update_radio_button_input}
\alias{f_update_check_box_input}
\alias{f_update_option_selectize_input}
\alias{f_update_inputs}
\title{List of functions used to update UI's inputs}
\usage{
f_update_maxitem_selectize_input(l_id_max_item, v_choices)

f_update_selectize_input(s_id, v_choices, v_selected, i_max_item = NULL)

f_update_text_field_input(s_id, s_value, b_remove_modal = F)

f_update_radio_button_input(s_id, s_selected)

f_update_check_box_input(s_id, b_value)

f_update_option_selectize_input(
  s_data_type,
  s_plot_type,
  s_mode = "marker",
  l_current_option,
  b_group = F
)

f_update_inputs(
  l_id_value,
  df_all = NULL,
  l_selectize_option = list(concat = rep(0, 2), sub_data = 0),
  i_delay = 0
)
}
\arguments{
\item{l_id_max_item}{is the list of ID (selectize) inputs with the maximum item
value.}

\item{v_choices}{is the vector of choices for selectize inputs.}

\item{s_id}{is the UI's input ID.}

\item{v_selected}{is the vector of selected values for selectize inputs.}

\item{i_max_item}{is the maximum item value for selectize inputs.}

\item{s_value}{is the value for text inputs.}

\item{b_remove_modal}{is a boolean value used to remove an existing modal window
(used with expand buttons).}

\item{s_selected}{is the value for radio button inputs.}

\item{b_value}{is the value for check box inputs.}

\item{s_data_type}{is the data type (3 values: "normal", "temporal", "ir").}

\item{s_plot_type}{is the plot type (5 values: "plot", "boxplot", "histplot",
"barplot", "corplot").}

\item{s_mode}{is the plotly graph mode (3 values: "marker", "line",
"line_marker").}

\item{l_current_option}{is a list with information ("choices" and "selected") on
"edit_option" selectize input. "choices"/"selected" return the vector of choices
and the selected value of the selectize input respectively.}

\item{b_group}{is a boolean value associated to the value of the Group radio
button (left panel, variable selection).}

\item{l_id_value}{is a list including UI's input IDs and values.}

\item{df_all}{are data saved in the e_data environment.}

\item{l_selectize_option}{is a list including binary values related to the
concatenation check boxes (1: checked; 0 else) and the sub-data create button (1:
clicked; 0 else).}

\item{i_delay}{is used to add a delay (in ms) and update commands (no delay as
default value).}
}
\description{
\code{f_update_maxitem_selectize_input} is used to create command lines to update the
maxitem option for selectize inputs.
\cr\code{f_update_selectize_input} is used to create command lines to update a
selectize input.
\cr\code{f_update_text_field_input} is used to create command lines to update a text
input.
\cr\code{f_update_radio_button_input} is used to create command lines to update a
radio button input.
\cr\code{f_update_check_box_input} is used to create command lines to update a check
box input.
\cr\code{f_update_option_selectize_input} is used to create command lines to update
option (label, color/opacity, point type/size) selectize input.
\cr\code{f_update_inputs} is used to aggregate all command lines to update the vector
of specified inputs.
}

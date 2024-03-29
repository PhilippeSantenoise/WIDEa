% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_current_flag.r
\encoding{UTF-8}
\name{f_add_current_flag}
\alias{f_add_current_flag}
\title{Creating a list related to flag data information}
\usage{
f_add_current_flag(
  s_data_type = "normal",
  s_dim_num = "2d",
  l_temp_coord = NULL,
  o_parameter,
  o_plot = NULL,
  o_click_ev,
  i_flag_num,
  l_click_info = NULL
)
}
\arguments{
\item{s_data_type}{is the data type (3 values: "normal", "temporal", "ir").}

\item{s_dim_num}{is the dimension number for the normal data type (2 values:
"2d", "3d").}

\item{l_temp_coord}{is a the list of (X, Y) coordinates used to draw intervals
(created from the \code{f_create_current_flag_data} function) for the temporal data
type (default = NULL).}

\item{o_parameter}{is a reactive value including parameters associated to the
left panel (sections after data loading) and top panels.}

\item{o_plot}{is a reactive value including main plotly data information.}

\item{o_click_ev}{are data associated to a click event ("plotly_click" with the
\code{event_data} function).}

\item{i_flag_num}{is an integer corresponding to the current flag number.}

\item{l_click_info}{is the list of supplementary informations associated to a
click event (created by the \code{f_create_click_info} function). This input is only
used for the ir data type and allows to identify the spectrum.}
}
\description{
\code{f_add_current_flag} allows to create a list of data correponding to the current
flag. This list is only used to update the graph ("addTraces" with the
\code{plotlyProxyInvoke} function).
}

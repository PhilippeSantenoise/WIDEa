% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_graph_option.r
\encoding{UTF-8}
\name{f_create_default_graph_opt_list}
\alias{f_create_default_graph_opt_list}
\alias{f_create_graph_opt_list}
\title{List of functions used to build (color, opacity and point type/size) options
applied to the main plotly graph}
\usage{
f_create_default_graph_opt_list(
  s_data_type = "normal",
  df_all = data.frame(),
  o_parameter
)

f_create_graph_opt_list(o_name_option, b_quant = F)
}
\arguments{
\item{s_data_type}{is the data type (3 values: "normal", "temporal", "ir").}

\item{df_all}{are data created from the \code{f_prepare_data} function (process = 1).
This input only concern the normal/ir data type.}

\item{o_parameter}{is a reactive value including parameters associated to the
left panel (sections after data loading) and top panels.}

\item{o_name_option}{is a reactive value with graph option (color, opacity, point
type/size, sorting) inventory (default, custom).}

\item{b_quant}{is a boolean value associated to the Group variable (only for
normal/ir data type: plot). The value is True if the Group variable is
quantitative, False else.}
}
\description{
\code{f_create_default_graph_opt_list} is used to create the list of graph default
color, opacity, point type and size.
\cr\code{f_create_graph_opt_list} is used to create the list of graph (custom) color,
opacity, point type and size (default is applied if no custom).
}

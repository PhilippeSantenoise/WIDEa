% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_tp1_inputs.r
\encoding{UTF-8}
\name{f_check_tp1_inputs}
\alias{f_check_tp1_inputs}
\title{Checking process on Graphic tab inputs (tp1)}
\usage{
f_check_tp1_inputs(l_opt_name, o_plot = NULL, o_parameter = NULL)
}
\arguments{
\item{l_opt_name}{is the list of tp1 input IDs ("dec_num", "bw", "fraction").}

\item{o_plot}{is a reactive value including data information of the main plotly
graph.}

\item{o_parameter}{is a reactive value including parameters associated to the
left panel (sections after data loading) and top panels.}
}
\description{
\code{f_check_tp1_inputs} is used to check tp1 input values (decimal number, histplot
bin width, Y-scale border fraction) and returns an error/warning message if one
of these input values are not correctly informed.
}

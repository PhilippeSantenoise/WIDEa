% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_trsf_variables.r
\encoding{UTF-8}
\name{f_check_trsf_variables}
\alias{f_check_trsf_variables}
\title{Checking process: fields used to apply (f, g, h) functions on (X, Y, Z) variables
are filled in without error ?}
\usage{
f_check_trsf_variables(s_data_type = "normal", df_all, o_parameter)
}
\arguments{
\item{s_data_type}{is the data type (2 values: "normal", "temporal").}

\item{df_all}{are data created by the \code{f_prepare_data} function (process 1).}

\item{o_parameter}{is a reactive value including parameters associated to the
left panel (sections after data loading) and top panels.}
}
\description{
\code{f_check_trsf_variables} execute a checking process in several steps:
\cr(1) Checking (X, Y, Z) variables codes filled in the (f, g, h) text fields
(variable code specified by the expand buttons);
\cr(2) Checking the calculation of (f, g, h) functions.
\cr\cr The output is a list including all transformed variables or an error
message if a problem occured with the checking process.
}

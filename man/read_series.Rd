% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_series.R
\name{read_series}
\alias{read_series}
\title{Read sensor data from plain-text files}
\usage{
read_series(filename, timestamp_column, sensor_column,
            data_row, header_row, col_names = NULL, ...)
}
\arguments{
\item{filename}{f}

\item{timestamp_column}{t}

\item{sensor_column}{s}

\item{data_row}{d}

\item{header_row}{h}

\item{col_names}{c}
}
\value{
A \code{data.frame}
}
\description{
reading data
}
\author{
Marko Smiljanic
}

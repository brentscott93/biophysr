% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_events.R
\name{measure_events}
\alias{measure_events}
\title{Measure single molecule events detected by Hidden Markov Model}
\usage{
measure_events(data, conversion, run_mean_data)
}
\arguments{
\item{conversion}{}
}
\value{
a tibble with event duration and step size for each event
}
\description{
Measure single molecule events detected by Hidden Markov Model
}
\examples{

measure_events(data = depmix_posterior,
conversion = raw_trace_length/running_calcs_length,
 run_mean = running_mean_object_given_2_hmm)
}

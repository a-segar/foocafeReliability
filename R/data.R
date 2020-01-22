#' Monthly number of failures for 47 supercomputer components
#'
#' A dataset containing the monthly number of failures of the Los Alamos
#'  National Laboratory Blue Mountain supercomputer components (shared memory
#'  processors or SMPs).  The supercomputer consists of 47 "identical" SMPs this
#'  dataset contains their monthly number of failures for the first month of
#'  operation.
#'
#' @format
#' \describe{A data frame with 47 rows and two columns
#'  \item{shared_memory_processor_id}{}
#'  \item{failure_count}{}
#' }
#'
"supercomputer_failures"


#' Failure times (in projection hours) for 31 LCD projector lamps
#'
#' In business and educational settings, computer presentations use liquid
#'  crystal display (LCD) projectors. The most common failure mode of these
#'  projectors is the failure of the lamp. Many manufacturers include the
#'  "expected" lamp life in their technical specification documents, and one
#'  manufacturer claims that users can expect 1,500 hours of projection time
#'  from each lamp used under "normal operating conditions". To test this claim,
#'  a large private university placed identical lamps in three projector models
#'  for a total of 31 projectors. The university staff recorded the number of
#'  projection hours (as measured by the projector) when each lamp burned out.
#'  This dataset are the results from that experiment.
#'
#' @format
#' \describe{A data frame with 31 rows and two columns
#'  \item{lcd_model}{}
#'  \item{projection_hours}{}
#' }
"lcd_projector_failures"


#' Failure times (in operating hours) for bearings in 66 prowler attack
#' aircraft.
#'
#' Prowler bearing failure times
#'
#' @format
#' \describe{A data frame with 66 rows and three columns
#'  \item{prowler_aircraft_id}{}
#'  \item{operating_hours}{}
#'  \item{right_censored}{}
#' }
"prowler_bearing_failures"

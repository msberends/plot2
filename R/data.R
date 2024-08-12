# ===================================================================== #
#  An R package for Fast 'ggplot2' Plotting:                            #
#  https://github.com/msberends/plot2                                   #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Example Data Set with Admitted Patients
#'
#' An auto-generated data set containing fictitious patients admitted to hospitals.
#' @format A [tibble]/[data.frame] with `r format(nrow(admitted_patients), big.mark = ",")` observations and `r ncol(admitted_patients)` variables:
#' - `date`\cr date of hospital admission
#' - `patient_id`\cr ID of the patient (fictitious)
#' - `gender`\cr gender of the patient
#' - `age`\cr age of the patient
#' - `age_group`\cr age group of the age of the patient, generated with [AMR::age_groups()]
#' - `hospital`\cr ID of the hospital, from A to D
#' - `ward`\cr type of ward, either ICU or Non-ICU
"admitted_patients"

#' Example Geography Data Set: the Netherlands
#'
#' A data set containing the geometies of the twelve provinces of the Netherlands, according to Statistics Netherlands (2021).
#' @format A [data.frame] with `r format(nrow(netherlands), big.mark = ",")` observations and `r ncol(netherlands)` variables:
#' - `province`\cr name of the Dutch province
#' - `area_km2`\cr area in square kilometres
#' - `geometry`\cr geometry of the province, of class `r paste0(class(netherlands$geometry), collapse = "/")`
"netherlands"

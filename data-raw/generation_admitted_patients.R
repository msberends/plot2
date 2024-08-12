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

library(dplyr)
admitted_patients <- AMR::example_isolates |>
  group_by(patient_id) |>
  filter(n() < 3) |>
  ungroup() |>
  transmute(date,
            patient_id,
            gender,
            age,
            age_group = AMR::age_groups(age),
            hospital = hospital_id,
            ward = ifelse(ward_icu, "ICU", "Non-ICU")) |>
  slice_sample(n = 250)

ind <- double(nrow(admitted_patients))
j <- 1
for (pat in unique(admitted_patients$patient_id)) {
  ind[which(admitted_patients$patient_id == pat)] <- j
  j <- j + 1
}
admitted_patients$patient_id <- ind
admitted_patients <- admitted_patients |> 
  arrange(date, patient_id)

# for the vignette, make 2014 wide in ggplot2 syntax
admitted_patients[which(format(admitted_patients$date, "%Y") == "2014" &
                          admitted_patients$ward == "ICU"), "data"] <- as.Date("2013-02-25")

usethis::use_data(admitted_patients, internal = FALSE, overwrite = TRUE, version = 2, compress = "xz")

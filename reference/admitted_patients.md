# Example Data Set with Admitted Patients

An auto-generated data set containing fictitious patients admitted to
hospitals.

## Usage

``` r
admitted_patients
```

## Format

A
[tibble](https://tibble.tidyverse.org/reference/tibble.html)/[data.frame](https://rdrr.io/r/base/data.frame.html)
with 250 observations and 7 variables:

- `date`  
  date of hospital admission

- `patient_id`  
  ID of the patient (fictitious)

- `gender`  
  gender of the patient

- `age`  
  age of the patient

- `age_group`  
  age group of the age of the patient, generated with
  [`AMR::age_groups()`](https://amr-for-r.org/reference/age_groups.html)

- `hospital`  
  ID of the hospital, from A to D

- `ward`  
  type of ward, either ICU or Non-ICU

## supercomputer_failures

supercomputer_failures <- tibble::tibble(
  shared_memory_processor_id = 1:47,
  failure_count = c(1,5,1,4,2,3,1,3,6,4,4,4,
                            2,3,2,2,4,5,5,2,5,3,2,2,
                            3,1,1,2,5,1,4,1,1,1,2,1,
                            3,2,5,3,5,2,5,1,1,5,2)
)

usethis::use_data(supercomputer_failures, overwrite = TRUE)


## lcd_projector_failures

lcd_projector_failures <- tibble::tibble(
  lcd_model = c(1,1,1,1,1,2,2,2,1,2,1,3,3,2,2,3,
                3,2,1,2,1,3,1,2,1,2,1,1,2,2,2),
  projection_hours = c(387,182,244,600,627,332,418,300,798,584,660,39,274,174,50,34,
                       1895,158,974,345,1755,1752,473,81,954,1407,230,464,380,131,1205)
)

usethis::use_data(lcd_projector_failures, overwrite = TRUE)


## prowler_bearing_failures

prowler_bearing_failures <- tibble::tibble(
  prower_aircraft_id = 1:66,
  operating_hours = c(1085,1500,1390,152,971,966,997,887,977,1022,2087,646,820,897,810,80,1167,711,1203,85,1070,719,
                     1795,1890,1145,1380,61,1165,437,1152,159,3428,555,727,2294,663,1427,951,767,546,736,917,2871,1231,
                     100,1628,759,246,861,462,1079,1199,424,763,1297,2238,1388,1153,2892,2153,853,911,2181,1042,799,750),
  right_censored = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,
                     TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                     TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE)
)

usethis::use_data(prowler_bearing_failures, overwrite = TRUE)


## lubricant_bearing_failures

lubricant_bearing_failures <- tibble::tibble(
  failure_times = c(130.3,135.2,152.4,161.7,74.0,155.0,141.2,167.8,137.2,110.1,
                    243.6,242.1,239.0,202.1,190.5,159.8,275.5,192.4,183.8,203.7,
                    71.3,137.8,101.2,75.3,164.5,113.9,54.7,224.0,171.7,226.5,
                    183.4,276.9,210.3,262.8,115.3,242.2,293.5,221.3,108.9,191.5,
                    132.9,74.0,169.2,126.4,79.9,139.7,139.0,104.3,100.2,108.2,
                    117.9,168.4,153.7,174.7,65.8,158.4,115.7,133.4,171.4,203.0,
                    208.5,135.2,217.7,158.5,215.7,136.6,223.3,188.2,190.3,159.8,
                    167.5,164.6,215.6,118.3,151.1,166.5,162.6,215.6,171.6,207.6,
                    94.2,113.0,180.2,90.4,118.0,101.8,97.8,104.6,154.9,181.3,
                    138.0,134.4,200.8,202.7,181.6,126.9,80.0,152.6,173.1,169.5),
  tester = rep(seq(1,10),each=10)
)

usethis::use_data(lubricant_bearing_failures, overwrite = TRUE)

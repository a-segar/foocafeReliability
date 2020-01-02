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


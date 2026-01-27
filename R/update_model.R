box::use(
  R / common / model_fitting[fit_handball_model],
  R / common / get_model_results[generate_model_results]
)

end_date <- Sys.Date()
sex <- "male"

fit_handball_model(
  sex = sex,
  # model_name = "2d_student_t_matchup_effects.stan",
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000,
  end_date = end_date
)

generate_model_results("male", end_date = end_date)

context("To test the APACHE Glasgow Coma Scale calculator")

test_that("To see if the gen_apache_gcs calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 2, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 3, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 3, replace = F),
                      "GCS - total" = c(10, NA, 15))
      
  gen_apache_gcs(dt = ddata, window = c(0,24))
  expect_equal(c(5, 0, NA), ddata[, apache_gcs])
  
  # ddata <- data.table("time" = c(5, 6, 22, 24, 30, 5, 8, 11, 12),
  #                     "site" = sample(c("YY", "ZY"), 9, replace = T),
  #                     "episode_id" = sample(seq(116, 130, 1), 9, replace = F),
  #                     "Heart rate" = c(25, 50, 59, 70, 70, 125, 155, 195 , NA),
  #                     "PaO2 - ABG" = c(),
  #                     "PaCO2 - ABG" = c(),
  #                     "PaO2/FiO2 ratio" = c())
  # 
  # expect_error(gen_grad(dt = ddata))

})

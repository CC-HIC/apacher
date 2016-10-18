context("To test the APACHE haematocrit calculator")

test_that("To see if the gen_apache_ht calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 7, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 8, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 8, replace = F),
                      "d_ht" = c( 0.19, 0.25, 0.35, 0.47, 0.58, 0.8, NA, 0.6))
      
  gen_apache_ht(dt = ddata, window = c(0,24))
  expect_equal(c(4, 2, 0, 1, 2, 4, 0, NA), ddata[, apache_ht])
  
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

context("To test the APACHE Potassium calculator")

test_that("To see if the gen_apache_K calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 7, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 8, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 8, replace = F),
                      "Potassium" =   c(7.5, 6.5, 5.8, 4, 3.2, 2.7, 2, 125))
      
  gen_apache_K(dt = ddata, window = c(0, 24))
  
  expect_equal(c(4, 3, 1, 0, 1, 2, 4, NA), ddata[, apache_K])
  
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

context("To test the APACHE Sodium calculator")

test_that("To see if the gen_apache_Na calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 8, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 9, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 9, replace = F),
                      "Sodium" =   c(190, 170, 156, 153, 135, 125, 115, 108, 125))
      
  gen_apache_Na(dt = ddata, window = c(0, 24))
  
  expect_equal(c(4, 3, 2, 1, 0, 2, 3, 4, NA), ddata[, apache_Na])
  
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

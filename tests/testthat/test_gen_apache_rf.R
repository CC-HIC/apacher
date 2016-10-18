context("To test the APACHE respiratory failure calculator")

test_that("To see if the gen_apache_rf calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(5, 6, 22, 24, 10, 5, 8, 6, 6, 30),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 130, 1), 10, replace = F),
                      "PaO2 - ABG" =                  c(7.5,   6,   9,  12, 7.5,   6,   9,  12, 12,  12),
                      "grad_rf" =                     c( 27,  17,  48,  70,  70,   8,   8,   8, NA,  27),
                      "PaO2/FiO2 ratio" =             c( 10,  10,  10,  10, 100, 100, 100, 100, NA, 100),
                      "Inspired fraction of oxygen" = c(0.8, 0.8, 0.8, 0.8, 0.3, 0.3, 0.3, 0.3, NA,   1))
      
  gen_apache_rf(dt = ddata, window = c(0, 24))
  
  expect_equal(c(2, 0, 3, 4, 3, 4, 1, 0, 0, NA), ddata[, apache_rf])
  
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

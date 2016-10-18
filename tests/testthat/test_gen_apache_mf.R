context("To test the APACHE metabolic failure calculator")

test_that("To see if the gen_apache_mf calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 15, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 16, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 16, replace = F),
                      "pH - ABG / VBG" =   c(7.8, 7.65, 7.55, 7.35, 7.28, 7.17, 6.9, NA, NA, NA, NA, NA, NA, NA, NA, 7.5),
                      "HCO3 - ABG / VBG" = c( NA,   NA,   NA,   NA,   NA,   NA,  NA, 54, 43, 35, 25, 19, 17, 14, NA,  45))
      
  gen_apache_mf(dt = ddata, window = c(0, 24))
  
  expect_equal(c(4, 3, 1, 0, 2, 3, 4, 4, 3, 1, 0, 2, 3, 4, 0, NA), ddata[, apache_mf])
  
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

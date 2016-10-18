context("To test the generation of Alveolar to arterial Gradient calculator")

test_that("To see if the gen_grad calculates the correct gradient",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(5, 6, 22, 24, 30, 5, 8, 6, 6),
                      "site" = sample(c("YY", "ZY"), 9, replace = T),
                      "episode_id" = sample(seq(116, 130, 1), 9, replace = F),
                      "PaO2 - ABG" = c(50, 50, 21, 40, 50, 50, NA, 50, 50),
                      "PaCO2 - ABG" = c(5.5, 5.5, 5.5, 8, 8, 8, 8, 8, NA),
                      "PaO2/FiO2 ratio" = c(100, 100, 100, 50, 50, 50, 50, 5, 50),
                      "Inspired fraction of oxygen" = c(50, 0.5, 0.21, 0.8, 1, NA, NA, NA, 1))
      
  gen_grad(dt = ddata)
  
  expect_equal(c(-8.6, -8.6, -6.802 , 27.04, 35.8, 35.8, NA, NA, NA), ddata[, grad_rf])
  
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

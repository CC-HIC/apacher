context("To test the APACHE Acute Kidney Injury calculator")

test_that("To see if the gen_apache_aki calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 12, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 13, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 13, replace = F),
                      "Creatinine" =                         c(53, 110, 135, 175, 310, 53, 110, 135, 175, 310, NA, NA, 145),
                      "Chronic renal replacement therapy" =  c( 0,   0,   0,   0,   0,  1,   1,   1,   1,   1, NA,  0,   1))
      
  gen_apache_aki(dt = ddata, crrt = "Chronic renal replacement therapy", window = c(0, 24))
  
  expect_equal(c(4, 0, 4, 6, 8, 2, 0, 2, 3, 4, 0, 0, NA), ddata[, apache_aki])
  
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

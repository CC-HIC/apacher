context("To test the haemoglobin to haematocrit calculator")

test_that("To see if the gen_haemo calculates the correct haematocrit",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(sample(seq(0, 24, 1), 3, replace = T), 35),
                      "site" = sample(c("YY", "ZY"), 4, replace = T),
                      "episode_id" = sample(seq(116, 150, 1), 4, replace = F),
                      "Haemoglobin" =         c(  150,   NA, 150,  NA),
                      "Haemoglobin ABG/VBG" = c(  150,  150, 150,  NA),
                      "mcch" =                c(  340,  340,  NA, 340))
      
  gen_haemo(dt = ddata, mcch = "mcch")
  expect_equal(c(44, 44, 45, NA), ddata[, d_ht])
  
  gen_haemo(dt = ddata, mcch = NULL)
  expect_equal(c(45, 45, 45, NA), ddata[, d_ht])
  
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

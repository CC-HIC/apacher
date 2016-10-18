context("To test the gen_apache_map function")

test_that("To see if the gen_apache_map give the right APACHE score for the right temperature",{
  ddata <- NULL

  
  # Various
  ddata <- data.table("time" = c(5, 6, 22, 24, 30, 5, 8, 11, 12, 6, 8, 10, 9),
                      "site" = sample(c("YY", "ZY"), 13, replace = T),
                      "episode_id" = sample(seq(116, 130, 1), 13, replace = F),
                      "Mean arterial blood pressure - Art BPMean arterial blood pressure" =           c(35, 50,  75, 125,  55, 145, 180, NA ,  NA, NA,  NA,  NA, 110),
                      "Mean arterial blood pressure - NBPMean arterial blood pressure" =              c(65, 85, 110, 140,  NA,  65,  55, 120,  NA, NA,  NA,  NA,  NA),
                      "Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure" =   c(75, 75,  85,  85, 110, 110,  75,  85, 135, NA, 167, 167,  NA),
                      "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure" = c(35, 35,  25,  25,  40,  40,  55,  55,  68, NA,  NA,  NA,  NA),
                      "Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure" =      c(75, 75,  85,  85, 110, 110,  75,  85, 135, 87,  NA, 140,  NA),
                      "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure" =    c(35, 35,  25,  25,  40,  40,  55,  55,  68, 44,  32,  32,  NA))
  
  ddata <- gen_apache_map(dt = ddata, window = c(0,24))
  
  expect_equal(c(4, 2, 0, 2, NA, 3, 4, 2, 0, 2, 0, 2, 2), ddata[, apache_map])
  
  
})

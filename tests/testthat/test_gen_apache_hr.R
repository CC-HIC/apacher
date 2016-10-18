context("To test the apache Heart Rate calculator")

test_that("To see if the gen_apache_hr give the right APACHE score for the right Heart Rate",{
  # ddata <- NULL
  # hr <- "Heart rate"
  # Various
  ddata <- data.table("time" = c(5, 6, 22, 24, 30, 5, 8, 11, 12),
                      "site" = sample(c("YY", "ZY"), 9, replace = T),
                      "episode_id" = sample(seq(116, 130, 1), 9, replace = F),
                      "Heart rate" = c(25, 50, 59, 70, 70, 125, 155, 195 , NA))
      
  gen_apache_hr(dt = ddata, window = c(0,24))
  
  expect_equal(c(4, 3, 2, 0, NA, 2, 3, 4, 0), ddata[, apache_hr])
  

})

context("To test the gen_apache_temp function")

test_that("To see if the gen_apache_temp give the right APACHE score for the right temperature",{
  ddata <- NULL

  
  # Various
  ddata <- data.table("time" = c(5, 6, 22, 24, 30, 5, 8, 98, 12, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "Temperature - Central" = c(34, 31, 44, 39.5, NA, 24, NA, 38.9, NA, 34.8),
                      "Temperature - Non-central" = c(38, 29, 38, NA, NA, 35, 32.6, 37.5, NA, NA))
  
  ddata <- gen_apache_temp(dt = ddata, window = c(0,24))
  
  expect_equal(c(1, 3, 4, 3, NA, 4, 2, NA, 0, 1), ddata[, apache_temp])
  
  
})

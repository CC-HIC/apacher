context("To test the gen_apache_rr function")

test_that("To see if the gen_apache_hr give the right APACHE score for the right respiratory rate",{
  ddata <- NULL

  
  # Various
  ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "q_rr" = c(55, 32, 12, 32, NA, 8, 10, 24, 12, NA))
  
  ddata <- gen_apache_rr(dt = ddata, window = c(0,24))
  
  expect_equal(c(4, NA, 0, 1, NA, 3, 2, NA, NA, 0), ddata[, apache_rr])
  
  
})

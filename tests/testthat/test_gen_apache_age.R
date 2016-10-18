context("To test the apache age calculator")

test_that("To see if the gen_apache_age give the right APACHE score for the right Age",{
  ddata <- NULL

  
  # APACHE HR score == 0 
  ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(32, 2), 24))
  gen_apache_age(dt = ddata, window = c(0,24))
  expect_equal(c(0, NA, 0), ddata[, apache_age])
  
  # APACHE HR score == 2 
  ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(45, 2), 54))
  gen_apache_age(dt = ddata, window = c(0,24))
  expect_equal(c(2, NA, 2), ddata[, apache_age])
  
  # APACHE HR score == 3 
  ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(56, 2), 62))
  gen_apache_age(dt = ddata, window = c(0,24))
  expect_equal(c(3, NA, 3), ddata[, apache_age])
  
  # APACHE HR score == 5 
  ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(72, 2), 71))
  gen_apache_age(dt = ddata, window = c(0,24))
  expect_equal(c(5, NA, 5), ddata[, apache_age])
  
  
  # APACHE HR score == 6 
  ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(84, 2), 102))
  gen_apache_age(dt = ddata, window = c(0,24))
  expect_equal(c(6, NA, 6), ddata[, apache_age])
  
  # Various
  ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "age" = c(102, 52, 72, 63, 55, 52, 43, 55, 26, 22))
  
  gen_apache_age(dt = ddata, window = c(0,24))
  
  expect_equal(c(6, NA, 5, 3, NA, 2, 0, NA, NA, 0), ddata[, apache_age])
  
  
})

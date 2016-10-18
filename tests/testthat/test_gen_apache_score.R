context("To test the APACHE Score function")

test_that("To see if the gen_apache_score calculates the correct APACHE score",{
  # ddata <- NULL
  # hr <- "Heart rate"
  options(warn= -1)
  # Apache Age
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_age" =   6)
  gen_apache_score(dt = ddata)
  expect_equal(6, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_age" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
 
  # Apache Chronic
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_chronic" =   3)
  gen_apache_score(dt = ddata)
  expect_equal(3, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_chronic" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache ht
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_ht" =   2)
  gen_apache_score(dt = ddata)
  expect_equal(2, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_ht" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache hr
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_hr" =   2)
  gen_apache_score(dt = ddata)
  expect_equal(2, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_hr" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache map
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_map" =   0)
  gen_apache_score(dt = ddata)
  expect_equal(0, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_map" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache aki
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_aki" =   8)
  gen_apache_score(dt = ddata)
  expect_equal(8, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_aki" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache wbc
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_wbc" =   1)
  gen_apache_score(dt = ddata)
  expect_equal(1, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_wbc" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache gcs
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_gcs" =   12)
  gen_apache_score(dt = ddata)
  expect_equal(12, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_gcs" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache rf
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rf" =   4)
  gen_apache_score(dt = ddata)
  expect_equal(4, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rf" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache mf
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_mf" =   4)
  gen_apache_score(dt = ddata)
  expect_equal(4, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_mf" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache Na
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_Na" =   2)
  gen_apache_score(dt = ddata)
  expect_equal(2, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_Na" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache K
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_K" =   3)
  gen_apache_score(dt = ddata)
  expect_equal(3, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_K" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache temp
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_temp" =   2)
  gen_apache_score(dt = ddata)
  expect_equal(2, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_temp" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Apache rr
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rr" =   4)
  gen_apache_score(dt = ddata)
  expect_equal(4, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rr" =   NA)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Sum
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rr" =   4,
                      "apache_aki" =   8,
                      "apache_rf" =   4)
  gen_apache_score(dt = ddata)
  expect_equal(16, ddata[, apache_score])
  expect_warning(gen_apache_score(ddata))
  
  ddata <- data.table("time" = 22,
                      "site" = "YY",
                      "episode_id" = 116,
                      "apache_rr" =   NA,
                      "apache_aki" =   8,
                      "apache_rf" =   4)
  gen_apache_score(dt = ddata)
  expect_true(is.na(ddata[, apache_score]))
  expect_warning(gen_apache_score(ddata))
  
  # Sum
  ddata <- data.table("time"            =   22,
                      "site"            = "YY",
                      "episode_id"      =  116,
                      "apache_rr"       =    4,
                      "apache_aki"      =    8,
                      "apache_rf"       =    2,
                      "apache_mf"       =    1,
                      "apache_hr"       =    0,
                      "apache_gcs"      =   12,
                      "apache_Na"       =    4,
                      "apache_K"        =    3,
                      "apache_ht"       =    2,
                      "apache_age"      =    1,
                      "apache_chronic"  =    0,
                      "apache_map"      =    4,
                      "apache_wbc"      =    3,
                      "apache_temp"     =    2
                      )
  gen_apache_score(dt = ddata)
  expect_equal(46, ddata[, apache_score])
  expect_false(is.na(ddata[,apache_score]))
  expect_failure(expect_warning(gen_apache_score(ddata)))
  
  
  
  
  options(warn= 0)
  

})

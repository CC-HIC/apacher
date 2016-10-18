context("To test the risk prediction calculator")

test_that("To see if the gen_apache_pred give the right calculation of predicted risk",{
  ddata <- NULL

  
  # ------------------------ 
  ddata <- data.table("time"         = c(5,12, 22),
                      "site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "apache_score" = c(5, 24, NA),
                      "weight"       = c(-3.353, -1.261, 0.113),
                      "emergent"     = c("Medical", "Post-operative", "Medical"))
  
  gen_apache_pred(dt = ddata, window = c(0,24))
  expect_equal(c(0.00, 0.42, NA), ddata[, risk])
  
  # ------------------------ 
  # Without time
  ddata <- data.table("site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "apache_score" = c(5, 24, NA),
                      "weight"       = c(-3.353, -1.261, 0.113),
                      "emergent"     = c("Medical", "Post-operative", "Medical"))
  
  gen_apache_pred(dt = ddata, window = c(0,24))
  expect_equal(c(0.00, 0.42, NA), ddata[, risk])
  
  # ------------------------ 
  # time = out of bounds
  ddata <- data.table("time"         = c(5, 32, 22),
                      "site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "apache_score" = c(5, 24, NA),
                      "weight"       = c(-3.353, -1.261, 0.113),
                      "emergent"     = c("Medical", "Post-operative", "Medical"))
  
  gen_apache_pred(dt = ddata, window = c(0,24))
  expect_equal(c(0.00, NA, NA), ddata[, risk])
  
  # ------------------------ 
  # without apache_score
  ddata <- data.table("time"         = c(5, 32, 22),
                      "site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "weight"       = c(-3.353, -1.261, 0.113),
                      "emergent"     = c("Medical", "Post-operative", "Medical"))
  expect_error(gen_apache_pred(dt = ddata, window = c(0,24)))
  
  # ------------------------ 
  # without weight
  ddata <- data.table("time"         = c(5, 32, 22),
                      "site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "apache_score" = c(5, 24, NA),
                      "emergent"     = c("Medical", "Post-operative", "Medical"))
  expect_error(gen_apache_pred(dt = ddata, window = c(0,24)))
  
  # ------------------------ 
  # without emergent
  ddata <- data.table("time"         = c(5, 32, 22),
                      "site"         = c(rep("YY", 2), "ZY"),
                      "episode_id"   = c(rep(116, 2),115),
                      "apache_score" = c(5, 24, NA),
                      "weight"       = c(-3.353, -1.261, 0.113))
  expect_error(gen_apache_pred(dt = ddata, window = c(0,24)))
  
  # # APACHE HR score == 2 
  # ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(45, 2), 54))
  # gen_apache_age(dt = ddata, window = c(0,24))
  # expect_equal(c(2, NA, 2), ddata[, apache_age])
  # 
  # # APACHE HR score == 3 
  # ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(56, 2), 62))
  # gen_apache_age(dt = ddata, window = c(0,24))
  # expect_equal(c(3, NA, 3), ddata[, apache_age])
  # 
  # # APACHE HR score == 5 
  # ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(72, 2), 71))
  # gen_apache_age(dt = ddata, window = c(0,24))
  # expect_equal(c(5, NA, 5), ddata[, apache_age])
  # 
  # 
  # # APACHE HR score == 6 
  # ddata <- data.table("time" = c(5,72, 22), "site" = c(rep("YY", 2), "ZY"), "episode_id" = c(rep(116, 2),115), "age" = c(rep(84, 2), 102))
  # gen_apache_age(dt = ddata, window = c(0,24))
  # expect_equal(c(6, NA, 6), ddata[, apache_age])
  # 
  # # Various
  # ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
  #                     "site" = sample(c("YY", "ZY"), 10, replace = T),
  #                     "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
  #                     "age" = c(102, 52, 72, 63, 55, 52, 43, 55, 26, 22))
  # 
  # gen_apache_age(dt = ddata, window = c(0,24))
  # 
  # expect_equal(c(6, NA, 5, 3, NA, 2, 0, NA, NA, 0), ddata[, apache_age])
  # 
  
})

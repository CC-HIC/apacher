context("To test the gen_q_rr function")

test_that("To test the quality and the prioritization process of fields related to respiratory rate",{
  ddata <- NULL

  
  # Various
  ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "Spontaneous Respiratory Rate" =        c(55, 32, 12,  9, NA, NA, 112,  2, NA, 16),
                      "Total respiratory rate (monitor)" =    c(12, 22, NA, NA, 36,  8,  NA, 24, 12,  4),
                      "Total respiratory rate (ventilator)" = c(NA, NA, 16, 16, 16, 16,  16, 16, 16, 16))
  ddata <- gen_q_rr(dt = ddata, qual = c(3,60))
  
  expect_equal(c(55, 32, 12, 9, 36, 8, 16, 24, 12, 16), ddata[, q_rr])
  
  
})

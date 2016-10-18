context("To test the gen_comorbidities helper function")

test_that("To wether the derived comorbidity field is correctly built",{
  
  ddata <- NULL
  # Various
  ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "Severe respiratory disease"  =       c(1, 0,  1,  1,  0, 1,  0,  1, 0, 0),
                      "Chronic renal replacement therapy" = c(0, 1,  1,  0, NA, 1,  0,  0, 1, 0),
                      "Steroid treatment" =                 c(0, 1,  1,  0,  0, 0,  1, NA, 1, 0),
                      "Biopsy proven cirrhosis" =           c(1, 0, NA, NA,  0, 0, NA,  1, 0, 0),
                      "HIV/AIDS" =                          c(1, 0,  0,  1,  1, 0,  1,  0, 1, 0))
  
  gen_comorbidity(dt = ddata, fields = c("Severe respiratory disease",
                                        "Chronic renal replacement therapy",
                                        "Steroid treatment",
                                        "Biopsy proven cirrhosis",
                                        "HIV/AIDS"))
  
  expect_equal(c(3, 2, 3, 2, 1, 2, 2, 2, 3, 0), ddata[, d_comorbidity])
  
  
})

context("To test the apache chronic health calculator")

test_that("To see if the gen_apache_chronic give the right APACHE score for the right chronic health status",{
  ddata <- NULL
  # Various
  ddata <- data.table("time" = c(5, 72, 22, 24, 30, 5, 8, 98, 55, 6 ),
                      "site" = sample(c("YY", "ZY"), 10, replace = T),
                      "episode_id" = sample(seq(116, 126, 1), 10, replace = F),
                      "d_comorbidity" =             c(  1,   0,   0,   0,   1,   1,   0,   0,  1,    1),
                      "Admission type"  =           c("S", "S",  NA, "M", "L", "M", "M",  NA, "S", "S"),
                      "classification of surgery" = c("S", "L", "U", "M",  NA, "S", "L", "U", "M",  NA))
                      
  gen_apache_chronic(dt = ddata)
  
  expect_equal(c(2, 0, 0, 0, 5, 5, 0, 0, 5, 2), ddata[, apache_chronic])
  

})

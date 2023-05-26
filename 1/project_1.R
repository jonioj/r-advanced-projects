library(testthat)

get_hrv <- function(distances,t = 'ms', met ="rmssd") {
  
  "
  Uasge
  
  get_hrv(distances, ...)
  
  Default
  get_hrv(distances, t = 'ms', met = 'rmssd')
  
  Arguments
  
  distances: numeric vector, length atleast > 60, contains only positive values
  t: time unit in distances. ms - miliseconds or s - seconds
  met: method used to calculate HRV, possible 'rmssd' or 'sdnn'
  
  Value
  
  numeric value depending on the method
  
  Reference
  
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5624990/
  "
  stopifnot (is.numeric(distances),
             length(distances) > 60,
             all(distances > 0),
             (t == 'ms' | t =='s')
             )
  if (t == 's'){
    distances = distances * 1000
  }
  if (met == 'rmssd'){
    hrv = (mean(diff(distances)**2))**0.5
    attr(hrv,'method') = "rmssd"
  }
  
  if (met == 'sdnn') {
    hrv = sd(distances)
    attr(hrv,'method') = 'sdnn'
  
  }
  
  attr(hrv, 'class') = 'hrv'
  hrv
}


classify_hrv = function(hrv){
  
  
  "
  Uasge
  
  get_hrv(distances, ...)
  
  Arguments
  
  hrv: object of class 'hrv'
  
  Value
  
  class 0 or 1 depending on the HRV value
  
  "
  
  stopifnot(attr(hrv,'class') == 'hrv',
            attr(hrv,'method') %in% c('rmssd','sdnn'))
  if (attr(hrv, 'method') == 'rmssd') {
   
    if (hrv > 40){
      class = 1
    }
    else class = 0
  
  }
  else if (attr(hrv,'method') == 'sdnn'){
    if (hrv > 20){
      class = 1
    }
    else class = 0
  }
  class
}


distances_1 = rep(800,times = 4000)
distances_2 = rep(c(800,780,840,790),times=1000)
distances_3 = rep(0.8,times = 4000)
dsitances_4 = rep(c(0.84,0.78,0.84,0.79), times = 1000)
distances_5 = 3
distances_6 = rep(-4, times = 4000)

hrv_1 = get_hrv(distances_1)
hrv_2 = get_hrv(distances_2)
hrv_3 = get_hrv(distances_3)
attr(hrv_3,'method') = 'asd'

test_that("hrv", {
  expect_equal(get_hrv(distances_1)[1], 0 )
  expect_gt(get_hrv(distances_2)[1], 0 )
  expect_error(get_hrv(distances_5))
  expect_error(get_hrv(distances_6))
  
})

test_that("classify_hrv", {
 expect_equal(classify_hrv(hrv_1), 0)
  expect_error(classify_hrv(0))
  expect_error(classify_hrv(hrv_3))
  
})

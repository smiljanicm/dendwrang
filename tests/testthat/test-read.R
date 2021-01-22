context("read in")

x <- 1:3

testthat::test_that("test code", {
  expect_identical(mean(x), 2)
})

# testthat::test_that('na_set', {
#   eval <- c('./data/Vilm-3_Sensors_2016.dat',
#             './data/Vilm-3_Sensors.dat') %>%
#     read_sensor(sensor_columns = c(12, 3)) %>%
#     sub_sample(start = as.POSIXct('2020-01-01', tz='UTC'), resolution = 300, end = as.POSIXct('2020-01-01 12:00', tz='UTC')) %>%
#     na_set(c(as.POSIXct('2020-01-01 01:00', tz='UTC'),
#              as.POSIXct('2020-01-01 02:00', tz='UTC'))) %>%
#     na_set(c('2020-01-01 06:00', '2020-01-01 07:00', '2020-01-01 08:00')) %>%
#     summarize(nas = sum(is.na(Sensor))) %>%
#     as.double()
#   testthat::expect_equal(eval, 15)
# })

# c('../PC200W/Vilm-3_Sensors_2016.dat',
#             '../PC200W/Vilm-3_Sensors.dat') %>% glimpse_sensor()

#system.file("extdata", 'Vilm-3_Sensors.dat', package = "dendwrang", mustWork = TRUE)


c('./inst/extdata/Vilm-3_Sensors_2016.dat',
                      './inst/extdata//Vilm-3_Sensors.dat') %>%
              read_sensor(sensor_columns = c(12, 3)) %>%
#      sub_sample(start = as.POSIXct('2020-01-01', tz='UTC'), resolution = 1800, end = as.POSIXct('2020-01-01 12:00', tz='UTC')) %>%
  na_set(c(as.POSIXct('2014-07-01', tz='UTC'),
           as.POSIXct('2014-11-25', tz='UTC'))) %>%
  na_set(c(as.POSIXct('2015-10-23 11:30', tz='UTC'),
           as.POSIXct('2015-10-23 12:00', tz='UTC'))) %>%
  jump_correction(pre = c(as.POSIXct('2017-02-15', tz='UTC'),
                          as.POSIXct('2017-03-13 11:30', tz='UTC')),
                  post = c(as.POSIXct('2017-03-13 11:30', tz='UTC'),
                           as.POSIXct('2017-04-15', tz='UTC')),
                  jump = c(as.POSIXct('2017-03-13 11:15', tz='UTC'),
                           as.POSIXct('2017-03-13 11:45', tz='UTC'))) %>%
  jump_correction(pre = c(as.POSIXct('2019-02-15', tz='UTC'),
                          as.POSIXct('2019-03-07 11:45', tz='UTC')),
                  post = c(as.POSIXct('2019-03-07 11:45', tz='UTC'),
                           as.POSIXct('2019-04-15', tz='UTC')),
                  jump = c(as.POSIXct('2019-03-07 11:30', tz='UTC'),
                           as.POSIXct('2019-03-07 12:00', tz='UTC'))) %>%
  jump_correction(pre = c(as.POSIXct('2019-08-15', tz='UTC'),
                          as.POSIXct('2019-09-26', tz='UTC')),
                  post = c(as.POSIXct('2020-04-07', tz='UTC'),
                           as.POSIXct('2020-05-15', tz='UTC')),
                  jump = c(as.POSIXct('2019-09-26', tz='UTC'),
                           as.POSIXct('2020-04-07', tz='UTC'))) -> eval
eval %>% compare_years() %>%
  normalize() %>% filter(Years > 2014) %>% plot_sensor() + facet_wrap(~Years)


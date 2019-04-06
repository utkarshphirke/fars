context('read files')

testthat::test_that(
  'correct filename', {
    testthat::expect_match(fars::make_filename(2013), '.accident_[0-9]{4}')
    testthat::gives_warning(fars::fars_read_years(1))
    testthat::throws_error(fars::fars_map_state(51, 2013))
  }
)

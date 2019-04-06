library(testthat)
test_that(
  'correct filename', {
    expect_match(fars::make_filename(2013), '.accident_[0-9]{4}')
    gives_warning(fars::fars_read_years(1))
    throws_error(fars::fars_map_state(51, 2013))
  }
)

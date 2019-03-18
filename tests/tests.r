library(testthat)
expect_that(make_filename(2013), is_a("character"))
expect_that(fars_read(make_filename(2013)),
            is_a("data.frame"))
expect_that(fars_read_years(c(2013, 2014, 2015)),
            is_a("list"))
expect_that(fars_summarize_years(c(2013, 2014, 2015)),
            is_a("data.frame"))

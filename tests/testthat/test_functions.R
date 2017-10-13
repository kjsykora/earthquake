testthat::context("Test functions from earthquake package")

test_that("eq_clean_data works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
  earthquake_clean <- earthquake_raw %>%
    eq_clean_data()
  expect_that(earthquake_clean[["DATE"]], is_a("Date"))
}
)

test_that("eq_location_clean works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
  earthquake_loc_clean <- earthquake_raw %>%
    eq_location_clean()
  expect_that(earthquake_loc_clean[["LOCATION_NAME"]], is_a("character"))
}
)

test_that("eq_map works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
  earthquake_leaflet <- earthquake_raw %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")
  expect_that(earthquake_leaflet, is_a("leaflet"))
}
)

test_that("eq_create_label works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
  earthquake_clean <- earthquake_raw %>%
    eq_clean_data()

  expect_that(eq_create_label(earthquake_clean),
              is_identical_to(base::paste("<br> <b>Location:</b>", earthquake_clean$LOCATION_NAME,
                                                                    "</br> <br> <b>Magnitude:</b>", earthquake_clean$EQ_PRIMARY,
                                                                    "</br> <br> <b>Total deaths:</b>", earthquake_clean$DEATHS, "</br>")))
}
)


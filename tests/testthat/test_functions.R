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


test_that("geom_timeline works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
   earthquake_clean <- earthquake_raw %>%
   eq_clean_data()
   gg_time <- ggplot(data=earthquake_clean %>%
           filter(COUNTRY %in% c("CHINA", "USA") ))+
    geom_timeline(aes(x = DATE, color = log(DEATHS, base = 10), y = COUNTRY,
                      alpha = 1, xmin=as.Date("2000-01-01"), 
                      xmax=as.Date("2015-12-31"), size = EQ_PRIMARY)) +
    scale_color_continuous()+
    theme_minimal()+theme(legend.position = "bottom")
  expect_that(gg_time, is_a("ggplot"))
}
)


test_that("geom_timelinelabel works",{
  data(earthquake_raw)
  earthquake_raw <- earthquake_raw[earthquake_raw$YEAR>0,]
  earthquake_clean <- earthquake_raw %>%
    eq_clean_data()
   gg_time <- ggplot(data=earthquake_clean %>%
            filter(COUNTRY %in% c("CHINA", "USA") ))+
     geom_timelinelabel(aes(x = DATE, label= LOCATION_NAME, y = COUNTRY,
                            xmin=as.Date("2000-01-01"), 
                            xmax=as.Date("2015-12-31"), 
                            mag = EQ_PRIMARY), n_max = 4)+
     geom_timeline(aes(x = DATE, color = log(DEATHS, base = 10), y = COUNTRY,
                       alpha = 1, xmin=as.Date("2000-01-01"), 
                       xmax=as.Date("2015-12-31"),
                        size = EQ_PRIMARY)) +
     scale_color_continuous()+
     theme_minimal()+theme(legend.position = "bottom")
  
  expect_that(gg_time, is_a("ggplot"))
}
)
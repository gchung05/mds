context("Device Events")

# Set params
data <- maude
data$region <- as.factor(data$region)
invivo <- round(250 * runif(nrow(data)))
invivo <- ifelse(invivo <= 30, NA, invivo)
data$invivo <- invivo
rm(invivo)
Ptime <- "date_received"
Pdevice_hierarchy <- c("device_name", "device_class")
Pevent_hierarchy <- c("event_type", "medical_specialty_description")
Pkey <- "report_number"
Pcovariates <- "region"
Pdescriptors <- "_all_"
Pvivo <- "invivo"
PdescriptorsV <- names(data)[which(!names(data) %in% c(Ptime,
                                                       Pdevice_hierarchy,
                                                       Pevent_hierarchy,
                                                       Pkey,
                                                       Pcovariates,
                                                       Pvivo))]
# Reference example
a1 <- deviceevent(
  data,
  time=Ptime,
  device_hierarchy=Pdevice_hierarchy,
  event_hierarchy=Pevent_hierarchy,
  key=Pkey,
  covariates=Pcovariates,
  descriptors=Pdescriptors,
  implant_days=Pvivo)


# Basic
# -----

# Return behavior
test_that("function returns the correct class", {
  expect_is(a1, "data.frame")
  expect_is(a1, "mds_de")
})
test_that("parameter requirements as expected", {
  expect_error(deviceevent())
  expect_error(deviceevent(data))
  expect_error(deviceevent(data, Ptime))
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1]))
  expect_is(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1]),
            "mds_de")
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                           key="foo"))
  expect_is(suppressWarnings(
    deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                covariates="_all_")),
    "mds_de")
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                           covariates="foo"))
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                           descriptors="foo"))
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                           implant_days="foo"))
  expect_error(deviceevent(data, Ptime, Pdevice_hierarchy[1], Pevent_hierarchy[1],
                           implant_days="brand_name"))
})
# Attribute check
test_that("attributes are fully described", {
  expect_equal(all(names(attributes(a1)) %in% c(
    "names", "row.names", "class", "time", "device_hierarchy",
    "event_hierarchy", "key", "covariates", "descriptors", "implant_days")), T)
  expect_equal(attributes(a1)$time, Ptime)
  expect_equal(attributes(a1)$device_hierarchy,
               setNames(Pdevice_hierarchy, c("device_1", "device_2")))
  expect_equal(attributes(a1)$event_hierarchy,
               setNames(Pevent_hierarchy, c("event_1", "event_2")))
  expect_equal(attributes(a1)$key, Pkey)
  expect_equal(attributes(a1)$covariates,
               setNames(Pcovariates, Pcovariates))
  expect_equal(attributes(a1)$descriptors,
               setNames(PdescriptorsV, PdescriptorsV))
})


# Fully specified behavior
# ------------------------

test_that("output shape is as expected", {
  expect_equal(nrow(a1), nrow(data))
  expect_equal(ncol(a1), ncol(data))
  expect_equal(sum(is.na(a1$model_number)), sum(is.na(data$model_number)))
  expect_equal(sum(is.na(a1$report_source_code)), sum(is.na(data$report_source_code)))
})

test_that("input variable matches mapped output variable", {
  expect_equal(as.character(a1$key), data$report_number)
  expect_equal(gsub("-", "", as.character(lubridate::ymd(a1$time))),
               data$date_received)
  expect_equal(as.character(a1$device_1), data$device_name)
  expect_equal(as.character(a1$device_2), data$device_class)
  expect_equal(as.character(a1$event_1), data$event_type)
  expect_equal(as.character(a1$event_2),
               data$medical_specialty_description)
  expect_equal(as.factor(a1$region), data$region)
  expect_equal(a1$product_problem_flag, data$product_problem_flag)
  expect_equal(a1$adverse_event_flag, data$adverse_event_flag)
  expect_equal(a1$report_source_code, data$report_source_code)
  expect_equal(a1$lot_number, data$lot_number)
  expect_equal(a1$model_number, data$model_number)
  expect_equal(a1$manufacturer_d_name, data$manufacturer_d_name)
  expect_equal(a1$manufacturer_d_country, data$manufacturer_d_country)
  expect_equal(a1$brand_name, data$brand_name)
  expect_equal(PdescriptorsV[which(!PdescriptorsV %in% names(a1))], character(0))
})

test_that("output variable class converted correctly", {
  expect_is(a1$key, "character")
  expect_is(a1$time, "Date")
  expect_is(a1$device_1, "factor")
  expect_is(a1$device_2, "factor")
  expect_is(a1$event_1, "factor")
  expect_is(a1$event_2, "factor")
  expect_is(a1$region, "factor")
})

test_that("no extra variables were created", {
  expect_null(a1$device_3)
  expect_null(a1$event_3)
})

test_that("descriptors were kept in source format", {
  expect_is(a1$key, "character")
  expect_equal(class(a1$product_problem_flag), class(data$product_problem_flag))
  expect_equal(class(a1$adverse_event_flag), class(data$adverse_event_flag))
  expect_equal(class(a1$report_source_code), class(data$report_source_code))
  expect_equal(class(a1$lot_number), class(data$lot_number))
  expect_equal(class(a1$model_number), class(data$model_number))
  expect_equal(class(a1$manufacturer_d_name), class(data$manufacturer_d_name))
  expect_equal(class(a1$manufacturer_d_country), class(data$manufacturer_d_country))
  expect_equal(class(a1$brand_name), class(data$brand_name))
})

test_that("implant_days is correctly formatted", {
  expect_is(a1$implant_days, "numeric")
  expect_equal(a1$implant_days, data$invivo)
})

# Barebones behavior
# ------------------

# Set params
Ptime="date_received"
Pdevice_hierarchy="device_name"
Pevent_hierarchy="event_type"
# Reference example
a1 <- deviceevent(
  maude,
  time=Ptime,
  device_hierarchy=Pdevice_hierarchy,
  event_hierarchy=Pevent_hierarchy)

test_that("minimal parameters output shape is as expected", {
  expect_equal(nrow(a1), nrow(maude))
  expect_equal(ncol(a1), 4)
  expect_equal(a1$key, as.character(c(1:nrow(maude))))
  expect_null(a1$report_source_code)
})


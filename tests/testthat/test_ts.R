context("Time Series")

# Set data
data <- maude
invivo <- round(250 * runif(nrow(data)))
invivo <- ifelse(invivo <= 30, NA, invivo)
data$invivo <- invivo
rm(invivo)
exposures <- sales

# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type", "medical_specialty_description"),
  key="report_number",
  covariates="region",
  descriptors="_all_",
  time_invivo="invivo")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  match_levels="region",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  event_level="event_type",
  exposure=Pexp,
  covariates="region",
  invivo=T)

# Reference examples
a1 <- time_series(Pda[1:3], Pde, Pexp)
a2 <- time_series(Pda[[1]], Pde, Pexp)

# Basic
# -----

# Return behavior
test_that("function returns the correct class", {
  expect_is(a1, "list")
  expect_is(a1[[1]], "mds_ts")
  expect_is(a2, "mds_ts")
})
test_that("parameter requirements as expected", {
  expect_error(time_series())
  expect_error(time_series(list()))
  expect_error(time_series(Pda[[1]]))
  expect_error(time_series(Pda[[1]], foo))
  expect_error(time_series(Pda[[1]], Pde, foo))
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[1]], Pde, Pexp), "mds_ts")
})


# Fully specified behavior
# ------------------------

# Attribute check
test_that("attributes are fully described", {
  expect_equal(all(names(attributes(a2)) %in% c(
    "names", "row.names", "class", "title", "analysis", "exposure", "dpa",
    "dpa_detail", "covar_data")),
    T)
  expect_equal(length(attributes(a2)$title), 1)
  expect_equal(length(attributes(a2)$analysis), 19)
  expect_equal(length(attributes(a2)$dpa_detail), 7)
  expect_is(attributes(a2)$covar_data, "data.frame")
  expect_is(attributes(a2)$exposure, "logical")
  expect_is(attributes(a2)$dpa, "logical")
})

test_that("time series variable classes are correct", {
  expect_is(a2$time, "Date")
  expect_is(a2$nA, "integer")
  expect_is(a2$nB, "integer")
  expect_is(a2$nC, "integer")
  expect_is(a2$nD, "integer")
  expect_is(a2$ids, "list")
  expect_is(a2$exposure, "numeric")
  expect_is(a2$ids_exposure, "list")
})
test_that("ids carried over from source", {
  expect_equal(all(a2$ids[[1]] %in% Pde$key), T)
  expect_equal(all(a2$ids_exposure[[1]] %in% Pexp$key), T)
})

# Barebones behavior
# ------------------

# Reference example
a2 <- time_series(Pda[[1]], Pde)

# Attribute check
test_that("attributes are fully described", {
  expect_equal(all(names(attributes(a2)) %in% c(
    "names", "row.names", "class", "title", "analysis", "exposure", "dpa",
    "dpa_detail", "covar_data")), T)
  expect_equal(length(attributes(a2)$title), 1)
  expect_equal(length(attributes(a2)$analysis), 19)
  expect_equal(length(attributes(a2)$dpa_detail), 7)
  expect_is(attributes(a2)$exposure, "logical")
  expect_is(attributes(a2)$dpa, "logical")
})

test_that("time series variable classes are correct", {
  expect_is(a2$time, "Date")
  expect_is(a2$nA, "integer")
  expect_is(a2$nB, "integer")
  expect_is(a2$nC, "integer")
  expect_is(a2$nD, "integer")
  expect_is(a2$ids, "list")
  expect_null(a2$exposure)
  expect_null(a2$ids_exposure)
})
test_that("ids carried over from source", {
  expect_equal(all(a2$ids[[1]] %in% Pde$key), T)
  expect_equal(all(a2$ids_exposure[[1]] %in% Pexp$key), T)
})

# Test a series of analyses of differing variants
# -----------------------------------------------
anums <- c(1, 4, 5, 74, 77, 78, 79, 80)
for (i in anums){
  test_that(paste("Analysis", i, "runs correctly"), {
    expect_is(time_series(Pda[[i]], Pde), "mds_ts")
  })
}

# Differing types of input time series
# ------------------------------------
# No covar, only time in-vivo
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type", "medical_specialty_description"),
  key="report_number",
  descriptors="_all_",
  time_invivo="invivo")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  event_level="event_type",
  exposure=Pexp)
test_that("No covariates & only in-vivo", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})
# Covar only, no time in-vivo
# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type", "medical_specialty_description"),
  key="report_number",
  covariates="region",
  descriptors="_all_")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  match_levels="region",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  event_level="event_type",
  exposure=Pexp,
  covariates="region")
test_that("Covar only & no in-vivo", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})
# No covar, no time
# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type", "medical_specialty_description"),
  key="report_number")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  event_level="event_type",
  exposure=Pexp)
test_that("No covar no time", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})
# No dpa
# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name"),
  event_hierarchy=c("event_type"),
  key="report_number",
  covariates="region",
  descriptors="_all_",
  time_invivo="invivo")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  match_levels="region",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  event_level="event_type",
  exposure=Pexp,
  covariates="region",
  invivo=T)
test_that("No DPA", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})
# No event
# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type"),
  key="report_number",
  covariates="region",
  descriptors="_all_",
  time_invivo="invivo")
Pexp <- exposure(
  exposures,
  time="sales_month",
  device_hierarchy="device_name",
  match_levels="region",
  count="sales_volume"
)
Pda <- define_analyses(
  Pde, 
  device_level="device_name",
  exposure=Pexp,
  covariates="region",
  invivo=T)
test_that("No event", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})

# In-vivo only, nothing else
##################################
# Set params
Pde <- deviceevent(
  data,
  time="date_received",
  device_hierarchy=c("device_name"),
  event_hierarchy=c("event_type"),
  time_invivo="invivo")
Pda <- define_analyses(
  Pde, 
  device_level="device_name")
test_that("In-vivo only & nothing else", {
  expect_is(time_series(Pda[[1]], Pde), "mds_ts")
  expect_is(time_series(Pda[[2]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda)]], Pde), "mds_ts")
  expect_is(time_series(Pda[[length(Pda) - 1]], Pde), "mds_ts")
})

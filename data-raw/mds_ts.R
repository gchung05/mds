# Load maude and sales
load(file="data/maude.rda")
load(file="data/sales.rda")
# Standardize data
de <- deviceevent(maude, "date_received", "device_name", "event_type")
ex <- exposure(sales, "sales_month", "device_name", count="sales_volume")
# Define analyses
da <- define_analyses(de, "device_name", exposure=ex)
# Time series on three analyses
mds_ts <- time_series(da[1:3], de, ex)

devtools::use_data(mds_ts, overwrite=T)

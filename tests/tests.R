# TESTS
# +++++

# Read in complaints
s.data <- readxl::read_excel("E:/Projects/paradoxEgression/data/raw/VOC_CodesCombined_PI_Data_November2015_R2.xlsx")
# Read in sales
s.exp <- readxl::read_excel("E:/Projects/paradoxEgression/data/raw/VOC_ComplaintRate_November2015_R2.xls.xlsx")

# deviceevents()
# --------------
testDE <- deviceevents(
  s.data,
  key="Product Issue Number",
  time="Date",
  device_hierarchy=c("Functional Family", "PRODFAM2", "PRODFAM1"),
  event_hierarchy=c("VOC_New"),
  covariates=c("Modality", "Country", "MDR Decision"),
  descriptors=c("Product Qty Involved", "Product Code"))

# exposure()
# --------------
testEX <- exposure(
  s.exp,
  time="Month_desc",
  device_hierarchy=c("Functional Family", "PRODUCT_FAMILY_2", "PRODUCT_FAMILY_1"),
  event_hierarchy="VOC",
  count="TotalUnits"
)

# define_analyses()
# define_analyses_dataframe()
# summary.mdpms.define_analyses()
# -------------------------------
# options(warn=2)
# options(warn=1)
testDA <- define_analyses(
  testDE,
  testEX,
  date_level="months",
  date_level_n=3,
  device_level="Functional Family",
  covariates=c("Modality"),
  times_to_calc=6
)
testDAdf <- define_analyses_dataframe(testDA)
summary(testDA)

 # time_series()
# -------------
testTS <- time_series(
  testDA,
  deviceevents=testDE,
  exposure=testEX)

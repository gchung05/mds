# TESTS
# +++++



# Read in complaints
s.data <- readxl::read_excel("E:/Projects/paradoxEgression/data/raw/VOC_CodesCombined_PI_Data_November2015_R2.xlsx")
# Read in sales
s.exp <- readxl::read_excel("E:/Projects/paradoxEgression/data/raw/VOC_ComplaintRate_November2015_R2.xls.xlsx")

# deviceevents()
# --------------
# All covars
# testDE <- deviceevents(s.data,
#                        key="Product Issue Number",
#                        time="Date",
#                        device_hierarchy=c("Functional Family", "Product Family"),
#                        event_hierarchy=c("VOC_New"),
#                        covariates="_all_")
# Only a few
testDE <- deviceevents(
  s.data,
  key="Product Issue Number",
  time="Date",
  device_hierarchy=c("Functional Family", "PRODFAM2", "PRODFAM1"),
  event_hierarchy=c("VOC_New"),
  covariates=c("Modality", "Country", "MDR Decision"))

# exposure()
# --------------
testEX <- exposure(
  s.exp,
  time="Month_desc",
  device_hierarchy=c("Functional Family", "PRODUCT_FAMILY_2", "PRODUCT_FAMILY_1"),
  event_hierarchy="VOC",
  count="TotalUnits"
)

# define analyses
# ---------------
testDA <- define_analyses(
  testDE,
  device_level="Functional Family"
)
testDAdf <- define_analyses_dataframe(testDA)


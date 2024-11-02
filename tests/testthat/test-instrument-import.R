test_that("import xrd data", {
  projectName = paste0("xrd",floor(runif(1,0,100)))
  tmpDir = get_test_RAW_folder(10, projectName)

  instrument_list = list(XRD = instrumentXRD, AFM = instrumentAFM)
  expect_true(is(instrument_list,"list"))

  # check INITIALIZATION
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      instrument_list = instrument_list,
                      verbose=FALSE)


  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(length(dataRAW$ID), 10)

  # delete database
  file.remove(raw.getDatabase(rawBase))
})

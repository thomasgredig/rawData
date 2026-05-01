test_that("AFM testing", {
  # make a project name and set up files
  projectName = paste0("afm",floor(runif(1,0,100)))
  no_files <- 5
  tmpDir = get_test_RAW_folder(no_files, projectName)

  instrument_list = list(XRD = instrumentXRD, AFM = instrumentAFM)
  expect_true(is(instrument_list,"list"))

  # check INITIALIZATION
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      instrument_list = instrument_list,
                      quiet=TRUE)
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(length(dataRAW$ID), no_files)

  # add AFM file
  afm_sample_file = nanoAFMr::AFM.getSampleImages()[6]
  file.copy(from=afm_sample_file, to=file.path(tmpDir,basename(afm_sample_file)))
  rawBase <- raw.update(rawBase, project="afmnew")
  # check that item is added to history
  h <- rawBase$import_history
  expect_equal(nrow(h),2)

  # search for AFM file
  rawBase <- raw.update(rawBase, file_pattern="*.tiff")
  h <- rawBase$import_history
  expect_equal(nrow(h),3)
  dataRAW <- as.data.frame(rawBase$dataRAW)
  #print(dataRAW)
  # expect_equal(length(dataRAW$ID), no_files+1)
})


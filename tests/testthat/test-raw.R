test_that("find raw files", {
  tmpDir = tempdir()
  temp_file <- file.path(tmpDir, "20240909_spinPc_SC_XRR_Fe_FeP_7nm.txt")
  writeLines("This is a test file.", temp_file)

  temp_file <- file.path(tmpDir, "20240909_2spinPc_SC_XRR_Fe_FeP_7nm.txt")
  writeLines("This is a test file.", temp_file)

  s <- raw.find("spinpc", tmpDir)
  expect_equal(length(s), 1)
})

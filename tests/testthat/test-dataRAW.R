test_that("check S3 dataRAW", {
  tmpDir = get_test_RAW_folder(1,"spinPc")
  expect_true(dir.exists(tmpDir))
})

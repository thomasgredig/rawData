test_that("find raw files", {
  tmpDir = get_test_RAW_folder(2, "spinPc")

  rawBase=raw.rawBase("spinPc",paths=tmpDir, sqlPaths=tmpDir)

  s <- raw.find(rawBase)
  expect_true(length(s) >= 2)

})


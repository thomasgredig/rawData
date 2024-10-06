test_that("clean rawBase", {
  rawBase = list(sqlPaths = c("A","Ba","Cas","Ba"),
                 extraPaths = c("A","A","B","A","AB"))
  rawBase = .cleanRawBase(rawBase)
  expect_equal(length(rawBase$extraPaths), 3)
  expect_equal(length(rawBase$sqlPaths), 3)

  # make sure only Paths variables are cleaned
  rawBase = list(varWithDuplicates = c("letters","A","B","A","A","B","C"),
                myPaths = c("letters","A","B","A","A","B","C"))
  rawBase = .cleanRawBase(rawBase)
  expect_equal(length(rawBase$varWithDuplicates), 7)
  expect_equal(length(rawBase$myPaths), 4)
})



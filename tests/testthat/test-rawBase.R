test_that("clean rawBase", {
  rawBase = list(sql_paths = c("A","Ba","Cas","Ba"),
                 extra_paths = c("A","A","B","A","AB"))
  rawBase = .cleanRawBase(rawBase)
  expect_equal(length(rawBase$extra_paths), 3)
  expect_equal(length(rawBase$sql_paths), 3)

  # make sure only Paths variables are cleaned
  rawBase = list(varWithDuplicates = c("letters","A","B","A","A","B","C"),
                my_paths = c("letters","A","B","A","A","B","C"))
  rawBase = .cleanRawBase(rawBase)
  expect_equal(length(rawBase$varWithDuplicates), 7)
  expect_equal(length(rawBase$my_paths), 4)
})



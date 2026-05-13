test_that("check dataRAW update", {
  projectName = paste0("spin",floor(runif(1,0,100)))
  # create a folder with 10 RAW data files
  tmpDir = get_test_RAW_folder(10, projectName, recreate=TRUE)

  # check INITIALIZATION
  rawBase <- raw.init(projectName, paths=tmpDir, sqlPaths=tmpDir, recursive=FALSE, quiet=TRUE)
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(dataRAW$ID, 7:16)

  # check data type
  rawType = raw.getType(dataRAW, 7)
  expect_equal(rawType, "text")
  filename = raw.getFilename(rawBase, 7)
  expect_true(file.exists(filename))

  # check ADDING files
  tmpDir = get_test_RAW_folder(2, projectName)
  rawBase <- raw.update(rawBase, path=tmpDir, quiet=TRUE)
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(length(dataRAW$ID), 12)
  prevIDs = dataRAW$ID

  # check DELETING FILES
  # 2 files should appear now as missing
  file_delete = dir(tmpDir, pattern=projectName, full.names=TRUE)[1]
  expect_true(file.remove(file_delete))
  file_delete = dir(tmpDir, pattern=projectName, full.names=TRUE)[3]
  expect_true(file.remove(file_delete))
  rawBase <- raw.update(rawBase, path=tmpDir) # <<-----
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(dataRAW$ID, prevIDs)
  expect_equal(length(which(dataRAW$missing==TRUE)),2)

  # RENAME a file
  file_rename = dir(tmpDir, pattern=projectName, full.names=TRUE)[5]
  file.rename(file_rename, gsub("Sample","Probe", file_rename))
  rawBase <- raw.update(rawBase, path=tmpDir, quiet=TRUE)
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(sort(dataRAW$ID), prevIDs)

  # MOVE file to different folder
  newFolder = file.path(tmpDir,"RAW")
  dir.create(newFolder)
  file_move = dir(tmpDir, pattern=projectName, full.names=TRUE)[6]
  file_move_new = file.path(newFolder,basename(file_move))
  file.rename(file_move, file_move_new)
  rawBase <- raw.update(rawBase, path=newFolder, quiet=TRUE)
  dataRAW = as.data.frame(rawBase$dataRAW)
  expect_equal(sort(dataRAW$ID), prevIDs)

  # delete database
  file.remove(raw.getDatabase(rawBase))
})


test_that("update DB", {
  projectName = paste0("testDB",floor(runif(1,0,100)))
  tmpDir = get_test_RAW_folder(10, projectName)

  instrument_list = list(AFM = instrumentAFM)
  rawBase <- raw.init(projectName,
                      paths=tmpDir,
                      sqlPaths=tmpDir,
                      recursive=FALSE,
                      instrument_list = instrument_list,
                      quiet=TRUE)
  expect_true(is.null(check_rawBase(rawBase)))
  expect_true(length(rawBase$sql_paths)>0L)

  # make sure database is on current version
  dbFile = .getDatabaseName(rawBase, include_oldVersions = FALSE)
  expect_true(file.exists(dbFile))

  # delete database
  file.remove(raw.getDatabase(rawBase))

  # update should re-create database
  rawBase = raw.update(rawBase, quiet=TRUE)
  expect_true(file.exists(raw.getDatabase(rawBase)))

  # delete database
  file.remove(raw.getDatabase(rawBase))
})



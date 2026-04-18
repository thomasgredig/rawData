# RAW Data File Manager

This file manager helps analyzing scientific data from instruments (XRD, AFM, etc.) by assigning unique file IDs related a project. A raw data file contains data from an instrument and should not be altered. The file may be renamed or moved to a different directory or multiple copies are maintained in various folders. In order to make the data analysis possible on different platforms and computers, the analysis happens via the unique file ID rather than a local file name.

## Installation

You can install `rawData` as follows:

``` r
install.packages('rawData')
# create a rawBase S3 object for project spinPc
# this will also create a SQL database
rawBase <- raw.init("spinPc", instrument_list=
                      list(XRD=instrumentXRD, AFM=instrumentAFM)) 
```

## Instruments

The `rawBase` object contains information about all files in the project including unique CRC; this ensures that if files are moved between folders or renamed, there is a consistent and unique `ID` associated with that file from that project.

The content from the files can also be stored separately, then direct access to the files is not needed; since data is stored differently, there is an instrument function that translates the content from the instrument into a table or data frame that is stored as an `.rda` file. Several functions are provided that can translate instrument data:

-   **instrumentXRD**: for XRD data files from rigaku based on `rigakuR`

-   **instrumentAFM**: for AFM data files from different instruments, based on `nanoAFMr`

The instrument is added via `raw.update()`. The function `raw.updateInstrument` calls all instrument functions and updates the data.

## Scenario

After creating a new package `dataSpin` using the template `dataProjectTemplate`, you might want to add RAW data files into that package and create a variable `dataRAW`.

If the data is located in the `../RAW` folder, and files include an 8-digit date and the project name `spinPc`, then the following command will:

-   initialize a `rawBase` object containing relevant parameters
-   search the path for files recursively
-   add those files to the `dataRAW` data frame in `rawBase`
-   try to import relevant data from XRD, AFM files and store them in `dataXRD` and `dataAFM`.

``` r
# WORKFLOW: 
# create a folder with 10 RAW data files
tmpDir = get_test_RAW_folder(10, "spinPc")

# initialize raw folder and SQL database
rawBase <- raw.init("spinPc",paths=tmpDir, sqlPaths=tmpDir, recursive=FALSE)
d <- as.data.frame(rawBase$dataRAW) # data frame with file information

# later you might want to update your database
tmpDir = get_test_RAW_folder(2, "spinPc") # new files
rawBase <- raw.update(rawBase)

# save 
usethis::use_data(rawBase, overwrite = TRUE)
```

## Database

Some data is too large to be saved directly within the data package: usually AFM files. Loading R data packages that are more than 1 or 2 MB is not efficient at all. Therefore, the data is stored in a separate SQLite database that is referenced from the data package.

Therefore, in addition to installing the database package, you will also need to install the SQL database. Tables in the database:

-   **afmData**: contains file names with AFM data

-   **sqlHistory**: history of DB access contains a token

Each update to the dataRAW table creates a new token in rawBase; this token should agree with the token in the SQL database; otherwise, the database is out of sync.

If the SQL package has been moved to a new folder, then you can re-establish the folder by adding it to the package:

``` r
rawBase <- raw.update(rawBase, sqlPath = "/newPath/")
```

## Package

Run `covr::report()` to create a coverage report: **62.82%**

Run `pkgdown::build_site()` to create the documentation.

Use `devtools::build_manual(path=".")` to build the manual


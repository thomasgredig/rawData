# rawData

RAW data manager for scientific data investigations.

## Installation

You can install the development version of `rawData` like so:

``` r
install.packages('rawData')
load(rawBase) # configuration data
raw.getDatabase(rawBase, v=TRUE) # install database
```

## Scenario

After creating a new package `dataSpin` using the template `dataProjectTemplate`, you might want to add RAW data files into that package and create a variable `dataRAW`.

If the data is located in the `../RAW` folder, and files include an 8-digit date and the project name `spinPc`, then the following command will:

-   initialize a rawBase variable,
-   search the path for files
-   add those files to the `dataRAW` variable
-   try to import relevant data from XRD, AFM files and store them in `dataXRD` and `dataAFM`.

``` r
# create a folder with 10 RAW data files
tmpDir = get_test_RAW_folder(10, "spinnPc")

# initialize raw folder and SQL database
d <- raw.init("spinnPc",paths=tmpDir, sqlPaths=tmpDir, recursive=FALSE)
dataRAW = d$dataRAW
rawBase = d$rawBase

# add more RAW data files and add them
tmpDir = get_test_RAW_folder(2, "spinnPc")
d <- raw.update(rawBase, dataRAW, path=tmpDir)
dataRAW = d$dataRAW
rawBase = d$rawBase
```

## Database

Some data is too large to be saved directly within the data package: usually AFM files. Loading R data packages that are more than 1 or 2 MB is not efficient at all. Therefore, the data is stored in a separate SQLite database that is referenced from the data package.

Therefore, in addition to installing the database package, you will also need to install the SQL database. Tables in the database:

-   afmData: contains filenames with AFM data

-   sqlHistory: history of DB access contains a token

Each update to the dataRAW table creates a new token in rawBase; this token should agree with the token in the SQL database; otherwise, the database is out of sync.

## Package

Run `covr::report()` to create a coverage report: **70.34%**

Run `pkgdown::build_site()` to create the documentation.

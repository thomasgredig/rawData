# rawData

RAW data manager for scientific data investigations.

## Installation

You can install the development version of `rawData` like so:

``` r
install.packages('rawData')
```

## Scenario

After creating a new package `dataSpin` using the template `dataProjectTemplate`, you might
want to add RAW data files into that package and create a variable `dataRAW`.

If the data is located in the `../RAW` folder, and files include an 8-digit date and 
the project name `spinPc`, then the following command will:

* initialize a rawBase variable,
* search the path for files
* add those files to the `dataRAW` variable
* try to import relevant data from XRD, AFM files and store them in `dataXRD` and `dataAFM`.

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


## Package

Run `covr::report()` to create a coverage report: 50.7%

Run `pkgdown::build_site()` to create the documentation.

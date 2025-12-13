# rawData 0.2.7

* add `raw.getCRC`
* support legacy RAW ID update through `raw.update`

# rawData 0.2.5

* allow extensions in `raw.init()`, such that files without the project name can be loaded

# rawData 0.2.4

* make sure that the `data` file is not part of this package as created through test
* add instruments through the `raw.update()` at a later time

# rawData 0.2.3

* update SQL database version, if needed

# rawData 0.2.2

* add rawBase$import_history, which has a table with the history of folders to be added

# rawData 0.2.1

* add instrument data saving ability with instrumentXRD
* adding instrumentAFM for import

# rawData 0.2.0

* use a dataframe within the dataRAW S3 class
* store paths properly within the S3 class

# rawData 0.1.1

* Support legacy RAW-ID.csv files with IDs

# rawData 0.1.0

* Initial concept based on RAWdataR

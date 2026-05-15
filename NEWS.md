# rawData 0.2.16

* add `raw.findID()` to get ID from string
* support deposition data from `instrumentATE`

# rawData 0.2.15

* add `file_pattern`, but not fully implemented
* small fixes to rbind

# rawData 0.2.14

* add `raw.addSQLpath` to quickly add a SQL path without having to check all files

# rawData 0.2.13

* use `bind_rows` instead of `rbind` in AFM

# rawData 0.2.12

* add `file_pattern` argument to `raw.update` to include special files not included in project
* more testing for loading AFM images

# rawData 0.2.10

* if AFM data is already available, then do not search again, same for XRD
* update to `xrd_import` new version of rigakuXRD package

# rawData 0.2.9

* avoid overwriting instrument data (AFM)

# rawData 0.2.8

* Instruments return `rawBase`
* Update SQL database, or create it with the previous information, if possible

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

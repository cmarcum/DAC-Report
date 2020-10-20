# DAC-Report
This project is internal for a POC of automatic dbGaP reporting for Data Access Committees

![logo](icons/potential_icon_dbgap_1.png)

Currently we use the [dbGaP Data Access and Use Report page](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi) as our primary data source and this package serve as a tool to:
* Easily scrape the data from the webpage and save as an csv
* Automate typical analysis on csv file

# Workflow
Suppose we want to analyze requests data made to dbGaP from 2017 to 2019, we would:
1. Download the table containing the desired data from the desired time range by calling `download.dar.tables`
```r
download.dar.tables('table1','01/01/2017','12/31/2019','./data/table1_2017-2019.csv')
```
Alternatively, just download it from this github under csv_to_download

2. Use functions provided in the package to explore the dataset!
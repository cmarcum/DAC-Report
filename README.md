# DAC-Report
This project is internal for a POC of automatic dbGaP reporting for Data Access Committees

![logo](icons/potential_icon_dbgap_1.png)

Currently we use the [dbGaP Data Access and Use Report page](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi) as our primary data source and this package serve as a tool to:
* Easily scrape the data from the webpage and save as an csv
* Automate typical analysis on csv file

# Workflow
To retrieve the latest data (from the beginning of time to 10/22/2020), simply load the rda files stored in `/data` by using `load()`!
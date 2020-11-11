# DAC-Report

![R-CMD-check](https://github.com/cmarcum/DAC-Report/workflows/R-CMD-check/badge.svg?branch=devel)

This project is internal for a POC of automatic dbGaP reporting for Data Access Committees

![logo](icons/dac.png)

# Data Source
We use the [dbGaP Data Access and Use Report page](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi) as our primary data source and this package serve as a programmatic interface to easily retrieve the data and automatically generate data report. Currently the package stores all DAC action table data (last update: 11/04/2020) locally [(example)](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=04/23/2020&endDate=10/22/2020]). To load the data into R environment use:
```
data("nih_dac_action_table")
```
and use this to update all the locally stored tables with the latest data
```
update.every.table()
```

# Tables Available

One of the main goals of the package is to make summary statistic of DAR data easily accessible. Here are some of the tables you can easily compute using this package

`nih_dac_action_table`, for table schema see and example [link](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=04/23/2020&endDate=10/22/2020)

`all_nih_studies_table` for table schema see [link](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=04%2F28%2F2020&endDate=05%2F28%2F2020&retTable=tablea1) (equivalent of table a1)

`get.study.summary.table()`
Returns a summary dataframe of all DAC which had DAR approved in the given timeframe. See documentation for table schema.

`dar.review.timeline.summary()`
Returns a summary dataframe of all studies made in the given timeframe. Note that some studies can be both approved and rejected (ex. approved but later rejected ), or neither (still in process of approval) so it isn't necessary that Total Request = Approval + Reject. See documentation for table schema

For a complete list of all available tables, see files under the R/ directory and their associated documentation.

# Report Generation

To generate a summary report based on the current data. Use

```
compile.dac.report('NIAID','Hoyin Chu, Christopher Marcum', '2015-01-01', '2019-12-31')
```

And a .docx will be generated containing figures and report. Close the opened .docx file before generating a new one to avoid errors.

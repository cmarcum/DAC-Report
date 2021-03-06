# DAC-Report

![R-CMD-check](https://github.com/cmarcum/DAC-Report/workflows/R-CMD-check/badge.svg?branch=devel)

This project stems from the need for more accessible reporting of actions done by NIH Data Access Committees (DAC) within the dbGaP DAC environment.  The package has three main functions:<img align="left" src="icons/dac.png">
* stores, curates, and makes accessible tables from dbGaP's Data Access and Use Reports
* calculates summary-level statistics of specific DAC actions
* generates a ready-to-use report in MS Word Format from compiled statistics of specific DAC actions within a given timeframe. 


# Installing the package

To install the package, use the devtools package in R:

```
install_github("https://github.com/cmarcum/DAC-Report/") 
```
# Data Source
We use the [dbGaP Data Access and Use Report page](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi) as our primary data source and this package serves as a programmatic interface to easily retrieve the data and automatically generate a DAC-specific data report. Currently the package stores all DAC action data (last update: 11/04/2020) locally [(example)](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=04/23/2020&endDate=10/22/2020]). To use the data:
```
library(DACReportingTool)
table1 <- get.nih.dac.action.table()
```
And to update all locally stored data with the latest information from dbGaP, use:
```
dac.data.update.all()
```

# Tables Available

One of the main goals of the package is to make summary statistics of DAC activities data easily accessible. Here are some of the tables that can be computed using this package:

The NIH DAC Action Table, for table schema see  [link](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=04/23/2020&endDate=10/22/2020)

All NIH Studies Table, for table schema see [link](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?stDate=04%2F28%2F2020&endDate=05%2F28%2F2020&retTable=tablea1) (equivalent of Table A1 on dbGaP).

`get.study.summary.table()`
Returns a summary dataframe of all DAC which had DAR approved in the given timeframe. See documentation for table schema.

`get.dar.review.timeline.summary.table()`
Returns a summary dataframe of all studies made in the given timeframe. Note that some studies can be both approved and rejected (ex. approved but later rejected ), or neither (still in process of approval) so it isn't necessary that Total Request = Approval + Reject. See documentation for table schema

For a complete list of all available tables, see files under the R/ directory and their associated documentation.

# Report Generation

One of the key features of this package is the ability to generate a DAC-specific summary report for a given window of time. For example, to generate the Central-DAC report for all activities occurring between the first of January, 2015 and the end of June, 2015: 

```
compile.dac.report(dac='CDAC',author='Committee Chair', start.date='2019-01-01', end.date='2020-01-01')
```
This function uses an internal R Markdown template that compiles its output to a Microsoft Word (Report.docx) document. The report will automatically open using whatever compatible software you have available (i.e., MS-Word, Office 365, LibreOffice). The report contains figures, tables, and basic summary interpretation. Be sure to save this report to an appropriate location as it is only stored in a temporary location while it's open.  Users must close the opened Report.docx file before generating a new one to avoid errors.

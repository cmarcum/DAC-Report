# DAC-Report
This project is internal for a POC of automatic dbGaP reporting for Data Access Committees

![logo](icons/potential_icon_dbgap_1.png)

Currently we use the [dbGaP Data Access and Use Report page](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi) as our primary data source and this package serve as a programmatic interface to easily retrieve the data

# Workflow
The package stored all DAC action table data (example: https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/DataUseSummary.cgi?DAC=all&actType=all&stDate=04/23/2020&endDate=10/22/2020) till 10/22/2020. To access the data simply call
```
my_table <- DACReportingTool::nih_dac_action_table
```
To update the dataframe with the latest data you would need to access the `/data` directory which means you need to clone the repository. After doing so, you can build the package and update the data by doing:
```
> library('devtools')
> load_all()
> update.dac.action.table()
```
and it will update the table with the latest data! (Remember to reload!)

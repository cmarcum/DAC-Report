# Updates every table that are locally stored in data/ directory
update.every.table <- function() {
  dac.action.table.update()
  update.phs.studies.table()
  all_nih_dac_studies_table <- get.all.nih.dac.studies.table()
  save(all_nih_dac_studies_table, file = system.file("data","all_nih_dac_studies_table.rda", package = "DACReportingTool"), compress="xz")
  print("All tables updated")
}

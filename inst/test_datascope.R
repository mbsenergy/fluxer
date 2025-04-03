library(httr2)
library(jsonlite)
library(magrittr)
library(data.table)
devtools::load_all()

## instruments list https://select.datascope.refinitiv.com/dataScope/extractions/instrumentlists#0x08532ebecc787a72
## schedules https://select.datascope.refinitiv.com/DataScope/Extractions/Schedules#0x08531da9f2587a3d
## report templates https://select.datascope.refinitiv.com/DataScope/extractions/reports#0x07b2e832330d1c82
## extraction https://select.datascope.refinitiv.com/DataScope/extractions/files/EL_Extn_Preview.aspx?id=fwd_price_20250329.csv
## api quickstart example https://selectapi.datascope.refinitiv.com/RestApi.Help/Home/Quickstart?ctx=Extractions&opn=Extract&sce=Primary%20Example&stp=1a&tab=2&uid=QuickstartOnDemandExtraction
## api reference tree https://selectapi.datascope.refinitiv.com/RestApi.Help/Context/Entity?ctx=Extractions&ent=InstrumentList&sce=Instrument%20List%20Examples%20-%20Append%20Identifiers&stp=1&tab=2#


## GET THE TOKEN -----------------------------------------------------

# Define credentials
username = "9028810"
password = "Ref-e2021"

auth_token = datascope_token(username = username, password = password)


## GET INSTRUMENTS LIST ----------------------------------------------

dt_inst_list = datascope_instrument_lists(auth_token = auth_token)


## GET INSTRUMENTS LIST ITEMS ------------------

dt_inst_items = datascope_instrument_items(instrument_list_id = dt_inst_list[Name == 'Forward_commodities'][1]$ListId, auth_token = auth_token)
dt_inst_items = datascope_instrument_items(instrument_list_id = dt_inst_list[Name == 'Forward_commodities'][1]$ListId, auth_token = auth_token)


## GET REPORT TEMPLATES ------------------------------------

dt_report_templates = datascope_report_templates(auth_token = auth_token)


## GET REPORTS SCHEDULES ----------------------------------

dt_report_schedules = datascope_report_template_schedules(report_template_id = dt_report_templates[Name == 'eod_prices_volumes']$ReportTemplateId, auth_token = auth_token)


## GET REPORTS LAST EXTRACTION ----------------------------------

dt_report_lastxtraction = datascope_report_lastextraction(schedule_id = dt_report_schedules$ScheduleId[1], auth_token = auth_token)


## GET FILE ID FROM REPORTS LAST EXTRACTION ----------------------------------

dt_report_lastxtraction_file = datascope_report_extraction_files(report_extraction_id = dt_report_lastxtraction$ReportExtractionId[1], auth_token = auth_token)


## GET FILE FROM REPORTS LAST EXTRACTION ----------------------------------

datascope_download_extracted_file(extracted_file_id = dt_report_lastxtraction_file$ExtractedFileId[1], auth_token = auth_token, filename = 'raw.csv')



# TEST FOR JOB: ---------------------

## GET THE TOKEN -----------------------------------------------------

# Define credentials
username = "9028810"
password = "Ref-e2021"

auth_token = datascope_token(username = username, password = password)


## GET INSTRUMENTS LIST ----------------------------------------------

dt_inst_list = datascope_instrument_lists(auth_token = auth_token)


## GET INSTRUMENTS LIST ITEMS ------------------

dt_inst_items = datascope_instrument_items(instrument_list_id = dt_inst_list[Name == 'Forward_commodities'][1]$ListId, auth_token = auth_token)


## GET REPORT TEMPLATES ------------------------------------

dt_report_templates = datascope_report_templates(auth_token = auth_token)

eod_prices = datascope_report_template_schedules(report_template_id = dt_report_templates[Name == 'eod prices']$ReportTemplateId, auth_token = auth_token)
eod_prices_volumes = datascope_report_template_schedules(report_template_id = dt_report_templates[Name == 'eod_prices_volumes']$ReportTemplateId, auth_token = auth_token)


# prices
dt_report_schedules = copy(eod_prices)
dt_report_lastxtraction = datascope_report_lastextraction(schedule_id = dt_report_schedules$ScheduleId[1], auth_token = auth_token)
dt_report_lastxtraction_file = datascope_report_extraction_files(report_extraction_id = dt_report_lastxtraction$ReportExtractionId[1], auth_token = auth_token)
datascope_download_extracted_file(extracted_file_id = dt_report_lastxtraction_file$ExtractedFileId[1], auth_token = auth_token, filename = dt_report_lastxtraction_file$ExtractedFileName)

dt_report_lastxtraction = datascope_report_lastextraction(schedule_id = dt_report_schedules$ScheduleId[2], auth_token = auth_token)
dt_report_lastxtraction_file = datascope_report_extraction_files(report_extraction_id = dt_report_lastxtraction$ReportExtractionId[1], auth_token = auth_token)
datascope_download_extracted_file(extracted_file_id = dt_report_lastxtraction_file$ExtractedFileId[1], auth_token = auth_token, filename = dt_report_lastxtraction_file$ExtractedFileName)

# prices volumes
dt_report_schedules = copy(eod_prices_volumes)
dt_report_lastxtraction = datascope_report_lastextraction(schedule_id = dt_report_schedules$ScheduleId[1], auth_token = auth_token)
dt_report_lastxtraction_file = datascope_report_extraction_files(report_extraction_id = dt_report_lastxtraction$ReportExtractionId[1], auth_token = auth_token)
datascope_download_extracted_file(extracted_file_id = dt_report_lastxtraction_file$ExtractedFileId[1], auth_token = auth_token, filename = dt_report_lastxtraction_file$ExtractedFileName)

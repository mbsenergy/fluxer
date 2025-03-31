#' Authenticate with Refinitiv DataScope Select API
#'
#' This function sends a `POST` request to the Refinitiv DataScope Select API
#' to obtain an authentication token using the provided username and password.
#'
#' @param username A character string representing the Refinitiv username.
#' @param password A character string representing the Refinitiv password.
#'
#' @return A character string representing the authentication token.
#' @examples
#' \dontrun{
#'   token <- authenticate_refinitiv("9028810", "Ref-e2021")
#'   print(token)
#' }
#' @import httr2
#' @export
#'
datascope_token = function(username, password) {
    # Define API URL
    url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Authentication/RequestToken"

    # Create request body
    body = list(
        Credentials = list(
            Username = username,
            Password = password
        )
    )

    # Send the request
    resp = httr2::request(url) |>
        httr2::req_headers(
            "Prefer" = "respond-async",
            "Content-Type" = "application/json; odata=minimalmetadata"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_perform()

    # Extract and return the authentication token
    resp_json = resp |> httr2::resp_body_json()
    return(resp_json$value)
}


#' Retrieve Instrument Lists from Refinitiv DataScope Select API
#'
#' This function sends a `POST` request to the Refinitiv DataScope Select API
#' to retrieve instrument lists based on the provided authentication token.
#' It processes the response and returns the result as a data table.
#'
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the instrument lists retrieved from the API.
#' @examples
#' \dontrun{
#'   auth_token <- "<your_auth_token>"
#'   instrument_lists <- get_instrument_lists(auth_token)
#'   print(instrument_lists)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_instrument_lists = function(auth_token) {
    # Define API URL
    url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/InstrumentLists"

    # Create request body
    body = list(
        "@odata.type" = "#DataScope.Select.Api.Extractions.SubjectLists.InstrumentList",
        "Name" = "GetByName"
    ) |> toJSON(auto_unbox=TRUE)

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token),
            "Prefer" = "odata.maxpagesize=10; respond-async"
        ) |>
        req_perform()

    # Process the response
    resp = resp |> resp_body_json(pretty=TRUE)

    # Convert the response to a data table
    dt_lists = rbindlist(resp$value, fill=TRUE)

    return(dt_lists)
}


#' Retrieve Items from a Specific Instrument List in Refinitiv DataScope Select API
#'
#' This function sends a `GET` request to the Refinitiv DataScope Select API
#' to retrieve items from a specific instrument list based on the provided
#' instrument list ID and authentication token. It processes the response
#' and returns the result as a data table.
#'
#' @param instrument_list_id A character string representing the ID of the
#' instrument list from which to retrieve items.
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the items from the specified instrument list.
#' @examples
#' \dontrun{
#'   instrument_list_id <- "'0x08532ebecc787a72'"
#'   auth_token <- "<your_auth_token>"
#'   instrument_items <- get_instrument_items(instrument_list_id, auth_token)
#'   print(instrument_items)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_instrument_items = function(instrument_list_id, auth_token) {
    # Define base API URL
    base_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/InstrumentLists"

    # Add single quotes around the instrument_list_id
    instrument_list_id_quoted = paste0("'", instrument_list_id, "'")

    # Construct the full URL
    url = paste0(base_url, "(", instrument_list_id_quoted, ")/Items")

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token)
        ) |>
        req_perform()

    # Process the response
    resp = resp |> resp_body_json(pretty=TRUE)

    # Convert the response to a data table
    dt_listinstruments = rbindlist(resp$value, fill=TRUE)

    return(dt_listinstruments)
}


#' Retrieve All Report Templates from Refinitiv DataScope Select API
#'
#' This function sends a `GET` request to the Refinitiv DataScope Select API
#' to retrieve all available report templates. It processes the response
#' and returns the result as a data table.
#'
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the available report templates.
#' @examples
#' \dontrun{
#'   auth_token <- "<your_auth_token>"
#'   report_templates <- get_report_templates(auth_token)
#'   print(report_templates)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_report_templates = function(auth_token) {
    # Define base API URL for Report Templates
    url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/ReportTemplates"

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token)
        ) |>
        req_perform()

    # Process the response
    resp = resp |> resp_body_json(pretty=TRUE)

    # Convert the response to a data table
    dt_report_templates = rbindlist(resp$value, fill=TRUE)

    return(dt_report_templates)
}

#' Retrieve Schedules for a Specific Report Template from Refinitiv DataScope Select API
#'
#' This function sends a `GET` request to the Refinitiv DataScope Select API
#' to retrieve the schedules associated with a specific report template,
#' based on the provided report template ID and authentication token.
#' It processes the response and returns the result as a data table.
#'
#' @param report_template_id A character string representing the ID of the
#' report template from which to retrieve schedules.
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the schedules for the specified report template.
#' @examples
#' \dontrun{
#'   report_template_id <- "'0x12345abcde'"
#'   auth_token <- "<your_auth_token>"
#'   report_schedules <- get_report_template_schedules(report_template_id, auth_token)
#'   print(report_schedules)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_report_template_schedules = function(report_template_id, auth_token) {
    # Define base API URL
    # Define base API URL
    base_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/ReportTemplates"

    # Add single quotes around the report_template_id
    report_template_id_quoted = paste0("'", report_template_id, "'")

    # Construct the full URL
    url = paste0(base_url, "(", report_template_id_quoted, ")/Schedules")

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token)
        ) |>
        req_perform()

    # Process the response
    resp_json = resp |> resp_body_json()

    # Extract relevant fields from the response
    schedules_data <- lapply(resp_json$value, function(schedule) {
        # Extract relevant fields from the nested structure
        list(
            ScheduleId = schedule$ScheduleId,
            Name = schedule$Name,
            OutputFileName = schedule$OutputFileName,
            TimeZone = schedule$TimeZone,
            Recurrence_Days = paste(schedule$Recurrence$Days, collapse = ", "),
            Trigger_LimitReportToTodaysData = schedule$Trigger$LimitReportToTodaysData,
            Trigger_Hour = schedule$Trigger$At[[1]]$Hour,
            Trigger_Minute = schedule$Trigger$At[[1]]$Minute,
            UserId = schedule$UserId,
            CreateDate = schedule$CreateDate,
            LastChangeDate = schedule$LastChangeDate,
            ListId = schedule$ListId,
            ReportTemplateId = schedule$ReportTemplateId
        )
    })

    # Convert the extracted data into a data table
    dt_report_schedules = rbindlist(schedules_data, fill = TRUE)

    return(dt_report_schedules)
}



#' Retrieve Last Extraction for a Specific Schedule from Refinitiv DataScope Select API
#'
#' This function sends a `GET` request to the Refinitiv DataScope Select API
#' to retrieve the last extraction for a specific schedule, based on the provided
#' schedule ID and authentication token. It processes the response and returns
#' the result as a data table.
#'
#' @param schedule_id A character string representing the ID of the schedule.
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the last extraction details for the specified schedule.
#' @examples
#' \dontrun{
#'   schedule_id <- "'0x05807049631b1f86'"
#'   auth_token <- "<your_auth_token>"
#'   last_extraction <- get_last_extraction(schedule_id, auth_token)
#'   print(last_extraction)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_report_lastextraction = function(schedule_id, auth_token) {
    # Define base API URL
    base_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/Schedules"

    # Add single quotes around the schedule_id
    schedule_id_quoted = paste0("'", schedule_id, "'")

    # Construct the full URL
    url = paste0(base_url, "(", schedule_id_quoted, ")/LastExtraction")

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token),
            "Prefer" = "respond-async"
        ) |>
        req_perform()

    # Process the response
    resp_json = resp |> resp_body_json()

    return(resp_json)
}


#' Retrieve Files for a Specific Report Extraction from Refinitiv DataScope Select API
#'
#' This function sends a `GET` request to the Refinitiv DataScope Select API
#' to retrieve the files associated with a specific report extraction,
#' based on the provided report extraction ID and authentication token.
#' It processes the response and returns the result as a data table.
#'
#' @param report_extraction_id A character string representing the ID of the
#' report extraction from which to retrieve files.
#' @param auth_token A character string representing the authentication token
#' obtained from the Refinitiv DataScope Select API.
#'
#' @return A data table containing the files for the specified report extraction.
#' @examples
#' \dontrun{
#'   report_extraction_id <- "'6651000'"
#'   auth_token <- "<your_auth_token>"
#'   report_files <- get_report_extraction_files(report_extraction_id, auth_token)
#'   print(report_files)
#' }
#' @import httr2
#' @import jsonlite
#' @import data.table
#' @export
datascope_report_extraction_files = function(report_extraction_id, auth_token, file_type = ".csv") {
    # Define base API URL
    base_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/ReportExtractions"

    # Add single quotes around the report_extraction_id
    report_extraction_id_quoted = paste0("'", report_extraction_id, "'")

    # Construct the full URL
    url = paste0(base_url, "(", report_extraction_id_quoted, ")/Files")

    # Send the request
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token)
        ) |>
        req_perform()

    # Process the response
    resp_json = resp |> resp_body_json()

    # Extract relevant fields from the response
    files_data <- list()

    # Loop through each element in resp_json$value
    for (i in seq_along(resp_json$value)) {
        file <- resp_json$value[[i]]

        # Append the file data to the list
        files_data[[i]] <- list(
            ExtractedFileId = file$ExtractedFileId,
            ExtractedFileName = file$ExtractedFileName,
            ScheduleId = file$ScheduleId,
            FileType = file$FileType,
            LastWriteTimeUtc = file$LastWriteTimeUtc
        )
    }

    # Convert the extracted data into a data table
    dt_report_files = rbindlist(files_data, fill = TRUE)

    # Filter by file type extension if specified
    if (!is.null(file_type)) {
        dt_report_files = dt_report_files[grepl(paste0("\\", file_type, "$"), ExtractedFileName)]
    }

    return(dt_report_files)
}


#' Download Extracted File from Refinitiv Datascope
#'
#' This function downloads an extracted file from the Refinitiv Datascope API using the provided
#' `extracted_file_id` and `auth_token`. The file is saved to the specified `save_path`.
#'
#' @param extracted_file_id Character. The unique identifier of the extracted file.
#' @param auth_token Character. The authentication token for accessing the API.
#' @param save_path Character. The local file path where the downloaded file will be saved.
#'
#' @return Character. The path where the file has been saved.
#'
#' @examples
#' \dontrun{
#' download_extracted_file("VjF8fDMzNjE5Mjk", "<your_auth_token>", "CM_STRIKE_PRICE_20250329.csv")
#' }
#'
#' @export
datascope_download_extracted_file = function(extracted_file_id, auth_token, filename = 'raw.csv') {
    # Define the API endpoint
    base_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/ExtractedFiles"
    url = paste0(base_url, "('", extracted_file_id, "')/$value")

    # Send the request to download the file
    resp = request(url) |>
        req_headers(
            "Authorization" = paste("Token", auth_token), # Authentication header
            "Prefer" = "respond-async" # API request preference
        ) |>
        req_perform()

    # Get the binary content of the response
    file_content = resp |> resp_body_raw()

    # Write the file to the specified path
    writeBin(file_content, filename)

    message(crayon::green(paste("✔", filename, "downloaded correctly")))
}

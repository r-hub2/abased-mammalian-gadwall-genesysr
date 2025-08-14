# Copyright 2018 Global Crop Diversity Trust
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Max pages to retrieve
#' @keywords internal
.MAX_ALLOWED_PAGES <- 500

#' Who am i? Loads and prints the user profile from Genesys as JSON.
#' You need to be logged in.
#'
#' @examples
#' \dontrun{
#'   # Login
#'   setup_production()
#'   user_login()
#'   me()
#' }
#' 
#' @export
me <- function() {
  resp <- .api_call(api2_url("/me/profile"))
  message(jsonlite::toJSON(jsonlite::fromJSON(resp), pretty = TRUE))
  invisible(resp)
}


#' Get one page of accession passport data
#' 
#' @return table
#' @keywords internal
#' @importFrom utils read.csv
.list_accessions_page <- function(filters, page = 0, size = 1000000, select = NULL) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  query <- list(p = page, l = size)
  if (is.null(select)) {
    stop("select is required")
  }
  resp <- .postForm(path = api2_url("/acn/query"), query = query, accept = "text/csv,*/*", mcpd = "true", filter = jsonlite::toJSON(filters, auto_unbox = TRUE), select = select)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- utils::read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  message(paste("Retrieved page", page + 1, "with", nrow(data), "rows in", end_time - start_time, "ms."))
  
  data
}

#' List of all supported passport data fields
#' @export
SELECT_ALL_FIELDS <- c(
  "INSTCODE", "ACCENUMB", "DOI", "HISTORIC", "CURATION",
  "GENUS", "SPECIES", "SPAUTHOR", "SUBTAXA", "SUBTAUTHOR", "GRIN_TAXON_ID", "GRIN_NAME", "GRIN_AUTHOR", "CROPNAME", "CROPCODE",
  "SAMPSTAT", "ACQDATE", "ACCENAME",
  "ORIGCTY", "COLLSITE", "DECLATITUDE", "DECLONGITUDE", "COORDUNCERT", "COORDDATUM", "GEOREFMETH", "ELEVATION",
  "COLLDATE", "COLLSRC", "COLLNUMB", "COLLCODE", "COLLNAME", "COLLINSTADDRESS", "COLLMISSID",
  "DONORCODE", "DONORNAME", "DONORNUMB",
  "OTHERNUMB",
  "BREDCODE", "BREDNAME", "ANCEST",
  "DUPLSITE", "STORAGE", "MLSSTAT",
  "ACCEURL", "REMARKS", "DATAPROVIDERID", "PDCI", "UUID", "LASTMODIFIED"
)

#' List of passport data fields relevant for GIS analysis
#' @export
SELECT_GEO_FIELDS <- c(
  "INSTCODE", "ACCENUMB", "DOI", "HISTORIC", 
  "GENUS", "SPECIES", "SUBTAXA",
  "SAMPSTAT",
  "ORIGCTY", "COLLSITE", "DECLATITUDE", "DECLONGITUDE", "COORDUNCERT", "COORDDATUM", "GEOREFMETH", "ELEVATION"
)

#' List of basic passport data fields
#' @export
SELECT_INFO_FIELDS <- c(
  "INSTCODE", "ACCENUMB", "DOI", "HISTORIC", 
  "GENUS", "SPECIES", "SUBTAXA",
  "SAMPSTAT"
)

#' Get accession passport data as a data table.
#'
#' @param filters an R \code{structure} with Genesys filters
#' @param fields list of fields to fetch from Genesys. See examples. The following constants are also available SELECT_INFO_FIELDS, SELECT_GEO_FIELDS and SELECT_INFO_FIELDS (default).
#'
#' @seealso \code{\link{mcpd_filter}}
#' 
#' @examples
#' \dontrun{
#'   # Retrieve all accession data by country of origin (Slovenia, Ivory Coast)
#'   accessions <- genesysr::get_accessions(list(countryOfOrigin = list(code3 = list("SVN", "CIV"))))
#'
#'   # Focus on geolocation data
#'   musa <- genesysr::get_accessions(
#'     list(taxonomy = list(genus = list("Musa"))),
#'     select = genesysr::SELECT_GEO_FIELDS
#'   )
#'
#'   # Fetch only accession number and coordinates of Musa accessions
#'   musa <- genesysr::get_accessions(
#'     list(taxonomy = list(genus = list("Musa"))),
#'     fields = c("ACCENUMB", "DECLATITUDE", "DECLONGITUDE")
#'   )
#' }
#' 
#' @export
#' @return Data table
get_accessions <- function(filters = NULL, fields = SELECT_INFO_FIELDS) {
  
  if (is.null(filters)) {
    message("Please specify the passport data filters to apply")
    stop("Please specify the passport data filters to apply")
  }

  message(paste("Downloading", fields, "filtered by", jsonlite::toJSON(filters, auto_unbox = TRUE)))
  
  select <- .fieldsToSelect(fields)
  # message(paste("Selected fields:", select))
  
  page <- 0
  size <- 1000000

  # Fetch first page to determine number of records
  data <- .list_accessions_page(filters, page, size, select)
  message(paste("Downloaded", nrow(data), "rows from Genesys."))

  while (page < .MAX_ALLOWED_PAGES) {
    page <- page + 1
    if (page >= .MAX_ALLOWED_PAGES) {
      # Break if over max pages
      message(paste("Not requesting data after page", .MAX_ALLOWED_PAGES, "Stopping."))
      break
    }
    p <- .list_accessions_page(filters, page, size, select)
    
    if (nrow(p) == 0) {
      # print("Got last page")
      break
    }
    
    data[setdiff(names(p), names(data))] <- NA
    p[setdiff(names(data), names(p))] <- NA
    data <- rbind(data, p)
    message(paste("Downloaded", nrow(data), "rows from Genesys."))
  }
  
  data
}

#' Map MCPD column names to their Genesys JSON equivalents
#' @keywords internal
.fieldsToSelect <- function(fields = NULL) {
  if (is.null(fields)) {
    stop("Fields must be provided")
  }

  SELECTFIELDS <- c(
    INSTCODE = "instituteCode", 
    ACCENUMB = "accessionNumber", 
    DOI = "doi", 
    HISTORIC = "historic", 
    CURATION = "curationType", 
    GENUS = "taxonomy.genus", 
    SPECIES = "taxonomy.species", 
    SPAUTHOR = "taxonomy.spAuthor", 
    SUBTAXA = "taxonomy.subtaxa", 
    SUBTAUTHOR = "taxonomy.subtAuthor", 
    GRIN_TAXON_ID = "taxonomy.currentTaxonomySpecies.id", 
    GRIN_NAME = "taxonomy.currentTaxonomySpecies.name", 
    GRIN_AUTHOR = "taxonomy.currentTaxonomySpecies.nameAuthority", 
    CROPNAME = "cropName", 
    CROPCODE = "crop.shortName", 
    SAMPSTAT = "sampStat", 
    ACQDATE = "acquisitionDate", 
    ACCENAME = "accessionName", 
    ORIGCTY = "origCty", 
    COLLSITE = "coll.collSite", 
    DECLATITUDE = "latitude", 
    DECLONGITUDE = "longitude", 
    COORDUNCERT = "coordinateUncertainty", 
    COORDDATUM = "coordinateDatum", 
    GEOREFMETH = "georeferenceMethod", 
    ELEVATION = "elevation", 
    COLLDATE = "coll.collDate", 
    COLLSRC = "coll.collSrc", 
    COLLNUMB = "coll.collNumb", 
    COLLCODE = "coll.collCode", 
    COLLNAME = "coll.collName", 
    COLLINSTADDRESS = "coll.collInstAddress", 
    COLLMISSID = "coll.collMissId", 
    DONORCODE = "donorCode", 
    DONORNAME = "donorName", 
    DONORNUMB = "donorNumb", 
    OTHERNUMB = "aliases.name", 
    BREDCODE = "breederCode", 
    BREDNAME = "breederName", 
    ANCEST = "ancest", 
    DUPLSITE = "duplSite", 
    STORAGE = "storage", 
    MLSSTAT = "mlsStatus", 
    ACCEURL = "acceUrl", 
    REMARKS = "remarks", 
    DATAPROVIDERID = "dataProviderId", 
    PDCI = "pdci.score", 
    UUID = "uuid", 
    LASTMODIFIED = "lastModifiedDate"
  )

  result <- sapply(fields, function(fieldName) paste(SELECTFIELDS[[fieldName]], fieldName), USE.NAMES = F)
  return(paste(result, collapse = ","))
}


#' Download passport data for one genebank in Excel format and save it to disk
#'
#' @param instituteCode FAO WIEWS institute code
#' @param file Target file name. Defaults to Genesys-provided file name in the current working directory.
#'
#' @examples
#' \dontrun{
#'   # Download MCPD passport data for NGA039
#'   excelFile <- download_mcpd("NGA039")
#' }
#' 
#' @export
#' @return The downloaded MCPD file name
download_mcpd <- function(instituteCode, file = NULL) {
  if (is.na(instituteCode)) {
    stop("instituteCode parameter is required")
  }
  if (is.null(file)) {
    file <- paste0("genesys-accessions-", instituteCode, ".xlsx")
  }
  if (file.exists(file)) {
    stop(paste("Target file", file, "exists. Refusing to overwrite."))
  }

  outputFile <- file(description = file, blocking = T, raw = T, open = "wb")

  req <- .api_request(
    method = "post",
    path = api2_url(paste0("/wiews/", instituteCode, "/download")),
  ) %>%
    httr2::req_body_form(mcpd = "mcpd");

  con <- req |> httr2::req_perform_connection()
  while (!httr2::resp_stream_is_complete(con)) {
    bytes <- con |> httr2::resp_stream_raw(2)
    cat(".")
    writeBin(bytes, outputFile)
  }
  close(con)
  close(outputFile)
  message("Done.")
  invisible(file)
}


#' Download PDCI data for one genebank in Excel format and save it to disk.
#'
#' @param instituteCode FAO WIEWS institute code
#' @param file Target file name. Defaults to Genesys-provided file name in the current working directory.
#'
#' @examples
#' \dontrun{
#'   # Download PDCI  data for NGA039
#'   excelData <- download_pdci("NGA039")
#' }
#' 
#' @export
#' @return The downloaded PDCI file name
download_pdci <- function(instituteCode, file = NULL) {
  if (is.na(instituteCode)) {
    stop("instituteCode parameter is required")
  }
  if (is.null(file)) {
    file <- paste0("genesys-pdci-", instituteCode, ".xlsx")
  }
  if (file.exists(file)) {
    stop(paste("Target file", file, "exists. Refusing to overwrite."))
  }
  
  outputFile <- file(description = file, blocking = T, raw = T, open = "wb")
  
  req <- .api_request(
    method = "post",
    path = api2_url(paste0("/wiews/", instituteCode, "/download")),
  ) %>%
    httr2::req_body_form(pdci = "pdci");
  
  con <- req |> httr2::req_perform_connection()
  while (!httr2::resp_stream_is_complete(con)) {
    bytes <- con |> httr2::resp_stream_raw(2)
    cat(".")
    writeBin(bytes, outputFile)
  }
  close(con)  
  close(outputFile)
  message("Done.")
  invisible(file)
}



#' Fetch Genesys crops. Note that the list of Genesys crops does not fully
#' correspond with various CROPNAME in MCPD provided by genebanks.
#'
#' @examples
#' \dontrun{
#'   # Retrieve all Genesys crops
#'   crops <- genesysr::list_crops()
#' }
#' 
#' @export
#' @return Genesys crops
list_crops <- function() {
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .api_call(path = api2_url("/crops/list"), query = list(l=1000), accept = "text/csv")
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved crops in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}



#' Fetch taxonomic data of selected accessions.
#'
#' @param filters an R \code{structure} with Genesys filters
#'
#' @examples
#' \dontrun{
#'   # Retrieve taxa of selected accessions
#'   taxa <- genesysr::list_species(mcpd_filter(INSTCODE = c("LBN002", "MEX002")))
#' }
#' 
#' @seealso \code{\link{mcpd_filter}}
#'
#' @export
#' @return Taxonomic records of selected accessions
list_species <- function(filters = list()) {
  
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path = api2_url("/acn/species"), body = filters, accept = "text/csv")
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved taxonomic data in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}

#' Fetch a page of data from Genesys
#' 
#' @param path API path
#' @param filters Filters
#' @param accept Accepted content type
#' @param page Page to request
#' @param size Size of page
#' 
#' @keywords internal
.fetch_csv_page <- function(path, filters = list(), accept = "text/csv", page = 0, size = 1000) {
  start_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  resp <- .post(path, query = list(l = size, p = page), body = filters, accept = accept)
  end_time <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  message(paste("Retrieved institute data in", end_time - start_time, "ms."))
  
  if (nchar(trimws(resp)) == 0) {
    message(paste("Received 0 bytes"))
    data <- data.frame()
  } else {
    data <- read.csv(text = resp, quote = '"', sep = '\t', stringsAsFactors = FALSE)
  }
  data
}

#' List FAO WIEWS institutes.
#'
#' Institute filters:
#' - code: list of WIEWS institute codes
#' - accessions: boolean, TRUE list only institutes with accessions in Genesys, FALSE without accessions
#' - country$code3: list of ISO3166 country codes
#'
#' @param filters an R \code{structure} with Institute filters
#' @param at.least stop fetching when at.least records are received from Genesys
#'
#' @examples
#' \dontrun{
#'   # Retrieve taxa of selected accessions
#'   filters <- c();
#'   filters$accessions = TRUE; # Has accessions in Genesys
#'   institutes <- genesysr::list_institutes(filters)
#' }
#' 
#' @seealso \code{\link{mcpd_filter}}
#'
#' @export
#' @return List of institutes
list_institutes <- function(filters = list(), at.least = NULL) {

  path <- api2_url("/wiews/list")
  
  # Fetch first page to determine number of records
  data <- .fetch_csv_page(path, filters, page = 0, size = 100)
  pages <- .MAX_ALLOWED_PAGES
  
  for (page in 1:pages) {
    if (page > .MAX_ALLOWED_PAGES) {
      # Break if over max pages
      message(paste("Not requesting data after page", .MAX_ALLOWED_PAGES, "Stopping."))
      break
    }
    p <- .fetch_csv_page(path, filters, page = page, size = 100)

    if (nrow(p) == 0) {
      # print("Got last page")
      break
    }
    
    data[setdiff(names(p), names(data))] <- NA
    p[setdiff(names(data), names(p))] <- NA
    data <- rbind(data, p)

    if (length(p) == 0) {
      # print("Got last page")
      break
    }
    if (! is.null(at.least) && at.least <= length(data)) {
      message(paste("Received", length(data), "of", at.least, "requested. Stopping."))
      break
    }
  }

  data
}



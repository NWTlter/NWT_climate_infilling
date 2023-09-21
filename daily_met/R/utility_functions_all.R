## ----- UTILITY FUNCTIONS ----
## Need to improve documentation and consider splitting into multiple files?
findNonNumeric<-function(x){
  unique(suppressWarnings(x[is.na(as.numeric(x))]))
}

#borrowed from metajam
tabularize_eml <- function(eml, full = FALSE) {
  
  if (any(class(eml) == "emld")) {
    eml <- eml
  } else if (is.character(eml) | is.raw(eml)) {
    eml <- emld::as_emld(eml)
  } else {
    stop("The EML input could not be parsed.")
  }
  
  metadata <- eml %>%
    unlist() %>%
    tibble::enframe()
  
  if (full == FALSE) {
    metadata <- metadata %>%
      dplyr::mutate(name = dplyr::case_when(
        grepl("schemaLocation", name) ~ "eml.version",
        grepl("title", name) ~ "title",
        grepl("individualName", name) ~ "people",
        grepl("abstract", name) ~ "abstract",
        grepl("keyword", name) ~ "keyword",
        grepl("geographicDescription", name) ~ "geographicCoverage.geographicDescription",
        grepl("westBoundingCoordinate", name) ~ "geographicCoverage.westBoundingCoordinate",
        grepl("eastBoundingCoordinate", name) ~ "geographicCoverage.eastBoundingCoordinate",
        grepl("northBoundingCoordinate", name) ~ "geographicCoverage.northBoundingCoordinate",
        grepl("southBoundingCoordinate", name) ~ "geographicCoverage.southBoundingCoordinate",
        grepl("beginDate", name) ~ "temporalCoverage.beginDate",
        grepl("endDate", name) ~ "temporalCoverage.endDate",
        grepl("taxonRankValue", name) ~ "taxonomicCoverage",
        grepl("methods", name) ~ "methods",
        grepl("objectName", name) ~ "objectName",
        grepl("online.url", name) ~ "url"
      )) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::mutate(value = stringr::str_trim(value)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(name) %>%
      dplyr::summarize(value = paste(value, collapse = "; ")) %>%
      dplyr::mutate(value = gsub("\n", "", value)) #without this, fields get truncated in Excel
  }
  
  return(metadata)
}

#function to determine current version of data package on EDI
getCurrentVersion<-function(edi_id){
  require(magrittr)
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id), warn=FALSE)%>%
    as.numeric()%>%(max)
  packageid=paste0('knb-lter-nwt.', edi_id, '.', versions)
  return (packageid)
}

#function to download the EML file from EDI
getEML<-function(packageid){
  require(magrittr)
  myurl<-paste0("https://portal.lternet.edu/nis/metadataviewer?packageid=",
                packageid,
                "&contentType=application/xml")
  #myeml<-xml2::download_html(myurl)%>%xml2::read_xml()%>%EML::read_eml()
  myeml<-xml2::read_xml(paste0("https://portal.lternet.edu/nis/metadataviewer?packageid=",
                               packageid,
                               "&contentType=application/xml"))%>%EML::read_eml()
}

#function to get a single element anywhere in an eml
eml_get_simple <- function(x, element, from = "list", ...){
  doc <- as.character(emld::as_json(emld::as_emld(x, from = from)))
  out <- jqr::jq(doc, paste0("..|.", element, "? // empty"))
  json <- jqr::combine(out)
  robj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  return(robj)
}

#function for plot aesthetics
use_theme = function(){ 
  theme_bw()+
    theme(
      #this is size of the font on the panels
      strip.text.y = element_text(size = 3),
      #yaxis font size
      axis.text.y = element_text(size = 4),
      #axis.title.y = element_blank(),
      axis.text.x=element_text(angle=-90)
    )
}

# function to determine data package version number only (not return full data package ID)
getPackageVersion<-function(edi_id, site = "nwt"){
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-', site, '/', edi_id), warn=FALSE)
  currentV <- max(as.numeric(versions))
  return(currentV)
}

# function to get entity ID for current data package version
getEntityId <- function(edi_id, version, site = "nwt", datanum = 1){
  entID <- readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-', site, '/', edi_id, "/", version, "/"), warn=FALSE)[datanum]
  entID <- gsub(paste0("http.*/",edi_id,"/",version,"/"), "", entID) # remove all chars except what comes after last /
  return(entID)
}

# function to read in tabular csv dataset for data package (could make more generic with read table, but should know what you're reading in to use)
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " "), col_class = NULL, site = "nwt", datanum = 1){
  require(readr)
  v <- getPackageVersion(edi_id, site = site)
  id <- getEntityId(edi_id, v, site = site, datanum = datanum)
  dat <- readr::read_csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-", site, ".", edi_id, ".", v, 
                         "&entityid=", id),
                trim_ws =TRUE, na = na_vals, col_types = col_class,
                guess_max = 100000)
  dat <- as.data.frame(dat)
  print(paste0("Reading in knb-lter-", site, ".", edi_id, ".", v))
  return(dat)
}

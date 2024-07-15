# functions to read NWT and neighbor datasets dynamically

# ------------------------------------------------------------------------------
# Function to fetch Ameriflux Data via library(amerfluxr)
# ------------------------------------------------------------------------------

fetchAmeriFlux <- function(output_directory = getwd(), 
                       save_stacked_datafile = TRUE,
                       ameriflux_email = NULL,
                       ameriflux_user_id = NULL){
  #' Fetch AmeriFlux Data
  #'
  #' This function fetches AmeriFlux data using the amerfluxr package and does
  #' some light processing to stack them together in a particular way.
  #'
  #' @param output_directory A character string specifying the directory to save
  #'                        the files. Defaults to the current working directory.
  #' @param save_stacked_datafile A logical indicating whether to save the 
  #'                              stacked dataframe. Defaults to TRUE.
  #'
  #' @return A dataframe containing the stacked data from all specified stations.
  #' 
  #' 
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(purrr)
  require(lubridate)
  require(amerifluxr)
  
  if(is.null(ameriflux_user_id)) {
    stop('Please provide a username associated with an AmeriFlux account.')
  }
  if(is.null(ameriflux_email)) {
    stop('Please provide an email associated with an AmeriFlux account.')
  }
  cat('Downloading Ameriflux US-NR1, US-NR3, and US-NR4...\n')
  flocs <- amerifluxr::amf_download_base(user_id = ameriflux_user_id,
                             user_email = ameriflux_email,
                             site_id = c("US-NR1", "US-NR3", "US-NR4"),
                             data_product = "BASE-BADM",
                             data_policy = "CCBY4.0",
                             agree_policy = TRUE,
                             intended_use = "synthesis",
                             intended_use_text = "Infilling nearby climatology data",
                             verbose = TRUE,
                             out_dir = output_directory)
  cat('Files saved to:', output_directory, '\n')
  
  cat('Unzipping...')
  extract_file_paths <- lapply(flocs, function(x){
    unzip(x ,overwrite = TRUE, exdir = output_directory)
    })
  cat('Done.\n')
  
  cat('Reading in Ameriflux data and joining with metadata...')
  flux_files <- grep(".csv", 
                     list.files(output_directory, full.names = TRUE),
                     value = TRUE) |> unlist()
  names(flux_files) <- stringr::str_extract(flux_files, pattern = "US-NR[0-9]")
  
  flux_data <- lapply(
    flux_files,
    function(x){
      df <- read.csv(x, skip =2, colClasses = "character", na.strings = na_vals)
      return(df)
    })
  
  # append site info to data (metadata spreadsheet should be in same folder, if not ignore)
  fluxmeta <- lapply(output_directory, function(x) list.files(x, pattern = "[.]xl", 
                                                         full.names = T)) |> 
    unlist()
  names(fluxmeta) <- names(flux_files)

  # read in metadata via readxl and trim to site info and location metadata
  metalist <- lapply(fluxmeta, function(x) readxl::read_excel(x, trim_ws = T))
  metalist <- lapply(metalist, function(x) {
    subset(x, 
           grepl("elev$|lat$|long$|\\bsite_name", VARIABLE, ignore.case = T),
           select = c(SITE_ID, VARIABLE, DATAVALUE))})
  
  # metalist <- lapply(metalist, function(x) tidyr::pivot_wider(x[[1]], SITE_ID, VARIABLE, DATAVALUE))
  metalist <- purrr::map(metalist, function(df) {
    df |> 
      tidyr::pivot_wider(names_from = VARIABLE, values_from = DATAVALUE)
  })
  
  # column-bind  metadata to datlist data frames
  for(i in names(flux_data)){
    flux_data[[i]] <- cbind(data.frame(metalist[[i]]), data.frame(flux_data[[i]]))
  }
  cat('Done.\n')
  # return Ameriflux data with site metadata as leading columns
  return(flux_data)
  
}


# ------------------------------------------------------------------------------
# Function to fetch GHCN Data
# ------------------------------------------------------------------------------

fetchGHCND <- function(output_directory = getwd(), 
                        save_individual_files = TRUE,
                        save_stacked_datafile = TRUE){
  #' Fetch GHCND Data
  #'
  #' This function fetches data from the Global Historical Climatology Network Daily 
  #' (GHCND) for specified stations. It saves individual dataframes and/or the 
  #' stacked dataframe to the specified output directory & always returns the 
  #' stacked dataframe.
  #'
  #' @param output_directory A character string specifying the directory to save
  #'                        the files. Defaults to the current working directory.
  #' @param save_individual_files A logical indicating whether to save individual 
  #'                              dataframes. Defaults to TRUE.
  #' @param save_stacked_datafile A logical indicating whether to save the 
  #'                              stacked dataframe. Defaults to TRUE.
  #'
  #' @return A dataframe containing the stacked data from all specified stations.
  #' 
  #' 
  require(dplyr)
  require(lubridate)
  
  # Define the base URL and station list
  ghcnd_https <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"
  station_list <- c(
    "USW00094075", #BOULDER 14 W, CO US; 40.0354N	-105.541W	2995.6msl
    "USC00051681", #COAL CREEK CANYON, CO US, 39.8958	-105.385	2728
    "USC00052761", #ESTES PARK 3 SSE, CO US; 40.34875	-105.52	2381.1
    "USC00053116", #FRASER, CO US; 39.9425	-105.817	2609.1
    "USC00053496", #GRAND LAKE 1 NW, CO US; 40.2669	-105.832	2657.9
    "USC00053500", #GRAND LAKE 6 SSW, CO US; 40.1849	-105.867	2526.2
    "USC00059175", #WINTER PARK, CO US; 39.8898	-105.762	2772.2
    "USC00055878", # NEDERLAND 2 NNE",CO,"39.9833",-105.5000
    "USC00050183" # ALLENSPARK 2SE",CO,"40.1881",-105.5019
  )
  
  # Initialize a list to store individual dataframes
  data_stack <- lapply(station_list, function(station){
    cat("Reading in", station, "..." )
    df <- read.csv(paste0(ghcnd_https, station, ".csv")) |> 
      dplyr::mutate(DATE = lubridate::ymd(DATE))
    cat(' Done.\n')
    
    # Save individual dataframes if the flag is set
    if (save_individual_files) {
      write.csv(df, file.path(output_directory, paste0(station, ".csv")),
                row.names = FALSE)
    }
    
    return(df)
  }) 
  
  cat('Returning Stacked Data!\n')
  # Bind individual dataframes into a single dataframe
  data_stack <- dplyr::bind_rows(data_stack)
  
  # Save the stacked dataframe if the flag is set
  if (save_stacked_datafile) {
    write.csv(data_stack, file.path(output_directory, "ghcnd_all_data_stacked.csv"),
              row.names = FALSE)
  }
  
  return(data_stack)
}

# == FOR NWT DATA =====

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


# function to read a list of edi nwt package numbers and return data in list
getNWTdatlist <- function(datnums){
  # read a list of package numbers as list of data tables
  datlist <- lapply(datnums, function(x) getTabular(x))
  return(datlist)
}


# function to compile NWT raw chart ongoing climate data
getNWTcharts <- function(sites = c("C1", "SDL", "D1"), mets = c("temp", "ppt")){
  
  # specify data package numbers for each site
  datnums <- list(
    # daily air temperature in C
    C1temp = 411,
    SDLtemp = 413,
    D1temp = 412,
    # daily precipitation in mm
    C1ppt = 414,
    SDLppt = 416,
    D1ppt = 415
  )
  
  # pare datnums to sites and mets of interest
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"), names(datnums))]
  datnums <- datnums[grepl(stringi::stri_flatten(mets, collapse = "|"), names(datnums))]
  
  # read data to list
  getNWTdatlist(datnums)
  
}

# function to compile NWT infilled climate data
getNWTchartsinfilled <- function(sites = c("C1", "D1"), mets = c("temp", "ppt")){
  
  # specify data package numbers for each site
  datnums <- list(C1temp_infilled = 185,
                  D1temp_infilled = 187,
                  C1ppt_infilled = 184,
                  D1ppt_infilled = 186
                    
  )
  # pare datnums to sites and mets of interest
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"), names(datnums))]
  datnums <- datnums[grepl(stringi::stri_flatten(mets, collapse = "|"), names(datnums))]
  
  # read data to list and return
  getNWTdatlist(datnums)
}


# function to compile NWT raw data logger daily climate data
getNWTdailyloggers <- function(sites = c("C1", "SDL", "GL4", "D1"), startyr = 1986, endyr = NA){
  
  # store current year
  currentyr <- lubridate::year(Sys.Date())
  
  # assign current year if end date not specified
  if(is.na(endyr)){
    endyr = currentyr
  }
  
  # specify data package numbers for each site
  datnums <- list(C121x = 400,
                  SDL21x = 78,
                  D121x = 70,
                  C123x = 401,
                  SDL23x = 405,
                  D123x = 402,
                  GL4 = 148 #cr10 (1997-2013), cr1000 (2013-2017), cr850 (2017-2019)
                  
  )
  # pare datnums by years to search (all or only 2000 onwards)
  if(startyr>1999){
    # remove 21x from datnums
    datnums <- datnums[!grepl("21x", names(datnums))]
  }
  
  # select sites user specified
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"),names(datnums))]
  
  # read data to list and return
  getNWTdatlist(datnums)
}




# function to read NWT hourly infilled logger data (only one dataset -- all sites in one)
getNWThrlyinfilled <- function(sites = c("C1", "SDL", "D1")){
  
  # only one hourly infilled dataset (has all sites in one)
  dat <- getTabular(168)
  return(dat)
  
}


#c1aspirated <- getTabular(252)



# == FOR SNOTEL DATA ======
# function to read in snotel sites near NWT based on NRCS report generator url (v 2.0)
# > bc url, this may break in the future (go to report generator, select these sites, vars and flags, and use the text-file looking https that pops up when loaded)


getSnotelNeighbors <- function(sites = c("Niwot", "UniversityCamp", "LakeEldora", "HighLonesome", "Sawtooth")){
  
  # snotel sites near or at NWT LTER
  co_snotel_sites <- c(Niwot = 663, UniversityCamp = 838, LakeEldora = 564, HighLonesome = 1187, Sawtooth = 1251)
  
  # parts of a very long url...
  # basic type of report requested (use metric units, daily summaries, from start of record for each station)
  snotelreport <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultipleStationReport,metric/daily/start_of_period/"
  # sites to report
  # snotelsites <- "564:CO:SNTL%7C663:CO:SNTL%7C838:CO:SNTL%7C1251:CO:SNTL%7C1187:CO:SNTL%7Cid=%22%22%7Cname/"
  # snotelsites <- c("564:CO:SNTLCid=%22%22%7Cname/", "663:CO:SNTL%7Cid=%22%22%7Cname/", 
  #            "838:CO:SNTLCid=%22%22%7Cname/","1251:CO:SNTL%7Cid=%22%22%7Cname/",
  #            "1187:CO:SNTLCid=%22%22%7Cname/")
  
  # site metadata, observational data, and data flags requested
  # > could mod function to gsub out variables not interested in, but read in all for now
  snoteldats <- "POR_BEGIN,POR_END/stationId,name,elevation,latitude,longitude,PRCP::value,PRCP::qcFlag,PRCP::qaFlag,PRCPSA::value,PRCPSA::qcFlag,PRCPSA::qaFlag,TAVG::value,TAVG::qcFlag,TAVG::qaFlag,TMAX::value,TMAX::qcFlag,TMAX::qaFlag,TMIN::value,TMIN::qcFlag,TMIN::qaFlag?fitToScreen=false"
  
  # sub out any sites not interested in
  removesites <- co_snotel_sites[!names(co_snotel_sites) %in% sites]
  # if any sites named, remove from url string
  if(length(removesites) > 0){
    removesites <- paste0(removesites, ":CO:SNTL%7C")
    removesites <- stringr::str_flatten(removesites, collapse = "|")
    snotelsites <- gsub(removesites, "", snotelsites)
  }
  
  snoteldat <- lapply(co_snotel_sites, function(s){
    print(paste('Reading in SNOTEL Data from site', s, "..."))
    site <- paste0(s, ":CO:SNTL%7Cid=%22%22%7Cname/")
    # put parts of url together in order: report type, sites, data requested
    url <- paste0(snotelreport, site, snoteldats)
    # read in data
    dat <- read.csv(url, comment.char = "#", na.strings = c(NA, "NA", "", " "), strip.white = T, blank.lines.skip = T, stringsAsFactors = F)

    return(dat)
  }) |> dplyr::bind_rows()

  return(snoteldat)
}


# Snotel flag notes:
# Quality Control flags included:
#
# Flag    Name                Description
#  V      Valid               Validated Data
#  N      No Profile          No profile for automated validation
#  E      Edit                Edit, minor adjustment for sensor noise
#  B      Back Estimate       Regression-based estimate for homogenizing collocated Snow Course and Snow Pillow data sets
#  K      Estimate            Estimate
#  X      External Estimate   External estimate
#  S      Suspect             Suspect data
# 
# Quality Assurance flags included:
#
# Flag    Name                Description
#  U      Unknown             Unknown
#  R      Raw                 No Human Review
#  P      Provisional         Preliminary Human Review
#  A      Approved            Processing and Final Review Completed


# -- FOR TIDY CLIMATE DATA -----
# to read in climate data prepped in step 1 (not raw data, screened and formatted in pckg way)
get_tidydat <- function(filestring, rdsfiles, met){
  
  # flatten string if more than one
  if(length(met) > 1){
    met <- stringr::str_flatten(met, collapse = "|")
  }
  
  # read in data
  dat <- readRDS(rdsfiles[grepl(filestring, rdsfiles, ignore.case = T)])
  # subset just metric(s) of interest
  dat <- subset(dat, grepl(met, metric))
  return(dat)
}


# ----------------------------------------
# OLDER VERSIONS OF FUNCTIONS (save code)

# function to read in snotel sites near NWT based on urls (CTW can't figure out url pattern re: 05J##S, ## doesn't correspond to site ID)
# > meaning: urls may break in future, if so use snotelr package
# > why retired: CTW figured out how to modify reportGenerator url to read in data -- with flagging!
# getSnotelNeighbors <- function(sites = c("Niwot", "UniversityCamp","Sawtooth", "HighLonesome", "LakeEldora")){
#   
#   # store urls for nearby snotel sites in list
#   snotelpath <- list(Niwot = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J42S-NIWOT",
#                      UniversityCamp = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J08S-UNIVERSITY+CAMP",
#                      Sawtooth = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J45S-SAWTOOTH",
#                      HighLonesome = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J46S-HIGH+LONESOME",
#                      LakeEldora = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J41S-LAKE+ELDORA"
#   )
#   
#   # select sites specified by user
#   snotelpath <- snotelpath[sites]
#   
#   # read data to list
#   snoteldat <- lapply(snotelpath, function(x) read.csv(x, header = T, comment.char = "#", strip.white = T, blank.lines.skip = T))
#   
#   return(snoteldat)
# }
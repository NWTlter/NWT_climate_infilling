# functions to read NWT and neighbor datasets dynamically



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
  snotelsites <- "564:CO:SNTL%7C663:CO:SNTL%7C838:CO:SNTL%7C1251:CO:SNTL%7C1187:CO:SNTL%7Cid=%22%22%7Cname/"
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
  
  # put parts of url together in order: report type, sites, data requested
  snotelurl <- paste0(snotelreport, snotelsites, snoteldats)
  # read in data
  snoteldat <- read.csv(snotelurl, comment.char = "#", na.strings = c(NA, "NA", "", " "), strip.white = T, blank.lines.skip = T, stringsAsFactors = F)
  
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
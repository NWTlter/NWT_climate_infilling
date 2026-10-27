# Provenance metadata (LTER EML best practice: one methodStep/dataSource per source dataset)
# for the six infilled packages, written to provenance/knb-lter-nwt.<id>_provenance.txt for
# pasting into each package's EML methods. Run after 6_build_full_record.R, from the repo root.
#
# Sources per package: the target's own raw data, the previous version of the package (its
# rows are kept as posted), and every source_station code in the 1980+ rows of the
# full-record file. Sources used only for QC comparisons are not listed.
#   NWT packages   title, creators, contacts from the newest EML revision on EDI
#   GHCN-Daily     NCEI's ISO 19115 record (title, DOI, authors, point of contact)
#   AmeriFlux      the BIF files fetchAmeriFlux() downloads (DOI, citation, authors)
#   SNOTEL, NADP   written here (no machine-readable dataset record)

rm(list=ls())
library(xml2)
fr <- "daily_met/Infilling_2025/data/publish/full_record/"
fluxpath <- "daily_met/Infilling_2025/data/raw/AmeriFlux/"
out <- "daily_met/Infilling_2025/provenance/"
dir.create(out, showWarnings = FALSE)

esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
ind <- function(x, n) gsub("\n", paste0("\n", strrep(" ", n)), x)

# -- EDI sources: title, creators and contacts from the newest EML -----
# (the scripts read the newest revision too; if a source was revised after the run, set rev by hand)
edi_source <- function(id) {
  rev <- EDIutils::list_data_package_revisions("knb-lter-nwt", id, filter = "newest")
  x <- EDIutils::read_metadata(paste0("knb-lter-nwt.", id, ".", rev))
  keep <- "./individualName|./organizationName|./positionName|./electronicMailAddress|./userId"
  party <- function(node, tag) {
    kids <- xml_find_all(node, keep)
    paste0("<", tag, ">\n", paste0("   ", ind(sapply(kids, as.character), 3), collapse = "\n"), "\n</", tag, ">")
  }
  list(title = trimws(xml_text(xml_find_first(x, "./dataset/title"))),
       creators = sapply(xml_find_all(x, "./dataset/creator"), party, "creator"),
       contacts = sapply(xml_find_all(x, "./dataset/contact"), party, "contact"),
       url = paste0("https://pasta.lternet.edu/package/metadata/eml/knb-lter-nwt/", id, "/", rev),
       desc = "This online link references an EML document that describes data used in the creation of this derivative data package.")
}

# -- non-EDI sources -----
org <- function(tag, name, email = NULL, url = NULL) {
  paste0("<", tag, ">\n   <organizationName>", esc(name), "</organizationName>",
         if (!is.null(email)) paste0("\n   <electronicMailAddress>", email, "</electronicMailAddress>"),
         if (!is.null(url)) paste0("\n   <onlineUrl>", url, "</onlineUrl>"), "\n</", tag, ">")
}
person <- function(tag, given, sur, email = NULL, orgname = NULL, orcid = NULL) {
  paste0("<", tag, ">\n   <individualName>\n      <givenName>", given, "</givenName>\n      <surName>", sur, "</surName>\n   </individualName>",
         if (!is.null(orgname)) paste0("\n   <organizationName>", orgname, "</organizationName>"),
         if (!is.null(email)) paste0("\n   <electronicMailAddress>", email, "</electronicMailAddress>"),
         if (!is.null(orcid)) paste0("\n   <userId directory=\"https://orcid.org\">https://orcid.org/", orcid, "</userId>"),
         "\n</", tag, ">")
}
ext_desc <- "This online link references the data source used in the creation of this derivative data package."

ghcnd_names <- c(USC00050183 = "Allenspark 2 SE", USC00051681 = "Coal Creek Canyon", USC00052761 = "Estes Park 3 SSE",
                 USC00053116 = "Fraser", USC00053496 = "Grand Lake 1 NW", USC00053500 = "Grand Lake 6 SSW",
                 USC00055878 = "Nederland 2 NNE", USC00059175 = "Winter Park", USW00094075 = "Boulder 14 W")
# GHCN-Daily: title, DOI, authors and point of contact from NCEI's ISO 19115 record
ghcnd_iso_url <- "https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/xml"
ghcnd_iso <- read_xml(ghcnd_iso_url)
ghcnd_ns <- xml_ns(ghcnd_iso)
iso_txt <- function(n, p) trimws(xml_text(xml_find_all(n, p, ghcnd_ns)))
iso_party <- function(rp, tag) {
  nm <- iso_txt(rp, "./gmd:individualName/*")
  orgname <- iso_txt(rp, "./gmd:organisationName/*")
  pos <- iso_txt(rp, "./gmd:positionName/*")
  email <- iso_txt(rp, ".//gmd:electronicMailAddress/*")
  # ISO "Menne, Matthew J." -> EML givenName / surName
  ind_name <- if (length(nm)) paste0("\n   <individualName>\n      <givenName>", sub("^[^,]+,\\s*", "", nm),
                                     "</givenName>\n      <surName>", sub(",.*$", "", nm), "</surName>\n   </individualName>")
  paste0("<", tag, ">", ind_name,
         if (length(orgname)) paste0("\n   <organizationName>", esc(orgname), "</organizationName>"),
         if (length(pos)) paste0("\n   <positionName>", esc(pos), "</positionName>"),
         if (length(email)) paste0("\n   <electronicMailAddress>", email[1], "</electronicMailAddress>"),
         "\n</", tag, ">")
}
ghcnd_cit <- xml_find_first(ghcnd_iso, "//gmd:identificationInfo/*/gmd:citation/gmd:CI_Citation", ghcnd_ns)
ghcnd_role <- function(rp) xml_attr(xml_find_first(rp, "./gmd:role/*", ghcnd_ns), "codeListValue")
ghcnd_authors <- Filter(function(rp) ghcnd_role(rp) == "author",
                        xml_find_all(ghcnd_cit, "./gmd:citedResponsibleParty/gmd:CI_ResponsibleParty", ghcnd_ns))
ghcnd_poc <- xml_find_all(ghcnd_iso, "//gmd:identificationInfo/*/gmd:pointOfContact/gmd:CI_ResponsibleParty", ghcnd_ns)
ghcnd_doi <- sub("^doi:", "", grep("^doi:", iso_txt(ghcnd_cit, ".//gmd:identifier//gmd:code/*"), value = TRUE))
ghcnd_source <- function(stations) list(
  title = paste0(iso_txt(ghcnd_cit, "./gmd:title/*"), ". Stations: ",
                 paste0(stations, " (", ghcnd_names[stations], ")", collapse = ", ")),
  creators = sapply(ghcnd_authors, iso_party, "creator"),
  contacts = sapply(ghcnd_poc, iso_party, "contact"),
  url = paste0("https://doi.org/", ghcnd_doi), desc = ext_desc)

snotel_source <- function(sites) list(
  title = paste0("NRCS SNOTEL daily air temperature and precipitation, Colorado. Sites: ", paste(sites, collapse = ", ")),
  creators = org("creator", "USDA Natural Resources Conservation Service, National Water and Climate Center"),
  contacts = org("contact", "USDA Natural Resources Conservation Service, National Water and Climate Center", url = "https://www.nrcs.usda.gov/wps/portal/wcc/home/"),
  url = "https://wcc.sc.egov.usda.gov/reportGenerator/", desc = ext_desc)

# AmeriFlux: DOI, citation (title and version) and authors from each site's BIF file
amf_source <- function(site) {
  bif <- as.data.frame(readxl::read_excel(list.files(fluxpath, paste0("AMF_", site, "_BIF.*xlsx"), full.names = TRUE)))
  val <- function(var, grp = bif$GROUP_ID) bif$DATAVALUE[bif$VARIABLE == var & bif$GROUP_ID %in% grp]
  doi_grp <- bif$GROUP_ID[bif$VARIABLE == "DOI_DATAPRODUCT" & bif$DATAVALUE == "AmeriFlux"]
  # e.g. "John Knowles (2025), AmeriFlux BASE US-NR3 Niwot Ridge Alpine (T-Van West), Ver. 6-5, AmeriFlux AMP, (Dataset). https://..."
  title <- sub("^.*\\([0-9]{4}\\), (.*?), AmeriFlux AMP.*$", "\\1", val("DOI_CITATION", doi_grp))
  grps <- bif$GROUP_ID[bif$VARIABLE == "DOI_CONTRIBUTOR_DATAPRODUCT" & bif$DATAVALUE == "AmeriFlux"]
  grps <- grps[order(as.numeric(sapply(grps, function(g) { o <- val("DOI_CONTRIBUTOR_ORDINAL", g); if (length(o)) o else Inf })))]
  people <- function(tag, gs) sapply(gs, function(g) {
    nm <- val("DOI_CONTRIBUTOR_NAME", g)
    or <- val("DOI_CONTRIBUTOR_ORCID", g)
    em <- val("DOI_CONTRIBUTOR_EMAIL", g)
    person(tag, sub(" [^ ]+$", "", nm), sub("^.* ", "", nm), if (length(em)) em,
           val("DOI_CONTRIBUTOR_INSTITUTION", g), if (length(or)) or)
  })
  list(title = title, creators = people("creator", grps), contacts = people("contact", grps[1]),
       url = paste0("https://doi.org/", val("DOI", doi_grp)), desc = ext_desc)
}

npn_source <- list(
  title = "National Atmospheric Deposition Program (NRSP-3), daily precipitation, site CO90 (Niwot Ridge, ETI gauge at C1)",
  creators = org("creator", "National Atmospheric Deposition Program (NRSP-3)"),
  contacts = org("contact", "NADP Program Office, Wisconsin State Laboratory of Hygiene", "nadp@slh.wisc.edu", "https://nadp.slh.wisc.edu/"),
  url = "https://nadp.slh.wisc.edu/networks/nadp-precipitation-network/", desc = ext_desc)

# -- source_station codes -> NWT EDI package -----
nwt_pkg <- function(code) {
  case_when <- function(...) dplyr::case_when(...)
  case_when(code == "C1 chart" ~ 411, code == "D1 chart" ~ 412, code == "SDL chart" ~ 413,
            code %in% c("C1", "C1 Belfort Shielded") ~ 414, code %in% c("D1", "D1 Belfort Shielded") ~ 415,
            code %in% c("SDL", "SDL Belfort Shielded") ~ 416,
            code == "C1 logger CR21X" ~ 400, grepl("^C1 logger", code) ~ 401,
            code == "D1 logger CR21X" ~ 70, grepl("^D1 logger", code) ~ 402,
            code == "SDL logger CR21X" ~ 78, grepl("^SDL logger", code) ~ 405,
            grepl("^GL4 logger", code) ~ 148, TRUE ~ NA_real_)
}

pkgs <- list(
  list(id = 185, file = "c1_infilled_temp_daily.tk.data.csv", own = c(411, 185)),
  list(id = 187, file = "d1_infilled_temp_daily.tk.data.csv", own = c(412, 187)),
  list(id = 314, file = "sdl_daily_airtemp_gapfilled.cw.data.csv", own = c(314, 405)),
  list(id = 184, file = "c1_infilled_precip_daily.tk.data.csv", own = c(414, 184)),
  list(id = 186, file = "d1_infilled_precip_daily.tk.data.csv", own = c(415, 186)),
  list(id = 315, file = "sdl_daily_precip_gapfilled.cw.data.csv", own = 416))

data_source <- function(s) paste0(
  "<dataSource>\n<title>", esc(s$title), "</title>\n",
  paste(s$creators, collapse = "\n"), "\n",
  "<distribution>\n<online>\n<onlineDescription>", s$desc, "</onlineDescription>\n",
  "<url function=\"information\">", s$url, "</url>\n</online>\n</distribution>\n",
  paste(s$contacts, collapse = "\n"), "\n</dataSource>")

method_step <- function(s) paste0(
  "<methodStep>\n<description>\n",
  "<para>This method step describes provenance-based metadata as specified in the LTER EML Best Practices.</para>\n",
  "<para>This provenance metadata does not contain entity specific information.</para>\n",
  "</description>\n", data_source(s), "\n</methodStep>")

for (p in pkgs) {
  d <- read.csv(paste0(fr, p$file), colClasses = "character")
  src <- unlist(d[as.Date(d$date) >= as.Date("1980-01-01"), grep("^source_station", names(d))])
  codes <- sort(unique(src[!is.na(src) & src != "NaN"]))
  edi <- unique(c(p$own, sort(unique(na.omit(nwt_pkg(codes))))))
  gh <- sort(intersect(codes, names(ghcnd_names)))
  sn <- sort(codes[grepl("\\([0-9]+\\)$", codes)])
  af <- sort(codes[grepl("^US-NR[0-9]$", codes)])
  unknown <- setdiff(codes, c(gh, sn, af, "NPN", codes[!is.na(nwt_pkg(codes))]))
  if (length(unknown)) stop(p$id, ": unmapped source codes: ", paste(unknown, collapse = ", "))
  sources <- c(lapply(edi, edi_source),
               if (length(gh)) list(ghcnd_source(gh)),
               if (length(sn)) list(snotel_source(sn)),
               lapply(af, amf_source),
               if ("NPN" %in% codes) list(npn_source))
  writeLines(paste(sapply(sources, method_step), collapse = "\n"),
             paste0(out, "knb-lter-nwt.", p$id, "_provenance.txt"), useBytes = TRUE)
  cat(p$id, ": EDI", paste(edi, collapse = ", "), "| GHCNd", length(gh), "| SNOTEL", length(sn),
      "| AmeriFlux", paste(af, collapse = " "), "| NPN", "NPN" %in% codes, "\n")
}

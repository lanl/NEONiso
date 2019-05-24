#' Download NEON datasets from the API.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data.product  NEON code of data product you'd like to retrieve
#' @param site.req     Four-letter code of site that you'd like to retrieve data from.
#' @param site.month    Which site month would you like to download?

download_NEON_data <- function(data.product="DP4.00200.001",site.req="all",site.month="all") {

  # Define some initial parameters -------------------------------------------------

  # set NEON API address
  neon.api.address <- "http://data.neonscience.org/api/v0/products/"

  # specify core and relocatable terrestrial sites...
  core.sites <- terrestrial_core_sites()
  reloc.sites <- terrestrial_relocatable_sites()

  # Generate list of data files to download -------------------------------------
  # based on site.code, site.month(s), and data product -------------------------
  #------------------------------------------------------------------------------
  # query API
  data.request <- GET(paste0(neon.api.address, data.product))

  # parse JSON into R list.
  available <- content(data.request,as="parsed") # httr content automatically calls proper parsing function.

  # how many sites are available?
  nsites <- length(available$data$siteCodes)

  # don't need this line, but leaving for historical purposes.
  # print(paste(nsites, "sites with available eddy covariance data, but some of these may be aquatic sites..."))

  #===========================================================
  # LOOP THROUGH SITE MONTHS AND DOWNLOAD IF MISSING.
  # steps here:
  # 1) subset list if a particular site or site.month has been supplied.
  # 2) iterate over sites.
  # 3) check to see if site exists in data repository. if yes,
  #     continue to compare archive. if not, download all.

  # RPF 190523 - this could be made more efficient in the future.
  # currently runs through entire loop, don't need to if not downlading all
  # sites though.
  for (i in 1:nsites) {
    
    # get name site we're working on.
    site.name <- available$data$siteCodes[[i]]$siteCode

    print(site.name)
    
    # check to see if site is a terrestrial site, and site is requested.
    if (site.req != "all" & !(site.name %in% core.sites | site.name %in% reloc.sites)) {
      # skip to next iteration of loop since it's not a terrestrial site.
      next
    } else if (site.name != "all" & site.name != site.req) {
      next # skip to next site if not requested site.
    } else if (site.name == "WOOD") { # something currently messed up w/ this site, so skip. RPF 190502.
      next
    }

    # generate a list of site months that are available for this site.
    site.months <- unlist(available$data$siteCodes[[i]]$availableMonths)

    # check to see if there's a data folder for this site, create it if it doesn't exist.
    ifelse(!dir.exists(paste0(site.name)),
            dir.create(paste0(site.name)),
            FALSE)

    # loop through months requested for this site
    for (j in 1:length(site.months)) {

      # requery api w/ given site code and month.
      sitemonth.urls.json <- GET(unlist(available$data$siteCodes[[i]]$availableDataUrls[j]))

      # list files.
      sitemonth.urls.parsed <- content(sitemonth.urls.json,as="parsed")

      # extract just the file names and URLS.
      fnames <- sapply(sitemonth.urls.parsed$data$files,'[[',2) # extract 2nd element of each list index
      furls <- sapply(sitemonth.urls.parsed$data$files,'[[',4)

      # get the basic zipfile?
      fnames.basic <- (grepl("basic",fnames) & grepl("h5",fnames)) |
        grepl("xml",fnames) | grepl("txt",fnames)

      #--------------------------------------------------------
      # check to see if folder exists for site month, create if it doesn't.
      site.month.exists <- dir.exists(paste0(site.name,"/",site.months[j]))
      site.month.created <- ifelse(!dir.exists(paste0(site.name,"/",site.months[j])),
                                   dir.create(paste0(site.name,"/",site.months[j])),
                                   FALSE)

      if (site.month.exists == TRUE & site.month.created == TRUE) {
        # check to see if each file already exists?
        dl.names <- fnames[fnames.basic]
        dl.urls <- furls[fnames.basic]

        # check to see if file name exists, download if not.
        for (k in 1:length(dl.names)) {
          if (!file.exists(paste0(site.name,"/",site.months[j],"/",dl.names[k]))) {
            GET(url=dl.urls[k],
                write_disk(paste0(site.name,"/",site.months[j],"/",dl.names[k]),overwrite=TRUE))
          } else {
            print(paste("File ",dl.names[k]," exists - skipping to next file."))
          }
        }
      } else if (site.month.exists == TRUE & site.month.created == FALSE) {

        # check to see if each file already exists?
        dl.names <- fnames[fnames.basic]
        dl.urls <- furls[fnames.basic]

        # check to see if file name exists, download if not.
        for (k in 1:length(dl.names)) {
          if (!file.exists(paste0(site.name,"/",site.months[j],"/",dl.names[k]))) {
            GET(url=dl.urls[k],
                write_disk(paste0(site.name,"/",site.months[j],"/",dl.names[k]),overwrite=TRUE))
          } else {
            print(paste("File ",dl.names[k]," exists - skipping to next file."))
          }
        }

      } else {
        # just download all the files...
        dl.names <- fnames[fnames.basic]
        dl.urls <- furls[fnames.basic]

        for (k in 1:length(dl.names)) {
          GET(url=dl.urls[k],
              progress(),
              write_disk(paste0(site.name,"/",site.months[j],"/",dl.names[k]),overwrite=TRUE))
        }
      }
    }

  }


}
# load data management functions.
#source("functions/NEON_data_mgmt.R")

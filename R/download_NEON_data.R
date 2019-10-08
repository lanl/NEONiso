#' download_NEON_data
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data.product  NEON code of data product you'd like to retrieve.
#'                      For example, the eddy covariance bundle is DP4.00200.001.
#'                      (In this release, only DP4.00200.001 *should* work. Updates to fix soon.)
#' @param site.req     Four-letter code of site you're requesting data from.
#'                     This can also be "all," which will download data from all
#'                     terrestrial sites.
#' @param site.month   Month of data to retrieve, in yyyy-mm format. Can also
#'                     use "all," which will retrieve all available data for site.req.
#'
#' @return Requested data will be downloaded to the current working directory. 
#' A future release will allow more flexibility in download directory.
#' 
#' @details Additional notes about different data products used here.
#' 
#' 1) Eddy covariance data (DP4.00200.001) are stored on the NEON servers as 
#' monthly "basic" file or daily "extended" files. Currently, this script will
#' download the "basic" files and metadata files only. Extended files also include
#' footprint calculations, and can be added in a future release if needed (however,
#' this will require substantially more space to store). 
#' 
#' 2) Meteorological variables are stored on NEON servers as separate CSV files based on:
#' a) position with respect to the tower (e.g., height on tower, or distance from tower) and
#' b) time resolution - I think usually this is 30 minute or 1 minute time resolution for most
#' products, but this has not been confirmed. Currently, for space/time considerations, 
#' this script will download *only* the 30 minute files. If 1 minute files are required later,
#' please submit a commit modifying this code or open an issue on the SPATIAL-Lab/NEONiso repository page.
#' 
#' @examples 
#' # get EC data from all sites from January 2018
#' download_NEON_data(data.product="DP4.00200.001",site.req="all",site.month="2018-01")
#' 
#' @export
#' 
download_NEON_data <- function(data.product,site.req="all",site.month="all") {

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

    #print(site.name)
    
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
    available.months <- unlist(available$data$siteCodes[[i]]$availableMonths)

    # check to see if there's a data folder for this site, create it if it doesn't exist.
    ifelse(!dir.exists(paste0(site.name)),
            dir.create(paste0(site.name)),
            FALSE)

    # loop through months requested for this site
    for (j in 1:length(available.months)) {

      # check to see if this month was requested.
      if (site.month != "all" & site.month != available.months[j]) {
        next # skip to next iteration if not requested.
      }
      
      # requery api w/ given site code and month.
      sitemonth.urls.json <- GET(unlist(available$data$siteCodes[[i]]$availableDataUrls[j]))

      # list files.
      sitemonth.urls.parsed <- content(sitemonth.urls.json,as="parsed")

      # extract just the file names and URLS.
      fnames <- sapply(sitemonth.urls.parsed$data$files,'[[',2) # extract 2nd element of each list index
      furls <- sapply(sitemonth.urls.parsed$data$files,'[[',4)

      # subset the file list - for EC data, there are monthly basic files and
      # daily extended files. by default, download only basic.
      # likewise, for met data - download *only* 30 minute data.
      
      if (data.product == "DP4.00200.001") {
        fnames.subset <- (grepl("basic",fnames) & grepl("h5",fnames)) |
                          grepl("xml",fnames) | grepl("txt",fnames)
      } else {
        fnames.subset <- (grepl("expanded",fnames) & grepl("30min",fnames)) |
          grepl("xml",fnames) | grepl("txt",fnames) | grepl("variables",fnames)
      }


      #--------------------------------------------------------
      # check to see if folder exists for site month, create if it doesn't.
      site.month.exists <- dir.exists(paste0(site.name,"/",available.months[j]))
      site.month.created <- ifelse(!dir.exists(paste0(site.name,"/",available.months[j])),
                                   dir.create(paste0(site.name,"/",available.months[j])),
                                   FALSE)

      if (site.month.exists == TRUE & site.month.created == TRUE) {
        # check to see if each file already exists?
        dl.names <- fnames[fnames.subset]
        dl.urls <- furls[fnames.subset]

        # check to see if file name exists, download if not.
        for (k in 1:length(dl.names)) {
          if (!file.exists(paste0(site.name,"/",available.months[j],"/",dl.names[k]))) {
            GET(url=dl.urls[k],
                write_disk(paste0(site.name,"/",available.months[j],"/",dl.names[k]),overwrite=TRUE))
          } else {
            print(paste("File ",dl.names[k]," exists - skipping to next file."))
          }
        }
      } else if (site.month.exists == TRUE & site.month.created == FALSE) {

        # check to see if each file already exists?
        dl.names <- fnames[fnames.subset]
        dl.urls <- furls[fnames.subset]

        # check to see if file name exists, download if not.
        for (k in 1:length(dl.names)) {
          if (!file.exists(paste0(site.name,"/",available.months[j],"/",dl.names[k]))) {
            GET(url=dl.urls[k],
                write_disk(paste0(site.name,"/",available.months[j],"/",dl.names[k]),overwrite=TRUE))
          } else {
            print(paste("File ",dl.names[k]," exists - skipping to next file."))
          }
        }

      } else {
        # just download all the files...
        dl.names <- fnames[fnames.subset]
        dl.urls <- furls[fnames.subset]

        for (k in 1:length(dl.names)) {
          GET(url=dl.urls[k],
              progress(),
              write_disk(paste0(site.name,"/",available.months[j],"/",dl.names[k]),overwrite=TRUE))
        }
      }
    }
  }
}

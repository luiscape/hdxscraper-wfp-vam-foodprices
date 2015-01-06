## Scraper for RNI Colombia's Website.

# dependencies
library(rjson)
library(RCurl)

# SW helper function
onSw <- function(d = T) {
  if (d == T) return('tool/')
  else return('')
}

# code dependencies
source(paste0(onSw(), 'code/write_tables.R'))
source(paste0(onSw(), 'code/sw_status.R'))

###################################################
###################################################
######### Scraping data from RNI's website ########
###################################################
###################################################

# function that gets the list of documents from WHO
# website and assembles a nice data.frame
collectWFPVam <- function(verbose = F) {
  cat('----------------------------------------\n')
  cat('Collecting data from WFP VAM Unit.\n')
  cat('----------------------------------------\n')
  
  # config
  base_url = 'http://reporting.vam.wfp.org/JSON/GetMPI.aspx?iso3='
  country_list <- read.csv(paste0(onSw(),'data/country_list.csv'))
    
  # Iterating over the list of available countries. 
  total = nrow(country_list)
  pb <- txtProgressBar(min = 0, max = total, char = ".", style = 3)
  for (i in 1:total) {
    setTxtProgressBar(pb, i)
    
    tryCatch({
      url <- paste0(base_url, country_list$country[i])
      if (verbose) print(url)
      doc <- fromJSON(getURL(url))
    }
    ,error = function(e) {
      m = paste("The country", country_list$country[i], "has no data.")
    })
    
    # Collecting data for each country.
    for (j in 1:length(doc)) {
      country_it <- data.frame(ISO3 = country_list$country[i],
                       Market_Name = doc[[j]]$name,
                       Commodity = doc[[j]]$commodity,
                       Unit = doc[[j]]$unit,
                       Date = sapply(doc[[j]]$data, function(x) x[1]),
                       Value = sapply(doc[[j]]$data, function(x) x[2]))
      
      if (j == 1) country_out <- country_it
      else country_out <- rbind(country_out, country_it)
    }
    
    if (i == 1) out <- country_out
    else out <- rbind(out, country_out)
  
  }
  
  # Converting dates to date objects.
  out$Date <- as.POSIXct(out$Date / 1000, origin="1970-01-01", tz="GMT")

  # results
  cat('-------------------------------\n')
  cat('Done!\n')
  cat('-------------------------------\n')
  return(out)
}

runScraper <- function(csv = TRUE, db = TRUE, df = FALSE) {
  data <- collectWFPVam()
  if (csv) write.csv(data, paste0(onSw(),'data/wfp_vam_data.csv'), row.names = F)
  if (db) writeTable(data, 'wfp_vam_data', 'scraperwiki')
  if (df) return(data)
}

# Changing the status of SW.
tryCatch(runScraper(),
         error = function(e) {
           cat('Error detected ... sending notification.')
           system('mail -s "WFP VAM figures failed." luiscape@gmail.com')
           changeSwStatus(type = "error", message = "Scraper failed.")
          { stop("!!") }
         }
)

# If success:
changeSwStatus(type = 'ok')

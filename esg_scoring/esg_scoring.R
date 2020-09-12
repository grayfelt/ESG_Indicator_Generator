library(urltools)
library(httr)
library(robotstxt)
library(rvest)
library(stringr)
library(dplyr)
library(tibble)

## For each iteration of a loop we’ll receive an XML document (a web page) as a response. 
## This function extracts ESG data from that page. What data it extracts is determined by the
## XPath plugged into the function as an argument

fun_parse <- function(xpath, xmldoc = page.i) {
  
  ## extracts the content (initially a text string) of the node identified by the “xpath” argument
  x <- xmldoc %>% 
    html_nodes(xpath = xpath) %>%
    html_text(trim = TRUE)
  
  ## if the string is empty and the XPath is for Controversy Rating return “None” XPath quirks
  if (length(x) == 0 & xpath == '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[2]/div[2]/div[2]/div/div[2]/div[1]/span/span/span') {
    return("None")
  }
  
  ## if the string contains “% AUM” all non-numeric characters are stripped a percent is returned
  if (grepl("% AUM", x)) {
    return(as.numeric(sub("% AUM", "", sub("based on ", "", x))) / 100)
  }
  
  ## if the string contains all letters, it’s a rating (e.g., “Outperformer”); I only trim white space
  if (!grepl("\\d", x)) {
    return(trimws(x))
  } else { ## if the string contains any numbers at all it’s either a score, a percentile ranking, or a date
    if (grepl("percentile", x)) {
      return(x %>% str_replace_all("[^0-9\\.]", "") %>% as.numeric() / 100) ## if the string contains “percentile” it’s a ranking; numerals are extracted and divided by 100
    } else {
      if (grepl("updated on", x)) { ## if the string contains “updated on” it’s a date; the date string is coerced into a date object
        r <- sub("Last updated on ", "", x)
        r <- paste(unlist(strsplit(r, "/"))[2], unlist(strsplit(r, "/"))[1], sep = "-")
        return(anytime::anydate(r))
      } else { ## anything left at this point is a score (1-100 scale); the text integer is converted to numeric
        return(as.numeric(x))
      }
    }
  }
}

fun_lists <- function() {
  x <- page.i %>%
    html_nodes(xpath = '//*[@id="Col2-3-InvolvementAreas-Proxy"]/section/table') %>%
    html_table() %>%
    data.frame()
  n <- sum(grepl("Yes", x[, 2]))
  if (n == 0) return(NA)
  if (n == 1) return(x[grep("Yes", x[, 2]), 1])
  if (n >= 2) return(list(x[grep("Yes", x[, 2]), 1]))
}

fun_robots <- function(url = link.i) {
  base_url <- paste0(url_parse(url)$scheme, "://", domain(url))
  paths_allowed(
    paths = sub(base_url, "", link.i), 
    domain = domain(url), 
    bot = "*"
  )
}

var_agent <- "Grayson Felt (graysonfelt@aggiemail.usu.edu). Doing personal research."

# Note: ^GSPC is the symbol/ticker for the S&P 500 Index
dat_stocks <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_nodes("table[id='constituents']") %>%
  html_table() %>%
  data.frame() %>%
  as_tibble()

# rename columns
colnames(dat_stocks) <- c("company", "ticker", "filings", "sector", "industry", "location", "added", "cik", "founded")

# select columns
dat_stocks <- dat_stocks[, c("ticker", "company", "sector", "industry")]

# rename tickers
dat_stocks$ticker <- gsub("[.]", "-", dat_stocks$ticker)

dat_stocks$esgRating    <- as.character(NA) # ESG Rating
dat_stocks$esgScore.tot <- as.integer(NA)   # ESG Score (Total/Overall)
dat_stocks$esgScore.env <- as.integer(NA)   # ESG Score (Environmental)
dat_stocks$esgScore.soc <- as.integer(NA)   # ESG Score (Social)
dat_stocks$esgScore.gov <- as.integer(NA)   # ESG Score (Governance)
dat_stocks$esgRank.tot  <- as.numeric(NA)   # Percentile Rank (Total/Overall)
dat_stocks$esgRank.env  <- as.numeric(NA)   # Percentile Rank (Environmental)
dat_stocks$esgRank.soc  <- as.numeric(NA)   # Percentile Rank (Social)
dat_stocks$esgRank.gov  <- as.numeric(NA)   # Percentile Rank (Governance)
dat_stocks$conRating    <- as.character(NA) # Controversy Rating 
dat_stocks$conLevel     <- as.integer(NA)   # Controversy Level
dat_stocks$conAreas     <- as.character(NA) # Controversy Areas (Products)
dat_stocks$asOf         <- Sys.Date()       # Last Updated date

i <- 1
for (i in 1:nrow(dat_stocks)) {
  message(paste0(i, " of ", nrow(dat_stocks)))
  tryCatch({
    tick.i <- dat_stocks$ticker[i]
    link.i <- paste0("https://finance.yahoo.com/quote/", tick.i, "/sustainability")
    bots.i <- suppressMessages(fun_robots(link.i))
    if (bots.i) {
      Sys.sleep(runif(1, 0.5, 3.0))
      page.i <- GET(link.i, user_agent(var_agent)) %>% content()
      dat_stocks$esgRating[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[3]/div/span')
      dat_stocks$esgScore.tot[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[1]')
      dat_stocks$esgScore.env[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[2]/div/div[2]/div[1]')
      dat_stocks$esgScore.soc[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[3]/div/div[2]/div[1]')
      dat_stocks$esgScore.gov[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[4]/div/div[2]/div[1]')
      dat_stocks$esgRank.tot[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[2]/span/span')
      dat_stocks$esgRank.env[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[2]/div/div[2]/div[2]/span/span')
      dat_stocks$esgRank.soc[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[3]/div/div[2]/div[2]/span/span')
      dat_stocks$esgRank.gov[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[4]/div/div[2]/div[2]/span/span')
      dat_stocks$conRating[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[2]/div[2]/div[2]/div/div[2]/div[1]/span/span/span')
      dat_stocks$conLevel[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[2]/div[2]/div[2]/div/div[2]/div[1]/div')
      dat_stocks$conAreas[i] <- fun_lists()
      dat_stocks$asOf[i] <- fun_parse('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[3]/span[2]/span')
    }
  }, error=function(e){})
}
dat_stocks$asOf[which(is.na(dat_stocks$esgRating))] <- NA


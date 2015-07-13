# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
library(ggplot2) # for plotting
library(scales) # for greater legend/scale control
# devtools::install_github("srvanderplas/GoogleScholarScrapeR")
# library(GoogleScholarScrapeR)
# -------------------------------------------------------------------------------

# Hires Data --------------------------------------------------------------------
# Read in startup funding information
source("Code/CleanStartupFundingData.R")

objs <- ls()
rm(list=objs[!objs%in%c("hires", "serv", "new.ip", "fprof", "setupScholar")])
# -------------------------------------------------------------------------------

# Functions to work with Selenium and Tor ---------------------------------------


# Set tor password with tor --hash-password <password>
# Then save the following command as newIP:
# (echo authenticate '"password"'; echo signal newnym; echo quit) | nc localhost 2200
# make executable
new.ip <- function(){
  system('/home/susan/bin/newIP')
}

setupScholar <- function(serv){
  # set bibtex import settings
  try(serv$findElement(using='css selector', value="#gs_btnAD")$clickElement())
  try(serv$findElement(using='css selector', value=".gs_btnP > span:nth-child(1)")$clickElement())
  # serv$findElement(using='css selector', value="#gs_num")$setElementAttribute("value", "20")
  try(serv$findElement(using='css selector', value='#scis1')$clickElement())
  try(serv$findElement(using='css selector', value="button.gs_btn_act:nth-child(1)")$clickElement())
  
  # return(serv)
}

reset.ip <- function(requests, serv){
  if(requests%%100==99){
    oldurl <- serv$serverURL
    # Request new IP from tor
    new.ip()
    message("Deleting Cookies")
    serv$close()
    serv$open()
    try(serv$navigate(oldurl))
    try(setupScholar(serv))
  } else {
    requests <<- requests + 1
  }
  return(requests)
}

id.robot <- function(counter, serv){
  if(grepl("not a robot", as.character(serv$getPageSource()))){
    if(counter < 3){
      message("Robot Test!")
      # system("xdg-open 'https://www.youtube.com/watch?v=GWXLPu8Ky9k'")
      Sys.sleep(30)
      # try(serv$navigate(serv$serverURL))
      return(id.robot(counter + 1, serv))
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

id.scriptblock <- function(counter, serv){
  if(grepl("We're sorry", as.character(serv$getPageSource()))){
    message("Timed Out on this IP")
    oldurl <- serv$serverURL
    # Request new IP from tor
    new.ip()
    message("Deleting Cookies")
    serv$close()
    serv$open()
    requests <<-0
    try(serv$navigate(oldurl))
    
    # Check for robot message
    robot <- id.robot(0, serv)
    
    try(setupScholar(serv))
    if(robot){ # Failure
      message("Prove you are human to Google first")
      return(TRUE)
    } else { # Not a robot, therefore can continue
      setupScholar(serv)
      return(id.scriptblock(counter, serv))
    }
  } else { # No scriptblock detected
    return(FALSE)
  }
}


seleniumScrape <- function(serv, author,  institution="Iowa State University"){
  # serv$open()
  googleScholarUrl <- "http://scholar.google.com"
  
  # putting together the search-URL:
  query <- sprintf('author:"%s" "%s"&as_vis=1&hl=en&as_sdt=1,5&as_ylo=2005', author, institution)
  resultsURL <- paste0(
    "http://scholar.google.com/scholar?q=",
    query %>%
      str_replace_all(pattern=" ", replacement="%20"))
  
  #   message("Deleting Cookies")
  #   serv$deleteAllCookies()
  
  # get content and parse it:
  err <- try(serv$navigate(resultsURL))
  # try(setupScholar(serv))
  
  if(id.robot(0,serv)){
    return(data.frame(author=author, gslink = "ROBOT-ERROR", link=NA, bibtex=NA))
  }
  
  if(id.scriptblock(0,serv)){
    return(data.frame(author=author, gslink = "SCRIPTBLOCK-ERROR", link=NA, bibtex=NA))
  }
  
  if(!grepl("gs_nta gs_nph", as.character(serv$getPageSource()))){
    if(grepl("gs_ad_dd", as.character(serv$getPageSource()))){
      try(setupScholar(serv))      
      try(setupScholar(serv))
    }
  }
  
  pageno <- try(serv$findElement(using='css selector', value="#gs_ab_md")$getElementText()[[1]][1]%>% str_replace_all("(About )|( results.*$)", "") %>% as.numeric())
  
  # requests <<- reset.ip(requests, serv)
  
  if(is.character(err) | is.character(pageno)){
    return(data.frame(author=author, gslink = "ERROR", link=NA, bibtex=NA))
  }
  
  links <- serv$findElements(using='css selector', value="#gs_n > center:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) a") %>% lapply(FUN=function(x) x$getElementAttribute("href"))
  if(length(links)>0){
    links <- c(resultsURL, links[1:(length(links)-1)] %>% unlist())
  } else {
    links <- resultsURL
  }
  bibtexlinks <- NULL
  gslinks <- NULL
  
  for(i in 1:length(links)){
    if(i>1){
      serv$navigate(links[i])
      requests <<- reset.ip(requests, serv)
    }
    
    
    h1a <- serv$findElements(using='link text', value="Import into BibTeX")
    h1 <- serv$findElements(using='css selector', value="div.gs_ri a.gs_nta.gs_nph")
    h2 <- serv$findElements(using='css selector', value="div.gs_ri > h3.gs_rt")
    
    if(length(h1)==0){
      setupScholar(serv)
      h1 <- serv$findElements(using='css selector', value="div.gs_ri a.gs_nta.gs_nph")
    } 
    
    if(length(h1)>0){
      bibtexlinks <- c(bibtexlinks, lapply(h1, function(x) x$getElementAttribute("href")[[1]][1]) %>% unlist())
    }
    if(length(h2)>0){
      gslinks <- c(gslinks, lapply(h2, function(x) x$getElementText()[[1]][1]) %>% unlist)
    }
  }
  
  if(length(bibtexlinks)>0){
    bibtexentries <- rep("", length(bibtexlinks))
    for(i in 1:length(bibtexlinks)){
      serv$navigate(bibtexlinks[i])
      bibtexentries[i] <- serv$findElement(using='css', value='body > pre:nth-child(1)')$getElementText()[[1]][1]
      requests <<- reset.ip(requests, serv)
    }
    return(data.frame(author=author, gslink = gslinks, link=bibtexlinks, bibtex=bibtexentries))
  } else {
    return(data.frame(author=author, gslink = resultsURL, link=NA, bibtex=NA))
  }
}

trySelenium <- function(serv, author, institution=NULL){
  tmp <- try(seleniumScrape(serv, author, institution))
  if(mode(tmp)=="list") {
    return(tmp) 
  } else {
    return(data.frame(author=author, gslink = "ERROR-2", link=NA, bibtex=NA))
  }
}
# -------------------------------------------------------------------------------

# Setup Selenium ----------------------------------------------------------------
requests <<- 0

# RSelenium::startServer() if required
require(RSelenium)
RSelenium::checkForServer()
RSelenium::startServer()

fprof <- makeFirefoxProfile(list(
  "network.proxy.socks" = "localhost"
  , "network.proxy.socks_port" = 9050L
  , "network.proxy.type" = 1L
)
)

serv <- remoteDriver(remoteServerAddr = "localhost" 
                     , port = 4444
                     , browserName = "firefox"
                     , extraCapabilities = fprof
)
# -------------------------------------------------------------------------------

# Acquire Data from Selenium ----------------------------------------------------
# Open firefox window
serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# Show that you aren't a robot?

setupScholar(serv)

### Find citations for about 5 people at a time, then bind to list and save
# citations <- hires[1:5,] %>% group_by(Name) %>%  do(seleniumScrape(serv, .$Name, "Iowa State University"))
# citations <- rbind(citations, 
#                    hires[6:10,] %>% group_by(Name) %>% do(seleniumScrape(serv, .$Name, "Iowa State University")))
# save(citations, file="Citations.RData")
#
### Once through one time, re-try failed attempts. 
# load("Citations.RData")
# citations <- subset(citations, !grepl("ERROR", gslink))
# failedlist <- subset(hires, !Name %in% citations$Name)[,"Name"]
# failedlist$success <- failedlist$Name%in%citations$Name
# serv$open()
# # Show that you aren't a robot?
# serv$navigate("http://scholar.google.com/scholar?q=author:'John Doe'")
# setupScholar(serv)
# citationList <- subset(failedlist, !success)[1,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citationList <- subset(citationList, !grepl("ERROR", gslink))
# citations <- unique(rbind(citations, citationList))
# failedlist$success <- failedlist$Name%in%citations$Name
# sum(failedlist$success)
# save(citations, file="Citations.RData")
# serv$close()
# -------------------------------------------------------------------------------

# Clean Data --------------------------------------------------------------------
load("Citations.RData")
citations.full <- citations[,2:5]


citations.full$author <- citations.full$author %>% 
  str_replace("AILEEN, KEATING", "KEATING, AILEEN") %>%
  str_replace("MICHAEL, CASTELLANO", "CASTELLANO, MICHAEL") %>%
  str_replace("FERNANDO, MIGUEZ", "MIGUEZ, FERNANDO") %>%
  str_replace("ZHIYOU, WEN", "WEN, ZHIYOU") %>%
  str_replace("STAROBIN, SOKO", "SOKO, STAROBIN")%>%
  str_replace("RAJAGOPAL, LAKSHMAN", "LAKSHMAN, RAJAGOPAL") %>%
  str_replace("ZHENG, TIANSHU", "TIANSHU, ZHENG") %>%
  str_replace("LEE, A YOUNG", "YOUNG, LEE A") #%>% 

# Extract bibtex information from the bibtex column in df
extract_bibtex <- function(df){
  df$type <- with(df, str_extract(bibtex, "@(\\w{1,})") %>% str_replace("@", ""))
  df$title <- with(df, str_extract(bibtex, "title=\\{.*?\\},?\\n") %>% str_replace("title=\\{(.*)?\\},?\\n", "\\1"))
  df$booktitle <- with(df, str_extract(bibtex, "booktitle=\\{.*?\\},?\\n") %>% str_replace("booktitle=\\{(.*)?\\},?\\n", "\\1"))
  df$authors <- with(df, str_extract(bibtex, "author=\\{.*?\\},?\\n") %>% str_replace("author=\\{(.*)?\\},?\\n", "\\1")) %>%
    str_replace_all(fixed('{\\"u}'), "u") %>%
    str_replace_all(fixed('{\\"o}'), "o") %>%
    str_replace_all(fixed("{\\'a}"), "a") %>%
    str_replace_all(fixed('{\\v{z}}'), "z") %>%
    str_replace_all(fixed("{\\i}"), "i") %>%
    str_replace_all(fixed('{\\"U}'), "U") %>%
    str_replace_all(fixed("{\\~n}"), "n") %>%
    str_replace_all(fixed("{\\'o}"), "o") %>% 
    str_replace_all(fixed("{\\'n}"), "n") %>%
    str_replace_all(fixed("{\\k{e}}"), "e") %>%
    str_replace_all(fixed("{\\k{a}}"), "a") %>%
    str_replace_all(fixed("{\\`e}"), "e") %>%
    str_replace_all(fixed("{\\'c}"), "c")
  df$journal <- with(df, str_extract(bibtex, "journal=\\{.*?\\},?\\n") %>% str_replace("journal=\\{(.*)?\\},?\\n", "\\1"))
  df$publisher <- with(df, str_extract(bibtex, "publisher=\\{.*?\\},?\\n") %>% str_replace("publisher=\\{(.*)?\\},?\\n", "\\1"))
  df$year <- with(df, str_extract(bibtex, "year=\\{.*?\\},?\\n") %>% str_replace("year=\\{(.*)?\\},?\\n", "\\1") %>% as.numeric())
  author.list <- str_split_fixed(df$authors %>% str_to_title(), " and ", 11)
  author.lastname <- str_split_fixed(df$author %>% str_to_title(), ", ", 2)[,1] %>% str_to_title()
  
  df$author.num <- sapply(1:length(author.lastname), function(x) {
    tmp <- str_detect(author.list[x,], author.lastname[x])
    ifelse(sum(tmp)==0, NA, which(tmp))
  })
  df$author.match <- sapply(1:nrow(df), function(x) author.list[x, df$author.num[x]])
  df$num.authors <- str_split(df$authors, " and ") %>% sapply( function(x) length(unlist(x) ))
  return(df)
}

citations <- citations.full %>% rowwise() %>% extract_bibtex()
citations <- citations[,c("author", "author.match", "type", "year", "title", "booktitle", "authors", "journal", "publisher", "author.num", "num.authors", "link", "gslink", "bibtex")]

citations <- filter(citations, year>=2005)
citations$author <- citations$author %>% str_to_title()
citations$author.pub <- str_match(tolower(citations$authors), tolower(citations$author)) %>% str_to_title()
filter(citations, is.na(citations$author.pub))[1:10,c("author", "authors")] %>% unique %>% as.data.frame()

# Issues: 
# Anderson, David - need middle name/initial...


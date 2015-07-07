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

# Read in startup funding information
source("Code/CleanStartupFundingData.R")

objs <- ls()
rm(list=objs[!objs%in%c("hires", "serv", "new.ip", "fprof", "setupScholar")])

new.ip <- function(){
  system('/home/susan/bin/newIP')
}

setupScholar <- function(serv){
  # set bibtex import settings
  serv$findElement(using='css selector', value="#gs_btnAD")$clickElement()
  serv$findElement(using='css selector', value=".gs_btnP > span:nth-child(1)")$clickElement()
  serv$findElement(using='css selector', value="#gs_num")$setElementAttribute("value", "20")
  serv$findElement(using='css selector', value='#scis1')$clickElement()
  serv$findElement(using='css selector', value="button.gs_btn_act:nth-child(1)")$clickElement()
  
  # return(serv)
}

reset.ip <- function(requests){
  if(requests>50){
    # Request new IP from tor
    new.ip()
    
    requests <- 0
  } else {
    requests <- requests + 1
  }
  return(requests)
}

requests <<- 0

seleniumScrape <- function(serv, author,  institution="Iowa State University"){
  
  googleScholarUrl <- "http://scholar.google.com"
  
  # putting together the search-URL:
  query <- sprintf("author:'%s' '%s'", author, institution)
  resultsURL <- paste0(
    "http://scholar.google.com/scholar?q=",
    query %>%
      str_replace_all(pattern=" ", replacement="%20"))
  
  # get content and parse it:
  err <- try(serv$navigate(resultsURL))
  
  counter <- 0
  
  while(is.character(err) & counter<3){
    # If captcha:
    captcha <- try(serv$findElement("css selector", "#gs_captcha_ccl"))
    if(is.character(captcha)){
      captcha <- try(serv$findElement("css selector", "#captcha"))
    }
    if(is.character(captcha)){
      warning("Request failed without captcha.")
      Sys.sleep(time = 60)
      new.ip()
    } else {
      system("xdg-open 'https://www.youtube.com/watch?v=GWXLPu8Ky9k'")
      message("Captcha detected! Complete the captcha, then press [enter] to continue")
      number <- scan(n=1)
    }
    counter <- counter + 1
    err <- try(serv$navigate(resultsURL))
    requests <<- reset.ip(requests)
  }

  requests <<- reset.ip(requests)
  
  if(is.character(err)){
    return(data.frame(author=author, gslink = "ERROR", link=NA, bibtex=NA))
  }
  
  pageno <- serv$findElement(using='css selector', value="#gs_ab_md")$getElementText()[[1]][1]%>% str_replace_all("(About )|( results.*$)", "") %>% as.numeric()
  
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
    }
    requests <<- reset.ip(requests)
    
    h1 <- serv$findElements(using='css selector', value="div.gs_ri a.gs_nta.gs_nph")
    h2 <- serv$findElements(using='css selector', value="div.gs_ri > h3.gs_rt")
    
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
      requests <<- reset.ip(requests)
    }
    return(data.frame(author=author, gslink = gslinks, link=bibtexlinks, bibtex=bibtexentries))
  } else {
    return(data.frame(author=author, gslink = resultsURL, link=NA, bibtex=NA))
  }
}

trySelenium <- function(serv, author, institution=NULL){
  tmp <- try(seleniumScrape(serv, author, institution))
  if(mode(tmp)=="list") return(tmp) else return(data.frame(author=author, gslink = "ERROR-2", link=NA, bibtex=NA))
}

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

# Set tor password with tor --hash-password <password>
# Then save the following command as newIP:
# (echo authenticate '"password"'; echo signal newnym; echo quit) | nc localhost 2200
# make executable


# Open firefox window
serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# Show that you aren't a robot?

setupScholar(serv)


citations <- hires[1:5,] %>% group_by(Name) %>%  do(seleniumScrape(serv, .$Name, "Iowa State University"))
citations <- rbind(citations, 
                   hires[6:10,] %>% group_by(Name) %>% do(seleniumScrape(serv, .$Name, "Iowa State University")))
save(citations, file="Citations.RData")

load("Citations.RData")
citationList <- hires[11,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))


serv$close()


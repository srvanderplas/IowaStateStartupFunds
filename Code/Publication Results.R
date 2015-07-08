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

id.robot <- function(counter, serv){
  if(grepl("not a robot", as.character(serv$getPageSource()))){
    if(counter < 5){
      system("xdg-open 'https://www.youtube.com/watch?v=GWXLPu8Ky9k'")
      Sys.sleep(60)
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
    new.ip()
    message("Deleting Cookies")
    serv$deleteAllCookies()
    try(serv$navigate(serv$serverURL))
    
    # Check for robot message
    robot <- id.robot(0, serv)
    
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
  
  if(id.robot(0,serv)){
    return(data.frame(author=author, gslink = "ROBOT-ERROR", link=NA, bibtex=NA))
  }
  
  if(id.scriptblock(0,serv)){
    return(data.frame(author=author, gslink = "SCRIPTBLOCK-ERROR", link=NA, bibtex=NA))
  }
  
  if(!grepl("BibTeX", as.character(serv$getPageSource()))){
    setupScholar(serv)
  }
  
  pageno <- try(serv$findElement(using='css selector', value="#gs_ab_md")$getElementText()[[1]][1]%>% str_replace_all("(About )|( results.*$)", "") %>% as.numeric())
  
  requests <<- reset.ip(requests)
  
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
      requests <<- reset.ip(requests)
    }
    
    
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
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[11:15,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[16:20,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()


serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[21:25,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[25:30,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[31:40,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[41:50,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(citations, citationList)
save(citations, file="Citations.RData")
serv$close()

serv$open()
serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
load("Citations.RData")
citationList <- hires[hires$Name%in%citations$Name[citations$gslink=="ROBOT-ERROR"],] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citations <- rbind(subset(citations, gslink!="ROBOT-ERROR"), citationList)
save(citations, file="Citations.RData")
serv$close()

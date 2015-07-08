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
  serv$open()
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
  try(setupScholar(serv))
  
  if(id.robot(0,serv)){
    
    serv$close()
    return(data.frame(author=author, gslink = "ROBOT-ERROR", link=NA, bibtex=NA))
  }
  
  if(id.scriptblock(0,serv)){
    
    serv$close()
    return(data.frame(author=author, gslink = "SCRIPTBLOCK-ERROR", link=NA, bibtex=NA))
  }
  
  if(!grepl("BibTeX", as.character(serv$getPageSource()))){
    setupScholar(serv)
  }
  
  pageno <- try(serv$findElement(using='css selector', value="#gs_ab_md")$getElementText()[[1]][1]%>% str_replace_all("(About )|( results.*$)", "") %>% as.numeric())
  
  requests <<- reset.ip(requests)
  
  if(is.character(err) | is.character(pageno)){
    
    serv$close()
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
    
    serv$close()
    return(data.frame(author=author, gslink = gslinks, link=bibtexlinks, bibtex=bibtexentries))
  } else {
    
    serv$close()
    return(data.frame(author=author, gslink = resultsURL, link=NA, bibtex=NA))
  }
  
  serv$close()
}

trySelenium <- function(serv, author, institution=NULL){
  tmp <- try(seleniumScrape(serv, author, institution))
  if(mode(tmp)=="list") {
    return(tmp) 
  } else {
    serv$close(); return(data.frame(author=author, gslink = "ERROR-2", link=NA, bibtex=NA))
  }
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
# 
# 
# citations <- hires[1:5,] %>% group_by(Name) %>%  do(seleniumScrape(serv, .$Name, "Iowa State University"))
# citations <- rbind(citations, 
#                    hires[6:10,] %>% group_by(Name) %>% do(seleniumScrape(serv, .$Name, "Iowa State University")))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[11:15,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[16:20,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[21:25,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[25:30,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[31:40,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[41:50,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# load("Citations.RData")
# citationList <- hires[hires$Name%in%citations$Name[citations$gslink=="ROBOT-ERROR"],] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(subset(citations, gslink!="ROBOT-ERROR"), citationList)
# save(citations, file="Citations.RData")
# serv$close()
# 
# load("Citations.RData")
# citationList <- hires[51:60,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
# 
# citationList <- hires[hires$Name%in%citations$Name[which(grepl("ERROR",citations$gslink))],] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(subset(citations, !grepl("ERROR", gslink)), citationList)
# save(citations, file="Citations.RData")
# 
# load("Citations.RData")
# serv$open()
# citationList <- hires[61:66,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# 
# citationList <- hires[67:80,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# 
# citationList <- hires[81:90,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# 
# citationList <- hires[91:100,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# 
# serv$close()
# serv$open()
# citationList <- hires[101:120,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# 
# serv$close()
# serv$open()
# citationList <- hires[121:130,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# 
# serv$close()
# serv$open()
# citationList <- hires[131:140,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# setupScholar(serv)
# citationList <- hires[141:150,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[151:160,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[161:170,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[171:180,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[181:190,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[191:200,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[201:210,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[211:220,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[221:230,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[231:240,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[241:250,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[251:260,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[261:270,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[271:280,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[281:290,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[291:300,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[291:300,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[301:310,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[311:320,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[321:330,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[331:340,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[341:350,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[351:360,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[361:370,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[371:380,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[381:390,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[391:400,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[401:410,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[421:430,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[431:440,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[441:450,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[451:460,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[461:470,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[471:480,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[481:490,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[491:500,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[501:510,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[511:520,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[521:530,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[531:540,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[541:550,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[551:560,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[561:570,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[571:580,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[581:590,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[591:600,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[600:610,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[611:620,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[621:630,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[631:640,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[641:650,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[651:660,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[661:670,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[671:680,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[681:690,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[691:700,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[701:710,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[711:720,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[721:730,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[731:740,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[741:750,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[751:760,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[761:770,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[771:780,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[781:790,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[791:800,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()

# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- hires[801:807,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- unique(rbind(citations, citationList))
# save(citations, file="Citations.RData")
# serv$close()
# 
# load("Citations.RData")
# citations <- subset(citations, !grepl("ERROR", gslink))
# failedlist <- subset(hires, !Name %in% citations$Name)[,"Name"]
# failedlist$success <- FALSE
# serv$open()
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- failedlist[1:10,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citationList <- subset(citationList, !grepl("ERROR", gslink))
# citations <- unique(rbind(citations, citationList))
# failedlist$success <- failedlist$Name%in%citations$Name
# save(citations, file="Citations.RData")
# serv$close()
# 
# citations <- subset(citations, !grepl("ERROR", gslink))
# failedlist <- subset(hires, !Name %in% citations$Name)[,"Name"]
# failedlist$success <- FALSE
# serv$open()
# # Show that you aren't a robot?
# setupScholar(serv)
# citationList <- subset(failedlist, !success)[1:10,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citationList <- subset(citationList, !grepl("ERROR", gslink))
# citations <- unique(rbind(citations, citationList))
# failedlist$success <- failedlist$Name%in%citations$Name
# sum(failedlist$success)
# save(citations, file="Citations.RData")
# serv$close()
# 
# serv$open()
# # Show that you aren't a robot?
# serv$navigate("http://scholar.google.com/scholar?q=author:'John Doe'")
# setupScholar(serv)
# citationList <- subset(failedlist, !success)[1:10,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citationList <- subset(citationList, !grepl("ERROR", gslink))
# citations <- unique(rbind(citations, citationList))
# failedlist$success <- failedlist$Name%in%citations$Name
# sum(failedlist$success)
# save(citations, file="Citations.RData")
# serv$close()


serv$open()
# Show that you aren't a robot?
serv$navigate("http://scholar.google.com/scholar?q=author:'John Doe'")
setupScholar(serv)
citationList <- subset(failedlist, !success)[1:10,] %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
citationList <- subset(citationList, !grepl("ERROR", gslink))
citations <- unique(rbind(citations, citationList))
failedlist$success <- failedlist$Name%in%citations$Name
sum(failedlist$success)
save(citations, file="Citations.RData")
serv$close()
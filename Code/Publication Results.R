# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
library(ggplot2) # for plotting
library(scales) # for greater legend/scale control
# -------------------------------------------------------------------------------

# # Hires Data --------------------------------------------------------------------
# # Read in startup funding information
# source("Code/CleanStartupFundingData.R")
# 
# objs <- ls()
# rm(list=objs[!objs%in%c("hires", "serv", "new.ip", "fprof", "setupScholar")])
# # -------------------------------------------------------------------------------

# # Functions to work with Selenium and Tor ---------------------------------------
# 
# 
# # Set tor password with tor --hash-password <password>
# # Then save the following command as newIP:
# # (echo authenticate '"password"'; echo signal newnym; echo quit) | nc localhost 2200
# # make executable
# new.ip <- function(){
#   system('/home/susan/bin/newIP')
# }
# 
# setupScholar <- function(serv){
#   # set bibtex import settings
#   try(serv$findElement(using='css selector', value="#gs_btnAD")$clickElement())
#   try(serv$findElement(using='css selector', value=".gs_btnP > span:nth-child(1)")$clickElement())
#   # serv$findElement(using='css selector', value="#gs_num")$setElementAttribute("value", "20")
#   try(serv$findElement(using='css selector', value='#scis1')$clickElement())
#   try(serv$findElement(using='css selector', value="button.gs_btn_act:nth-child(1)")$clickElement())
#   
#   # return(serv)
# }
# 
# reset.ip <- function(requests, serv){
#   if(requests%%100==99){
#     oldurl <- serv$serverURL
#     # Request new IP from tor
#     new.ip()
#     message("Deleting Cookies")
#     serv$close()
#     serv$open()
#     try(serv$navigate(oldurl))
#     try(setupScholar(serv))
#   } else {
#     requests <<- requests + 1
#   }
#   return(requests)
# }
# 
# id.robot <- function(counter, serv){
#   if(grepl("not a robot", as.character(serv$getPageSource()))){
#     if(counter < 3){
#       message("Robot Test!")
#       # system("xdg-open 'https://www.youtube.com/watch?v=GWXLPu8Ky9k'")
#       Sys.sleep(30)
#       # try(serv$navigate(serv$serverURL))
#       return(id.robot(counter + 1, serv))
#     } else {
#       return(TRUE)
#     }
#   } else {
#     return(FALSE)
#   }
# }
# 
# id.scriptblock <- function(counter, serv){
#   if(grepl("We're sorry", as.character(serv$getPageSource()))){
#     message("Timed Out on this IP")
#     oldurl <- serv$serverURL
#     # Request new IP from tor
#     new.ip()
#     message("Deleting Cookies")
#     serv$close()
#     serv$open()
#     requests <<-0
#     try(serv$navigate(oldurl))
#     
#     # Check for robot message
#     robot <- id.robot(0, serv)
#     
#     try(setupScholar(serv))
#     if(robot){ # Failure
#       message("Prove you are human to Google first")
#       return(TRUE)
#     } else { # Not a robot, therefore can continue
#       setupScholar(serv)
#       return(id.scriptblock(counter, serv))
#     }
#   } else { # No scriptblock detected
#     return(FALSE)
#   }
# }
# 
# 
# seleniumScrape <- function(serv, author,  institution="Iowa State University"){
#   # serv$open()
#   googleScholarUrl <- "http://scholar.google.com"
#   
#   # putting together the search-URL:
#   query <- sprintf('author:"%s" "%s"&as_vis=1&hl=en&as_sdt=1,5&as_ylo=2005', author, institution)
#   resultsURL <- paste0(
#     "http://scholar.google.com/scholar?q=",
#     query %>%
#       str_replace_all(pattern=" ", replacement="%20"))
#   
#   #   message("Deleting Cookies")
#   #   serv$deleteAllCookies()
#   
#   # get content and parse it:
#   err <- try(serv$navigate(resultsURL))
#   # try(setupScholar(serv))
#   
#   if(id.robot(0,serv)){
#     return(data.frame(author=author, gslink = "ROBOT-ERROR", link=NA, bibtex=NA))
#   }
#   
#   if(id.scriptblock(0,serv)){
#     return(data.frame(author=author, gslink = "SCRIPTBLOCK-ERROR", link=NA, bibtex=NA))
#   }
#   
#   if(!grepl("gs_nta gs_nph", as.character(serv$getPageSource()))){
#     if(grepl("gs_ad_dd", as.character(serv$getPageSource()))){
#       try(setupScholar(serv))      
#       try(setupScholar(serv))
#     }
#   }
#   
#   pageno <- try(serv$findElement(using='css selector', value="#gs_ab_md")$getElementText()[[1]][1]%>% str_replace_all("(About )|( results.*$)", "") %>% as.numeric())
#   
#   # requests <<- reset.ip(requests, serv)
#   
#   if(is.character(err) | is.character(pageno)){
#     return(data.frame(author=author, gslink = "ERROR", link=NA, bibtex=NA))
#   }
#   
#   links <- serv$findElements(using='css selector', value="#gs_n > center:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) a") %>% lapply(FUN=function(x) x$getElementAttribute("href"))
#   if(length(links)>0){
#     links <- c(resultsURL, links[1:(length(links)-1)] %>% unlist())
#   } else {
#     links <- resultsURL
#   }
#   bibtexlinks <- NULL
#   gslinks <- NULL
#   
#   for(i in 1:length(links)){
#     if(i>1){
#       serv$navigate(links[i])
#       requests <<- reset.ip(requests, serv)
#     }
#     
#     
#     h1a <- serv$findElements(using='link text', value="Import into BibTeX")
#     h1 <- serv$findElements(using='css selector', value="div.gs_ri a.gs_nta.gs_nph")
#     h2 <- serv$findElements(using='css selector', value="div.gs_ri > h3.gs_rt")
#     
#     if(length(h1)==0){
#       setupScholar(serv)
#       h1 <- serv$findElements(using='css selector', value="div.gs_ri a.gs_nta.gs_nph")
#     } 
#     
#     if(length(h1)>0){
#       bibtexlinks <- c(bibtexlinks, lapply(h1, function(x) x$getElementAttribute("href")[[1]][1]) %>% unlist())
#     }
#     if(length(h2)>0){
#       gslinks <- c(gslinks, lapply(h2, function(x) x$getElementText()[[1]][1]) %>% unlist)
#     }
#   }
#   
#   if(length(bibtexlinks)>0){
#     bibtexentries <- rep("", length(bibtexlinks))
#     for(i in 1:length(bibtexlinks)){
#       serv$navigate(bibtexlinks[i])
#       bibtexentries[i] <- serv$findElement(using='css', value='body > pre:nth-child(1)')$getElementText()[[1]][1]
#       requests <<- reset.ip(requests, serv)
#     }
#     return(data.frame(author=author, gslink = gslinks, link=bibtexlinks, bibtex=bibtexentries))
#   } else {
#     return(data.frame(author=author, gslink = resultsURL, link=NA, bibtex=NA))
#   }
# }
# 
# trySelenium <- function(serv, author, institution=NULL){
#   tmp <- try(seleniumScrape(serv, author, institution))
#   if(mode(tmp)=="list") {
#     return(tmp) 
#   } else {
#     return(data.frame(author=author, gslink = "ERROR-2", link=NA, bibtex=NA))
#   }
# }
# # -------------------------------------------------------------------------------
# 
# # Setup Selenium ----------------------------------------------------------------
# requests <<- 0
# 
# # RSelenium::startServer() if required
# require(RSelenium)
# RSelenium::checkForServer()
# RSelenium::startServer()
# 
# fprof <- makeFirefoxProfile(list(
#   "network.proxy.socks" = "localhost"
#   , "network.proxy.socks_port" = 9050L
#   , "network.proxy.type" = 1L
# )
# )
# 
# serv <- remoteDriver(remoteServerAddr = "localhost" 
#                      , port = 4444
#                      , browserName = "firefox"
#                      , extraCapabilities = fprof
# )
# # -------------------------------------------------------------------------------

# Acquire Data from Selenium ----------------------------------------------------
# Open firefox window
# serv$open()
# serv$navigate(sprintf("http://scholar.google.com/scholar?q=author:'%s' 'Iowa State University'", sample(hires$Name, 1)))
# # Show that you aren't a robot?
# 
# setupScholar(serv)

### Find citations for about 5 people at a time, then bind to list and save
# citations <- hires[1:5,] %>% group_by(Name) %>%  do(seleniumScrape(serv, .$Name, "Iowa State University"))
# citations <- rbind(citations, 
#                    hires[6:10,] %>% group_by(Name) %>% do(seleniumScrape(serv, .$Name, "Iowa State University")))
# save(citations, file="Citations.RData")
#
## Find citations for people with incorrect names in the database
# citationList <- data.frame(Name="LEE, YOUNG-A") %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(subset(citations, author!="Young, Lee A"), citationList)
# save(citations, file="Citations.RData")
# citationList <- data.frame(Name="ANDERSEN, DAVID") %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citations <- rbind(subset(citations, author!="Anderson, David"), citationList)
# save(citations, file="Citations.RData")
# citationList <- data.frame(Name="KING, DR") %>% group_by(Name) %>% do(trySelenium(serv=serv, author=.$Name, institution="Iowa State University"))
# citationList$author <- "KING, DAVID"
# citations <- rbind(citations, citationList)
# save(citations, file="Citations.RData")
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
  str_replace("ZHENG, TIANSHU", "TIANSHU, ZHENG") %>%
  str_replace("LEE, A", "LEE, YOUNG-A") %>% 
  str_replace("RAMAN, RAJ", "RAJ, RAMAN")

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
    str_replace_all(fixed("{\\'e}"), "e") %>%
    str_replace_all(fixed("{\\'c}"), "c") %>%
    str_replace_all(fixed("{\\`y}"), "y")
  df$journal <- with(df, str_extract(bibtex, "journal=\\{.*?\\},?\\n") %>% str_replace("journal=\\{(.*)?\\},?\\n", "\\1"))
  df$publisher <- with(df, str_extract(bibtex, "publisher=\\{.*?\\},?\\n") %>% str_replace("publisher=\\{(.*)?\\},?\\n", "\\1"))
  df$year <- with(df, str_extract(bibtex, "year=\\{.*?\\},?\\n") %>% str_replace("year=\\{(.*)?\\},?\\n", "\\1") %>% as.numeric())
  author.list <- str_split_fixed(df$authors %>% str_to_title(), " [Aa]nd ", 11)
  author.lastname <- str_split_fixed(df$author %>% str_to_title(), ", ", 2)[,1]
  
  df$author.num <- sapply(1:length(author.lastname), function(x) {
    tmp <- str_detect(author.list[x,], author.lastname[x])
    tmp2 <- str_detect(author.list[x,], df$author[x])
    if(length(which(tmp))>1){
      res <- which.min(adist(df$author[x], author.list[x,]))
    } else {
      res <- which(tmp)
    }
    ifelse(sum(tmp)==0, NA, res)
  })
  df$author.match <- sapply(1:nrow(df), function(x) author.list[x, df$author.num[x]])
  df$num.authors <- str_split(df$authors, " and ") %>% sapply( function(x) length(unlist(x) ))
  return(df)
}

citations <- citations.full %>% rowwise() %>% extract_bibtex()
citations <- citations[,c("author", "author.match", "type", "year", "title", "booktitle", "authors", "journal", "publisher", "author.num", "num.authors", "link", "gslink", "bibtex")]

citations <- filter(citations, year>=2005)
citations$author <- citations$author %>% str_to_title() %>% str_replace("O'd", "O'D")
citations <- filter(citations, !(author=="Coetzee, Johann" & is.na(author.num) & num.authors<10))

#--- Fix author.match issues
# Fix single-author issues
idx <- citations$num.authors==1 & is.na(citations$author.match)
citations$author.match[idx] <- citations$authors[idx]
citations$author.num[idx] <- 1
# Fix O'Donnell issues
idx <- which(citations$authors=="O’Donnell, Jennifer and Kaler, Eric W")
citations$author.match[idx] <- "O’Donnell, Jennifer"
citations$author.num[idx] <- 1
# Fix Salas-Fernandez issues
idx <- which(citations$author=="Salas-Fernandez, Maria")
author.list <- str_split_fixed(citations$authors[idx] %>% str_to_title(), " [Aa]nd ", 11)
author.num <- sapply(1:nrow(author.list), function(x) {
  tmp <- str_detect(author.list[x,], "Fernandez")
  ifelse(sum(tmp)==0, NA, which(tmp))
})
citations$author.match[idx] <- sapply(1:nrow(author.list), function(x) author.list[x, author.num[x]])
citations$author.num[idx] <- author.num
rm(author.list, author.num)
# Fix "too many authors" issues
idx <- citations$num.authors==11 & is.na(citations$author.match)
citations$author.match[idx] <- "Others"
citations$author.num[idx] <- 11
rm(idx)

#--- Fix Name match issues


# Issues: 
# Andersen, David J
citations <- subset(citations, author.match!="Andersen, David E")
citations <- subset(citations, author!="Anderson, David")
# Arruda, Paulo H.E.
citations <- subset(citations, author.match!="Arruda, Paulo Elias")
# Brown, Eric A.
tmp <- subset(citations, author=="Brown, Eric")
citations <- subset(citations, author!="Brown, Eric")
tmp <- subset(tmp, grepl("Brown, Eric A?$", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Brown, James Robert
tmp <- subset(citations, author=="Brown, James")
citations <- subset(citations, author!="Brown, James")
tmp <- subset(tmp, grepl("Brown, James R$", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Coetzee, Johann
tmp <- subset(citations, author=="Coetzee, Johann")
citations <- subset(citations, author!="Coetzee, Johann")
tmp <- subset(tmp, grepl("(Coetzee, Johann( F)?(rancois)?$)|(Others)", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Dong, Liang
citations <- subset(citations, author.match!="Liang, Dong-Shong")
# Feng, Hui
citations <- subset(citations, !(author=="Feng, Hui" & author.match!="Feng, Hui"))
# Jeffery, Nicholas David
citations <- subset(citations, author.match!="Jeffery, Nicholas W")
# Johnson, Anna Kerr; Butters-Johnson, Anna Kerr
tmp <- subset(citations, author=="Johnson, Anna")
citations <- subset(citations, author!="Johnson, Anna")
tmp <- subset(tmp, grepl("(Johnson, Anna K)|(Others)", authors))
tmp$author.match[tmp$author.match=="Johnson, Colin"] <- "Johnson, Anna K"
citations <- rbind(citations, tmp) %>% arrange(author)
# Johnston, Douglas M.
citations <- subset(citations, author.match!="Johnston, Douglas R")
# Jones, Phillip Harrison
citations <- subset(citations, author.match!="Jones, Phillip J")
# Kanthasamy, Arthi
citations$author.match[citations$author=="Kanthasamy, Arthi"] <- "Kanthasamy, Arthi"
# Kim, Su Jung
citations <- subset(citations, !(author=="Kim, Su" & author.match!="Kim, Su Jung"))
# Kim, Tae
citations <- subset(citations, !(author=="Kim, Tae" & !grepl("Kim, Tae Hyun", authors)))
citations$author.match[citations$author=="Kim, Tae"] <- "Kim, Tae Hyun"
# King, David R
tmp <- subset(citations, author=="King, David")
citations <- subset(citations, author!="King, David")
tmp <- subset(tmp, grepl("King, David( R)?$", author.match) & year>2010)
tmp$author.match[tmp$author.match=="King, David"] <- "King, David R"
citations <- rbind(citations, tmp) %>% arrange(author)
# Miller, David Marshall
tmp <- subset(citations, author=="Miller, David")
citations <- subset(citations, author!="Miller, David")
tmp <- subset(tmp, grepl("Miller, David Marshall", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Peterson, David
tmp <- subset(citations, author=="Peterson, David")
citations <- subset(citations, author!="Peterson, David")
tmp <- subset(tmp, grepl("Peterson, David Am", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Schofield, Thomas J
citations <- subset(citations, author.match!="Schofield, Thomas K")
# White, Robert Ensign
citations <- subset(citations, !(author=="White, Robert" & author.match!="White, Robert E"))
# Zhang, Wensheng
citations$author.match[citations$author=="Zhang, Wensheng" & (grepl("Zhang, Wensheng", citations$authors)|grepl("Wensheng, Zhang", citations$authors))] <- "Zhang, Wensheng"
# Zhang, Wei
# ISU webpage only lists publications with Ajay Kalra as a coauthor
tmp <- subset(citations, author=="Zhang, Wei")
citations <- subset(citations, author!="Zhang, Wei")
tmp <- subset(tmp, grepl("Kalra, Ajay", authors))
citations <- rbind(citations, tmp) %>% arrange(author)
# Zhang, Song
citations <- subset(citations, !(author=="Zhang, Song" & author.match!="Zhang, Song"))
# Zhang, Qijing
tmp <- subset(citations, author=="Zhang, Qijing")
citations <- subset(citations, author!="Zhang, Qijing")
tmp$author.match[!tmp$author.match%in%c("Others", "Zhang, Qijing")] <- "Zhang, Qijing"
citations <- rbind(citations, tmp) %>% arrange(author)
# Zhang, Jing
tmp <- subset(citations, author=="Zhang, Jing")
citations <- subset(citations, author!="Zhang, Jing")
tmp <- subset(tmp, !grepl("Vaccine|Chemistry|Physics|Engineering|virological|Climate|MRS|Hydrological|Catalysis|immunopharmacology|Polymer|Energy|Electric|Spectrometry|biotechnology|Science", tmp$journal) & !grepl("pyrolysis|gravity|Voltage|Food", tmp$title))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wu, Yue
# Chemical Engineering prof... probably not the same fellow publishing in Ethos
citations <- subset(citations, !(author=="Wu, Yue" & journal=="Ethos"))
# Wang, Yu
tmp <- subset(citations, author=="Wang, Yu")
citations <- subset(citations, author!="Wang, Yu")
tmp <- subset(tmp, grepl("Energy|Electricity|Climate|Reactor", title) & grepl("Energy|Environment", journal) & grepl("Brown", authors))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wang, Wei
tmp <- subset(citations, author=="Wang, Wei")
citations <- subset(citations, author!="Wang, Wei")
tmp <- subset(tmp, grepl("immun|transcription|transporter|cell", title))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wang, Zhi J
tmp <- subset(citations, author=="Wang, Zhi")
citations <- subset(citations, author!="Wang, Zhi")
tmp <- subset(tmp, grepl("Wang, Zhi J(ian)?", author.match))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wang, Tao
tmp <- subset(citations, author=="Wang, Tao")
citations <- subset(citations, author!="Wang, Tao")
tmp <- subset(tmp, grepl("History|Cold|War", title))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wang, Qian
tmp <- subset(citations, author=="Wang, Qian")
citations <- subset(citations, author!="Wang, Qian")
tmp <- subset(tmp, grepl("Accounting|Auditing", journal))
citations <- rbind(citations, tmp) %>% arrange(author)
# Wang, Chong
tmp <- subset(citations, author=="Wang, Chong")
citations <- subset(citations, author!="Wang, Chong")
tmp <- subset(tmp, !grepl("IEEE|nanoparticle", journal) & !grepl("defense|political|Ectopic", title))
citations <- rbind(citations, tmp) %>% arrange(author)
# Tang, Liang
tmp <- subset(citations, author=="Tang, Liang")
citations <- subset(citations, author!="Tang, Liang")
tmp <- subset(tmp, !grepl("Virology|Computers", journal))
citations <- rbind(citations, tmp) %>% arrange(author)
# Smith, Emily
tmp <- subset(citations, author=="Smith, Emily")
citations <- subset(citations, author!="Smith, Emily")
tmp <- subset(tmp, !grepl("FASEB|Animal Industry", journal))
tmp <- subset(tmp, author.match=="Smith, Emily A")
citations <- rbind(citations, tmp) %>% arrange(author)
# Smith, Carl
tmp <- subset(citations, author=="Smith, Carl")
citations <- subset(citations, author!="Smith, Carl")
tmp <- subset(tmp, type!="incollection")
citations <- rbind(citations, tmp) %>% arrange(author)
# Lee, Young-A
citations <- subset(citations, !(author=="Lee, Young-A" & author.match!="Lee, Young-A"))
# Han, Gang
citations <- subset(citations, author.match!="Kwak, Han-Gang")

# Non-European character sets
tmp <- citations$title[grep("_ERR_", iconv(citations$title, "UTF-8", "CP1252", sub="_ERR_"))]
# subset(tmp, !grepl("`|′|׳|“|″|'|′|≤|′|′|⋆|∗|𝛿|ϵ|∆|≫|≈|𝑝", tmp))
# -------------------------------------------------------------------------------

# Summarize Data ----------------------------------------------------------------

# Clean up
suppressWarnings(rm(citations.full, requests, serv, tmp, extract_bibtex, id.robot, id.scriptblock, new.ip, reset.ip, seleniumScrape, setupScholar, trySelenium))
# -------------------------------------------------------------------------------

save(citations, file="Data/Publications.RData")


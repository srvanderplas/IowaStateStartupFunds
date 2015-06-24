# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
# -------------------------------------------------------------------------------

# Data --------------------------------------------------------------------------
# Original Excel files converted to csv, with non-header rows removed

# 2005-2010 awards --------------------------------------------------------------

# data is not cleaned
awards.05 <- read.csv("Data/Awards FY06 to FY10.csv", stringsAsFactors = F)

# Ensure variable formatting matches variable type
var.cat <- c("Month", "agency", "lastname", "firstname", "dept", "center", "proposal_num", "purpose", "status", "admin_unit", "agency.1", "title", "program")
var.num <- c("Year", "FY", "direct", "indirect", "total", "principal", "id", "account_num", "type")
var.date <- c("award_date", "period_from", "period_to")

awards.05[,which(names(awards.05)%in%var.cat)] <- apply(awards.05[,which(names(awards.05)%in%var.cat)], 2, str_trim)
awards.05[,which(names(awards.05)%in%var.num)] <- apply(awards.05[,which(names(awards.05)%in%var.num)], 2, str_replace_all, pattern="FY ", replacement="")
awards.05[,which(names(awards.05)%in%var.num)] <- apply(awards.05[,which(names(awards.05)%in%var.num)], 2, str_trim)
awards.05[,which(names(awards.05)%in%var.num)] <- apply(awards.05[,which(names(awards.05)%in%var.num)], 2, as.numeric)
# awards.05[,which(names(awards.05)%in%var.num)] <- apply(awards.05[,which(names(awards.05)%in%var.num)], 2, function(i) ifelse(is.na(i), 0, i))
awards.05$award_date <- mdy_hm(awards.05$award_date)
awards.05$period_to <- mdy_hm(awards.05$period_to)
awards.05$period_from <- mdy_hm(awards.05$period_from)

# Remove rows which don't have IDs or submit dates later than 2004
awards.05 <- subset(awards.05, !is.na(id) & !is.na(lastname) & award_date>mdy("01-01-2004"))

# Format Investigator name similar to 2011-2015 file
awards.05$Investigator <- with(awards.05, paste(toupper(lastname), toupper(firstname), sep=", "))

# Transform type information
type.map <- c("ISUF Gift", "Grant", "Contract", "Fed Contract", "Cooperative Agreement", "Other", "MTA", "NDA", "Master Agreement")
type_fed.map <- c("", "Agriculture (USDA)", "Defense", "HHS", "NASA", "Commerce", "Interior", "Labor", "8", "Energy", "NSF", "EPA",  "Other", "Education", "Transportation")

# Create awards total by PI
awards.05.tot <- awards.05 %>% 
  group_by(id, title, status, type, type_fed, purpose, agency, account_num, Investigator, principal)  %>%
  summarize(payment_start=min(period_from), payment_end=max(period_to), tot.amt.paid=sum(total), n=length(total))%>% 
  group_by(id, title, status, purpose, agency) %>% 
  mutate(type = type.map[type+1], 
         type_fed = type.map[type_fed+1], 
         n.PI = length(unique(Investigator)), 
         tot.amt.paid.all = sum(tot.amt.paid), 
         Role = c("PI", "COI")[as.numeric(principal>1)+1]) %>% 
  arrange(id) 

awards.05.tot$status <- "Closed"
awards.05.tot$purpose <- str_to_title(awards.05.tot$purpose)
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Scholarship", "Fellowship")] <- "Scholarships & Fellowships"
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Extension/Public")] <- "Public Service"
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Instruction/Training")] <- "Instruction"


awards.05.tot <- awards.05.tot[,c("id", "title", "status", "type", "purpose", "agency", "account_num", "payment_start", "payment_end", "tot.amt.paid", "Role", "Investigator")]
names(awards.05.tot) <- c("Award.Number", "Awards.Title", "Award.Status", "Award.Type", "Activity.Type", "Sponsor", "Account", "Start.Date", "End.Date", "Award.Amount", "Role", "Investigator")
awards.05.tot$Source <- "Awards FY06 to FY10.csv"

# 2011-2015 awards --------------------------------------------------------------
# cleaned and formatted more carefully

awards.11 <- read.csv("Data/KC Investigator Award ALL 06072015.csv", stringsAsFactors = F)

var.cat <- c("Award.Number", "Award.Title", "Award.Status", "Award.Type", "Activity.Type", "Sponsor", "Sponsor.Award.Number", "Role", "Investigator")
var.num <- c("Account", "Award.Amount")
var.date <- c("Start.Date", "End.Date")

awards.11[,which(names(awards.11)%in%var.cat)] <- apply(awards.11[,which(names(awards.11)%in%var.cat)], 2, str_trim)
awards.11[,which(names(awards.11)%in%var.num)] <- apply(awards.11[,which(names(awards.11)%in%var.num)], 2, str_replace_all, pattern="\\$|,", replacement="")
awards.11[,which(names(awards.11)%in%var.num)] <- apply(awards.11[,which(names(awards.11)%in%var.num)], 2, as.numeric)
awards.11$Start.Date <- mdy(awards.11$Start.Date)
awards.11$End.Date <- mdy(awards.11$End.Date)

# Format investigator name as in other file
awards.11$Investigator <- str_replace(awards.11$Investigator, " ", ", ")

# Remove awards without an investigator
awards.11 <- subset(awards.11, nchar(Investigator)>1)

awards.11$Source <- "KC Investigator Award ALL 06072015.csv"


# Combined awards ---------------------------------------------------------------
awards <- rbind.fill(awards.05.tot, awards.11)

# Checking to ensure variables make sense
table(awards$Activity.Type, awards$Source, useNA='ifany')
table(awards$Award.Status, awards$Source, useNA='ifany')
table(awards$Award.Type, awards$Source, useNA='ifany')
names(table(awards$Investigator))


# Fixing names ------------------------------------------------------------------
invest.names <- as.data.frame(str_split_fixed(awards$Investigator, ",? ", 3))
names(invest.names) <- c("Last", "First", "MI")
awards$InvestigatorFull <- awards$Investigator
awards$Investigator <- paste0(invest.names$Last, ", ", invest.names$First)
# Fix some shortened/abbreviated names
awards$Investigator <- str_replace(awards$Investigator, "TUGGLE, CHRIS(TOPHER)?", "TUGGLE, CHRISTOPHER")
awards$Investigator <- str_replace(awards$Investigator, "THOEN, $", "THOEN, CHARLES")
awards$Investigator <- str_replace(awards$Investigator, "\\.", "")
awards$Investigator <- str_replace(awards$Investigator, "RUSSELL, ANNE", "RUSSELL, ANN")
awards$Investigator <- str_replace(awards$Investigator, "LANNINGHAM-FOSTER, L", "LANNINGHAM-FOSTER, LORRAINE")
rm(invest.names)

# Cleaning up -------------------------------------------------------------------
rm(var.cat, var.date, var.num, type.map, type_fed.map, awards.11, awards.05.tot, awards.05)
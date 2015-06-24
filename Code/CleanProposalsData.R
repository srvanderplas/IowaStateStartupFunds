# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
# -------------------------------------------------------------------------------

# Data --------------------------------------------------------------------------
# Original Excel files converted to csv, with non-header rows removed

# 2005-2010 Proposals -----------------------------------------------------------

# data is not cleaned
prop.05 <- read.csv("Data/Proposals FY06 to FY10.csv", stringsAsFactors = F)

# Ensure variable formatting matches variable type
var.cat <- c("Month", "agency", "lastname", "firstname", "dept", "center", "proposal_num", "purpose", "status", "admin_unit",  "title", "program")
var.num <- c("Year", "FY", "proposed_amount", "principal", "id", "account_num", "type", "type_fed")
var.date <- c("submit_datetime", "period_from", "period_to")

prop.05[,which(names(prop.05)%in%var.cat)] <- apply(prop.05[,which(names(prop.05)%in%var.cat)], 2, str_trim)
prop.05[,which(names(prop.05)%in%c("purpose", "status"))] <- apply(prop.05[,which(names(prop.05)%in%c("purpose", "status"))], 2, str_to_title)
prop.05[,which(names(prop.05)%in%var.num)] <- apply(prop.05[,which(names(prop.05)%in%var.num)], 2, str_replace_all, pattern="FY ", replacement="")
prop.05[,which(names(prop.05)%in%var.num)] <- apply(prop.05[,which(names(prop.05)%in%var.num)], 2, str_trim)
prop.05[,which(names(prop.05)%in%var.num)] <- apply(prop.05[,which(names(prop.05)%in%var.num)], 2, as.numeric)
# prop.05[,which(names(prop.05)%in%var.num)] <- apply(prop.05[,which(names(prop.05)%in%var.num)], 2, function(i) ifelse(is.na(i), 0, i))
prop.05$submit_datetime <- mdy_hm(prop.05$submit_datetime)
prop.05$period_to <- mdy_hm(prop.05$period_to)
prop.05$period_from <- mdy_hm(prop.05$period_from)

prop.05$status[prop.05$status=="Archive"] <- "Rejected"

# Remove rows which don't have IDs or submit dates later than 2004
prop.05 <- subset(prop.05, !is.na(id) & !is.na(lastname) & submit_datetime>mdy("01-01-2004") & status!="Executed")

# Format Investigator name similar to 2011-2015 file
prop.05$Investigator <- with(prop.05, paste(toupper(lastname), toupper(firstname), sep=", "))

# Transform type information
type.map <- c("ISUF Gift", "Grant", "Contract", "Fed Contract", "Cooperative Agreement", "Other", "MTA", "NDA", "Master Agreement")
type_fed.map <- c("", "Agriculture (USDA)", "Defense", "HHS", "NASA", "Commerce", "Interior", "Labor", "8", "Energy", "NSF", "EPA",  "Other", "Education", "Transportation")

# Create prop total by PI
prop.05.tot <- prop.05 %>% 
  group_by(id, title, status, type, type_fed, purpose, agency, account_num, Investigator, principal, submit_datetime)  %>%
  summarize(proposal_start=min(period_from), proposal_end=max(period_to), tot.amt.prop=sum(proposed_amount), n=length(proposed_amount))%>% 
  group_by(id, title, status, purpose, agency) %>% 
  mutate(type = type.map[type+1], 
         type_fed = type.map[type_fed+1], 
         n.PI = length(unique(Investigator)), 
         tot.amt.prop.all = sum(tot.amt.prop),
         submit_datetime = max(floor_date(submit_datetime, unit=c("day"))),
         Role = c("PI", "COI")[as.numeric(principal>1)+1]) %>% 
  arrange(id) 

prop.05.tot$purpose <- str_to_title(prop.05.tot$purpose)
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Scholarship", "Fellowship")] <- "Scholarships & Fellowships"
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Extension/Public")] <- "Public Service"
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Instruction/Training")] <- "Instruction"


prop.05.tot <- prop.05.tot[,c("id", "title", "status", "type", "purpose", 
                              "agency", "type_fed", "proposal_start", "proposal_end", "submit_datetime", 
                              "tot.amt.prop", "Role", "Investigator")]
names(prop.05.tot) <- c("Proposal.Number", "Proposal.Title", "Proposal.Status", "Grant.Type", "Activity.Type", 
                        "Sponsor", "Sponsor.Type", "Start.Date", "End.Date", "Submit.Date", 
                        "Proposal.Amount", "Role", "Investigator")
prop.05.tot$Source <- "Proposals FY06 to FY10.csv"

# 2011-2015 Proposals -----------------------------------------------------------
# cleaned and formatted more carefully

prop.11 <- read.csv("Data/KC Investigator Proposal ALL 06072015.csv", stringsAsFactors = F)

var.cat <- c("Proposal.Number", "Proposal.Title", "Proposal.Status", "Proposal.Type", "Activity.Type", "Sponsor", "Sponsor.Type", "Role", "Investigator")
var.num <- c("Proposal.Amount", "Direct.Cost", "Indirect.Cost")
var.date <- c("Start.Date", "End.Date", "Submit.Date")

prop.11[,which(names(prop.11)%in%var.cat)] <- apply(prop.11[,which(names(prop.11)%in%var.cat)], 2, str_trim)
prop.11[,which(names(prop.11)%in%var.num)] <- apply(prop.11[,which(names(prop.11)%in%var.num)], 2, str_replace_all, pattern="\\$|,", replacement="")
prop.11[,which(names(prop.11)%in%var.num)] <- apply(prop.11[,which(names(prop.11)%in%var.num)], 2, as.numeric)
prop.11$Start.Date <- mdy(prop.11$Start.Date)
prop.11$End.Date <- mdy(prop.11$End.Date)
prop.11$Submit.Date <- mdy(prop.11$Submit.Date)

# Format investigator name as in other file
prop.11$Investigator <- str_replace(prop.11$Investigator, ",? ", ", ")

# Remove prop without an investigator
prop.11 <- subset(prop.11, nchar(Investigator)>1)

prop.11$Source <- "KC Investigator Proposal ALL 06072015.csv"


# Combined prop -----------------------------------------------------------------
prop <- rbind.fill(prop.05.tot, prop.11)

# Checking to ensure variables make sense
table(prop$Activity.Type)
table(prop$Proposal.Type, prop$Source, useNA='ifany')
table(prop$Grant.Type, prop$Source, useNA='ifany')
table(prop$Proposal.Status, prop$Source, useNA='ifany')
table(prop$Investigator)

# Fixing names ------------------------------------------------------------------
invest.names <- as.data.frame(str_split_fixed(prop$Investigator, ",? ", 3))
names(invest.names) <- c("Last", "First", "MI")
prop$InvestigatorFull <- prop$Investigator
prop$Investigator <- paste0(invest.names$Last, ", ", invest.names$First)
# Fix some shortened/abbreviated names
prop$Investigator <- str_replace(prop$Investigator, "TUGGLE, CHRIS(TOPHER)?", "TUGGLE, CHRISTOPHER")
prop$Investigator <- str_replace(prop$Investigator, "THOEN, $", "THOEN, CHARLES")
prop$Investigator <- str_replace(prop$Investigator, "\\.", "")
prop$Investigator <- str_replace(prop$Investigator, "RUSSELL, ANNE", "RUSSELL, ANN")
prop$Investigator <- str_replace(prop$Investigator, "LANNINGHAM-FOSTER, L", "LANNINGHAM-FOSTER, LORRAINE")
rm(invest.names)

# Cleaning up -------------------------------------------------------------------
rm(var.cat, var.date, var.num, type.map, type_fed.map, prop.11, prop.05.tot, prop.05)
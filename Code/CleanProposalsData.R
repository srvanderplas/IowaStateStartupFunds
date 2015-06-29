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
prop.05 <- read.csv("Data/Proposals FY06 to FY10 with Witdrawn added 06202015.csv", stringsAsFactors = F)
names(prop.05) <- c("Month", "Year", "FY", "id", "lastname", "firstname", "principal", "dept", "center", "proposal_num", "account_num", "purpose", "status", "submit_datetime", "type", "type_fed", "period_from", "period_to", "proposed_amount", "admin_unit", "agency", "title", "program")

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
prop.05$submit_datetime <- ymd_hms(prop.05$submit_datetime)
prop.05$period_to <- ymd_hms(prop.05$period_to)
prop.05$period_from <- ymd_hms(prop.05$period_from)

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
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Extension/Public", "Dept/Admin Support", "Miscellaneous")] <- "Public Service"
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Instruction/Training")] <- "Instruction"
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Research", "Equipment")] <- "Research"
prop.05.tot$purpose[prop.05.tot$purpose%in%c("Building")] <- "Operations & Maintenance"

prop.05.tot$title <- toupper(prop.05.tot$title)

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
prop.11.m <- read.csv("Data/KC Proposal Report FY11 to FY15 through 06072015.csv", stringsAsFactors = F)
prop.11 <- subset(prop.11, Proposal.Number%in%prop.11.m$Proposal.Number)
rm(prop.11.m)

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
prop.11$Investigator <- prop.11$Investigator %>%
  str_replace(" {1,}", " ") %>%
  str_replace("^DEL ", "DEL-") %>%
  str_replace("^DE LA ", "DE-LA-") %>%
  str_replace("^DE,? LA,? ([A-Z]*)", "DE-LA-\\1,") %>%
  str_replace("^DE ", "DE-") %>%
  str_replace("^VAN DER ", "VAN-DER-") %>%
  str_replace("^VAN,? ([A-Z]*)", "VAN-\\1,") %>%
  str_replace("^VAN-DER,? ([A-Z]*)", "VAN-DER-\\1")
prop.11$Investigator <- str_replace(prop.11$Investigator, ",? ", ", ")

# Remove prop without an investigator
prop.11 <- subset(prop.11, nchar(Investigator)>1)

# Title in all caps
prop.11$Proposal.Title <- toupper(prop.11$Proposal.Title)

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
prop$Investigator <- prop$Investigator %>%
  str_replace(" {1,}", " ") %>%
  str_replace("^DEL ", "DEL-") %>%
  str_replace("^DE LA ", "DE-LA-") %>%
  str_replace("^DE ", "DE-") %>%
  str_replace("^VAN DER ", "VAN-DER-") %>%
  str_replace("^VAN ", "VAN-")

invest.names <- as.data.frame(str_split_fixed(prop$Investigator, ",? ", 3))
names(invest.names) <- c("Last", "First", "MI")
prop$InvestigatorFull <- prop$Investigator
prop$Investigator <- paste0(invest.names$Last, ", ", invest.names$First)
# Fix some shortened/abbreviated names
prop$Investigator <-  prop$Investigator %>% 
  str_replace("DE-LA-", "DE LA ") %>%
  str_replace("DE-", "DE ") %>%
  str_replace("DEL-", "DEL ") %>%
  str_replace("VAN-DER-", "VAN DER ") %>%
  str_replace("VAN-", "VAN ") %>%
  str_replace("DE, BRABANTER", "DE BRABANTER, KRIS") %>%
  str_replace("DEL, CASTILLO", "DEL CASTILLO, LINA") %>%
  str_replace("TUGGLE, CHRIS(TOPHER)?", "TUGGLE, CHRISTOPHER") %>%
  str_replace("THOEN, $", "THOEN, CHARLES") %>% 
  str_replace("\\.", "") %>%
  str_replace("RUSSELL, ANNE", "RUSSELL, ANN") %>%
  str_replace("LANNINGHAM-FOSTER, L$", "LANNINGHAM-FOSTER, LORRAINE") %>%
  str_replace("BARTON, TOMMY", "BARTON, TOM") %>%
  str_replace("BOSSELMAN, BOB", "BOSSELMAN, ROBERT") %>%
  str_replace("BROSHAR, DONNIE", "BROSHAR, DON") %>%
  str_replace("CACKLER, E$", "CACKLER, ELLS") %>%
  str_replace("CHANDLER, CHRIS$", "CHANDLER, CHRISTOPHER") %>%
  str_replace("CHEN, YU-CHE", "CHEN, YU") %>%
  str_replace("CHUKHAREV-KHUDILAYNEN EV$", "CHUKHAREV-KHUDILAYNEN EVGENY") %>%
  str_replace("CORREIA, ANA-PAULA", "CORREIA, ANA") %>%
  str_replace("DAS, BISWARANJAN", "DAS, BISWA") %>%
  str_replace("GANAPATHYSUBRAMANIAN, B$", "GANAPATHYSUBRAMANIAN, BASKAR") %>%
  str_replace("GEOFFROY, GREG$", "GEOFFROY, GREGORY") %>%
  str_replace("GOPALAKRISHNAN, KASTHURIR$", "GOPALAKRISHNAN, KASTHURIRA") %>%
  str_replace("GRANSBERG, DOUG$", "GRANSBERG, DOUGLAS") %>%
  str_replace("HARRIES, ADELA$", "HARRIES, ADELAIDA") %>%
  str_replace("HAYNES, CINDY", "HAYNES, CYNTHIA") %>%
  str_replace("JANE-TOPEL, JAY-LIN", "JANE, JAY-LIN") %>%
  str_replace("KANWAR, RAMESHWAR", "KANWAR, RAMESH") %>%
  str_replace("KENEALY, M$", "KENEALY, MICHAEL") %>%
  str_replace("LAROCK, RICH$", "LAROCK, RICHARD") %>%
  str_replace("LIEBMAN, MATT$", "LIEBMAN, MATTHEW") %>%
  str_replace("MALLAPRAGADA, S$", "MALLAPRAGADA, SURYA") %>%
  str_replace("MARTIN_X000D_\nMARTIN", "MARTIN") %>%
  str_replace("MARTIN MARTIN", "MARTIN") %>%
  str_replace("MILLLER, WYATT", "MILLER, WYATT") %>%
  str_replace("NETTLETON, DAN$", "NETTLETON, DANIEL") %>%
  str_replace("NG, SIU", "NG, SIU-HUNG") %>%
  str_replace("OBRIEN, JASON", "O'BRIEN, JASON") %>%
  str_replace("ONEAL, MATTHEW", "O'NEAL, MATTHEW") %>%
  str_replace("POWELL-COFFMAN, JO$", "POWELL-COFFMAN, JOANNE") %>%
  str_replace("PROULX, STEPHEN", "PROULX, STEVEN") %>%
  str_replace("RAMAN, DAVE", "RAMAN, DAVID") %>%
  str_replace("RANDALL, JESSE", "RANDALL, JESSIE") %>%
  str_replace("REFSNIDER, JEANINE", "REFSNIDER-STREBY, JEANINE") %>%
  str_replace("REILLY, PETE$", "REILLY, PETER") %>%
  str_replace("ROLLILNS, DERRICK", "ROLLINS, DERRICK") %>%
  str_replace("RUMBEIHA, WILSON", "RUMBEITHA, WILSON") %>%
  str_replace("SAFTIG, DAN$", "SAFTIG, DANIEL") %>%
  str_replace("SONGER, J$", "SONGER, JOSEPH") %>%
  str_replace("SOULEYRETTE, REG", "SOULEYRETTE, REGINALD") %>%
  str_replace("THIPPESWAMY, THIMMASETTAP$", "THIPPESWAMY, THIMMASETTAPPA") %>%
  str_replace("TRAVESSET-CASAS, ALEJANDR$", "TRAVESSET-CASAS, ALEJANDRO") %>%
  str_replace("TRUJILLO, JESSE", "TRUJILLO, JESSIE") %>%
  str_replace("VANDERLUGT, KRISTIN", "THOMAS-VANDERLUGT, KRISTIN") %>%
  str_replace("WILLIAMS, R$", "WILLIAMS, RYAN") %>%
  str_replace("YANG, XIAO$", "YANG, XIAO-BING") %>%
  str_replace("ZHANG, WENSHEN$", "ZHANG, WENSHENG")

  


# Cleaning up -------------------------------------------------------------------
rm(var.cat, var.date, var.num, type.map, type_fed.map, prop.11, prop.05.tot, prop.05)
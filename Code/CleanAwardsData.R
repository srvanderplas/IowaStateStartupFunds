# R libraries -------------------------------------------------------------------
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
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
  summarise(record_start=min(award_date), record_end=max(award_date), payment_start=min(period_from), payment_end=max(period_to), tot.amt.paid=sum(total), n=length(total))%>% 
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
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Extension/Public", "Dept/Admin Support", "Miscellaneous")] <- "Public Service"
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Instruction/Training")] <- "Instruction"
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Building")] <- "Operations & Maintenance"
awards.05.tot$purpose[awards.05.tot$purpose%in%c("Research", "Equipment")] <- "Research"

# award title in upper case
awards.05.tot$title <- toupper(awards.05.tot$title)

# Fix award numbers
awards.05.tot$id <- gsub("-00001", "", awards.05.tot$id)


awards.05.tot <- awards.05.tot[,c("id", "title", "status", "type", "purpose", 
                                  "agency", "account_num", "record_start", "record_end", "tot.amt.paid", 
                                  "Role", "Investigator")]
names(awards.05.tot) <- c("Award.Number", "Award.Title", "Award.Status", "Award.Type", "Activity.Type", 
                          "Sponsor", "Account", "Start.Date", "End.Date", "Award.Amount", 
                          "Role", "Investigator")
awards.05.tot$Source <- "Awards FY06 to FY10.csv"

# 2011-2015 awards --------------------------------------------------------------
# cleaned and formatted more carefully

awards.11 <- read.csv("Data/KC Investigator Award ALL 06072015.csv", stringsAsFactors = F)
awards.11.m <- read.csv("Data/KC Award Report FY11 to FY15 through 06072015.csv", stringsAsFactors = F)
awards.11 <- subset(awards.11, Award.Number%in%awards.11.m$Award.Number)
rm(awards.11.m)

var.cat <- c("Award.Number", "Award.Title", "Award.Status", "Award.Type", "Activity.Type", "Sponsor", "Sponsor.Award.Number", "Role", "Investigator")
var.num <- c("Account", "Award.Amount")
var.date <- c("Start.Date", "End.Date")

awards.11[,which(names(awards.11)%in%var.cat)] <- apply(awards.11[,which(names(awards.11)%in%var.cat)], 2, str_trim)
awards.11[,which(names(awards.11)%in%var.num)] <- apply(awards.11[,which(names(awards.11)%in%var.num)], 2, str_replace_all, pattern="\\$|,", replacement="")
awards.11[,which(names(awards.11)%in%var.num)] <- apply(awards.11[,which(names(awards.11)%in%var.num)], 2, as.numeric)
awards.11$Start.Date <- mdy(awards.11$Start.Date)
awards.11$End.Date <- mdy(awards.11$End.Date)


awards.11$Award.Type[awards.11$Award.Type%in%c("SFA Scholarship (no sponsored account)")] <- "SFA Scholarship"

# Format investigator name as in other file
awards.11$Investigator <- awards.11$Investigator %>%
  str_replace(" {1,}", " ") %>%
  str_replace("^DEL ", "DEL-") %>%
  str_replace("^DE LA ", "DE-LA-") %>%
  str_replace("^DE,? LA,? ([A-Z]*)", "DE-LA-\\1,") %>%
  str_replace("^DE ", "DE-") %>%
  str_replace("^VAN DER ", "VAN-DER-") %>%
  str_replace("^VAN ", "VAN-") %>% 
  str_replace("^VAN,? ([A-Z]*)", "VAN-\\1,") %>%
  str_replace("^VAN-DER,? ([A-Z]*)", "VAN-DER-\\1")
awards.11$Investigator <- str_replace(awards.11$Investigator, " ", ", ")

# Remove awards without an investigator
awards.11 <- subset(awards.11, nchar(Investigator)>1)

# Fix award numbers
awards.11$Award.Number <- gsub("-00001", "", awards.11$Award.Number)

# Award title in caps
awards.11$Award.Title <- toupper(awards.11$Award.Title)

awards.11$Source <- "KC Investigator Award ALL 06072015.csv"

# Remove awards that were purged or have zero value


# Combined awards ---------------------------------------------------------------
awards <- rbind.fill(awards.05.tot, awards.11)

# Checking to ensure variables make sense
table(awards$Activity.Type, awards$Source, useNA='ifany')
table(awards$Award.Status, awards$Source, useNA='ifany')
table(awards$Award.Type, awards$Source, useNA='ifany')
names(table(awards$Investigator))


# Fixing names ------------------------------------------------------------------
awards$Investigator <- awards$Investigator %>%
  str_replace(" {1,}", " ") %>%
  str_replace("^DE ", "DE-") %>%
  str_replace("^DEL ", "DEL-") %>%
  str_replace("^DE-LA ", "DE-LA-") %>%
  str_replace("^VAN ", "VAN-") %>%
  str_replace("^VAN-DER ", "VAN-DER-")

invest.names <- as.data.frame(str_split_fixed(awards$Investigator, ",? ", 3))
names(invest.names) <- c("Last", "First", "MI")
awards$InvestigatorFull <- awards$Investigator
awards$Investigator <- paste0(invest.names$Last, ", ", invest.names$First)
# Fix some shortened/abbreviated names
awards$Investigator <- awards$Investigator %>% 
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
  str_replace("PETERS, $", "PETERS, FRANK") %>%
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

rm(invest.names)

awards2 <- awards

awards <- awards %>% 
  group_by(Award.Number, Award.Title, Award.Status, Award.Type, Activity.Type, Sponsor, Account, Role, Investigator)  %>%
  summarize(Start.Date=min(Start.Date), End.Date=max(End.Date), Award.Amount=sum(Award.Amount), Source=unique(Source))%>% 
  arrange(Award.Number)
  
awards$Award.Status <- awards$Award.Status %>% 
  str_replace("Purged from KFS", "Closed") %>% 
  str_replace("Final", "Closed")

awards <- filter(awards, Award.Status != "Executed")




# Cleaning up -------------------------------------------------------------------
rm(awards2, var.cat, var.date, var.num, type.map, type_fed.map, awards.11, awards.05.tot, awards.05)

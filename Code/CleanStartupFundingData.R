# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
# -------------------------------------------------------------------------------

# Data --------------------------------------------------------------------------
# Original Excel files converted to csv, with non-header rows removed
# Dates fixed in FacStUpMaster08.csv:
#  Row 238: what was the starting date for Gordon Arbuckle? – start date is 7/1/2007
#  Row 240: Was the starting date for Chenxu Yu really 11/2007? – yes, start date is 11/1/2007 as Asst Prof


# Function to strip extra columns (blank columns without header labels) from the data
stripExtraCols <- function(data){
  emptycols <- grepl("^X\\.?\\d{0,}$", names(data)) | grepl("[Empty]", names(data), fixed=T)
  data[,!emptycols]
}

# Function to format columns appropriately given diverse dataset formats
format.cols <- function(data){

  # Fix column names
  dnames <- names(data)
  new.dnames <- dnames %>%
    str_replace("FY\\.Start", "Start.Date") %>%
    str_replace("Comp\\.\\.peripherals", "Computer.peripherals") %>%
    str_replace("Graduate\\.Asst\\.Profs", "Graduate.assistants") %>%
    str_replace("IPRT(\\.Ames\\.Lab)?", "IPRT.Ames.Lab.Funding") %>%
    str_replace("Dept\\.1", "Dept.Funding") %>%
    str_replace("College\\.1", "College.Funding") %>%
    str_replace("PSI", "PSI.Funding") %>%
    str_replace("VPRED", "VPRED.Funding") %>%
    str_replace("VPRES", "VPRED.Funding") %>%
    str_replace("VProv\\.Research", "VPRED.Funding") %>%
    str_replace("Total\\.cost", "Total.Cost") %>%
    str_replace("Total\\.Source", "Total.Funding") %>%
    str_replace("[SE]VP\\.P(rovost)?", "EVP.P.Funding") %>% # Executive VP vs. Senior VP?
    str_replace("^Provost", "EVP.P.Funding") %>%
    str_replace("Biotech", "Biotech.Funding") %>%
    str_replace("Other$", "Other.Costs") %>%
    str_replace("Other\\.1$", "Other.Funding") %>%
    str_replace("\\.\\.", ".")

  names(data) <- new.dnames

  # Create Computer+peripherals column since some sheets have that separated
  if(sum(c("Computer", "peripherals") %in% dnames)>0){
    data$Computer.peripherals <- data$Computer + data$peripherals
  }
  # Remove unnecessary columns
  data <- data[,!names(data)%in%c("Computer", "peripherals")]

  # Turn start dates into FY notation (FY05 implies a start date near August 1, 2004)
  if(sum(!grepl("FY", data$Start.Date))>0){
    data$Start.Date.mdy <- mdy(data$Start.Date)
    year(data$Start.Date.mdy) <- year(data$Start.Date.mdy) %% 2000 + 2000
    tmp2 <- sapply(data$Start.Date.mdy, function(i) {
      ifelse(is.na(i), NA, sprintf("FY%02d", year(round_date(i, "year"))%%2000))
    })
    data$Start.Date[!is.na(tmp2)] <- tmp2[!is.na(tmp2)]
  }

  # Ensure categorical variables are formatted as strings
  Categ <- c("College", "Name", "Dept", "Start.Date", "Faculty.rank", "Pay.base", "Beginning", "Ending", "Remarks")
  data[,which(names(data)%in%Categ)] <- apply(data[,which(names(data)%in%Categ)], 2, as.character)
  data[,which(names(data)%in%Categ)] <- apply(data[,which(names(data)%in%Categ)], 2, str_trim)

  # Ensure expenses are formatted as numeric variables (remove commas if present from Excel export)
  Expenses <- c("Starting.salary", "Lab.space.equipment", "Graduate.assistants", "Summer.support", "Moving.expenses", "Research.support", "Computer.peripherals", "Other.Costs", "Total.Cost")
  data[,which(names(data)%in%Expenses)] <- apply(data[,which(names(data)%in%Expenses)], 2, function(i) str_replace_all(i, ",", ""))
  data[,which(names(data)%in%Expenses)] <- apply(data[,which(names(data)%in%Expenses)], 2, as.numeric)
  data[,which(names(data)%in%Expenses)] <- apply(data[,which(names(data)%in%Expenses)], 2, function(i) ifelse(is.na(i), 0, i))

  # Ensure funding sources are formatted as numeric variables (remove commas if present from Excel export)
  Funding <- new.dnames[grepl("Funding", new.dnames)]
  data[,which(names(data)%in%Funding)] <- apply(data[,which(names(data)%in%Funding)], 2, function(i) str_replace_all(i, ",", ""))
  data[,which(names(data)%in%Funding)] <- apply(data[,which(names(data)%in%Funding)], 2, as.numeric)
  data[,which(names(data)%in%Funding)] <- apply(data[,which(names(data)%in%Funding)], 2, function(i) ifelse(is.na(i), 0, i))

  data
}

# List all csvs in the data directory, by most recent
csvlist <- c(
  "Data/Academic Div Faculty Startup FY15.csv", 
  "Data/Acad Div-ver10-16-13 ----Y14 Faculty Start-up compiled rpt.csv", 
  "Data/FY13 Faculty start-up report-Acad Division.csv", 
  "Data/FY12-MASTERFacultyStartupRpt.csv", 
  "Data/FY11MasterFacStUp.csv", 
  "Data/FY09-11FacStUp.csv", 
  "Data/FacStUpMaster08.csv", 
  "Data/FY07-05 file to IR.csv")
csvlist <- factor(csvlist, levels=csvlist, ordered=T)
# csvlist <- paste0("Data/", list.files("Data/", "*.csv"))

fylist <- c(2015, 2014, 2013, 2012, 2011, 2010, 2008, 2006)

# Read in all of the csv files, then remove extra columns and format the columns appropriately.
tmp <- lapply(csvlist, function(i) {
  j <- read.csv(as.character(i), stringsAsFactors=F) %>% stripExtraCols %>% format.cols
  j$Source <- i
  j[nchar(j$Name)>0,]
  }
)

# Merge all datasets together
hires <- do.call("rbind.fill", tmp) %>% arrange(Name, Source)

# Order columns sensibly
col.order <- c("Name", "College", "Dept", "Start.Date", "Faculty.rank", "Pay.base", "Starting.salary", "Computer.peripherals", "Lab.space.equipment", "Graduate.assistants", "Summer.support", "Moving.expenses", "Research.support", "Other.Costs", "Total.Cost", "Dept.Funding", "College.Funding", "PSI.Funding", "IPRT.Ames.Lab.Funding", "VPRED.Funding", "EVP.P.Funding", "Biotech.Funding", "Other.Funding", "Total.Funding", "Start.Date.mdy", "Beginning", "Ending", "Remarks", "Source")
hires <- hires[,col.order] %>% arrange(Name, Source)



# Fix Names
hires$Name <- str_to_upper(hires$Name)
hires$NameFull <- hires$Name
hires$Name <- hires$Name %>%
  str_replace(" {1,}", " ") %>% 
  str_replace("BANG EUN JIN", "BANG, EUNJIN") %>%
  str_replace("CHO, IN HO", "CHO, IN-HO") %>%
  str_replace("KELLY, KRISTY A. COSTABILE", "COSTABILE, KRISTY K") %>%
  str_replace("KIM,GAP-YOUNG", "KIM, GAP-YONG") %>%
  str_replace("PEREIRA, PEREIRA, BEATRIZ DE CASTRO SEBASTIAO", "DE-CASTRO-PEREIRA, BEATRIZ") %>%
  str_replace("RAMAN, D. RAJ", "RAMAN, RAJ") %>%
  str_replace("WATANABE,OLENA", "WATANABE, OLENA") %>%
  str_replace("THOMAS-VANDER LUGT", "THOMAS-VANDERLUGT") %>%
  str_replace("TUCHIN, KIRIL", "TUCHIN, KIRILL") %>%
  str_replace("VALENZUELA, M", "VALENZUELA-CASTRO, MARIA") 

hire.names <- as.data.frame(str_split_fixed(hires$Name, ",? ", 3))
names(hire.names) <- c("Last", "First", "MI")
hires$Name <- paste0(hire.names$Last, ", ", hire.names$First)

hires$Name <- hires$Name %>% 
  str_replace(", $", "") %>%
  str_replace("ARBUCKLE, J\\.", "ARBUCKLE, J") %>%
  str_replace("BASMAJIAN$", "BASMAJIAN, CARLTON") %>%
  str_replace("BELLAIRE$", "BELLAIRE, BRYAN") %>%
  str_replace("BENSHLOMO$", "BEN-SHLOMO, GIL") %>%
  str_replace("BLITVICH$", "BLITVICH, BRADLEY") %>%
  str_replace("BREHAM-STECHER, BYRON", "BREHM-STECHER, BYRON") %>%
  str_replace("BROWN, ERIC A\\.", "BROWN, ERIC") %>%
  str_replace("BURNS, ROBERT T", "BURNS, ROBERT") %>%
  str_replace("CAMPBELL$", "CAMPBELL, CHRISTINA") %>%
  str_replace("CARLSON, STEVE", "CARLSON, STEVEN") %>%
  str_replace("CHARRON KATHERINE MELLEN", "CHARRON, KATHERINE") %>%
  str_replace("COOK, KENNETH L", "COOK, KENNETH") %>%
  str_replace("CORDOBA, JUAN CARLOS", "CORDOBA, JUAN") %>% 
  str_replace("DE, BRABANTER", "DE BRABANTER, KRIS") %>%
  str_replace("DEL, CASTILLO", "DEL CASTILLO, LINA") %>%
  str_replace("KIMBER$", "KIMBER, MICHAEL") %>%
  str_replace("DE-CASTRO-PEREIRA, BEATRIZ", "DE CASTRO PEREIRA, BEATRIZ") %>%
  str_replace("DAS, BISWAS", "DAS, BISWA") %>%
  str_replace("GRIESDORN, TIM", "GRIESDORN, TIMOTHY") %>%
  str_replace("HARPOLE, W", "HARPOLE, WILLIAM") %>%
  str_replace("HEDLUND", "HEDLUND, CHERYL") %>%
  str_replace("HERTZLER,STEVE", "HERTZLER, STEVE") %>%
  str_replace("HOLME,, THOMAS", "HOLME, THOMAS") %>%
  str_replace("HOLTKAMP$", "HOLTKAMP, DERALD") %>% 
  str_replace("HONG, GONG$", "HONG, GONG-SOOG") %>%
  str_replace("^JEFFREY$", "JEFFERY, NICHOLAS") %>%
  str_replace("JONES, PHILIP", "JONES, PHILLIP") %>%
  str_replace("MALDONADO, MARTA", "MALDONADO-PABON, MARTA") %>%
  str_replace("MILLER$", "MILLER, CATHY") %>%
  str_replace("MUELLER, DARREN", "MUELLER, DAREN") %>%
  str_replace("OPRIESSNIG$", "OPRIESSNIG, TANJA") %>%
  str_replace("RAJAGOPAL, LAKSHAM", "RAJAGOPAL, LAKSHMAN") %>%
  str_replace("RIZO-ARBUCKLE, ELISA", "RIZO, ELISA") %>%
  str_replace("STANLEY, MATHEW", "STANLEY, MATTHEW") %>%
  str_replace("TERPENNEY, JANIS", "TERPENNY, JANIS") %>%
  str_replace("THOMAS, NICOLAS", "THOMAS, NICHOLAS") %>%
  str_replace("WINTER, MATT$", "WINTER, MATTHEW") %>%
  str_replace("ZHANG, WENSHEN$", "ZHANG, WENSHENG") %>%
  str_replace("VANLOOCKE, ANDY", "VANLOOCKE, ANDREW")

hires$Faculty.rank <- hires$Faculty.rank %>%
  str_replace("Professor", "Prof") %>% 
  str_replace("Asst\\.", "Asst") %>%
  str_replace("Adjunct", "Adj") %>%
  str_replace("Assistant", "Asst") %>%
  str_replace("Associate Prof", "Assoc Prof") %>%
  str_replace("Associate$", "Assoc") %>%
  str_replace("Assoc$", "Assoc Prof") %>%
  str_replace("Asst$", "Asst Prof") %>%
  str_replace("Assiociate", "Assoc") %>%
  str_replace("Distinguished", "Distg") %>%
  str_replace("Full Prof", "Prof") %>%
  str_replace("/", " & ") %>%
  str_replace("Prof\\.", "Prof") %>%
  str_replace("Porf", "Prof") %>%
  str_replace("[;,]{1,} ", " & ") %>%
  str_replace("Prorfessor", "Prof") %>%
  str_replace("[[:punct:]](.*)[[:punct:]]", "& \\1")

hires$ProfRank <- str_extract(hires$Faculty.rank, "((Adj Assoc)|(Adj Asst)|(Adj)|(Affiliate)|(Assoc)|(Asst)|(Distg)|(^)|(&)) ?((Lecturer)|(Prof)|(Clinician))")
hires$ProfRank <- hires$ProfRank %>% str_replace("^& ", "") %>% str_replace("(Adj (.*)?Prof)|(Lecturer)", "Adjunct/Lecturer")
hires$ProfRank[is.na(hires$ProfRank)] <- "Other"


hires$Admin <- str_detect(hires$Dept, "(College Admin)|(Director)") | str_detect(hires$Faculty.rank, "(Chair)|(Dir)|(VPR)|(Dean)|(Director)|(Provost)|(Prov)")

col.order <- c(col.order[1:4], "ProfRank", "Admin", col.order[-c(1:5)], "Faculty.rank")

# Remove duplicates
hires <- hires %>% group_by(Name) %>% arrange(Name, Source) %>% slice(1L)

# Clean up
rm(csvlist,tmp)

# Create variable for High impact hires, so that the (HIH) notation can be removed from the college
hires$High.Impact <- grepl("HIH", hires$College)
col.order <- c(col.order[1:6], "High.Impact", col.order[-c(1:6)])
# Format College appropriately
hires$College <- hires$College %>%
  str_replace(fixed(" (HIH)"), "") %>%
  str_replace("^AG$", "CALS") %>%
  str_replace("Agriculture", "CALS") %>%
  str_replace("CALS", "Ag & Life Science") %>%
  str_replace("AGLS", "Ag & Life Science") %>%
  str_replace("^BUS$", "Business") %>%
  str_replace("^CVM$", "Vet Med") %>%
  str_replace("^LA[Ss]$", "Liberal Arts & Science")  %>%
  str_replace("^E[Nn][Gg](ineering)?([Rr])?", "Engineering") %>%
  str_replace("Engineering & ", "Engineering/") %>%
  str_replace("^DSN$", "Design") %>%
  str_replace("^(ED)|(FCS)|(HS)|(CHS)$", "Hum Sci")%>%
  str_replace("^C?Hum Sci/Ext$", "Hum Sci")

# For faculty with two colleges, create a secondary college
hires$College.old <- hires$College
hires$CollegeSecondary <- str_extract(hires$College.old, "[/](.*)$") %>% str_replace("/", "")
hires$College <- str_extract(hires$College.old, "^[A-Za-z &]*/?") %>% str_replace("/", "")
col.order <- c(col.order[1:2], "CollegeSecondary", col.order[-c(1:2)], "College.old")
hires <- hires[,col.order] %>% arrange(Name, College, Dept)

# Read in program list
dept.prog <- read.csv("Data/ISUDepartments.csv", stringsAsFactors = F)

# Format Department appropriately
hires$Dept.old <- hires$Dept
hires$Extension <- str_detect(hires$Dept, "Ext(ension)?")
hires$Dept <- hires$Dept %>%
  str_replace("(ACCTG?)|(Accounting)", "Acct") %>%
  str_replace("A[eE][rR]O? ?E", "AerE") %>%
  str_replace("A&D", "Art") %>%
  str_replace("AESC?HM", "AESHM") %>%
  str_replace("Ag Ed & Studies", "AgEd") %>%
  str_replace("[aA][gG][rR][oO][nN](omy)?", "Agron") %>%
  str_replace("ABE & ", "ABE/") %>%
  str_replace("A & D", "Art") %>%
  str_replace("A[nN] S[cC][iI]", "AN SCI") %>%
  str_replace("Anthro(pology)", "Anthro") %>%
  str_replace("Arch(it?ecture)?", "Arch") %>%
  str_replace("Chemistry", "Chem") %>%
  str_replace("Community and Regional Planning", "CRP") %>%
  str_replace("Software Eng", "SoftE") %>%
  str_replace("^C( & )?I", "SOE") %>%
  str_replace("College Admin, E CPE", "ECpE") %>%
  str_replace("Comp?(uter)? S(ci)?", "Com S") %>%
  str_replace("E[cC][oO][nN](omics)?", "Econ") %>%
  str_replace("E?E ?CPE", "ECpE") %>%
  str_replace(", ", "/") %>%
  str_replace("EL ?PS", "ELPS") %>%
  str_replace("English", "Engl") %>%
  str_replace(" - IEOP", "/IEOP") %>%
  str_replace("Entomology", "ENT") %>%
  str_replace("(Fam Ext)|(Ext to Families)", "Extension") %>%
  str_replace("Finance", "FIN") %>%
  str_replace("(GE ?AT)|(Geology & Atm Sci)", "GeAt") %>%
  str_replace("Graphic Design", "GrD") %>%
  str_replace("History", "Hist") %>%
  str_replace("/Director of African American Studies", "/A&AAS") %>%
  str_replace(" \\(US Latino/a Studies\\)", "USLS")%>%
  str_replace("(HORT)|(Horticulture)", "Hort") %>%
  str_replace("Industrial Design", "IndD") %>%
  str_replace("Integrated Studio Art", "A&VC") %>%
  str_replace("Interior Design", "IntD") %>%
  str_replace("KIN", "Kin") %>%
  str_replace("Landscape Arch(itecture)?", "LA") %>%
  str_replace("(Management)|(MGMT)", "Mgmt") %>%
  str_replace("(Marketing)|(MKT)", "Mktg") %>%
  str_replace("Mathematics", "Math") %>%
  str_replace("MSE & ECpE", "MSE/ECpE") %>%
  str_replace("P(lant)?(LANT)? P(ath)?(ATH)?", "PLPM") %>%
  str_replace("(PATH)|(Pathology)", "VPath") %>%
  str_replace("Phil(osophy)? & Rel St", "Phil & RS") %>%
  str_replace(" & Astr(onomy)?(pn)?", "") %>%
  str_replace("Pol(itical)? Sci(ence)", "Pol Sci") %>%
  str_replace("Psyc?h?(ology)?", "Psych") %>%
  str_replace("S[oO][cC](iolo?ogy)?", "Soc") %>%
  str_replace("ELPS", "SOE") %>%
  str_replace("CCE ?E", "CCEE") %>%
  str_replace("S[Tt][Aa][Tt](istics)?", "Stat") %>%
  str_replace("VDPA[mM]", "VDPAM") %>%
  str_replace("V*[Pp][aA]?[tT][hH]", "VPath") %>%
  str_replace("WLC", "WL&C") %>%
  str_replace(" & African American Studies", "/A&AAS") %>%
  str_replace("Music & Theatre", "M&T") %>%
  str_replace("ECpE & MSE", "ECpE/MSE") %>%
  str_replace("Extension", "") %>%
  str_replace("^/|/$", "") %>% 
  str_replace("LOMIS", "SCIS") %>%
  str_replace("Art", "A&VC") %>%
  str_replace("RISE", "SOE") %>%
  str_replace(fixed("??"), "Acct") %>% # Dr. Lamboy-Ruiz is an accounting professor according to business.iastate.edu.
  str_replace("EADM", "ECpE") %>%
  str_replace("Animal Science", "AN SCI") %>%
  str_replace("HHP", "Kin")

hires$Dept1 <- str_extract(hires$Dept, "^[A-Za-z &]*/?") %>% str_replace("/", "")
hires$Dept2 <- str_extract(hires$Dept, "[/](.*)$") %>% str_replace_all("/", "")

dept2.is.prog <- hires$Dept2%in%dept.prog$Abbr[dept.prog$Type=="Prog"]

hires$Prog <- NA
hires$Prog[dept2.is.prog] <- hires$Dept2[dept2.is.prog]
hires$Dept[dept2.is.prog] <- str_replace(hires$Dept[dept2.is.prog], hires$Dept2[dept2.is.prog], "") %>% str_replace("(^/)|(/$)", "")
rm(dept2.is.prog)

hires$Dept1 <- str_extract(hires$Dept, "^[A-Za-z &]*/?") %>% str_replace("/", "")
hires$Dept2 <- str_extract(hires$Dept, "[/](.*)$") %>% str_replace_all("/", "")

col.order <- c(col.order[1:3], "Dept1", "Dept2", "Prog", "Extension", col.order[-c(1:3)], "Dept.old")

# Create  year variable for plotting
hires$Year <- as.numeric(gsub("FY", "", hires$Start.Date)) + 2000
hires$Year[!grepl("FY", hires$Start.Date)] <- NA
col.order <- c(col.order[1:2], "Year", col.order[-c(1:2)])
hires <- hires[,col.order]

# Create separate funding/allocation datasets
funding <- hires[,c("Name", "College", "Year", "Dept", "Dept1", "Dept2", "Prog", "Extension", "Admin", "Start.Date", "Faculty.rank", "High.Impact", "Pay.base", "Starting.salary", names(hires)[grepl("Funding", names(hires))])]

funding <- melt(funding, id.vars=c("Name", "College", "Year", "Dept", "Dept1", "Dept2", "Prog", "Extension", "Admin", "Start.Date", "Faculty.rank", "High.Impact", "Pay.base", "Starting.salary", "Total.Funding"), variable.name="Source", value.name="Amount")
funding$Source <- str_replace(funding$Source, "\\.Funding", "")

startup.package <- hires[,c("Name", "College", "Year", "Dept", "Dept1", "Dept2", "Prog", "Extension", "Admin", "Start.Date", "Faculty.rank", "High.Impact", "Pay.base", "Starting.salary", "Computer.peripherals", "Lab.space.equipment", "Graduate.assistants", "Summer.support", "Moving.expenses", "Research.support", "Other.Costs", "Total.Cost")]
startup.package <- melt(startup.package, id.vars=c("Name", "College", "Year", "Dept", "Dept1", "Dept2", "Prog", "Extension", "Admin", "Start.Date", "Faculty.rank", "High.Impact", "Pay.base", "Starting.salary", "Total.Cost"), variable.name="Item", value.name="Amount")

# Cleaning up -------------------------------------------------------------------
rm(format.cols, stripExtraCols, dept.prog, col.order, fylist)

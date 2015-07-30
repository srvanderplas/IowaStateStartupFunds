# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
library(ggplot2) # for plotting
library(scales) # for greater legend/scale control
# -------------------------------------------------------------------------------


# Read in and clean data --------------------------------------------------------
source("Code/CleanAwardsData.R")
source("Code/CleanProposalsData.R")
source("Code/CleanStartupFundingData.R")
# -------------------------------------------------------------------------------


# Summarize and merge -----------------------------------------------------------

received.money <- awards %>% 
  group_by(Investigator) %>% 
  summarize(grant.received = sum(Award.Amount), 
            n.awards = length(unique(Award.Number)))
received.money$grant.received[is.na(received.money$grant.received)] <- 0

submitted.proposals <- prop %>% 
  group_by(Investigator) %>% 
  summarize(total.proposal.money = sum(Proposal.Amount), 
            n.proposals = length(unique(Proposal.Number)), 
            prop.fund.pct = mean(Proposal.Status=="Funded"))
submitted.proposals$total.proposal.money[is.na(submitted.proposals$total.proposal.money)] <- 0

tmp <- merge(hires, received.money, by.x="Name", by.y="Investigator", all.x=T, all.y=F)
tmp <- merge(tmp, submitted.proposals, by.x="Name", by.y="Investigator", all.x=T, all.y=F)
tmp$grant.received[is.na(tmp$grant.received)] <- 0
tmp$total.proposal.money[is.na(tmp$total.proposal.money)] <- 0

tmp$ProfRankFac <- tmp$ProfRank %>% 
  str_replace("(Adjunct/Lecturer)|(Affiliate Prof)|(Clinician)", "Other") %>%
  str_replace("Distg ", "") %>%
  factor(levels=c("Asst Prof", "Assoc Prof", "Prof", "Other"), labels=c("Assistant", "Associate", "Professor", "Other"))

tmp$CollegeFac <- tmp$College %>% factor(labels=c("AGLS", "Bus", "Des", "Engr", "Hum Sci", "LAS", "Vet Med"))
tmp$College2 <- tmp$CollegeFac
tmp$College2[CollegeFac=="LAS" & Dept1%in%c("Anthro", "Econ", "Greenlee", "Pol Sci", "Psych", "Soc")] <- "LAS: Social Sci"
tmp$College2[CollegeFac=="LAS" & Dept1%in%c("BBMB", "Chem", "Com S", "EEOB", "GDCB", "GeAt", "Math", "Physics", "Stat")] <- "LAS: Science"
tmp$College2[CollegeFac=="LAS" & Dept1%in%c("Engl", "Hist", "Music", "Phil & RS", "WL&C")] <- "LAS: Arts"


# -------------------------------------------------------------------------------

# Graphs ------------------------------------------------------------------------


qplot(Year, grant.received, data=tmp, geom="jitter", color=CollegeFac, shape=CollegeFac, size=I(4)) + 
  scale_shape_manual(values=c('A', 'B', 'D', 'E', 'H', 'L', 'V')) + 
  guides(shape=guide_legend(ncol=4), color=guide_legend(ncol=4)) + 
  facet_grid(~ProfRankFac) + 
  xlab("Year Hired") + ylab("Total Grant $ Received") + ggtitle("Grant Receipts by Year of Hire") + 
  theme_bw() +  theme(legend.position="bottom")


qplot(Total.Cost, grant.received, data=tmp, geom="point", color=CollegeFac, shape=CollegeFac, size=I(4)) + 
  geom_smooth(method="loess", inherit.aes=F, aes(x=Total.Cost, y=grant.received)) + 
  scale_shape_manual(values=c('A', 'B', 'D', 'E', 'H', 'L', 'V')) + 
  guides(shape=guide_legend(ncol=4), color=guide_legend(ncol=4)) + 
  scale_x_continuous(breaks=c(0, 30000, 100000, 300000, 2000000), 
                     labels=c("$0", "$30K", "$100K", "$300K", "$2M"), 
                     trans="sqrt", limits=c(0, 2000000)) + 
  scale_y_continuous(breaks=c(0, 50000, 100000, 300000, 1000000, 10000000, 50000000), 
                     labels=c("$0", "$50K", "$100K", "$300K", "$1M", "$10M", "$50M"), 
                     trans="sqrt", limits=c(0, 50000000)) + 
  xlab("Startup Package Cost") + ylab("Total Grant $ Received") + 
  ggtitle("Startup Package Cost/Benefit in Grant Receipts") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical")

qplot(Total.Cost+1, grant.received+1, data=subset(tmp, Total.Cost>1000 & ProfRankFac!="Other" & !CollegeFac%in%c("Bus", "Des") & Year<2014), geom="point", color=CollegeFac, shape=CollegeFac, size=I(4)) + 
  geom_smooth(method="lm", aes(x=Total.Cost, y=grant.received), se=F, color="black") + 
  scale_shape_manual(values=c('A', 'E', 'H', 'L', 'V')) + 
  guides(shape=guide_legend(ncol=4), color=guide_legend(ncol=4)) + 
  facet_grid(CollegeFac~ProfRankFac, scales="free_y") + 
  xlab("Startup Package Cost") + ylab("Total Grant $ Received") + 
  ggtitle("Startup Package Cost/Benefit in Grant Receipts (FY06-13)") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical") +
  scale_x_continuous(breaks=c(10000, 100000, 1000000, 2000000), 
                     labels=c("$10K", "$100K", "$1M", "$2M"), trans="log") + 
  scale_y_continuous(breaks=c(1, 50000, 300000, 1000000, 10000000, 50000000), 
                     labels=c("$0", "$50K",  "$300K", "$1M", "$10M", "$50M"), trans="log") 


qplot(Total.Cost, total.proposal.money, data=tmp, geom="point", color=CollegeFac, shape=CollegeFac, size=I(4)) + 
  geom_smooth(method="loess", inherit.aes=F, aes(x=Total.Cost, y=total.proposal.money)) + 
  scale_shape_manual(values=c('A', 'B', 'D', 'E', 'H', 'L', 'V')) + 
  guides(shape=guide_legend(ncol=4), color=guide_legend(ncol=4)) + 
  scale_x_continuous(breaks=c(0, 30000, 100000, 300000, 2000000), 
                     labels=c("$0", "$30K", "$100K", "$300K", "$2M"), 
                     trans="sqrt", limits=c(0, 2000000)) + 
  scale_y_continuous(breaks=c(0, 10000, 100000, 300000, 1000000, 10000000, 100000000, 300000000), 
                     labels=c("$0", "$50K", "$100K", "$300K", "$1M", "$10M", "$100M", "$300M"), 
                     trans="sqrt", limits=c(0, 300000000)) + 
  xlab("Startup Package Cost") + ylab("Total $ in Grants Proposed") + 
  ggtitle("Startup Package Cost/Benefit in Proposals") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical")


qplot(Total.Cost/100000, total.proposal.money/100000, 
      data=subset(tmp, ProfRankFac!="Other" & 
                    !CollegeFac%in%c("Bus", "Des") & Year<2014), 
      geom="point", color=CollegeFac, shape=CollegeFac, size=I(4)) + 
  geom_smooth(method="lm", se=F, color="black") + 
  scale_shape_manual(values=c('A', 'E', 'H', 'L', 'V'), guide=F) + 
  scale_color_discrete(guide=F) + 
  facet_grid(CollegeFac~ProfRankFac, scales="free_y") + 
  xlab("Startup Package Cost ($100K)") + ylab("Total $ in Grants Proposed ($100K)") + 
  ggtitle("Startup Package Cost/Benefit in Proposals (FY06-13)") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical") +
  scale_x_sqrt(breaks=c(0, 1, 5, 10, 15)) + 
  scale_y_sqrt(breaks=c(0, 10, 100, 1000, 2500))


ggplot(aes(x=Total.Cost/100000, y=grant.received/100000, color=CollegeFac), data=tmp) + 
  geom_density2d() + 
  # geom_point() +
  geom_abline(aes(intercept=0, slope=1)) + 
  facet_wrap(~CollegeFac, scales="free") + 
  guides(shape=guide_legend(ncol=4), color=guide_legend(ncol=4)) + 
  xlab("Startup Package Cost ($100K)") + ylab("Total $ in Grants Proposed ($100K)") + 
  ggtitle("Startup Package Cost/Benefit in Proposals (FY06-15)") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical")

ggplot(data=tmp, aes(x=Year, y=grant.received/100000, fill=CollegeFac, group=round_any(Year, 1))) + 
  geom_boxplot(color="black") + facet_wrap(~CollegeFac, scales="free_y", ncol=4) + scale_fill_discrete(guide=F) +
  theme_bw() +
  xlab("Year") + ylab("Grants Received, $100K")

ggplot(data=tmp, aes(x=Year, y=grant.received/100000, fill=CollegeFac, group=round_any(Year, 1))) + 
  geom_jitter(aes(color=CollegeFac)) + facet_wrap(~CollegeFac, scales="free_y", ncol=4) + 
  scale_color_discrete(guide=F) + scale_fill_discrete(guide=F) +
  theme_bw() +
  xlab("Year") + ylab("Grants Received, $100K") + ggtitle("Grants Received by Year of Hire")
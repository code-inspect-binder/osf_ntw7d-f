##### DATA WRANGLING #####

### Data import ###

# Read and make an object for  SRMA data sheet 
library(tidyverse)

srmadata <- read_csv("SRMAdata.csv")

View(srmadata)

### Data organization ###

# Rename columns/variable names to make it simpler variable names

colnames(srmadata)
 
srmadata <- srmadata %>% rename(assessors = 2, journal = 3, pubyear = 4, 
                                pubmonth = 5, study.title = 6, pmid = 7, 
                                regist = 8, regist.num = 9, protocol = 10, 
                                title.ident = 11, ab.sources = 12, ab.eleg.crit = 13, 
                                ab.particip = 14, ab.interv = 15, ab.effect = 16,
                                ab.included = 17, ab.outcome = 18, in.picos = 19, 
                                me.database = 20, me.search.avai = 21, me.grey.lit = 22, 
                                me.date.just = 23, me.lang.num = 24, me.picos.desc = 25,
                                me.sele.dup = 26, me.extr.dup = 27, me.rob.desc = 28, 
                                me.rob.dup = 29, me.stat.desc = 30, me.heterog = 31,
                                item.removed01 = 32, re.flowdia = 33, re.ssizes = 34,
                                re.picos.desc = 35, re.lengths = 36, re.estim.desc = 37, 
                                re.meta.studies = 38, re.rob = 39, re.deviations = 40, di.spin = 41,
                                di.rob.studies = 42, di.limitations = 43, data.statem = 44,
                                fund.statem = 45, funders = 46, coi.statem = 47)


# Delete column ("item.removed01") which removed from SEES assessment form during 2019

srmadata$item.removed01 <- NULL

View(srmadata)

# Redefine columns types to factor

srmadata <- data.frame(srmadata)
srmadata %>% mutate_if(is.character, as.factor) %>% str()

# Set dataset for transparency domain, 4 variables included

transparency <- srmadata[c("regist", "protocol", "me.search.avai", "data.statem")]

# Set dataset for completeness domain, 11 variables included

completeness <- srmadata[c("title.ident", "ab.sources", "ab.eleg.crit", "ab.included", "in.picos", "me.picos.desc",
                           "re.flowdia", "re.ssizes", "re.lengths", "fund.statem", "coi.statem")]

# Set dataset for participants domain, 2 variable included

participant <- srmadata[c("ab.particip", "re.picos.desc")]

# Set dataset for intervention/exposure domain, 1 variable included

intervention <- srmadata[c("ab.interv", "re.picos.desc")]

# Set dataset for outcome domain, 5 variables included

outcome <- srmadata[c("ab.outcome", "me.stat.desc", "me.heterog", "re.estim.desc", "re.meta.studies")]

# Set dataset for methodological rigor domain, 7 variables included

rigor <- srmadata[c("me.grey.lit", "me.date.just", "me.lang.num", "me.sele.dup", "me.extr.dup", "me.rob.desc", "me.rob.dup")]

# Set dataset for critical appraisal domain, 5 variables included

appraisal <- srmadata[c("re.rob", "re.deviations", "di.spin", "di.rob.studies", "di.limitations")]

### Data standardization (cell levels) ###

# transparency, replace "Yes - Registered in PROSPERO" by "Yes"

transparency$regist[transparency$regist == "Yes - Registered in PROSPERO"] <- "Yes"

# completeness, replace all the 3 suboptions of "Yes" for COI disclosure by "Yes"

completeness$coi.statem[completeness$coi.statem == "Yes, authors declare non-financial COIs"] <- "Yes"
completeness$coi.statem[completeness$coi.statem == "Yes, authors declare financial COIs"] <- "Yes"
completeness$coi.statem[completeness$coi.statem == "Yes, authors declare financial and non-financial COIs"] <- "Yes"

# methodological rigor, replace long sentence for searches since database inception to "Yes" (7 "Yes" occurences already existing)

rigor$me.date.just[rigor$me.date.just == "The searches were carried out from the database inception (earliest date)"] <- "Yes"

# critical appraisal, replace suboptions of "Yes" (e.g., "Partial Yes") to "Yes"

appraisal$re.rob[appraisal$re.rob == "Yes (FULL description)"] <- "Yes"
appraisal$re.deviations[appraisal$re.deviations == "Yes (FULL description)"] <- "Yes"
appraisal$di.limitations[appraisal$di.limitations == "Yes (FULL description)"] <- "Yes"

##### DESCRIPTIVE ANALYSIS #####

library(DescTools)
Desc(transparency, plotit=F)
Desc(completeness, plotit=F)
Desc(participant, plotit=F)
Desc(intervention, plotit=F)
Desc(outcome, plotit=F)
Desc(rigor, plotit=F)
Desc(appraisal, plotit=F)

### Add labels to variables in transparency dataframe; then, generate table

table1::label(transparency$regist) <- "Registration"
table1::label(transparency$protocol) <- "Protocol"
table1::label(transparency$me.search.avai) <- "Available searches"
table1::label(transparency$data.statem) <- "Data Statement"

table1::table1(~regist + protocol + me.search.avai + data.statem, data = transparency)

### Add labels to variables in completeness dataframe; then, generate table

table1::label(completeness$title.ident) <- "Title as SRMA"
table1::label(completeness$ab.sources) <- "Data sources (ab)"
table1::label(completeness$ab.eleg.crit) <- "Key eligibility criteria (ab)"
table1::label(completeness$ab.included) <- "Number of included studies (ab)"
table1::label(completeness$in.picos) <- "Research question"
table1::label(completeness$me.picos.desc) <- "PICOS explanation"
table1::label(completeness$re.flowdia) <- "Number of references"
table1::label(completeness$re.ssizes) <- "Description of sample sizes"
table1::label(completeness$re.lengths) <- "Duration of included studies"
table1::label(completeness$fund.statem) <- "Sources of funding"
table1::label(completeness$coi.statem) <- "Potential conflicts of interest"

table1::table1(~title.ident + ab.sources + ab.eleg.crit + ab.included + in.picos + me.picos.desc +
re.flowdia + re.ssizes + re.lengths + fund.statem + coi.statem, data = completeness)

### Add labels to variables in participant dataframe; then, generate table

table1::label(participant$ab.particip) <- "Description of participants (ab)"
table1::label(participant$re.picos.desc) <- "Detailed studies' characteristics"

table1::table1(~ab.particip + re.picos.desc, data = participant)

### Add labels to variables in intervention/exposure dataframe; then, generate table

table1::label(intervention$ab.interv) <- "Description of interventions/exposures (ab)"
table1::label(intervention$re.picos.desc) <- "Detailed studies' characteristics"

table1::table1(~ab.interv + re.picos.desc, data = intervention)

### Add labels to variables in outcome dataframe; then, generate table

table1::label(outcome$ab.outcome) <- "Main outcome of interest (ab)"
table1::label(outcome$me.stat.desc) <- "Statistical methods"
table1::label(outcome$me.heterog) <- "Statistical heterogeneity"
table1::label(outcome$re.estim.desc) <- "Meta-analytic summary estimates"
table1::label(outcome$re.meta.studies) <- "Statistics per study"

table1::table1(~ab.outcome + me.stat.desc + me.heterog + re.estim.desc + re.meta.studies, data = outcome)

### Add labels to variables in rigor dataframe; then, generate table

table1::label(rigor$me.grey.lit) <- "Searches in grey literature"
table1::label(rigor$me.date.just) <- "Searches from inception or with justification"
table1::label(rigor$me.lang.num) <- "Number of languages"
table1::label(rigor$me.sele.dup) <- "Study selection in duplicate"
table1::label(rigor$me.extr.dup) <- "Data extraction in duplicate"
table1::label(rigor$me.rob.desc) <- "Description of RoB assessment"
table1::label(rigor$me.rob.dup) <- "RoB assessment in duplicate"

table1::table1(~me.grey.lit + me.date.just + me.lang.num + me.sele.dup + me.extr.dup + me.rob.desc + me.rob.dup, data = rigor)

### Add labels to variables in appraisal dataframe; then, generate table

table1::label(appraisal$re.rob) <- "RoB results within studies"
table1::label(appraisal$re.deviations) <- "Description of protocol deviations"
table1::label(appraisal$di.spin) <- "Presence of spin bias"
table1::label(appraisal$di.rob.studies) <- "Discussion addressing RoB"
table1::label(appraisal$di.limitations) <- "Limitations thoroughly addressed"

table1::table1(~re.rob + re.deviations + di.spin + di.rob.studies + di.limitations, data = appraisal)

# Variables with results others than "Yes"/"No", per domain:
# transparency: none
# completeness: re.lengths
# participant: none
# intervention: none
# outcome: none
# rigor: me.date.just, me.lang.num, me.sele.dup, me.extr.dup, me.rob.dup
# appraisal: re.rob, re.deviations, di.limitations

### Copy dataframes which will be changed for binary setting (yes/no)

completenessbinary <- completeness
rigorbinary <- rigor
appraisalbinary <- appraisal

### Replace non-binary variables (re.lengths) to a binary variable

View(completenessbinary)
completenessbinary$re.lengths[completenessbinary$re.lengths == "Does not apply"] <- "Yes"

### Replace non-binary variables (me.lang.num; me.sele.dup; me.extr.dup) to a binary variable
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "No statement"] <- "No"
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "1"] <- "No"
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "2"] <- "Yes"
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "3"] <- "Yes"
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "4"] <- "Yes"
rigorbinary$me.lang.num[rigorbinary$me.lang.num == "No restriction"] <- "Yes"

rigorbinary$me.sele.dup[rigorbinary$me.sele.dup == "Partial Yes (e.g., a sample of 50% of studies were checked by two independent researchers)"] <- "Yes"

rigorbinary$me.extr.dup[rigorbinary$me.extr.dup == "Partial Yes (e.g., a sample of 50% of studies were checked by two independent researchers)"] <- "Yes"

### Replace non-binary variables (re.rob; re.deviations; di.limitations) to a binary variable

appraisalbinary$re.rob[appraisalbinary$re.rob == "Partial Yes (there are individual results without specification of specific criteria/domains)"] <- "Yes"

appraisalbinary$re.deviations[appraisalbinary$re.deviations == "Does not apply"] <- "Yes"
appraisalbinary$re.deviations[appraisalbinary$re.deviations == "Unclear"] <- "No"

appraisalbinary$di.limitations[appraisalbinary$di.limitations == "Yes, BOTH for study and review levels"] <- "Yes"
appraisalbinary$di.limitations[appraisalbinary$di.limitations == "Yes, ONLY for the review level (limitation within or across studies not mentioned)"] <- "Yes"
appraisalbinary$di.limitations[appraisalbinary$di.limitations == "Yes, ONLY for the study and/or outcome level (review processes not mentioned)"] <- "Yes"

##### HISTOGRAM #####

### Generate a histogram of density of scores achieved by the 104 assessed studies

# Create a 0 or 1 dataset, where No=0 and Yes=1, with respective study IDs (variable "id")
id <- (1:104)
allbinary <- data.frame(id, transparency, completenessbinary, participant, intervention, outcome, rigorbinary, appraisalbinary)
allbinary[allbinary == "Yes"] <- "1"
allbinary[allbinary == "No"] <- "0"
lapply(allbinary,as.numeric)

# Invert the coding (0 or 1) for spin bias, because "Yes" is a practice NOT recommended for this variable and "No" is recommended practice (do not exaggerate evidence interpretation)
allbinary$di.spin[allbinary$di.spin == "0"] <- "No"
allbinary$di.spin[allbinary$di.spin == "1"] <- "Yes"
allbinary$di.spin[allbinary$di.spin == "No"] <- "1"
allbinary$di.spin[allbinary$di.spin == "Yes"] <- "0"

# Set the dataframe as numeric so that we can sum up recommended practices for each study (variable "yes.score")
allbinary[] <- lapply(allbinary, function(x) as.numeric(as.character(x)))

# Create a new dataframe (binarytotalsdf) with a new variable (yes.score) that reflects the number of recommended practices from each study
binary-scoredf <- allbinary %>% 
    mutate(yes.score = regist + protocol + me.search.avai + data.statem + + title.ident +
                     + ab.sources + ab.eleg.crit + ab.included + in.picos + me.picos.desc + re.flowdia +  
                     + re.ssizes + re.lengths + fund.statem + coi.statem + ab.particip + re.picos.desc + 
                     + ab.interv + re.picos.desc + ab.outcome + me.stat.desc + me.heterog + re.estim.desc + 
                     + re.meta.studies + me.grey.lit + me.date.just + me.lang.num + me.sele.dup + me.extr.dup + 
                     + me.rob.desc + me.rob.dup +re.rob + re.deviations + di.spin + di.rob.studies +
                     + di.limitations)

# Create the histogram

library(ggplot2)

histplot.score <- ggplot(binary-scoredf, aes(x=yes.score)) + 
    geom_histogram(binwidth=1, color="black", fill="lightblue")

histplot.score + scale_x_continuous(name="Number of recommended practices (max: 36 items)", breaks=seq(0,36,2)) +
             scale_y_continuous(name="Frequency of publications", limits=c(0, 20)) +
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Create the waflle plot - Made in flourish.studio (external website)

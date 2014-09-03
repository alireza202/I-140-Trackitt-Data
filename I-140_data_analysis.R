##########################################################################################
# R script to analyse I-140 data from trackitt.com
##########################################################################################

library(ggplot2)

##########################################################################################
stat = function(x, column) {
    # this function creates a new df from the main df based on the factors from one column
    # the new df contains the percentage of cases for each factor in "column", and also
    # the percentages of approved, pending, and denied cases for each factor.
    # x is a data frame
    # column is a column name
    l = length(unique(x[, column]))
    num = length(x[, column])
    
    y = data.frame(types = unique(x[, column]), 
                   percentage = rep(NA, l),
                   accepted = rep(NA, l),
                   pending = rep(NA, l),
                   denied = rep(NA, l))
    for (case in unique(x[, column])) {
        y$percentage[y$types == case] = 
            sum(x[, column] == case) / num * 100
        y$accepted[y$types == case] = 
            sum(x[, column] == case & x$Application_Status == "approved") / 
            sum(x[, column] == case) * 100
        y$pending[y$types == case] = 
            sum(x[, column] == case & x$Application_Status == "pending") / 
            sum(x[, column] == case) * 100
        y$denied[y$types == case] = 
            sum(x[, column] == case & x$Application_Status == "denied") / 
            sum(x[, column] == case) * 100
    }
    y
}

##########################################################################################
# reading in the formatted information into the main dataframe
# put the correct path to the file here
mt <- read.delim("~/.../I_140_all_info_formatted.txt", 
                 quote="",
                 na.strings = ".",
                 stringsAsFactors = TRUE)

##########################################################################################
# cleaning the data
# correcting the date format
mt$Application_Filed = as.Date.character(mt$Application_Filed, format = "%m/%d/%y")
mt$USCIS_Received_Date =  as.Date.character(mt$USCIS_Received_Date, format = "%m/%d/%y")
mt$USCIS_Notice_Date =  as.Date.character(mt$USCIS_Notice_Date, format = "%m/%d/%y")
mt$RFE_Received_Date =  as.Date.character(mt$RFE_Received_Date, format = "%m/%d/%y")
mt$RFE_Replied_Date =  as.Date.character(mt$RFE_Replied_Date, format = "%m/%d/%y")
mt$Approval_Denial_Date =  as.Date.character(mt$Approval_Denial_Date, format = "%m/%d/%y")

# removing the incomplete and useless data
mt$Reason_RFE = NULL
mt = mt[- which(is.na(mt$Category)), ]
mt = mt[- which(is.na(mt$Nationality)), ]
mt = mt[- which(is.na(mt$Application_Filed) & is.na(mt$USCIS_Received_Date)), ]

# choosing the start date to be the date that the application was filed. In case it's NA, the value will be taken from the date that it was received by USCIS
mt$start_date = mt$Application_Filed
mt$start_date[is.na(mt$start_date)] = mt$USCIS_Received_Date[is.na(mt$start_date)]

mt$total_days = as.numeric(mt$Approval_Denial_Date - mt$start_date)
mt = mt[ - which(mt$Application_Status == "approved" & is.na(mt$total_days)), ]
mt = mt[ - which(mt$total_days < 0), ]

# removing the cases before 2000
mt = mt[ - which(mt$start_date < as.Date("2000-01-01")), ]

# removing EB4 and EB5
mt = mt[ - which(mt$Category == "EB4" | mt$Category == "EB5"), ]

##########################################################################################
# percentage of applicants in each category, and the correspondent percentages of accepted, pending, and denied cases
stat_all_cases = stat(mt, "Category")
stat_all_cases = stat_all_cases[order(stat_all_cases$types),]
print(stat_all_cases)

# countries leading in the number of applicants
cat("EB2-NIW")
NIW = mt[ mt$Category == "EB2-NIW", ]
n = dim(NIW)[1]
head(sort(table(NIW$Nationality) / n * 100 , decreasing = TRUE))

cat("All categories")
head(sort(table(mt$Nationality) / N * 100, decreasing = TRUE))

# how about checking if there is any significant difference between countries 
# in NIW category?
niw_countries = names(head(sort(table(NIW$Nationality) / n * 100 , decreasing = TRUE)))
mt_countries = names(head(sort(table(mt$Nationality) / N * 100 , decreasing = TRUE)))

NIW_top_countries = NIW[ NIW$Nationality %in% niw_countries, ]
stat_niw_top_countries = stat(NIW_top_countries, "Nationality")
stat_niw_top_countries

pvalue = fisher.test(as.matrix(floor(
    stat_niw_top_countries[stat_niw_top_countries$types 
                           %in% c("Pakistan", "South Korea"), c(3, 4, 5)])))$p.value

# what about for all categories?
mt_top_countries = mt[ mt$Nationality %in% mt_countries, ]
stat_mt_top_countries = stat(mt_top_countries, "Nationality")
stat_mt_top_countries

pvalue = fisher.test(as.matrix(floor(
    stat_mt_top_countries[stat_mt_top_countries$types 
                          %in% c("Pakistan", "South Korea"), c(3, 4, 5)])))$p.value

# focusing on the approved case, I was curious to see what's the difference 
# between the total time it takes to be approved between different categories. 
# categories are further divided by their processing speed.

mt_subset = mt[ mt$category_speed %in% c("EB2_regular", "EB2_premium", "EB2-NIW_regular", "EB3_regular", "EB3_premium") & mt$Application_Status == "approved" & mt$start_date > as.Date("2004-01-01"), ]
ggplot(data = mt_subset, aes(x = start_date, y = total_days, color = category_speed)) +
    geom_point(alpha = 0.1) +
    geom_smooth(size = 1) +
    scale_y_continuous(limits = c(0, 400)) +
    xlab("Submission Date") +
    ylab("Total Days") +
    theme_bw()

# focusing on EB2-NIW, we see that the processing speed has a clear periodic pattern!
mt_subset = mt[ mt$category_speed %in% c("EB2-NIW_regular") & mt$Application_Status == "approved" & mt$start_date > as.Date("2000-01-01"), ]
ggplot(data = mt_subset, aes(x = start_date, y = total_days, color = category_speed)) +
    geom_point(alpha = 0.4) +
    stat_smooth(size = 1, level = 0.99) +
    scale_y_continuous(limits = c(0, 400)) +
    xlab("Submission Date") +
    ylab("Total Days")

# does total time it takes to be accepted depend on nationality?
niw_countries = names(head(sort(table(NIW$Nationality) / n * 100 , decreasing = TRUE)))
mt_subset = NIW[ NIW$Nationality %in% niw_countries, ]
ggplot(data = mt_subset, aes(x = factor(Nationality), y = total_days)) +
    geom_boxplot(aes(fill = Nationality)) +
    geom_jitter(alpha = 0.2) +
    xlab("") +
    ylab("Total Days") +
    theme_bw() +
    theme(legend.position="none")

# again in EB2-NIW, you can either apply for I-140 concurrently with I-485, 
# or not. but would that change the processing time, or the likelihood to be approved?
cat("Concurrent application")
summary(mt$total_days[ mt$I.140_Filing == "concurrent" & mt$Category == "EB2-NIW"])

cat("Non-concurrent application")
summary(mt$total_days[ mt$I.140_Filing == "non-concurrent" & mt$Category == "EB2-NIW"])

pval = t.test(mt$total_days[ mt$I.140_Filing == "concurrent" & mt$Category == "EB2-NIW"],
              mt$total_days[ mt$I.140_Filing == "non-concurrent" & mt$Category == "EB2-NIW"],
              alternative = "two.sided")$p.value
stat_conc = stat(NIW, "I.140_Filing")
stat_conc

# how do pending cases accumulate over time?
mt_subset = mt[ mt$Category == "EB2-NIW" & mt$Application_Status == "pending", ]
ggplot(mt_subset, aes(x = start_date)) + 
    geom_histogram(binwidth = 60, color="white") +
    theme_bw()

# how about RFE? How does receiving one changes the chance of getting approved?
mt$RFE_Received[is.na(mt$RFE_Received)] = "no"
stat_niw_rfe = stat(mt[ mt$Category == "EB2-NIW", ], "RFE_Received")
print(stat_niw_rfe)
pval = fisher.test(as.matrix(floor(stat_niw_rfe[, c(3, 4, 5)])))$p.value
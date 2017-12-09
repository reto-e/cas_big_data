#read emails
library(rio)
raw <- import("C:/Users/reei/Documents/zue_done_folder.CSV")
str(raw)
mails <- data.frame(raw$`From: (Name)`, raw$`To: (Name)`)
head(mails)
colnames(mails) <- c("from", "to")

# get a small sample
mails_sample <- mails[sample(nrow(mails), 1000), ]

# turn factors into characters
mails_sample[] <- lapply(mails_sample, as.character)

# remove empty values
mails_sample <- mails_sample[mails_sample$from != "" & mails_sample$to != "", ]
i <- 1
mail_data <- data.frame(from = character(), to = character())
for (i in 1: nrow(mails_sample)) {
  # split to into vector of single receipients
  from <- as.character(mails_sample[i,1])
  tos <- mails_sample[i,2]
  tos <- as.character(tos)
  tos <- strsplit(tos, ";")
  to <- unlist(tos)
  
  # create new data.frame with split values
  newLine <- data.frame(from, to)
  
  # rbind new data.frame to data.frame
  mail_data <- rbind(mail_data, newLine)
  
}
dat <- table(mail_data$from, mail_data$to)
dat <- as.data.frame(dat)
dat <- dat[dat$Freq > 3, ] # keep only entries with frequency > 1 :: -> grosser Aha-Moment!

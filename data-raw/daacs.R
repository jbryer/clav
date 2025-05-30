load("data-raw/DAACS-WGU-SRL.rda")

cluster_vars <- c('Motivation', 'Metacogntion', 'Strategies', 'Mathematics', 'Reading', 'Writing')
names(daacs.wgu)[1:6] <- cluster_vars

daacs.wgu <- daacs.wgu[complete.cases(daacs.wgu[,cluster_vars]),]
daacs.wgu$LogFeedbackViews <- log(daacs.wgu$FeedbackViews)

daacs <- daacs.wgu[,c(cluster_vars, 'FeedbackViews', 'OnTime_Term1')]
names(daacs)[8] <- 'TermSuccess'

usethis::use_data(daacs, overwrite = TRUE)

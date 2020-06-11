
df <- data.frame(a = c('a', 'b', 'c'), b = c(1, 2, 3))

#setup a new trackr
t <- new_trackr(df, timepoint_message = 'Start')

#make some change to the data 
t <- t %>% mutate(b = b + 1)

#log this change to the data
t <- trackr_timepoint(t, timepoint_message = 'Change point #1')

#repeat for subsequent processing steps
t <- t %>% mutate(b = b + 300)

t <- trackr_timepoint(t, timepoint_message = 'Change point #2')

#what if rows are rearranged?
t <- t %>% arrange(-b)

#should register as no changes - correct
trackr_timepoint(t, timepoint_message = 'Change point #3')

#splitting a single record into multiples
t <- rbind(t, t %>% mutate(b = b -300))

t <- trackr_timepoint(t, timepoint_message = 'Dividing rows')

#summarising data
t <- t %>% group_by(a)

t <- trackr_summarise(t, n = n())

t %>% pull(1) %>% length()

#output needs to remain grouped - 
t <- trackr_timepoint(t, timepoint_message = 'Summarising rows')

t %>% pull(1) %>% length()

t <- t %>% mutate(n = n**runif(3))

t <- trackr_timepoint(t, timepoint_message = 'Further Processing')

trackr_network('~/Documents/Personal/trackr/trackr_dir')






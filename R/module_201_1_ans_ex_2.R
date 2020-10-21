## module 1, exercise 2 answer

# use with other code from the module. 

# bar plot
ggplot(data_1j_lf, aes(key, value)) +
    geom_bar(stat = "summary", fun = "mean")

# calculating
data_1j_lf %>%
    group_by(key) %>%
    summarise(mean(value)) -> data_1j_stats
colnames(data_1j_stats) <- c("key", "mean")

# mean as a line and geom_jitter
ggplot(data_1j_stats, aes(key, mean)) +
    geom_point(shape = 95, size = 8) +
    geom_jitter(data=data_1j_lf,
    aes(key, value, colour=key), width = 0.1)




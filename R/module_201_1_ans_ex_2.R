## module 1, exercise 2 answer

# use with other code from the module. 

# bar plot
ggplot(data_1j_lf, aes(name, value)) +
    geom_bar(stat = "summary", fun = "mean")

# calculating
data_1j_lf %>%
    group_by(name) %>%
    summarise(mean(value)) -> data_1j_stats
colnames(data_1j_stats) <- c("name", "mean")

# mean as a line and geom_jitter
ggplot(data_1j_stats, aes(name, mean)) + 
    geom_point(shape = 95, size = 8) +
    geom_jitter(data=data_1j_lf, 
                aes(name, value, colour=name), width = 0.1)



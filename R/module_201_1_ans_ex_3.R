## --------------------------------------------------------------------------------
## module 1, exercise 3 answer

# use with other code from the module. 

ggplot(data_1j_lf, aes(key, value, colour=key)) +
    geom_boxplot() +
    geom_jitter(width = 0.1)

ggplot(data_1j_lf, aes(key, value, colour=key)) +
    geom_violin() +
    geom_jitter(width = 0.1)



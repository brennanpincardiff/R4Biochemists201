# start with our data
tidy_data %>%
    # filter the row with the Cxcl14 measurements
    filter(Cytokine == "MMP9") -> mmp9

ggplot(mmp9, aes(x=thy_status, y= value)) + 
    geom_bar(stat = "summary", fun = "mean") -> plot

plot

# calculate stats using a t.test using t.test()
output <- t.test(mmp9[1:6,3], mmp9[7:12,3])
output # gives the results
output$p.value # gives the p-value

# the package ggpubr extends ggplot
# we can use the function stat_compare_means with the t.test method. 
# gives us a nice label on our graph...
# Welch Two Sample t-test
plot + stat_compare_means(method = "t.test")

plot + stat_compare_means(method = "t.test") +
    labs(x = "Thy Status",
         y = "MMP9 levels (ng/ml-1)", 
         title = "from Figure 4a",
         subtitle = "Croft et al, Nature, 2019") +
    theme_classic()


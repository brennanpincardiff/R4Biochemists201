
# read in easier data than the download.
link <- ("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/fig_2d_altered.xlsx")
download.file(url=link, destfile="fig2d_altered.xlsx", mode="wb")

data <- read_excel("fig2d_altered.xlsx", skip = 1)

# gather(), separate() and mutate() into factors...
tidy_data <- data %>%
    gather() %>%
    separate(key, into=c("names", "DTR_status"), sep = "/") %>%
    # need to turn the names into factors...
    mutate(DTR_status = factor(DTR_status)) %>%
    mutate(names = factor(names, c("bone_eros_CT", "bone_form_CT", 
                                   "area_bone_eros", "area_bone_form",
                                   "area_pan", "area_dest_cart")))


# simple bar plot calculating the means and using facet_wrap()
ggplot(tidy_data, aes(x=DTR_status, y=value, fill = DTR_status)) +
    stat_summary(fun="mean", geom="bar") +   # calculates means
    scale_fill_manual(values=c("white", "grey")) + 
    facet_wrap(~names, scales ="free_y", ncol = 2)

# to put on error bars need to summarise to calculate mean and SD. 
# more on error bars: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
tidy_data %>%
    group_by(names, DTR_status) %>%
    summarise(sd = sd(value), value = mean(value)) -> tidy_data_stats

# just stats first...
my_comparisons <- list( c("DTR-", "DTR+"))
p1 <- ggplot(data = tidy_data, aes(x=DTR_status, y=value)) + 
    stat_compare_means(comparisons = my_comparisons, method = "t.test",
                       paired = TRUE, size = 3,
                       symnum.args = list(cutpoints = c(0,0.0001,0.005,0.001, 1), 
                                          symbols = c("****","***", "***", "ns"))) +
    theme_classic2() +
    theme(strip.background = element_rect(fill = FALSE)) +
    facet_wrap(~names, scales ="free_y", ncol = 2)

p1

# then add bars
p1 <- p1 +  
    geom_bar(data = tidy_data_stats, 
             aes(x=DTR_status, y=value,  fill=DTR_status),
             stat="identity",
             position=position_dodge(),
             color = "black") +  
    scale_fill_manual(values=c("white", "grey"), name = "") +
    theme(legend.title = NULL, legend.position="top") +
    
    facet_wrap(~names, scales ="free_y", ncol = 2,              
               # moving title to side of plot
               strip.position = "left",
               # full titles for the y-axis
               labeller = as_labeller(c(bone_eros_CT = "Bone eroision \n score micro-CT",
                                        bone_form_CT= "Bone formation \n score micro-CT",
                                        area_bone_eros= "Area bone \n erosion (%)",
                                        area_bone_form = "Area bone \n formation (%)",
                                        area_pan = "Area pannus \n tissue (%)", 
                                        area_dest_cart = "Area destained \n cartilage (%)"))) +
    # geom_errorbar() adds the error bars :-)
    geom_errorbar(data = tidy_data_stats, 
                  aes(ymin=value-sd, ymax=value+sd), 
                  width=.2,
                  position=position_dodge(.9))

### then add geom_jitter to put points at the top...

p1 <- p1 + geom_jitter(data = tidy_data, 
                       aes(x=DTR_status, y=value))

# then add styling and titles...

p1 <- p1 + ylab(NULL) + xlab(NULL) +
    # move strips outside of numbers
    theme(strip.background = element_blank(),
          strip.placement = "outside") + 
    # add labels...
    labs(title = "Figure 2d",
         subtitle = "Croft et al, Nature, 2019")

p1

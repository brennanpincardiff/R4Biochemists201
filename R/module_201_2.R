## ----packages_we_need--------------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggpubr)


## ----get_data_for_Fig_4a-----------------------------------------------------------
link <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-019-1263-7/MediaObjects/41586_2019_1263_MOESM10_ESM.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# check worksheet names 
sheet_names <- excel_sheets("file.xlsx")
data <- read_excel("file.xlsx", sheet_names[1])


## ----download_cleaner_data_for_Fig_4a----------------------------------------------
link <- ("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/figure_4a_data_altered.xlsx")
download.file(url=link, destfile="file.xlsx", mode="wb")

data_t <- read_excel("file.xlsx")



## ----tidy_data_for_Fig_4a----------------------------------------------------------
# here we have a selection of functions piped together to form a 
# script to tidy our data...
data_t %>%
    pivot_longer(-Cytokine) %>%
    separate(name, into=c(NA, "thy_status", NA)) %>%
    mutate(thy_status = factor(thy_status)) %>%
    mutate(Cytokine = str_to_upper(Cytokine)) %>%
    mutate(Cytokine = factor(Cytokine, c("CXCL14", "CXCL5", "CXCL2", "CXCL12",
                                         "CCL2", "CCL7", "CCL5", "CCL12","IL6",
                                         "MMP3", "MMP9", "MMP13"))) -> tidy_data


# functions used in this...
# pivot_longer()
    # the pivot_longer() function turns the data into long format. 
    # when we exclude the Cytokine column, the function recognises this
# separate()
    # the separate() function cleans up the thy status column 
    # it uses the _ and . as separators and only keeps the middle values 
    # "plus" and "minus"
# mutate()
    # the mutate() function to create factors using the factor() function
# str_to_upper
    # make the cytokine name all uppercase



## ----exercise_1--------------------------------------------------------------------
# in R
# import a cleaner file from Excel

# tidy it with various functions...







## ----start_with_cxcl14-------------------------------------------------------------
# start with out data
tidy_data %>%
    # filter the row with the Cxcl14 measurements
    filter(Cytokine == "CXCL14") -> cxcl14

plot <- ggplot(cxcl14,aes(x=thy_status, y= value)) + 
        geom_bar(stat = "summary", fun = "mean")

plot


## ----calc_t_test_and_add_with_ggpubr-----------------------------------------------
# calculate stats using a t.test using t.test()
output <- t.test(cxcl14[1:6,3], cxcl14[7:12,3])
output # gives the results
output$p.value # gives the p-value

# the package ggpubr extends ggplot
# we can use the function stat_compare_means with the t.test method. 
# gives us a nice label on our graph...
# Welch Two Sample t-test
plot + stat_compare_means(method = "t.test")


## ----exercise_2--------------------------------------------------------------------
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



## ----summarise_and_bar_chart-------------------------------------------------------
# DATA SUMMARISE
# calculate means and create a new file with the means
tidy_data %>%
    group_by(Cytokine, thy_status) %>%
    summarise(mean =mean(value)) -> cyto_means

# FIRST VISUALISATION of the means
ggplot(cyto_means, aes(x = thy_status, 
    y = mean)) + 
    geom_col() + 
    facet_wrap(~Cytokine, scales ="free_y")


## ----more_complex_viz--------------------------------------------------------------
# MORE COMPLEX VISUALISATION
# let's try adding the points and then the stat tests...
# we go back to the original data 
# without the summarise step
# use stat_summary() to calculate mean and plot the bars...
# and geom_jitter()
plot <- ggplot(tidy_data, aes(x=thy_status, y=value, fill = thy_status)) +
    stat_summary(fun="mean", geom="bar") +   # calculates means
    scale_fill_manual(values=c("blue", "red")) + 
    facet_wrap(~Cytokine, scales ="free_y") +
    geom_jitter() 

plot


## ----add_stats_with_stat_compare_means---------------------------------------------

# then the stats tests with stat_compare_means()
my_comparisons <- list( c("minus", "plus"))
plot + stat_compare_means(comparisons = my_comparisons)
# default Wilcoxon test - non parametric
# so values are different to the paper



## ----add_t-tests_again-------------------------------------------------------------

# change method to t.test and paired - true
plot + stat_compare_means(comparisons = my_comparisons, method = "t.test",
        paired = TRUE, size = 2)
# these don't look the same as the values in the paper
# the paper says paired T-test but perhaps I don't have the pairings correct...
# I don't think there is a way to do this without more information
# from the authors... 



## ----stars-------------------------------------------------------------------------

# can add some stars for significance but as the values aren't the same
# it won't be quite the same as the paper... 
# create an object, called plot, with the graph 
plot <- plot + stat_compare_means(comparisons = my_comparisons, method = "t.test",
        paired = TRUE, size = 2,
        symnum.args = list(cutpoints = c(0,0.0001,0.005,0.001, 1), 
            symbols = c("****","***", "***", "ns"))) +
    theme(strip.background = element_rect(fill = FALSE))

# show the object
plot



## ----add_titles--------------------------------------------------------------------

# add titles and reformat them a bit
# change the formatting and move legend
plot + 
    labs(x = "Thy Status",
        y = "Cytokine levels", 
        title = "Figure 4a",
        subtitle = "Croft et al, Nature, 2019") +
    theme_classic() +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position="bottom")



## ----exercise_3--------------------------------------------------------------------
# answers_201_2_exercise_3
my_comparisons <- list( c("minus", "plus"))

boxplot <- ggplot(tidy_data, aes(x=thy_status, y=value, fill = thy_status)) + 
    geom_boxplot() +
    scale_fill_manual(values=c("blue", "red")) +
    facet_wrap(~Cytokine, scales ="free_y") +
    geom_jitter() 

boxplot <- boxplot + stat_compare_means(comparisons = my_comparisons, method = "t.test",
                                  paired = TRUE, size = 2,
                                  symnum.args = list(cutpoints = c(0,0.0001,0.005,0.001, 1), 
                                                     symbols = c("****","***", "***", "ns"))) +
    theme(strip.background = element_rect(fill = FALSE))

boxplot + 
    labs(x = "Thy Status",
         y = "Cytokine levels", 
         title = "Figure 4a",
         subtitle = "Croft et al, Nature, 2019") +
    theme_classic() +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position="bottom")


v_plot <- ggplot(tidy_data, aes(x=thy_status, y=value, fill = thy_status)) + 
    geom_violin() +
    scale_fill_manual(values=c("blue", "red")) +
    facet_wrap(~Cytokine, scales ="free_y") +
    geom_jitter() 

v_plot <- v_plot + stat_compare_means(comparisons = my_comparisons, method = "t.test",
                                        paired = TRUE, size = 2,
                                        symnum.args = list(cutpoints = c(0,0.0001,0.005,0.001, 1), 
                                                           symbols = c("****","***", "***", "ns"))) +
    theme(strip.background = element_rect(fill = FALSE))

v_plot + 
    labs(x = "Thy Status",
         y = "Cytokine levels", 
         title = "Figure 4a",
         subtitle = "Croft et al, Nature, 2019") +
    theme_classic() +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position="bottom")



## ----exercise_4--------------------------------------------------------------------

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
    theme_bw() +
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




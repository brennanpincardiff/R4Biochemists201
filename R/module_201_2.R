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

data_c <- read_excel("file.xlsx")



## ----tidy_data_for_Fig_4a----------------------------------------------------------
# here we have a selection of functions piped together to form a 
# script to tidy our data...
data_c %>%
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
# separate file. 

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
# separate file

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
            symbols = c("****","***", "**", "ns"))) +
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
# available separately

## ----exercise_4--------------------------------------------------------------------
# available separately



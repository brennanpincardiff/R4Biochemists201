## ----packages_we_need------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


## ----get_data_for_Fig_1b---------------------------------------------------------
link <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-019-1263-7/MediaObjects/41586_2019_1263_MOESM8_ESM.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# check worksheet names 
sheet_names <- excel_sheets("file.xlsx")
data <- read_excel("file.xlsx", sheet_names[1])


## ----tidy_data_for_Fig_1b--------------------------------------------------------
# data in wide format...
# put into long tidy format... using gather()...

data_2 <- gather(data, na.rm = TRUE)
# make the key a factor - important for graphing and stats
data_2 %>% mutate(key = factor(key, c("Control", "Resolving", "RA"))) -> data_3


## ----figure_1j-------------------------------------------------------------------
data_1j <- read_excel("file.xlsx", sheet_names[5])



## ----barchart--------------------------------------------------------------------
# bar chart plot with the mean of each column...
ggplot(data_3, aes(key, value)) +
    geom_bar(stat = "summary", fun = "mean")  


## ----plot_means_as_lines---------------------------------------------------------
# bar chart plot
data_3 %>%
    group_by(key) %>%
    summarise(mean(value), sd(value)) -> data_3_stats
colnames(data_3_stats) <- c("key", "value", "sd")

# create plot with summary data...
# add the mean value as a line
ggplot(data_3_stats, aes(key, value)) + 
    geom_point(shape = 95, size = 8)

# we can also use the mean and standard deviation to add a error bar
# to this or to our bar chart...

# bar chart plot
ggplot(data_3, aes(key, value)) +
    geom_bar(stat = "summary", fun.y = "mean") +
    geom_errorbar(data = data_3_stats, aes(ymin=value-sd, ymax=value+sd), 
                  width=.2,
                  position=position_dodge(.9)) 


## ----geom_jitter-----------------------------------------------------------------
# then add points with geom_jitter()
# but control shapes/colours 
# solid black circles, hollow black circles, solid grey circles
ggplot(data_3, aes(key, value, shape = key, colour=key)) +
     geom_jitter(width = 0.1)



## ----mean_and_geom_jitter--------------------------------------------------------
# plot the mean first...
plot <- ggplot(data_3_stats, aes(key, value))
# add the mean value as a line
plot <- plot + geom_point(shape = 95, size = 8)


plot <- plot + geom_jitter(data=data_3, 
    aes(key, value, colour=key), width = 0.1) 
plot





## ----box_whisker-----------------------------------------------------------------
ggplot(data_3, aes(key, value, shape = key, colour=key)) +
     geom_boxplot() 



## ----box_whisker_with_points-----------------------------------------------------
ggplot(data_3, aes(key, value, shape = key, colour=key)) +
     geom_boxplot(outlier.size=0) +
    geom_jitter(width = 0.1)



## ----violin_plot-----------------------------------------------------------------
ggplot(data_3, aes(key, value, shape = key, colour=key)) +
     geom_violin() +
     geom_jitter(width = 0.1)



## --------------------------------------------------------------------------------
data %>%
    gather(na.rm = TRUE) %>%
    mutate(key = factor(key, c("Control", "Resolving", "RA"))) %>%
    ggplot(aes(key, value, shape = key, colour=key)) +
     geom_boxplot(outlier.size=0) +
     geom_jitter(width = 0.1) +
     ylim(0,0.2) + 
     labs(x = "",
     y = "FAPalpha expression (pixels per unit area)", 
     title = "Figure 1b",
     subtitle = "Croft et al, Nature, 2019") +
     theme_classic() + theme(legend.position="none")



## --------------------------------------------------------------------------------
data_1j %>%
    gather(na.rm = TRUE) %>%
    mutate(key = factor(key, c("Control", "STIA"))) %>%
    ggplot(aes(key, value, shape = key, colour=key)) +
     geom_boxplot(outlier.size=0) +
     geom_jitter(width = 0.1) +
     ylim(0,0.2) + 
     labs(x = "",
     y = "Bioluminescence", 
     title = "Figure 1j",
     subtitle = "Croft et al, Nature, 2019") +
     theme_classic() + theme(legend.position="none")



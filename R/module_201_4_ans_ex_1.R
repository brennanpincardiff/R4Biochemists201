# module_201_4_ans_ex_1

### activate the packages
library(tidyverse)
library(readxl)
library("ggpubr")

### download the data
link <- c("https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/fig_4f.xlsx?raw=true")

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

data <- read_excel("file.xlsx")

### this function is will add the ANOVA 
### is uses the ggboxplot() from the ggpubr package
stat_flow <- function(data, a_cell_type){
    # select out the rows we want with filter() function
    data %>% 
        filter(cell_type == a_cell_type) %>% 
        gather(cell_number, mice, -cell_type, -treatment)  %>%    
        mutate(treatment = factor(treatment, c("Sham_1", "THY-", "Sham_2","THY+"))) -> data_2
    
    ggboxplot(data_2, x = "treatment", y = "mice", add = "jitter",
              xlab = "",
              ylab = "") +
        stat_compare_means(method = "anova")
    }

### use the function
p1 <- stat_flow(data, "Leucocytes")
p2 <- stat_flow(data, "Neutrophils")
p3 <- stat_flow(data, "Macrophages")

### arrange the three graphs on one page...
figure <- ggarrange(p1, p2, p3, nrow=1,
          labels = c("A.Leucocytes",
                     "B.Neutrophils",
                     "C.Macrophages"),
          vjust=1)  # pushing labels above the plot

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

annotate_figure(figure, 
    top = text_grob("Figure 4f from Croft et al (2019) Nature 570:246–251",
    color = "red", face = "bold", size = 14))

# needs to be looked at width 1000 by height 600




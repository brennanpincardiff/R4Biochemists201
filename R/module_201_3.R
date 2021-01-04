## ----packages_we_need, message=FALSE---------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)


## ----download_data_for_Fig_1h----------------------------------------------------------
link <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-019-1263-7/MediaObjects/41586_2019_1263_MOESM8_ESM.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# get worksheet names 
sheet_names <- excel_sheets("file.xlsx")


## ----test_file_name, echo=FALSE--------------------------------------------------------
# we can add a little test into our data to ensure we have the correct data.
if (sheet_names[4] == "Figure 1h") {
print("We wanted Figure 1h so we have the correct file")
}else {
print("I don't think we have the correct file, please check your code...")
}


## ----import_data_for_Fig_1h------------------------------------------------------------
data <- read_excel("file.xlsx", sheet_names[4], skip = 1)



## ----basic_line_graph------------------------------------------------------------------
# simplify column names for ease...
colnames(data) <- c("thick", "biolum")

# basic dot plot with a fitted line
ggplot(data, aes(thick, biolum)) +
    geom_point() +
    stat_smooth(method = "lm") 


## ----more_customised_line_graph--------------------------------------------------------
# more detailed dot plot with a fitted line
ggplot(data, aes(thick, biolum)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE, color="black") +   
  xlab("Thickness (mm)") +   # label x-axis
  ylab("Bioluminescence") +    # label y-axis
  ggtitle("Croft et al, 2019, Figure 1h") +  # add a title
  expand_limits(y=c(0,10), x = c(0,2)) +
  theme_classic()


## ----line_graph_with_stats-------------------------------------------------------------
# stats with ggscatter
ggscatter(data, x = "thick", y = "biolum",
    add = "reg.line",  # Add regression line
    cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
    cor.coeff.args = list(method = "spearman"),
    xlab = "Thickness (mm)",
    ylab = "Bioluminesence",
    ylim = c(0, 10),
    xlim = c(0,2),
    title = "Croft et al, 2019, Figure 1h"
)


## ---- download_Rmd_mod_3, eval= FALSE--------------------------------------------------
## # download Rmd file for module 3 from the internet and open in R-Studio
## link <- "https://raw.githubusercontent.com/brennanpincardiff/R4Biochemists201/master/RMarkdown_files/module_201_3.Rmd"
## download.file(url=link, destfile="module_201_3.Rmd", mode="wb")
## file.edit("module_201_3.Rmd")


## ---- download_Rmd_paul_story, eval= FALSE---------------------------------------------
## link <- "https://raw.githubusercontent.com/brennanpincardiff/R4Biochemists201/master/RMarkdown_files/data_for_good_Cardiff.Rmd"
## download.file(url=link, destfile="paul_R_story.Rmd", mode="wb")
## file.edit("paul_R_story.Rmd")


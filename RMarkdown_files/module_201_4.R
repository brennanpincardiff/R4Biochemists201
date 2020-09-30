## ----packages_we_need---------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


## ----get_data_for_Fig_4f------------------------------------------------------
link <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-019-1263-7/MediaObjects/41586_2019_1263_MOESM10_ESM.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# check worksheet names 
sheet_names <- excel_sheets("file.xlsx")
data <- read_excel("file.xlsx", sheet_names[6])


## ----get_easier_data_for_Fig_4f-----------------------------------------------
link <- c("https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/fig_4f.xlsx?raw=true")

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

data <- read_excel("file.xlsx")


## ----first_workflow-----------------------------------------------------------
# select out the rows we want with filter() function for "Leucocytes"
# gather without cell_type and treatment
# mutate treatment into a factor to plot in the correct order
data %>% 
    filter(cell_type =="Leucocytes") %>% 
    gather(cell_number, mice, -cell_type, -treatment) %>%
    mutate(treatment = factor(treatment, c("Sham_1", "THY-", "Sham_2","THY+"))) -> data_2

# make the plot
set.seed(1)   # geom_jitter has a random element so set.seed() makes this same
plot_l <- ggplot(data = data_2,
    aes(treatment, mice, colour = treatment)) +
    geom_boxplot() +
    geom_jitter(width=0.15) +
    theme_bw() +
    labs(title = "Leucocytes", x="", y="",
         subtitle = "Croft et al (2019) Nature 570:246–251 (2019)") +
    theme_classic() + theme(legend.position="none")

plot_l


## ----make_function------------------------------------------------------------
## make a function called my_workflow

my_workflow <- function(data, a_cell_type){
    # select out the rows we want with filter() function
    data %>% 
        filter(cell_type == a_cell_type) %>% 
        gather(cell_number, mice, -cell_type, -treatment)  %>%    
        mutate(treatment = factor(treatment, c("Sham_1", "THY-", "Sham_2","THY+"))) -> data_2
        
    # make the plot
    set.seed(1)
    plot <- ggplot(data = data_2,
        aes(treatment, mice, colour = treatment)) +
        geom_boxplot() +
        geom_jitter(width=0.15) +
        theme_bw() +
        labs(title = a_cell_type, x="", y="",
             subtitle = "Croft et al (2019) Nature 570:246–251 (2019)") +
        theme_classic() + theme(legend.position="none")
    
    # this returns the plot and is key!  
    return(plot)
}



## ----check_function-----------------------------------------------------------
# check_function
cell_types <- unique(data$cell_type)
cell_types[1] # this is Leucocytes and should work...
leuco_plot <- my_workflow(data, cell_types[1])
leuco_plot

# do a visual check that the same object as the script above


## ----use_function-------------------------------------------------------------
# now plot for other cell types

cell_types[2] # this is Leucocytes and should work...
neutro_plot <- my_workflow(data, cell_types[2])
macro_plot <- my_workflow(data, cell_types[3])

# now we have three plots all made the same way.

neutro_plot
macro_plot


## ----exercise_1_answer--------------------------------------------------------
# us ggpubr


## ----bring_in_function, eval=FALSE--------------------------------------------
## source("/Users/paulbrennan/Documents/R_for_Biochemists_201_files/module_201_5_files/workflow_function.R")
## 


## ----download_from_Bioconductor-----------------------------------------------
# install BiocManager
# install.packages("BiocManager")
# BiocManager::install("flowCore")

library("flowCore")

# download a flow data file
link <- "https://github.com/brennanpincardiff/R_for_Biochemists_201/blob/master/data/A01%20CFSE%20profiles%20Day%203.fcs?raw=true"

download.file(url=link, destfile="file.fcs", mode="wb")

data <-read.FCS("file.fcs", alter.names = TRUE)
data

n <- as.data.frame(exprs(data))

ggplot(n, aes(x = FL1.H)) + geom_density()
# open it and plot it...


# want two data files with different CFSE profiles....


## ----download_from_github-----------------------------------------------------
## install from Github
# remove the hash tag to run. 
#devtools::install_github("jespermaag/gganatogram")
library(gganatogram)
library(dplyr)
library(viridis)
library(gridExtra)

organPlot <- data.frame(organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"), 
 type = c("circulation", "circulation",  "nervous system", "nervous system", "digestion", "digestion", "digestion"), 
 colour = c("red", "red", "purple", "purple", "orange", "orange", "orange"), 
 value = c(10, 5, 1, 8, 2, 5, 5), 
 stringsAsFactors=F)

 head(organPlot)

gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='male', fill="colour")



## ----download_ggseg-----------------------------------------------------------
# install
# devtools::install_github("LCBC-UiO/ggseg", build_vignettes = TRUE)
library(ggseg)
ggseg(atlas=aseg)



## ----download_drawProteins----------------------------------------------------
# download drawProteins from Bioconductor
# install BiocManager
# install.packages("BiocManager")
# BiocManager::install("drawProteins")

library(drawProteins)
# show the vignette for drawProteins
vignette("drawProteins_BiocStyle")
# sample code is here... 

# integrate your learning by adding the code and output to the Github page we used above...


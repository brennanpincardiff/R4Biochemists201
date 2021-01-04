## ----packages_we_need, message=FALSE---------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


## ----get_data_for_Fig_4f, message=FALSE------------------------------------------------
link <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-019-1263-7/MediaObjects/41586_2019_1263_MOESM10_ESM.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# check worksheet names 
sheet_names <- excel_sheets("file.xlsx")
data <- read_excel("file.xlsx", sheet_names[6])


## ----get_easier_data_for_Fig_4f--------------------------------------------------------
link <- c("https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/fig_4f.xlsx?raw=true")

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

data <- read_excel("file.xlsx")


## ----first_workflow--------------------------------------------------------------------
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


## ----make_function---------------------------------------------------------------------
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
    
    # this returns the plot that we want.   
    return(plot)
}



## ----check_function--------------------------------------------------------------------
# check_function
cell_types <- unique(data$cell_type)
cell_types[1] # this is Leucocytes and should work...
leuco_plot <- my_workflow(data, cell_types[1])
leuco_plot

# do a visual check that the same object as the script above
# it is possible to automatic checks too but that's for the future. 


## ----use_function----------------------------------------------------------------------
# now plot for other cell types

cell_types[2] # this is Leucocytes and should work...
neutro_plot <- my_workflow(data, cell_types[2])
macro_plot <- my_workflow(data, cell_types[3])

# now we have three plots all made the same way.

neutro_plot
macro_plot



## ----bring_in_function, eval=FALSE-----------------------------------------------------
## source("/Users/paulbrennan/Documents/R4Biochemists201/R/workflow_fig4f.R")
## 


## ----roxygen_demo, eval=FALSE----------------------------------------------------------
## #' Title
## #'
## #' @param data
## #' @param a_cell_type
## #'
## #' @return
## #' @export
## #'
## #' @examples
## 
## @param refers to paramaters
## @return tells you what is returned.
## @export is necessary if you are including your function in a package.


## ----download_from_Bioconductor--------------------------------------------------------
# install BiocManager
# install.packages("BiocManager")
# BiocManager::install("flowCore")

library("flowCore")
library(ggplot2)
library(ggpubr)


link1 <- "https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/cfse_data_20111028_Bay_d7/A01.fcs?raw=true"

# download first data file
download_data <- function(link){
    download.file(url=link, destfile="file.fcs", mode="wb")
    data <- flowCore::read.FCS("file.fcs", alter.names = TRUE)}

data <- download_data(link1)

#with colours indicating density
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
# this colour palette can be changed to your taste 

vals <- as.data.frame(exprs(data))
ggplot(vals, aes(x=FSC.A, y=SSC.A)) +
    ylim(0, 500000) +
    xlim(0,5000000) +
    stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
    scale_fill_gradientn(colours=colfunc(400)) + # gives the colour plot
    geom_density2d(colour="black", bins=5) # draws the lines inside


# write function for plotting CFSE data...
plot_cfse <- function(data){
    # this exprs() function pulls out the numbers. 
    vals_1 <- as.data.frame(exprs(data))
    # I want to exclude small debris and just analyse cells. 
    vals_f <- dplyr::filter(vals_1, FSC.A>1000000)
    # then draw the plot
    ggplot(vals_f, aes(x=FL1.H)) + geom_density() +
        xlim(1000,1000000) + scale_x_log10()
}

# plot first data set
p1 <- plot_cfse(data)

# download and plot second data set
link2 <- "https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/cfse_data_20111028_Bay_d7/A02_top_dose.fcs?raw=true"
data2 <- download_data(link2)
p2 <- plot_cfse(data2)

# download and plot third data set
link3 <- "https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/cfse_data_20111028_Bay_d7/A04.fcs?raw=true"
data3 <- download_data(link3)
p3 <- plot_cfse(data3)


text <- paste("Each decrease in fluorescence represents a population division.")

text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")

ggarrange(p1, p2, p3, text.p, ncol=1,
          labels = c("A.Cell proliferation in control",
                     "B.Inhibited by Bay top dose",
                     "C.Half Bay dose inhibits less"))



## ----download_from_github--------------------------------------------------------------
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

gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='female', fill="colour")



## ----download_drawProteins, eval=FALSE-------------------------------------------------
## # download drawProteins from Bioconductor
## # install BiocManager
## # install.packages("BiocManager")
## # BiocManager::install("drawProteins")
## 
## library(drawProteins)
## # show the vignette for drawProteins in the Help tab
## vignette("drawProteins_BiocStyle")
## # sample code is in the vignette... explore it.
## 
## # integrate your learning by adding the code and output to your RMarkdown file
## # and/or the Github page you created above...



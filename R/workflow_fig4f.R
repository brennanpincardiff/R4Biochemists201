#' A workflow to create the three figures in Fig 4f in Croft et al (2019)
#'
#' @param data a data file with cell numbers
#' @param a_cell_type could be leucocytes, macrophages or neutrophils
#'
#' @return A ggplot object
#'
#' @examples
#' ## ----get_easier_data_for_Fig_4f-----------------------------------------------
#' link <- c("https://github.com/brennanpincardiff/R4Biochemists201/blob/master/data/fig_4f.xlsx?raw=true")
#' the download.file() function downloads and saves the file with the name given
#' download.file(url=link, destfile="file.xlsx", mode="wb")
#' data <- read_excel("file.xlsx")
#' leuco_plot <- my_workflow(data, "Leucocytes")
#' 
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
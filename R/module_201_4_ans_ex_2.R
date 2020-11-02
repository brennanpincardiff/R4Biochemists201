#' A workflow to create graphs for Figure 4f and add an ANOVA
#' It uses the ggboxplot() from the ggpubr package
#'
#' @param data 
#' @param a_cell_type 
#'
#' @return a ggplot object created by ggboxplot
#'
#' @examples
#' 
### this function is will add the ANOVA 
### 
stat_flow <- function(data, a_cell_type){
    # select out the rows we want with filter() function
    data %>% 
        dplyr::filter(cell_type == a_cell_type) %>% 
        dplyr::gather(cell_number, mice, -cell_type, -treatment)  %>%    
        dplyr::mutate(treatment = factor(treatment, c("Sham_1",
                                                      "THY-", 
                                                      "Sham_2",
                                                      "THY+"))) -> data_2
    
    ggpubr::ggboxplot(data_2, x = "treatment", y = "mice", add = "jitter",
              xlab = "",
              ylab = "") +
        ggpubr::stat_compare_means(method = "anova")
    }
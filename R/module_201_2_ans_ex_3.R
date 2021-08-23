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
                                                           symbols = c("****","***", "**", "ns"))) +
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
                                                         symbols = c("****","***", "**", "ns"))) +
    theme(strip.background = element_rect(fill = FALSE))

v_plot + 
    labs(x = "Thy Status",
         y = "Cytokine levels", 
         title = "Figure 4a",
         subtitle = "Croft et al, Nature, 2019") +
    theme_classic() +
    theme(strip.background = element_rect(fill = "white", colour = "white")) +
    theme(legend.position="bottom")


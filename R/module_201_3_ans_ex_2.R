## ---- exercise_2-----------------------------------------------------------------------------------
data_2 <- read_excel("file.xlsx", sheet_names[8], skip = 1)
colnames(data_2) <- c("thick", "express")

# you can plot it in ggplot but you will need to calculate the stats 
# and add them manually
ggplot(data_2, aes(thick, express)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE, color="black") +   
    xlab("Thickness (mm)") +   # label x-axis
    ylab("mRNA expression") +    # label y-axis
    ggtitle("Croft et al, 2019, Figure 1m") +  # add a title
    scale_y_continuous(limits=c(-4,12), breaks=c(-4,-2,0,2,4,6,8,10,12)) +
    theme_classic()


## ---- more_for_exercise_2--------------------------------------------------------------------------
# make the plot with ggscatter()
plot <- ggscatter(data_2, x = "thick", y = "express",
                  add = "reg.line",  # Add regression line
                  cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                  cor.coeff.args = list(method = "spearman"),
                  xlab = "Thickness (mm)",
                  ylab = "mRNA expression",
                  xlim = c(0,1.5),
                  title = "Croft et al, 2019, Figure 1m"  # add a title
)
# use scale_y_continuous() to alter the y axis 
plot + scale_y_continuous(limits=c(-4,12), breaks=c(-4,-2,0,2,4,6,8,10,12))

## ----exercise_1_answer-----------------------------------------------------------
data_1j <- read_excel("file.xlsx", sheet_names[5])

# data in wide format...
# put into long tidy format... with pivot_longer()
data_1j_l <- pivot_longer(data_1j, cols = Control:STIA)
# this will work but will require changes later. 
# data_1j_l <- gather(data_1j, na.rm = TRUE)
# make the key a factor - important for graphing and stats
data_1j_l %>% mutate(name = factor(name, c("Control", "STIA"))) -> data_1j_lf

## ----exercise_1_answer-----------------------------------------------------------
data_1j <- read_excel("file.xlsx", sheet_names[5])

# data in wide format...
# put into long tidy format... with gather and spread...
data_1j_l <- gather(data_1j, na.rm = TRUE)
# make the key a factor - important for graphing and stats
data_1j_l %>% mutate(key = factor(key, c("Control", "STIA"))) -> data_1j_lf


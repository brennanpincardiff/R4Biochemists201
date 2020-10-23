# import a cleaner file from Excel
# read in easier data than the download.
link <- ("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/fig_2d_altered.xlsx")
download.file(url=link, destfile="fig2d_altered.xlsx", mode="wb")

data <- read_excel("fig2d_altered.xlsx", skip = 1)

# tidy it with various functions...
# gather(), separate() and mutate() into factors...
tidy_data <- data %>%
    gather() %>%
    separate(key, into=c("names", "DTR_status"), sep = "/") %>%
    # need to turn the names into factors...
    mutate(DTR_status = factor(DTR_status)) %>%
    mutate(names = factor(names, c("bone_eros_CT", "bone_form_CT", 
                                   "area_bone_eros", "area_bone_form",
                                   "area_pan", "area_dest_cart")))


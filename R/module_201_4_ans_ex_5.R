## ----drawProteins_example------------------------------------------------------------
library(drawProteins)
drawProteins::get_features("Q04206") -> rel_json
feature_to_dataframe(rel_json) -> rel_data
draw_canvas(rel_data) -> p
p <- draw_chains(p, rel_data)
p <- draw_domains(p, rel_data)
draw_regions(p, rel_data)

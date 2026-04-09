# For more info, see https://pkg.yihui.org/gglite/
library(gglite)
g2(penguins, bill_len ~ bill_dep, color = ~ species)
g2(penguins, bill_len ~ species)

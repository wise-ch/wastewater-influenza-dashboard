
source('helper_code/Re_helper_functions_variant.R')
library(ggpattern)

re_ww <- read_csv('rww_data/variant_re.csv')

re_cc_var <- read_csv('rww_data/variant_rcc.csv')

re_var <- re_ww %>% bind_rows(re_cc_var)



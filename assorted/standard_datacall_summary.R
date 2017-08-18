library(dplyr)
library(scales)

# outlier check
# dave said drop any jcr > 5000 and pi > $1 bil (unless it's been vetted)
fy_2016 %>% mutate(jcr = Jobs.Saved + Jobs.Created) %>% ggplot(., aes(x = jcr)) + geom_histogram()
ggplot(fy_2016, aes(x = Private.Investment)) + geom_histogram()

# summarize all records
summarize(count = n(), eda_funding = sum(EDA.Funding, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
          jc = sum(Est.Jobs.Created, na.rm = TRUE),
          jcr = sum(js, jc, na.rm = TRUE), jcr_ratio = eda_funding / jcr, pi = sum(Est.Private.Investment, na.rm = TRUE),
          pi_ratio = pi / eda_funding) %>% mutate(eda_funding = dollar(eda_funding),
                                                  jcr = comma(jcr), jcr_ratio = dollar(jcr_ratio), 
                                                  pi = dollar(pi), pi_ratio = dollar(pi_ratio))

# summarize for OEA standard bullet, splitting by awards with/without jcr/pi
mutate_each(funs(ifelse(is.na(.), 0, .)), c(Est.Private.Investment, Est.Jobs.Created, Est.Jobs.Saved)) %>%
        mutate(jcr_pi_flag = case_when(.$Est.Jobs.Created > 0 | .$Est.Jobs.Saved > 0 | .$Est.Private.Investment > 0 ~ 1, TRUE ~ 0)) %>% 
        group_by(jcr_pi_flag) %>% summarize(count = n(), eda_funding = sum(EDA.Funding, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                                            jc = sum(Est.Jobs.Created, na.rm = TRUE),
                                            jcr = sum(js, jc, na.rm = TRUE), jcr_ratio = eda_funding / jcr, pi = sum(Est.Private.Investment, na.rm = TRUE),
                                            pi_ratio = pi / eda_funding) %>% mutate(eda_funding = dollar(eda_funding),
                                                                                    jcr = comma(jcr), jcr_ratio = dollar(jcr_ratio), 
                                                                                    pi = dollar(pi), pi_ratio = dollar(pi_ratio))

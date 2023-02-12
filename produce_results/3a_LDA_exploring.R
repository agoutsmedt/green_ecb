source("packages_and_data.R")
source("function/functions_for_topic_modelling.R")
K = 70
lda_data <- readRDS(here(data_path, 
             "topic_modelling",
             paste0("LDA_", K, "_data.rds")))


#' We first explore the data per year. 
rects <- tribble(~start, ~end,
                 "1998-11-20", "2011-11-08",
                 "2011-11-08", "2021-09-01",
                 "2021-09-01", "2023-02-01") %>% 
  mutate(start = date(start),
         end = date(end))

topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>% 
  ggplot(aes(date, gamma, color = topic_name)) +
  geom_smooth(show.legend = FALSE, method = "loess", span = 0.2) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
 # geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9)) 

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date.png")), topic_per_date,
       device = ragg::agg_png,
       width = 60, height = 55, units = "cm", res = 300)

cb_focus <- c("european central bank",
              "bundesbank",
              "bank of france")
              
topic_per_date_cb <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(central_bank %in% cb_focus) %>% 
  ggplot(aes(date, gamma, color = topic_name, linetype = central_bank)) +
  geom_smooth(alpha = 0.8, size = 0.8, se = FALSE) +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name) +
  theme(strip.text = element_text (size = 9), legend.position = "bottom") +
  guides(color = "none")

ggsave(here("pictures", glue::glue("TM_LDA_topic_per_date_cb.png")), topic_per_date_cb,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)

#' Evolution of specific topics
#' 
#' 

topic_focus <- c(17, 37, 48, 63, 20, 56)

focus_topic_per_date <- lda_data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 30))) %>%
  filter(topic %in% topic_focus) %>% 
  ggplot(aes(date, gamma, color = topic_name, linetype = central_bank)) +
  geom_smooth(method = "loess", span = 0.2, show.legend = FALSE) +
  geom_vline(xintercept = date(c("2011-11-08", "2021-09-01"))) +
#  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end,), ymax = 1, ymin = 0, color="transparent", fill="grey", alpha=0.3) +
  facet_wrap(~ topic_name, ncol = 2) +
  theme(strip.text = element_text (size = 15), legend.position = "bottom") +
  guides(color = "none")

ggsave(here("pictures", glue::glue("TM_LDA_main_topic_per_date.png")), focus_topic_per_date,
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)


#' We create two subsamples of years: 2006-2008, 2021-2022
#' 
data_year_subset <- lda_data %>% 
  mutate(period = case_when(between(date, "1998-11-20", "2011-11-08") ~ "Period_1 (1998-2011)",
                            between(date, "2011-11-08", "2021-09-01") ~ "Period_2 (2011-2021)",
                            between(date, "2021-09-01", "2023-02-01") ~ "Period_3 (2021-)")) %>% 
  filter(! is.na(period)) 


data <- data_year_subset %>% 
  group_by(topic, period) %>% 
  mutate(mean = mean(gamma),
         st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  select(topic, topic_name, period, mean, st_err) %>% 
  unique()

data %>% 
  mutate(topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(mean, fct_reorder(topic_name, mean), color = period)) +
  geom_errorbar(aes(xmin = mean - st_err, xmax = mean + st_err)) +
  geom_point(show.legend = FALSE) +
  labs(x = "Prevalence of the topic")

ggsave(here("pictures", glue::glue("TM_mean_LDA_topic_period.png")),
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)

#data %>% 
#  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
#  mutate(differential = mean_Period_2 - mean_Period_1,
 #        st_err = (st_err_Period_1 + st_err_Period_2)/2,
  #       topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  #ggplot(aes(differential, fct_reorder(topic_name, differential))) +
  #geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  #geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  #geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6)

#ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period.png")),
 #      device = ragg::agg_png,
  #     width = 50, height = 45, units = "cm", res = 300)

#data_ecb <- data_year_subset %>% 
 # filter(central_bank %in% cb_focus) %>% 
  #group_by(topic, period, central_bank) %>% 
  #mutate(mean = mean(gamma),
  #       st_err = sd(gamma)/sqrt(length(gamma))) %>% 
  #select(topic, topic_name, period, central_bank, mean, st_err) %>% 
  #unique()

data_ecb %>% 
  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
  mutate(differential = mean_Period_2 - mean_Period_1,
         st_err = (st_err_Period_1 + st_err_Period_2)/2,
         topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(differential, fct_reorder(topic_name, differential), color = central_bank)) +
  geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6)

ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period_cb.png")),
       device = ragg::agg_png,
       width = 50, height = 45, units = "cm", res = 300)

data_ecb %>% 
  pivot_wider(names_from = "period", values_from = c("mean", "st_err"),) %>% 
  mutate(differential = mean_Period_2 - mean_Period_1,
         st_err = (st_err_Period_1 + st_err_Period_2)/2,
         topic_name = paste0("topic ", topic, ": ", str_wrap(topic_name, 50))) %>% 
  ggplot(aes(differential, fct_reorder(topic_name, differential), color = central_bank)) +
  geom_errorbar(aes(xmin = differential - st_err, xmax = differential + st_err)) +
  geom_point(aes(size = mean_Period_1 + mean_Period_2), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  facet_wrap(~central_bank, ncol = 3) +
  theme(legend.position = "bottom")
  

ggsave(here("pictures", glue::glue("TM_differential_LDA_topic_period_per_cb.png")),
       device = ragg::agg_png,
       width = 60, height = 45, units = "cm", res = 300)


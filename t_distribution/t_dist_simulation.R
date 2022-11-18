library(tidyverse)

sample_size <- 50

t_dist <- qt(seq(0.001,.999,.001), df = sample_size-1)

real_mean_diff <- .05
real_sd_diff <- .05

# how many samples shall we collect?
sampling_iterations <- 40000 # number of samples
plot_data <- NULL
# conduct sampling process
for (i in 1:sampling_iterations) {

  sample <- rnorm(sample_size, mean = real_mean_diff, sd = real_sd_diff)
  plot_data <- rbind(plot_data, as.numeric(t.test(sample, mu = 0)[['statistic']]))

}

criterion_t_high_2t <- qt(.975, df = sample_size-1)
# criterion_t_low_2t <- qt(.025, df = sample_size-1)
# criterion_t_high_1t <- qt(.95, df = sample_size-1)

sample_text = str_c("Distribution of sample t values: sample size = ", sample_size,
                    " ; mean diff = ", real_mean_diff,
                    " ; sd of diff = ", real_sd_diff)

# plot the t distribution
ggplot() +
  geom_density(aes(x = t_dist),
               fill = "blue",
               alpha = .4) +
  geom_vline(aes(xintercept = criterion_t_high_2t), colour = "orange", size = 2, linetype = "dashed") +
  theme_classic() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-6,10,.5)) +
  labs(x = "t value")



# overlay the distribution of sample t values
ggplot() +
  geom_density(aes(x = t_dist),
               fill = "blue",
               alpha = .4) +
  geom_vline(aes(xintercept = criterion_t_high_2t), colour = "orange", size = 2, linetype = "dashed") +
  geom_density(aes(x = plot_data),
               fill = "red",
               alpha = .4) +
  annotate(geom = "text",
           label = str_wrap(sample_text,width = 20),
           x = mean(plot_data),
           y = .1,
           size = 5,
           colour = "white") +
  theme_classic() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-6,10,.5)) +
  labs(x = "t value")

# plot the t distribution
ggplot() +
  # t distribution centred on 0
  geom_density(aes(x = t_dist),
               fill = "blue",
               alpha = .5) +
  # # shifted t distribution (positive effect)
  # geom_density(aes(x = t_dist + 4),
  #              fill = "red",
  #              alpha = .5) +
  geom_vline(aes(xintercept = criterion_t_high_2t),
                 colour = "orange", size = 2, linetype = "dashed") +
#  blanking rectangle for highlighting sections
  # geom_rect(aes(xmin = -4,
  #               xmax = criterion_t_high_1t,
  #               ymin = 0,
  #               ymax = .4),
  #           fill = "white", alpha = .8) +
  annotate(geom = "text",
           label = str_c("N = ", sample_size),
           x = -3,
           y = .3,
           size = 10,
           colour = "black") +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5,.5)) +
  scale_y_continuous(limits = c(0,.4))
  # labs(x = "t value")
  # ggsave("t_dist_sample_one-tailed_alpha.png")


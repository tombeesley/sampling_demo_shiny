library(tidyverse)
library(patchwork)
theme_set(theme_classic(base_size = 16))

# generate population of IQ scores and plot
pop_size <- 100000
IQ <- rnorm(pop_size,100,15) # normal distribution
#IQ <- runif(pop_size,70,130) # uniform distribution
ggplot() +
  geom_histogram(aes(IQ))


# set our experimental sampling conditions
sample_size <- 50

# plot one sample mean on the distribution of population
sample <- round(runif(sample_size,0,pop_size),0)
ggplot() +
  geom_histogram(aes(IQ)) +
  geom_point(aes(x = IQ[sample]), y = 0,
             size = 8, colour = "red") +
  geom_vline(xintercept = mean(IQ[sample]),
             colour = "red")

# how many samples shall we collect?
sampling_iterations <- 1000 # number of samples
plot_data <- NULL
# conduct sampling process
for (i in 1:sampling_iterations) {

  sample <- round(runif(sample_size,0,pop_size),0)
  plot_data <- rbind(plot_data, mean(IQ[sample]))

}

# plot the sampled data
ggplot() +
  geom_histogram(aes(plot_data)) +
  scale_x_continuous(limits = c(85,115),
                     breaks = seq(85,115,5))

# plot the sampled data over the population data
pop_plot <- ggplot() +
  geom_histogram(aes(IQ),
                 fill = "blue",
                 binwidth = 1)  +
  labs(x = "Population data") +
  scale_x_continuous(limits = c(60,140),
                     breaks = seq(60,140,10))

sample_plot <- ggplot() +
  geom_histogram(aes(plot_data),
                 fill = "red",
                 binwidth = 1) +
  labs(x = "Distribution of sampled means") +
  scale_x_continuous(limits = c(60,140),
                     breaks = seq(60,140,10))

pop_plot/sample_plot




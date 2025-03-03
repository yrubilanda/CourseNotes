#load needed datasets
library(tidyverse)
library(mosaic)
library(infer)
library(ggplot2)

#Step 1
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/tbs-2006-2008-ranges.csv"

d <- read_csv(f, col_names = TRUE)

#reduce dataset to variables of interest (sex and kernel95)

d <- d|> 
  select(sex, kernel95)

#determine mean, standard deviation, and standard error in kernel95 for each sex
sample_stats <- d |>
  group_by(sex) |> #groub by sex
  summarize( #summarise() is typically used on grouped data created by group_by(). The output will have one row for each group.
    mean = mean(kernel95), #mean
    sd = sd(kernel95), #standard deviation
    n = n(), #sample size
    se = sd/sqrt(n)) #standar error

#create boxplots comparing kernel95 home range by sex
p <- ggplot(d, aes(x = sex, y = kernel95, fill = sex, col = sex)) + #in aes add a fill and col section to control your colors
  geom_boxplot() +
  scale_fill_manual(values = c(F = "plum1", M = "lightsteelblue1")) + #scale fill manual will add color to the inside of your boxplot
  scale_colour_manual(values=c(F = "black", M = "black")) + #scale colour manual for the outline
  geom_jitter() 
p + labs(title = "Home Range by Sex", x = "Sex", y = "Home Range")

#For each sex, generate a bootstrap distribution for mean kernel95 home range size.
n_boot <- 10000 #boot reps
s <- d |>
  filter(sex == "M")

boot <- do(n_boot) * mean(sample(s$kernel95, size = length(s$kernel95), replace = TRUE))
boot <- boot$mean #pull out mean column as vector

#plot resulting bootstrap sampling distribution and plot an appropriate normal distribution
se <- sd(boot)
histogram(boot, col = "pink", border = "white")
plotDist("norm", mean(boot), se, add = TRUE, col = "slategray", lwd = 3) #plot a normal distribution over the histogram for the mean of boot and se of boot

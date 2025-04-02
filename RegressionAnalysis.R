#ELEMENTS OF REGRESSION ANALYSIS

#The goal of regression is to partition variants in the outcome
#we can seperate/partition variance into our y variable into that explained by our
#model and that which is leftover as "error"

#SSY(sum of squares) = SSR(regression sum of squares) + SSE(error sum of squares)

library(readr)

f<-"https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"

d<-read_csv(f, col_names = TRUE)

#regression model with out zombie dataset
m <- lm(data = d, height ~ weight)

#SSY = height - mean(height)
SSY <- sum((m$model$height - mean(m$model$height))^2)
#SSR = predicted height - mean height
SSR <- sum((m$fitted.values - mean(m$model$height))^2)
#SSE = height - predicted height
SSE <- sum((m$model$height - m$fitted.values)^2)


# mean overall variance
MSY <- SSY/(nrow(d)-1) 
# mean variance explained by the regression equation
MSR <- SSR/(1)
# mean remaining variance
MSE <- SSE/(nrow(d)-1-1) 


#F ratio
fratio <- MSR/MSE #ratio of explained to unexplained

#where does it fall relative to an expectation, compare to f distribution
#if high it is mostly explained
#if low you have not explained very much

#p value - proportion of f distribution that lies between 0 and fratio
pf(q = fratio, df1 = 1, df2 = 998, lower.tail = FALSE)
#OR
1 - pf(q=fratio, df1=1, df2 = 998)


mosaic::plotDist("f", df1 = 1, df2 = 998)

#proportion of varianced described by regression, YOUR R SQUARED VALUE
(rsq <- SSR/SSY)

summary(m)
anova(m)

new_d <- tibble(
  x=rnorm(1000, mean = 100, sd = 25),
  y = rnorm(1000, mean = 10, sd = 2))
                
plot(new_d$x, new_d$y)                

m <- lm(data = new_d, y ~ x)
  
#SSY = height - mean(height)
(SSY <- sum((m$model$y - mean(m$model$y))^2))
#SSR = predicted height - mean height
(SSR <- sum((m$fitted.values - mean(m$model$y))^2))
#SSE = height - predicted height
(SSE <- sum((m$model$y - m$fitted.values)^2))
  
lm(formula = y ~ x, data = new_d)
anova(m)


#L inearity of realtionship between variabeles
#I ndependence of residuals
#N ormality of residuals
#E quality of variance ("homoscedasticity) of the residuals

#L, N, E can be evaluated by residual analysis

#par(mfrow = c(2,2))
plot(m)


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE)
names(d)

plot(d$Body_mass_female_mean, d$MaxLongevity_m)

plot(log(d$Body_mass_female_mean), log(d$MaxLongevity_m))

m1 <- lm(data = d, MaxLongevity_m ~ Body_mass_female_mean) 
m2 <- lm(data = d, MaxLongevity_m ~ log(Body_mass_female_mean))
m3 <- lm(data = d, log(MaxLongevity_m) ~ log(Body_mass_female_mean))


par(mfrow = c(2, 2))
p1 <- plot(m1)
p2 <- plot(m2)
p3 <- plot(m3)


plot(m1)
plot(m2)
plot(m3)

shapiro.test(m1$residuals)
shapiro.test(m2$residuals)
shapiro.test(m3$residuals)

#AVONET DATASET
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)

library(dplyr)
d <- d |>
  select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, 
         Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, 
         Habitat, Migration, Trophic.Level, Trophic.Niche, Min.Latitude, 
         Max.Latitude, Centroid.Latitude, Primary.Lifestyle, Range.Size)
library(ggplot2)
library(tidyverse)

#log essentially transforms your data
ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Trophic.Level, y = log(Mass)))+
  geom_boxplot() +
  geom_jitter()

ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Migration, y = log(Mass)))+
  geom_boxplot() +
  geom_jitter()

ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Migration, y = log(Mass)))+
  geom_violin() #still shows density and relationship between your data, similar to jitter


m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
m2 <- lm(log(Mass) ~ as.factor(Migration), data = d)

#or

d <- d |> mutate(Migration = as.factor(Migration))
m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
m2 <- lm(log(Mass) ~ as.factor(Migration), data = d)
summary(m1)
summary(m2)


#behind the scenes R has converted it to 4 different groups and has made carnivore the baselevel group because it sorts it alphabetically
ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Trophic.Level, y = log(Mass)))+
      geom_boxplot()
#visualizing difference between groups according to the base level
d <- d |> mutate(Trophic.Level = relevel(as.factor(Trophic.Level), ref = "Omnivore"))

(pairwise.t.test(log(d$Mass), d$Trophic.Level,
                 p.adj = "bonferroni"))
m1aov <- aov(log(Mass) ~ Trophic.Level, data = d)
(posthoc <- TukeyHSD(m1aov, which = "Trophic.Level"))


library(infer)
d <- d |> mutate(logMass = log(Mass))
permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")

m <- lm(log(Mass) ~ Trophic.Level, data = d)

original.F <- aov(log(Mass) ~ Trophic.Level, data = d) |>
  broom::tidy() |>
  filter(term == "Trophic.Level")

visualize(permuted.F) +
  shade_p_value(obs_stat = original.F$statistic, direction = "greater")

p.value <- permuted.F |>
  get_p_value(obs_stat = original.F$statistic, direction = "greater")

original.F  
  
  
  
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE) 

library(ggplot2)

#create log mass variable
d <- d |> mutate(logMass = log(Mass))

#create relative beak length
beak_model <- lm(log(Beak.Length_Culmen) ~ logMass, data = d)
d <- d |> mutate(relative_beak_length = resid(beak_model))

#create relative tarsus length
tarsus_model <- lm(log(Tarsus.Length) ~ logMass, data = d)
d <- d |> mutate(relative_tarsus_length = resid(tarsus_model))

#boxplot of relative tarsus length by primary lifestyle
ggplot(d, aes(x = Primary.Lifestyle, y = relative_tarsus_length)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Relative Tarsus Length by Primary Lifestyle",
       x = "Primary Lifestyle", y = "Relative Tarsus Length")

#boxlpot of relative beak length
ggplot(d, aes(x = Trophic.Level, y = relative_beak_length)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Relative Beak Length by Trophic Niche",
       x = "Trophic Niche", y = "Relative Beak Length")

d <- d |> mutate(logRange = log(Geographic.Range.Area))

anova_model <- lm(logRange ~ Migration, data = d)
summary(anova_model)
anova(anova_model)


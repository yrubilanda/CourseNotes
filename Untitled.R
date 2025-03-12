install.packages("lmodel2")
install.packages("sjPlot")
install.packages("broom")
install.packages("manipulate")
install.packages("pathcwork")

library(lmodel2)
library(sjPlot)
library(broom)
library(manipulate)
library(patchwork)
library(readr)


#regreesion - common form of data modeling
#simple (general) - outcome id a continous numerical variable, single predictor that is either numerical or categorical

#multiple(general) - outcome is a continous numerical variable, multiple predictors that are either numerical or categorical

#ANOVA/ANCOVA - focuses on categorical predictors

#"generalized" linear regression - allows for binary, categorical, count variables as outcomes

#PROGRAMMING EXERCISE

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

h <- d$height - mean(d$height)
w <- d$weight - mean(d$weight)
cov <- sum(h*w)/(length(d$height) - 1)
cov

cov(d$height, d$weight)



#OLS
d <- mutate(d, centered_height = height - mean(height))
d <- mutate(d, centered_weight = weight - mean(weight))

p1 <- ggplot(data = d, aes(x = weight, y = height)) + geom_point()
p2 <- ggplot(data = d, aes(x = centered_weight, y = centered_height)) + geom_point()

p1 + p2

slope.test <- function(beta1, data) {
  g <- ggplot(data = data, aes(x = centered_weight, y = centered_height))
  g <- g + geom_point()
  g <- g + geom_abline(intercept = 0, slope = beta1, size = 1, colour = "blue",
                       alpha = 1/2)
  ols <- sum((data$centered_height - beta1 * data$centered_weight)^2)
  g <- g + ggtitle(paste("Slope = ", beta1, "\nSum of Squared Deviations = ", round(ols,
                                                                                    3)))
  g
}

manipulate(slope.test(beta1, data = d), beta1 = slider(-1, 1, initial = 0, step = 0.005))

b1 <- (cor(d$weight, d$height) * (sd(d$height))/sd(d$weight))
b0 <- mean(d$height) + b1 * mean(d$weight)

b1
b0

m <- lm(height~weight, data = d) #linear model that predicts height on basis of weight
summary(m)

plot(m)

lm(formula = height ~ weight, data = d)
names(m)
m$coefficients
head(m$fitted.values)
head(m$residuals) #should be normally distributed
plot(m)

hist(m$residuals) #this looks likw aht it should look normally
qqplot(m) #standarized residual, how many standard deviations they are away from 0
broom::tidy(m)
confint(m)

b1 <- cor(d$weight, d$weight)/(sd(d))


#calculating T statistic and p value by hand
39.6/.596 #estimate/std. error (intercept)
.195/.00411 #estimate/std. error (weight)

pt(47.5, df = 998, lower.tail = FALSE) +
pt(-47.5, df = 998, lower.tail = TRUE)
broom::glance(m)
abline(coef(m1), col = 'blue')

m2 <- lmodel2(height~weight, data=d,
              range.y = "relative", range.x = "relative",
              nperm = 1000)
m2
summary(m2)

betas <- broom::tidy(m2) |>
  filter(method == "OLS") |>
  pull(estimate)
abline(betas, col = "blue")
plot(m2, "OLS")


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/Street_et_al_2017.csv"

d <- read_csv(f, col_names = TRUE)

par(mfrow = c(2,2))
plot(d$Longevity, d$ECV)
plot(d$Group_size, d$ECV)
plot(d$Weaning, d$ECV)
plot(d$Repro_lifespan, d$ECV)
m1 <- lm(formula = ECV~Longevity, data =d)
m1
broom::tidy(m1)

m2 <- lm(formula = ECV~Group_size, data = d)
m2

m3 <- lm(formula = ECV~Weaning, data = d)
m3

m4 <- lm(formula = ECV~Repro_lifespan, data = d)
m4

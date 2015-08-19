
#q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

mu <- 0.1471
sum(w * (x - mu) ^ 2)

#ans = 0.1471


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ 0 + x)
#0.8263
lm(y ~ x)
#1.567

data(mtcars)

lm(mpg ~ wt, data = mtcars)
#-5.344


#q4 ans = 1
#q5 ans = 0.6

sd(mtcars$wt)
sd(mtcars$mpg)
cor(mtcars$wt, mtcars$mpg)
lm(mtcars$wt ~ mtcars$mpg)

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x)) / sd(x)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
sum((x - 0.573) ^ 2)
sum((x - 0.8) ^ 2)
sum((x - 0.36) ^ 2)
sum((x - 0.44) ^ 2)
#0.573

#var(Y)/Var(X)

#quiz 2

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y ~ x))

yx <- lm(y ~ x)
sd(yx$residuals)
sqrt(deviance(yx) / df.residual(yx))

mpgPred <- lm(mpg ~ wt, data = mtcars)

predict(mpgPred, data.frame(wt = mean(mtcars$wt)), interval = "confidence")

help(mtcars)

predict(mpgPred, data.frame(wt = 3), interval = "prediction")

confint(mpgPred) * 2

#It would get multiplied by 100.
#The new intercept would be ??^0???c??^1


sum(lm(mpg ~ 1, data = mtcars)$residuals) / sum(mpgPred$residuals)
#0.25
#If an intercept is included, then they will sum to 0.

#quiz 3

mtcars$cyl <- as.factor(mtcars$cyl)
mpgPred2 <- lm(mpg ~ cyl + wt, data = mtcars)
summary(mpgPred2)
mpgPred3 <- lm(mpg ~ cyl, data = mtcars)
summary(mpgPred3)
#Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

mpgPred4 <- lm(mpg ~ cyl * wt, data = mtcars)
summary(mpgPred4)

anova(mpgPred2, mpgPred4)
#The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary.

#question 4
#The estimated expected change in MPG per half ton increase in weight.

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
hatvalues(lm(y ~ x))

which(hatvalues(lm(y ~ x)) == max(hatvalues(lm(y ~ x))))
dfbetas(lm(y ~ x))[5, ]

#question 7
#It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.


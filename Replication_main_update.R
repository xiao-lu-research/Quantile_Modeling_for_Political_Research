##############################################
# Replication File
# Quantile Modeling for Political Research
# Cambridge University Press
# by Xiao Lu
##############################################
rm(list=ls())
### Chapter 1

## Figure 1.4
# Illustration of unconditional quantile using ethnic voting example
# Load data from Hadzic, Carlson and Tavits 2017 BJPS: How Exposure to Violence Affects Ethnic Voting. Source file link: https://dx.doi.org/doi:10.7910/DVN/EUAIFA
library(ggplot2)  # Load ggplot2 package for visualization

# Load ethnic voting data
dat_violence <- read.csv(file="MainData.csv")

# Calculate median, mean, lower and upper quantiles of ethnic vote share
m1 <- median(dat_violence$Ethnic_Vote_Share)  # Calculate median
m2 <- mean(dat_violence$Ethnic_Vote_Share)    # Calculate mean
q95 <- quantile(dat_violence$Ethnic_Vote_Share, 0.95)  # Calculate 95th percentile
q05 <- quantile(dat_violence$Ethnic_Vote_Share, 0.05)  # Calculate 5th percentile
mean1 <- mean(dat_violence$Ethnic_Vote_Share)  # Calculate mean (duplicate for clarity)
sd1 <- sd(dat_violence$Ethnic_Vote_Share)      # Calculate standard deviation
sd95 <- mean1 + 1.96 * sd1  # Upper bound of 95% confidence interval
sd05 <- mean1 - 1.96 * sd1  # Lower bound of 95% confidence interval

# Create first violin plot showing mean and 95% confidence interval
pdf("Fig1.4a.pdf", width = 4, height = 4)
ggplot(dat_violence, 
       aes(y = Ethnic_Vote_Share, x = 1)) +
  geom_violin(alpha = 0.5,
              width = .2, fill = "lightblue", color = "gray") +
  labs(title = "", 
       x = "",
       y = "Ethnic Vote Share (%)") +
  geom_hline(yintercept = mean1, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = mean1 + 3, label = paste0("Mean: ", round(mean1, 2)), col = "black") +
  geom_hline(yintercept = sd95, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = sd95 + 3, label = paste0("Upper limit of 95% CI: ", round(sd95, 2)), col = "black") +
  geom_hline(yintercept = sd05, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = sd05 + 3, label = paste0("Lower limit of 95% CI: ", round(sd05, 2)), col = "black") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(20, 120) +
  ggtitle("Mean")
dev.off()

# Create second violin plot showing median and quantiles
pdf("Fig1.4b.pdf", width = 4, height = 4)
ggplot(dat_violence, 
       aes(y = Ethnic_Vote_Share, x = 1)) +
  geom_violin(alpha = 0.5,
              width = .2, fill = "lightblue", color = "gray") +
  labs(title = "", 
       x = "",
       y = "Ethnic Vote Share (%)") +
  geom_hline(yintercept = m1, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = m1 + 3, label = paste0("Median: ", round(m1, 2)), col = "black") +
  geom_hline(yintercept = q95, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = q95 + 3, label = paste0("0.95th Quantile: ", round(q95, 2)), col = "black") +
  geom_hline(yintercept = q05, size = 1.5, color = "black", linetype = "dashed") +
  geom_text(x = 1, y = q05 + 3, label = paste0("0.05th Quantile: ", round(q05, 2)), col = "black") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(20, 120) +
  ggtitle("Quantile")
dev.off()

## Figure 1.5 
# Generate plot comparing quantile regression with OLS
pdf("Fig1.5.pdf", width = 4.5, height = 4.5)
ggplot(dat_violence, 
       aes(x = Log_Casualty, 
           y = Ethnic_Vote_Share)) +
  # Add quantile regression lines at 5th, 50th, and 95th percentiles
  geom_quantile(quantiles = c(0.05, 0.5, 0.95), size = 2, linetype = "dashed") +
  # Add data points with transparency
  geom_point(alpha = 0.5) +
  # Add OLS regression line
  geom_smooth(method = 'lm', color = "red") +
  labs(title = "", 
       x = "Logged Casualty",
       y = "Ethnic Vote Share (%)") +
  # Add labels for the regression lines
  geom_text(x = 0, y = 90, label = "0.5th Conditional Quantile (Median)", col = "blue") +
  geom_text(x = 0, y = 100, label = "0.95th Conditional Quantile", col = "blue") +
  geom_text(x = 0, y = 45, label = "0.05th Conditional Quantile", col = "blue") +
  geom_text(x = 0.6, y = 77, label = "OLS", col = "red") +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()


### Chapter 2
## Figure 2.1
# Illustration of quantile check functions for different tau values

# Define check function for tau = 0.5 (median)
eq1 <- function(x) {
  return(x * (0.5 - ifelse(x < 0, 1, 0)))
}
# Define check function for tau = 0.1 (lower quantile)
eq2 <- function(x) {
  return(x * (0.1 - ifelse(x < 0, 1, 0)))
}
# Define check function for tau = 0.9 (upper quantile)
eq3 <- function(x) {
  return(x * (0.9 - ifelse(x < 0, 1, 0)))
}

# Create plot showing the three check functions
pdf("Fig2.1.pdf", width = 5, height = 4)
plot(eq1(-1:1), type = "l", xlab = "x", ylab = "y", lwd = 2, ylim = c(-0.18, 0.5))
lines(eq2(-1:1), lty = "dashed", lwd = 2)
lines(eq3(-1:1), lty = "dotted", lwd = 2)
legend("bottom", inset = .02, title = expression(paste("Value of ", tau)),
       c("0.1", "0.5", "0.9"), lty = c("dashed", "solid", "dotted"), horiz = TRUE, cex = 0.8)
dev.off()

# Table 2.1
# Load the ethnic voting data
data <- read.csv(file = "MainData.csv")
# View the names of all variables
names(data)
# Extract key variables and store them in a separate file
data_explore <- subset(data, select = c("Municipality", "Year", "Ethnic_Vote_Share", "Casualty", "Log_Casualty"))
# First 5 observations of the data
head(data_explore, 5)


## Figure 2.2
# Create scatter plots to visualize relationship between variables
# Scatter plot of Casualty and Ethnic_Vote_Share
plot(x = data_explore$Casualty, y = data_explore$Ethnic_Vote_Share, 
     pch = 18, col = "gray", xlab = "Casualty", ylab = "Ethnic Vote Share")
# Scatter plot of Log_Casualty and Ethnic_Vote_Share
plot(x = data_explore$Log_Casualty, y = data_explore$Ethnic_Vote_Share, 
     pch = 18, col = "gray", xlab = "Casualty (Logged)", ylab = "Ethnic Vote Share")

## Figure 2.3
# Load quantile regression package
library(quantreg)
# Fit simple quantile regression model for quantiles 0.1 through 0.9
mod1 <- rq(Ethnic_Vote_Share ~ Log_Casualty, data = data, tau = seq(0.1, 0.9, 0.1))
summary(mod1)
# Create plot of quantile regression coefficients
pdf("Fig2.3.pdf", width = 5, height = 5)
plot(mod1)
dev.off()

## Figure 2.4
# Difference-in-differences design using quantile regression
# Generate dummy matrix distinguishing between years before and after war
dummy.matrix <- as.matrix(cbind(as.numeric(dat_violence$Year == 2006), 
                               as.numeric(dat_violence$Year == 2010), 
                               as.numeric(dat_violence$Year == 2014)))
# Quantile estimation with municipality-fixed effects
mod2 <- rq(Ethnic_Vote_Share ~ Log_Casualty:dummy.matrix + dummy.matrix + Municipality - 1, 
          data = data, tau = seq(0.1, 0.9, 0.1))
# OLS estimation for comparison
mod2_ols <- lm(Ethnic_Vote_Share ~ Log_Casualty:dummy.matrix + dummy.matrix + Municipality - 1, 
             data = data)
# Summarize the results
mod2_est <- summary(mod2)
mod2_est_ols <- summary(mod2_ols)

# Looking at 0.1th quantile estimates
round(mod2_est[[1]]$coefficients, 2)

# Load packages for clustered standard errors
library(multiwayvcov)
library(lmtest)
# Calculate clustered standard errors for OLS model
vcovCL1 <- cluster.vcov(mod2_ols, ~data$Municipality + data$Year)
mod2_est_ols <- coeftest(mod2_ols, vcovCL1) # results

# Extract coefficient estimates of the main independent variables while omitting others
extract_coef <- function(x) return(x$coefficients[111:113, ])
# Put coefficients into a single data frame
coef_df <- as.data.frame(do.call("rbind", lapply(mod2_est, extract_coef)))
coef_df$Year <- rep(c(2006, 2010, 2014), 9)
coef_df$quantile <- rep(seq(0.1, 0.9, 0.1), each = 3)
names(coef_df)[2:3] <- c("lower", "upper")

# Create data frame for OLS estimates
tmp <- as.data.frame(mod2_est_ols[111:113, ])
tmp$coefficients <- tmp$Estimate
tmp$lower <- tmp$coefficients - 1.96 * tmp$`Std. Error`
tmp$upper <- tmp$coefficients + 1.96 * tmp$`Std. Error`
tmp$Year <- c(2006, 2010, 2014)
tmp1 <- tmp[rep(c(1, 2, 3), 9), ]
tmp1$quantile <- rep(seq(0.1, 0.9, 0.1), each = 3)
tmp1 <- subset(tmp1, select = c(
  "coefficients", "lower", "upper", "Year", "quantile"
))
names(coef_df)
coef_df$model <- "Quantile"
tmp1$model <- "OLS"
coef_df <- rbind(coef_df, tmp1)

# Visualize the coefficient estimates for both OLS and quantile models
library(ggplot2)
pdf("Fig2.4.pdf", width = 6.5, height = 4.5)
ggplot(coef_df, aes(x = quantile, y = coefficients, fill = model, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
  facet_wrap(~Year) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Quantiles") +
  ylab("Effect of Wartime Violence on Ethnic Vote Share") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

## Figure 2.5
library(foreign)  # Load foreign package to read Stata files

# Load data from Weschle 2017 BJPS: The Impact of Economic Crises on Political Representation in Public Communication: Evidence from the Eurozone. 
# Source file link: https://doi.org/10.7910/DVN/XVPHO1
data <- read.dta("data.dta")
dat <- data[which(data$type == 3), ]  # Filter data for type=3

# Keep only the main variables for analysis
dat2 <- subset(dat, select = c(
  gdppcgrowth, openness, population_log, elec, nparties, events
))

# Reformat data by repeating observations
dat3 <- dat2[rep(1:24579, 10), ]
coopscore <- c(as.matrix(dat[, 10:19]))  # Extract cooperation scores
coopscore_mean <- c(as.matrix(dat[, 510:519]))  # Extract mean cooperation scores
dat3$coopscore <- coopscore
dat3$coopscore_mean <- coopscore_mean

# Create visualization showing relationship between GDP growth and cooperation scores with quantile regression
pdf("Fig2.5.pdf", width = 6, height = 4.5)
ggplot(dat, aes(x = gdppcgrowth, y = coopscore_1)) +
  # Add scatter points
  geom_point(color = "gray", alpha = 0.5,          
             size = 2, shape = 1) +
  # Add quantile regression lines for 1st, 50th, and 99th percentiles
  geom_quantile(quantiles = c(0.01, 0.5, 0.99),
                aes(linetype = factor(after_stat(quantile))),
                linewidth = 0.8, color = "black",      
                show.legend = TRUE) +
  # Customize line types and legend
  scale_linetype_manual(
    name = "Quantiles",
    values = c("dotted", "solid", "dashed"),  
    labels = c("0.01th Quantile", "0.5th Quantile (Median)", "0.99th Quantile")) +
  # Add axis labels
  labs(x = "GDP Growth per Capita (%)",
       y = "Cooperation Score") +
  # Customize theme elements
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(
    face = "bold", size = 14,
    hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = c(0.3, 0.85),  
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.key.width = unit(1.5, "cm"),  
    plot.caption = element_text(hjust = 0, face = "italic")) +
  # Set y-axis limits
  coord_cartesian(ylim = c(-0.15, 0.15)) +
  # Add annotation explaining the plot elements
  annotate(
    "text", x = Inf, y = -Inf,
    label = "Gray circles: Observation points\nLine types: Quantile regression estimates",
    hjust = 1.1, vjust = -0.5,
    size = 3, color = "black")
dev.off()


## Figure 2.6
# Rescale the response variable by a factor of 1000 for better visualization
dat$coopscore_1_rescale <- dat$coopscore_1 * 1000

# Estimate quantile regression model at quartiles (0.25, 0.5, 0.75)
mod1 <- rq(coopscore_1_rescale ~ gdppcgrowth + openness + population_log + elec + nparties + 
          events + coopscore_mean_1, tau = c(0.25, 0.5, 0.75), data = dat)
summary(mod1)

# Plot quantile regression coefficients
pdf("Fig2.6.pdf", width = 7, height = 6)
plot(mod1)
dev.off()

## Table 2.3
# Fit OLS model for comparison with quantile regression
mod_ols <- lm(coopscore_1_rescale ~ gdppcgrowth + openness + population_log + elec + 
            nparties + events + coopscore_mean_1, data = dat)

# Fit quantile regression model at quartiles
mod1 <- rq(coopscore_1_rescale ~ gdppcgrowth + openness + population_log + elec + 
         nparties + events + coopscore_mean_1, tau = c(0.25, 0.5, 0.75), data = dat)

# Print model results
summary(mod_ols)
summary(mod1)

# Extract coefficient estimates
coef_ols <- summary(mod_ols)
coef_qut <- summary(mod1)

# Display coefficients rounded to 3 decimal places
# Mean (OLS) estimates
round(coef_ols$coefficients, 3)
# 0.25th quantile estimates
round(coef_qut[[1]]$coefficients, 3)
# 0.5th quantile (median) estimates
round(coef_qut[[2]]$coefficients, 3)
# 0.75th quantile estimates
round(coef_qut[[3]]$coefficients, 3)


### Chapter 3
## Figure 3.1
# Function to predict probabilities without heterogeneity
pred_prob_no_het <- function(beta0, beta1, epsilon, x) {
  # Calculate linear predictors with mean and quantiles of epsilon
  ystarmean <- beta0 + beta1 * x + mean(epsilon)
  ystar25 <- beta0 + beta1 * x + quantile(epsilon, probs = 0.025)
  ystar975 <- beta0 + beta1 * x + quantile(epsilon, probs = 0.975)
  
  # Convert to probabilities using logistic function
  pr_mean <- exp(ystarmean) / (1 + exp(ystarmean))
  pr_25 <- exp(ystar25) / (1 + exp(ystar25))
  pr_975 <- exp(ystar975) / (1 + exp(ystar975))
  
  # Create plot
  plot(NA, xlim = c(min(x), max(x)), ylim = c(0, 1),
       xlab = "X value",
       ylab = "Predicted probability")
  lines(x, pr_mean)
  lines(x, pr_25, lty = "dashed")
  lines(x, pr_975, lty = "dashed")
}

# Asymmetric Laplace Distribution (ALD) functions
# Density function for ALD
dald <- function(x, p) {
  out <- ifelse(x < 0, p * (1 - p) * exp(-x * (p - 1)), p * (1 - p) * exp(-x * p))
  return(out)
}
# Cumulative distribution function for ALD
pald <- function(x, p) {
  out <- ifelse(x < 0, p * exp(x * (1 - p)), 1 - (1 - p) * exp(-x * (p)))
  return(out)
}
# Function to compute inverse of a function (for quantile function)
inverse <- function(f, p, lower = -10000, upper = 10000) {
  function(y, p) uniroot((function(x) f(x, p) - y), lower = lower, upper = upper)[1]
}
# Quantile function for ALD
qald <- inverse(pald)
# Random number generator for ALD
rald <- function(n, p) {
  tmp <- runif(n)
  out <- NA
  for (i in 1:n) {
    out[i] <- qald(tmp[i], p)
  }
  return(unlist(out))
}

# Generate random data for simulation
set.seed(12345678)
N <- 300  # Sample size
df <- 3   # Degrees of freedom for t and chi-squared distributions

# Generate various error distributions
epsilon1 <- rnorm(N)                         # Normal distribution
epsilon2 <- rt(N, df)                        # t distribution with df=3
epsilon3 <- rchisq(N, df)                    # Chi-squared with df=3
epsilon4 <- rald(N, 0.3)                     # Asymmetric Laplace with tau=0.3
epsilon5 <- 2/3 * rnorm(100) + 1/3 * rnorm(100, sd = 1/10)  # Mixture of normals: kurtotic
epsilon6 <- 1/2 * rnorm(100, -1, 2/3) + 1/2 * rnorm(100, 1, 2/3)  # Bimodal distribution
epsilon7 <- 1/5 * rnorm(100, -22/25, 1) + 1/5 * rnorm(100, -49/125, 3/2) + 3/5 * rnorm(100, 29/250, 5/9)  # Skewed

# Generate predictor variable
x <- runif(N, 0, 5)
qtau <- 0.3  # Quantile of interest
beta0 <- 2   # Intercept
beta1 <- -1  # Slope

# Generate binary outcomes under different error distributions
# Normal errors
pr <- 1 - pnorm(-beta0 - beta1 * x)
y1 <- rbinom(N, 1, pr)
# t distribution errors
pr <- 1 - pt(-beta0 - beta1 * x, 3)
y2 <- rbinom(N, 1, pr)
# Chi-squared errors
pr <- 1 - pchisq(-beta0 - beta1 * x, 3)
y3 <- rbinom(N, 1, pr)
# ALD errors
pr <- 1 - pald(-beta0 - beta1 * x, 0.3)
y4 <- rbinom(N, 1, pr)
# Mixture of normals: kurtotic
pr <- 1 - (2/3 * pnorm(-beta0 - beta1 * x) + 1/3 * pnorm(-beta0 - beta1 * x, sd = 1/10))
y5 <- rbinom(N, 1, pr)
# Bimodal distribution
pr <- 1 - (1/2 * pnorm(-beta0 - beta1 * x, mean = -1, sd = 2/3) + 1/2 * pnorm(-beta0 - beta1 * x, mean = 1, sd = 2/3))
y6 <- rbinom(N, 1, pr)
# Skewed distribution
pr <- 1 - (1/5 * pnorm(-beta0 - beta1 * x, mean = -22/25, sd = 1) + 1/5 * pnorm(-beta0 - beta1 * x, mean = -49/125, sd = 1/10) + 3/5 * pnorm(-beta0 - beta1 * x, mean = 49/250, sd = 5/9))
y7 <- rbinom(N, 1, pr)

# Create evaluation grid for predictions
xs <- data.frame(x = seq(0, 5, length.out = N))

# Calculate true probabilities for each distribution
pr1 <- 1 - pnorm(-beta0 - beta1 * (xs$x))
pr2 <- 1 - pt(-beta0 - beta1 * (xs$x), 3)
pr3 <- 1 - pchisq(-beta0 - beta1 * (xs$x), 3)
pr4 <- 1 - pald(-beta0 - beta1 * xs$x, 0.3)
pr5 <- 1 - (2/3 * pnorm(-beta0 - beta1 * xs$x) + 1/3 * pnorm(-beta0 - beta1 * xs$x, sd = 1/10))
pr6 <- 1 - (1/2 * pnorm(-beta0 - beta1 * xs$x, mean = -1, sd = 2/3) + 1/2 * pnorm(-beta0 - beta1 * xs$x, mean = 1, sd = 2/3))
pr7 <- 1 - (1/5 * pnorm(-beta0 - beta1 * xs$x, mean = -22/25, sd = 1) + 1/5 * pnorm(-beta0 - beta1 * xs$x, mean = -49/125, sd = 1/10) + 3/5 * pnorm(-beta0 - beta1 * xs$x, mean = 49/250, sd = 5/9))

# Fit probit models to the generated data
probitout1 <- glm(y1 ~ x, family = binomial("probit"))
probitout2 <- glm(y2 ~ x, family = binomial("probit"))
probitout3 <- glm(y3 ~ x, family = binomial("probit"))
probitout4 <- glm(y4 ~ x, family = binomial("probit"))
probitout5 <- glm(y5 ~ x, family = binomial("probit"))
probitout6 <- glm(y6 ~ x, family = binomial("probit"))
probitout7 <- glm(y7 ~ x, family = binomial("probit"))

# Generate binary predictions from probit models
probit_predict1 <- ifelse(predict(probitout1, type = "response") > 0.5, 1, 0)
probit_predict2 <- ifelse(predict(probitout2, type = "response") > 0.5, 1, 0)
probit_predict3 <- ifelse(predict(probitout3, type = "response") > 0.5, 1, 0)
probit_predict4 <- ifelse(predict(probitout4, type = "response") > 0.5, 1, 0)
probit_predict5 <- ifelse(predict(probitout5, type = "response") > 0.5, 1, 0)
probit_predict6 <- ifelse(predict(probitout6, type = "response") > 0.5, 1, 0)
probit_predict7 <- ifelse(predict(probitout7, type = "response") > 0.5, 1, 0)

# Prediction grid
xs <- data.frame(x = seq(0, 5, length.out = N))

# Create multi-panel plot to compare probit estimates with true probabilities
pdf("Fig3.1.pdf", width = 7, height = 7)
# Set up 3x3 plot layout (with last panel for legend)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 8), 3, 3, byrow = TRUE))

# Distribution 1: Normal
pred_prob1 <- predict(probitout1, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob1$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "Normal distribution")
lines(xs$x, pred_prob1$fit + -1.96 * pred_prob1$se.fit, lty = "dashed")
lines(xs$x, pred_prob1$fit + 1.96 * pred_prob1$se.fit, lty = "dashed")
lines(xs$x, pr1, col = "red")

# Distribution 2: t distribution
pred_prob2 <- predict(probitout2, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob2$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "T distribution (df=3)")
lines(xs$x, pred_prob2$fit + -1.96 * pred_prob2$se.fit, lty = "dashed")
lines(xs$x, pred_prob2$fit + 1.96 * pred_prob2$se.fit, lty = "dashed")
lines(xs$x, pr2, col = "red")

# Distribution 3: Chi-square
pred_prob3 <- predict(probitout3, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob3$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "Chi-square distribution (df=3)")
lines(xs$x, pred_prob3$fit + -1.96 * pred_prob3$se.fit, lty = "dashed")
lines(xs$x, pred_prob3$fit + 1.96 * pred_prob3$se.fit, lty = "dashed")
lines(xs$x, pr3, col = "red")

# Distribution 4: ALD
pred_prob4 <- predict(probitout4, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob4$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "ALD (tau=0.3)")
lines(xs$x, pred_prob4$fit + -1.96 * pred_prob4$se.fit, lty = "dashed")
lines(xs$x, pred_prob4$fit + 1.96 * pred_prob4$se.fit, lty = "dashed")
lines(xs$x, pr4, col = "red")

# Distribution 5: Mixture of Normal distributions (Kurtotic)
pred_prob5 <- predict(probitout5, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob5$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "Mixture of Normal distributions:\nKurtotic")
lines(xs$x, pred_prob5$fit + -1.96 * pred_prob5$se.fit, lty = "dashed")
lines(xs$x, pred_prob5$fit + 1.96 * pred_prob5$se.fit, lty = "dashed")
lines(xs$x, pr5, col = "red")

# Distribution 6: Bimodal
pred_prob6 <- predict(probitout6, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob6$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "Bimodal distribution")
lines(xs$x, pred_prob6$fit + -1.96 * pred_prob6$se.fit, lty = "dashed")
lines(xs$x, pred_prob6$fit + 1.96 * pred_prob6$se.fit, lty = "dashed")
lines(xs$x, pr6, col = "red")

# Distribution 7: Skewed
pred_prob7 <- predict(probitout7, xs, type = "response", se.fit = T)
plot(xs$x, pred_prob7$fit, type = "l", xlab = "X", ylab = "Predicted Probability", 
     bty = "l", main = "Skewed distribution")
lines(xs$x, pred_prob7$fit + -1.96 * pred_prob7$se.fit, lty = "dashed")
lines(xs$x, pred_prob7$fit + 1.96 * pred_prob7$se.fit, lty = "dashed")
lines(xs$x, pr7, col = "red")

# Legend panel
plot(NA, xlim = c(0, 5), ylim = c(0, 1), axes = F,
     xlab = "",
     ylab = "")
legend("top", legend = c("True predicted probability", "Estimated predicted probability from Probit", "95% Confidence Interval"),
       lty = c("solid", "solid", "dashed"), col = c("red", "black", "black"), cex = 1.5, bty = "n")
dev.off()


## Section 3.4
# Source: Rasmussen, Anne. 2010. Early Conclusion in Bicameral Bargaining: Evidence from the Co-decision Legislative Procedure of the European Union. European Union Politics, 12(1), 41–64.
# Link: https://doi.org/10.1177/1465116510388675
# Load EU Legislative data
load("data_legislature.RData")
# Check variable names in dataset
names(data_legislature)
# Examine first 5 rows of the dataset
head(data_legislature, 5)

# Load Bayesian quantile regression package
library(bayesQR)
# Estimate Bayesian quantile regression model at multiple quantiles
mod1 <- bayesQR(choice ~ var6 + var3 + var5 + var10 + var15 + urgcy_yes + urgcy_noleg + var4 + var11 + var14 + var7 + var17, 
                data = data_legislature, quantile = c(0.1, 0.25, 0.5, 0.75, 0.9), ndraw = 10000)
# Summarize the results
mod1_out <- summary(mod1)

# Fit mean-based model (logit) for comparison
mod1_logit <- glm(choice ~ var6 + var3 + var5 + var10 + var15 + urgcy_yes + urgcy_noleg + var4 + var11 + var14 + var7 + var17, 
                 data = data_legislature, family = "binomial")
logit_out <- summary(mod1_logit)

## Figure 3.2
# Create traceplot of MCMC draws to assess convergence
oldpar <- par()  # Save current graphical parameters
# pdf("Fig3.2.pdf", width = 5, height = 4)
# par(mfrow = c(2, 3), mar = c(4, 4, 1, 2))  # Set up 2x3 panel layout
plot(mod1, var = "var3", plottype = "trace", burnin = 100, main = "", ylab = "Coefficient", xlab = "Iteration")
# dev.off()
par(oldpar)  # Restore original graphical parameters

## Figure 3.3
# Create coefficient plot showing effect across quantiles
pdf("Fig3.3.pdf", width = 5, height = 4)
plot(mod1, var = "var3", plottype = "quantile", burnin = 1000, xlab = "Quantile", ylab = "Estimated Effect")
abline(h = 0, lty = "dashed")  # Add reference line at zero
# Add logit estimate and confidence interval
abline(h = logit_out$coefficients[3, 1], lty = "solid", col = "red")
abline(h = logit_out$coefficients[3, 1] + 1.96 * logit_out$coefficients[3, 2], lty = "dashed", col = "red")
abline(h = logit_out$coefficients[3, 1] - 1.96 * logit_out$coefficients[3, 2], lty = "dashed", col = "red")
dev.off()

## Figure 3.4
# Plot all coefficients across quantiles
pdf("Fig3.4.pdf", width = 8, height = 8)
par(mfrow = c(5, 3), mar = c(4, 4, 1, 2))  # Set up 5x3 panel layout
for (i in 1:13) {
  plot(mod1, var = i, plottype = "quantile", burnin = 1000, main = "", ylab = "", xlab = "Quantile")  
  abline(h = 0, lty = "dashed")  # Add reference line at zero
}
dev.off()
par(oldpar)  # Restore original graphical parameters


## Section 3.5
# Source: Miller, Warren E, Kinder, Donald R, & Rosenstone, Steven J. 1993. The National Election Studies. 1993. American National Election Study, 1992: Pre-and Post-election Survey CPS Early Release Version, computer file. Ann Arbor: University of Michigan, Center for Political Studies, and Inter-University Consortium for Political and Social Research; Alvarez, R Michael, & Nagler, Jonathan. 1995. Economics, Issues and the Perot Candidacy: Voter Choice in the 1992 Presidential Election. American Journal of Political Science, 39(3), 714–744.
# Load US presidential vote data
load("data_vote.RData")
# Take a subset of data_vote with only the first 30 choice sets for computational efficiency
obs <- unique(data_vote$chid)[1:30]
data <- subset(data_vote, data_vote$chid %in% obs)

# Load conditional binary quantile package
library(cbq)
# Fit quantile model at 0.1 (lower tail)
mod_vote1 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.1)

# Fit standard logit model for comparison
mod_glm <- glm(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                 respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                 deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
               + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                 respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                 deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton, 
               data = data_vote, family = binomial(logit))

# Print model results
print(mod_vote1)
# Examine coefficient estimates
coef(mod_vote1)
# Check MCMC convergence
plot(mod_vote1)
# Plot coefficient estimates
plot(mod_vote1, "coef")

# Calculate predicted probabilities at 0.1th quantile
predict(mod_vote1)

# Extract real choices from data
ids <- data_vote$chid
real_choice <- rep(0, 909)
for (i in 1:909) {
  nnn <- data_vote$choice[which(ids == i)]
  real_choice[i] <- which.max(nnn)
}

# Calculate prediction from quantile model
tmp <- predict(mod_vote1)[, 1]
pred1 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred1[i] <- which.max(nnn)
}

# Calculate prediction from logit model
tmp <- predict(mod_glm)
pred_glm <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred_glm[i] <- which.max(nnn)
}

## Table 4.1
# Create confusion tables for both models
table(real_choice, pred_glm)  # Logit model confusion matrix
table(real_choice, pred1)     # Quantile model confusion matrix
# Calculate prediction accuracy for quantile model
length(which(real_choice == pred1)) / 909

## Quantile models at other quantile locations
mod_vote2 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.2, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote3 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.3, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote4 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.4, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote5 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.5, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)


mod_vote6 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.6, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote7 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.7, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote8 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.8, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

mod_vote9 <- cbq(choice ~ dist + respfinp.bush + natlec.bush + respgjob.bush + resphlth.bush + 
                   respblk.bush + respab.bush + east.bush + south.bush + west.bush + newvoter.bush + termlim.bush +
                   deficit.bush + dem.bush + rep.bush + women.bush + educ.bush + age1829.bush + age3044.bush + age4559.bush
                 + respfinp.clinton + natlec.clinton + respgjob.clinton + resphlth.clinton + 
                   respblk.clinton + respab.clinton + east.clinton + south.clinton + west.clinton + newvoter.clinton + termlim.clinton +
                   deficit.clinton + dem.clinton + rep.clinton + women.clinton + educ.clinton + age1829.clinton + age3044.clinton + age4559.clinton|chid, 
                 data = data_vote, 
                 q = 0.9, 
                 vi = FALSE, 
                 nsim = 1000, 
                 grad_samples = 1, 
                 elbo_samples = 100, 
                 tol_rel_obj = 0.01, 
                 output_samples = 2000, 
                 burnin = NULL, 
                 thin = 1, 
                 CIsize = 0.95, 
                 nchain = 1, 
                 seeds = 12345, 
                 inverse_distr = FALSE, 
                 offset = 1e-20, 
                 mc_core = TRUE 
)

## Table 4.2
# Extract real choices from the data
ids <- data_vote$chid
real_choice <- rep(0, 909)
for (i in 1:909) {
  nnn <- data_vote$choice[which(ids == i)]
  real_choice[i] <- which.max(nnn)
}

# Generate predictions from logit model
tmp <- predict(mod_glm)
pred_glm <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred_glm[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred_glm)
length(which(real_choice == pred_glm)) / 909

# Generate predictions from 0.1 quantile model
tmp <- predict(mod_vote1)[, 1]
pred1 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred1[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred1)
length(which(real_choice == pred1)) / 909

# Generate predictions from 0.2 quantile model
tmp <- predict(mod_vote2)[, 1]
pred2 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred2[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred2)
length(which(real_choice == pred2)) / 909

# Generate predictions from 0.3 quantile model
tmp <- predict(mod_vote3)[, 1]
pred3 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred3[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred3)
length(which(real_choice == pred3)) / 909

# Generate predictions from 0.4 quantile model
tmp <- predict(mod_vote4)[, 1]
pred4 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred4[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred4)
length(which(real_choice == pred4)) / 909

# Generate predictions from 0.5 quantile model
tmp <- predict(mod_vote5)[, 1]
pred5 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred5[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred5)
length(which(real_choice == pred5)) / 909

# Generate predictions from 0.6 quantile model
tmp <- predict(mod_vote6)[, 1]
pred6 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred6[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred6)
length(which(real_choice == pred6)) / 909

# Generate predictions from 0.7 quantile model
tmp <- predict(mod_vote7)[, 1]
pred7 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred7[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred7)
length(which(real_choice == pred7)) / 909

# Generate predictions from 0.8 quantile model
tmp <- predict(mod_vote8)[, 1]
pred8 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred8[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred8)
length(which(real_choice == pred8)) / 909

# Generate predictions from 0.9 quantile model
tmp <- predict(mod_vote9)[, 1]
pred9 <- rep(0, 909)
for (i in 1:909) {
  nnn <- tmp[which(ids == i)]
  pred9[i] <- which.max(nnn)
}
# Create confusion matrix and calculate accuracy
table(real_choice, pred9)
length(which(real_choice == pred9)) / 909

# Load original dataset for plotting
dat <- read.table("nes9212r.asc")
# Define variable names for the dataset
varnames <- c("preschc",    # choice: 1 Bush 2 Clinton 3 Perot
              "educ",       # Respondents education
              "east",       # Region (East)
              "south",      # Region (South)
              "west",       # Region (West)
              "women",      # Gender (Female)
              "respfinp",   # Felt personal finance were worse
              "natlec",     # Felt national economy was worse
              "bclibdis",   # Ideological Distance
              "gblibdis",   # Ideological Distance
              "rplibdis",   # Ideological Distance
              "dem",        # Democrat
              "rep",        # Republican
              "respgjob",   # Oppose government jobs
              "resphlth",   # Oppose government health care
              "respblk",    # Oppose government minority assistance
              "respab",     # Abortion
              "termlim",    # Term limits
              "age1829",    # Age: 18-29
              "age3044",    # Age: 30-44
              "age4559",    # Age: 45-59
              "newvoter",   # New or returning voter
              "deficit")    # Felt deficit was a major problem

# Assign variable names to dataset
names(dat) <- varnames
# Create candidate choice variable
dat$choice <- "Bush"
dat$choice[which(dat$preschc == 2)] <- "Clinton"
dat$choice[which(dat$preschc == 3)] <- "Perot"

# Rename distance variables to include candidate names
names(dat)[9] <- "dist.Clinton"
names(dat)[10] <- "dist.Bush"
names(dat)[11] <- "dist.Perot"

# Load predicted vote shares for all quantiles
load("pred_vote_share_all_quantile.RData")
pos_seq <- seq(from = 0.1, to = 7, by = 0.02)
# Extract predictions for each quantile model
preds1 <- preds_all_q[[1]]
preds2 <- preds_all_q[[2]]
preds3 <- preds_all_q[[3]]
preds4 <- preds_all_q[[4]]
preds5 <- preds_all_q[[5]]
preds6 <- preds_all_q[[6]]
preds7 <- preds_all_q[[7]]
preds8 <- preds_all_q[[8]]
preds9 <- preds_all_q[[9]]

# Load mean-based predictions
load("pred_voteshare.RData")

# Combine predictions from all quantile models
predsa1 <- c(preds1[1:346, 1], preds1[1:346, 2], preds1[1:346, 3])
predsa2 <- c(preds2[1:346, 1], preds2[1:346, 2], preds2[1:346, 3])
predsa3 <- c(preds3[1:346, 1], preds3[1:346, 2], preds3[1:346, 3])
predsa4 <- c(preds4[1:346, 1], preds4[1:346, 2], preds4[1:346, 3])
predsa5 <- c(preds5[1:346, 1], preds5[1:346, 2], preds5[1:346, 3])
predsa6 <- c(preds6[1:346, 1], preds6[1:346, 2], preds6[1:346, 3])
predsa7 <- c(preds7[1:346, 1], preds7[1:346, 2], preds7[1:346, 3])
predsa8 <- c(preds8[1:346, 1], preds8[1:346, 2], preds8[1:346, 3])
predsa9 <- c(preds9[1:346, 1], preds9[1:346, 2], preds9[1:346, 3])

# Stack all predictions together
predsa <- c(predsa1,
           predsa2,
           predsa3,
           predsa4,
           predsa5,
           predsa6,
           predsa7,
           predsa8,
           predsa9
)

# Create variable indicating the candidate
choice_alt <- rep(rep(c("Bush", "Clinton", "Perot"), each = 346), 9)
# Create variable indicating the quantile level
quantiles <- c(rep(0.1, 346 * 3),
              rep(0.2, 346 * 3),
              rep(0.3, 346 * 3),
              rep(0.4, 346 * 3),
              rep(0.5, 346 * 3),
              rep(0.6, 346 * 3),
              rep(0.7, 346 * 3),
              rep(0.8, 346 * 3),
              rep(0.9, 346 * 3)
)

# Create position values for x-axis
xs <- rep(pos_seq, 3 * 9)

# Create dataframe for plotting
plt_df <- as.data.frame(cbind(xs, predsa, choice_alt, quantiles))
plt_df$predsa <- as.numeric(plt_df$predsa)
plt_df$xs <- as.numeric(plt_df$xs)
plt_df$model <- "Quantile"
names(plt_df)

# Prepare data for mean-based model
xs1 <- rep(pos_seq, 3)
choice_alt1 <- rep(c("Perot", "Bush", "Clinton"), each = 346)
predsb <- c(preds[2:347, 1], preds[2:347, 2], preds[2:347, 3])

# Create dataframe for mean-based model
plt_df1 <- as.data.frame(cbind(xs1, predsb, choice_alt1))
plt_df1$predsb <- as.numeric(plt_df1$predsb)
plt_df1$xs1 <- as.numeric(plt_df1$xs1)
plt_df1$quantiles <- NA
plt_df1$model <- "Mean-based"
names(plt_df1)[1:3] <- c("xs", "predsa", "choice_alt")

# Combine quantile and mean-based dataframes
plt_df2 <- rbind(plt_df, plt_df1)
# Add points for real election outcome
plt_df2$x1 <- NA
plt_df2$y1 <- NA
plt_df2$x1[c(1, 347, 693)] <- 5.32
plt_df2$y1[c(1, 347, 693)] <- c(0.3410341, 0.4576458, 0.2013201)
plt_df2$text <- NA
plt_df2$text[c(1, 347, 693)] <- "Real outcome"

# Create Figure 4.3
library(ggrepel)
pdf("Fig4.3.pdf", width = 7, height = 5.5)
ggplot(plt_df2, aes(x = xs, y = predsa, group = quantiles, col = model, linetype = model)) +
  geom_line() +
  facet_wrap(~choice_alt) +
  ylab("Vote Share") +
  xlab("Bush's Position") +
  geom_point(aes(x = x1, y = y1), color = "red") +
  geom_text(aes(x = x1, y = y1, label = text), color = "red", vjust = -0.5) +
  scale_color_hue(l = 40, c = 35) +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()  


####################################
## Coalition government formation
# Source: Martin, Lanny W, & Stevenson, Randolph T. 2001. Government Formation in Parliamentary Democracies. American Journal of Political Science, 45(1), 33–50.
# Load coalition dataset
load("data_coalition.RData")
# Select subset of data for analysis (first 10 cases)
obs <- unique(data_coalition$case)[1:10]
data <- subset(data_coalition, data_coalition$case %in% obs)

## Estimate models at different quantiles
# 0.1th quantile model
mod_coa1 <- cbq(realg ~ minor + minwin + numpar + dompar + median + gdiv1 + mgodiv1 + prevpm + sq + mginvest + anmax2 + pro + anti|case, 
                data = data, 
                q = 0.1) 
# 0.9th quantile model
mod_coa9 <- cbq(realg ~ minor + minwin + numpar + dompar + median + gdiv1 + mgodiv1 + prevpm + sq + mginvest + anmax2 + pro + anti|case, 
                data = data, 
                q = 0.9) 

# Print model summaries
print(mod_coa1)
print(mod_coa9)


############
# Chapter 5
#####################
# Quantile treatment effects
## Real example: vote buying
library(readstata13)  # Load package to read Stata 13 files
library(cbq)          # Load conditional binary quantile package

# Load vote buying data
dat_vote <- read.dta13("voteBuying.dta")

# Fit standard logistic regression model
mod_glm <- glm(neigift ~ treatment, data = dat_vote, family = "binomial")
summary(mod_glm)

# Fit quantile models for neighbor gifts at different quantiles
mod_cbq11 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.1)
mod_cbq12 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.2)
mod_cbq13 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.3)
mod_cbq14 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.4)
mod_cbq15 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.5)
mod_cbq16 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.6)
mod_cbq17 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.7)
mod_cbq18 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.8)
mod_cbq19 <- cbq(neigift ~ treatment, data = dat_vote, q = 0.9)

# Fit quantile models for individual gifts at different quantiles
mod_cbq21 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.1)
mod_cbq22 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.2)
mod_cbq23 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.3)
mod_cbq24 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.4)
mod_cbq25 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.5)
mod_cbq26 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.6)
mod_cbq27 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.7)
mod_cbq28 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.8)
mod_cbq29 <- cbq(indgift ~ treatment, data = dat_vote, q = 0.9)

# Print estimation results for all models
print(mod_cbq11); print(mod_cbq12); print(mod_cbq13)
print(mod_cbq14); print(mod_cbq15); print(mod_cbq16)
print(mod_cbq17); print(mod_cbq18); print(mod_cbq19)
print(mod_cbq21); print(mod_cbq22); print(mod_cbq23)
print(mod_cbq24); print(mod_cbq25); print(mod_cbq26)
print(mod_cbq27); print(mod_cbq28); print(mod_cbq29)

# Extract coefficient estimates for neighbor gifts models
c11 <- coef(mod_cbq11)
c12 <- coef(mod_cbq12)
c13 <- coef(mod_cbq13)
c14 <- coef(mod_cbq14)
c15 <- coef(mod_cbq15)
c16 <- coef(mod_cbq16)
c17 <- coef(mod_cbq17)
c18 <- coef(mod_cbq18)
c19 <- coef(mod_cbq19)

# Extract coefficient estimates for individual gifts models
c21 <- coef(mod_cbq21)
c22 <- coef(mod_cbq22)
c23 <- coef(mod_cbq23)
c24 <- coef(mod_cbq24)
c25 <- coef(mod_cbq25)
c26 <- coef(mod_cbq26)
c27 <- coef(mod_cbq27)
c28 <- coef(mod_cbq28)
c29 <- coef(mod_cbq29)

# Combine coefficient estimates into a single dataframe for plotting
coefs_cbq <- rbind(c11[2, 1:3],
                  c12[2, 1:3],
                  c13[2, 1:3],
                  c14[2, 1:3],
                  c15[2, 1:3],
                  c16[2, 1:3],
                  c17[2, 1:3],
                  c18[2, 1:3],
                  c19[2, 1:3],
                  c21[2, 1:3],
                  c22[2, 1:3],
                  c23[2, 1:3],
                  c24[2, 1:3],
                  c25[2, 1:3],
                  c26[2, 1:3],
                  c27[2, 1:3],
                  c28[2, 1:3],
                  c29[2, 1:3]
)
coefs_cbq <- as.data.frame(coefs_cbq)
coefs_cbq$type <- rep(c("Neighbor Gifts", "Individual Gifts"), each = 9)
coefs_cbq$qs <- rep(1:9/10, 2)

# Create coefficient plot for Figure 5.1
library(ggplot2)
pdf("Fig5.1.pdf", width = 5, height = 3.5)
ggplot(coefs_cbq, aes(Estimate, qs)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LB, xmax = UB)) +
  geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(~type) +
  coord_flip() +
  theme_bw() +
  ylab("Quantiles") +
  scale_y_continuous(breaks = 1:9/10)
dev.off()


######################################################
# Quantile survival analysis
## Example: 
### 'Government Capacity, Societal Trust or Party Preferences: What Accounts for the Variety of National Policy Responses to the COVID-19 Pandemic in Europe?' by Dimiter Toshkov.

# Load COVID-19 lockdown data
surv.l <- read.csv('surv_l_22032021.csv')

# List of variables in the dataset with their descriptions
var.list <- c('GE.EST', 'CC.EST', 'RL.EST', 'RQ.EST', 'gdp.pc', 
             'sgi_experts', 'ghsi', 'plans', 
             'hosp_beds', 'icu_beds', 
             'nurses', 'doctors', 'health.exp.pc', 'private.health.exp',
             'physicians', 
             "hm_min_type", "hm_medic",           
             'trust.p', 'trust_gov', 'free',
             'rai', 'federalism',
             'parl', 'n_parties', 'minority',
             'lr', 'lrecon', 'galtan')


var.names <- c('Government effectiveness', 'Control of corruption', 'Rule of law', 'Regulatory quality', 'GDP per capita',
              'Expert advice index (SGI)', 'Global health security index', 'Pandemic plan age',
              'Hospital beds', 'ICU beds', 
              'Nurses per capita', 'Doctors per capita',
              'Health expenditure per capita', 'Private health exp. per capita',
              'Physicians in hospitals',
              'Separate ministry', 'Minister med. doctor',
              'Personal trust', 'Trust in government', 'Freedom index',
              'Regionalism', 'Federalism',
              'Bicameral legislature', 'Number of parties', 'Minority government',
              'General Left-Right', 'Economic Left-Right', 'GAL-TAN')

# Load required packages for survival analysis
library(quantreg)
library(survival)

# Prepare survival data
surv.l <- data.frame(surv.l)
surv.l$surv <- Surv(surv.l$duration, surv.l$censored == 0) 

# Set random seed for reproducibility
set.seed(12345)

# Fit quantile regression for censored data (at 0.1 quantile)
mod_surv <- crq(Surv(duration, censored == 0, type = "right") ~ trust_gov + pop, 
                data = surv.l, method = "Portnoy", tau = 0.1)
summary(mod_surv)

# Fit same model at multiple quantiles and extract coefficients
set.seed(12345)
suv_coef <- summary(mod_surv, taus = 1:9/10)
suv_coefall <- rbind(suv_coef[[1]]$coefficients[2, 1:3],
                    suv_coef[[2]]$coefficients[2, 1:3],
                    suv_coef[[3]]$coefficients[2, 1:3],
                    suv_coef[[4]]$coefficients[2, 1:3],
                    suv_coef[[5]]$coefficients[2, 1:3],
                    suv_coef[[6]]$coefficients[2, 1:3],
                    suv_coef[[7]]$coefficients[2, 1:3],
                    suv_coef[[8]]$coefficients[2, 1:3],
                    suv_coef[[9]]$coefficients[2, 1:3]
)

# Convert to dataframe and add column names
suv_coefall <- as.data.frame(suv_coefall)
names(suv_coefall) <- c("Est", "LB", "UB")
suv_coefall$qs <- 1:9/10

# Fit Cox proportional hazards model for comparison
cox_model <- coxph(surv ~ trust_gov + pop, data = surv.l)
summary(cox_model)

# Load packages for survival visualization
library(ggsurvfit)
library(dplyr)

# Create Kaplan-Meier survival curve (Fig 5.2)
pdf("Fig5.2.pdf", width = 5, height = 3.5)
survfit(Surv(surv.l$duration, surv.l$censored == 0) ~ 1, data = surv.l) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Probability without Lockdown"
  ) +
  add_confidence_interval() +
  add_risktable()
dev.off()

# Create coefficient plot for quantile regression (Fig 5.3)
pdf("Fig5.3.pdf", height = 3.5, width = 5)
ggplot(suv_coefall, aes(qs, Est)) +
  geom_line() +
  geom_errorbar(aes(ymin = LB, ymax = UB)) +
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw() +
  scale_x_continuous(breaks = 1:9/10) +
  xlab("Quantile") +
  ylab("Estimates")
dev.off()


#########################################
# Quantile Lasso - Variable Selection Example
# Generate synthetic data
set.seed(123)
n <- 1000  # Number of observations
X <- matrix(rnorm(n * 10), ncol = 10)  # Ten independent variables
coefs <- c(2, 0, 2, 3, 2, 0, 1, 0.5, 0.5, 0)  # True coefficients (some zero)
Y <- X %*% matrix(coefs) + rnorm(n)  # Outcome variable with noise

# Combine data into a data frame
data <- data.frame(Y, X)

# Specify the quantiles of interest
tau <- c(0.1, 0.5, 0.9)

# Perform LASSO quantile regression for variable selection
lasso_model <- rq(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
                  data = data, tau = tau, method = "lasso")
print(lasso_model)
round(coef(lasso_model), 3)  # Print rounded coefficients


#####################################
# Model Comparison Methods for Quantile Regression
# Load quantreg package
library(quantreg)   

# With real data
# Load data from Weschle 2017 BJPS
library(foreign)
data <- read.dta("data.dta")

# Fit quantile regression model with country and year fixed effects
mod1 <- rq(coopscore_1 ~ gdppcgrowth + openness + population_log + elec + nparties + events + 
             coopscore_mean_1 + factor(country) + factor(year), 
           tau = c(0.25, 0.5, 0.75), data = data)
summary(mod1)

# Calculate predicted values for each quantile
preds <- predict(mod1)
pred1 <- preds[, 1]  # 0.25 quantile predictions
pred2 <- preds[, 2]  # 0.5 quantile predictions
pred3 <- preds[, 3]  # 0.75 quantile predictions

# Calculate Root Mean Squared Error (RMSE) for each quantile
rmse1 <- sqrt(mean((data$coopscore_1 - pred1)^2))
rmse2 <- sqrt(mean((data$coopscore_1 - pred2)^2))
rmse3 <- sqrt(mean((data$coopscore_1 - pred3)^2))
which.min(c(rmse1, rmse2, rmse3))  # Find quantile with lowest RMSE

# Calculate Mean Absolute Error (MAE) for each quantile
mae1 <- mean(abs(data$coopscore_1 - pred1))
mae2 <- mean(abs(data$coopscore_1 - pred2))
mae3 <- mean(abs(data$coopscore_1 - pred3))
which.min(c(mae1, mae2, mae3))  # Find quantile with lowest MAE

# Calculate check loss for each quantile
check_loss <- function(y, y_pred, tau) {
  residuals <- y - y_pred
  loss <- ifelse(residuals >= 0, tau * residuals, (tau - 1) * residuals)
  mean(loss)
}
cl1 <- check_loss(data$coopscore_1, pred1, 0.25)
cl2 <- check_loss(data$coopscore_1, pred2, 0.5)
cl3 <- check_loss(data$coopscore_1, pred3, 0.75)
which.min(c(cl1, cl2, cl3))  # Find quantile with lowest check loss

# Compare models using AIC
aics <- AIC(mod1)
which.min(aics)  # Find quantile with lowest AIC

# Calculate BIC for each quantile
bics <- AIC(mod1) - 2 * dim(coef(mod1))[1] + dim(coef(mod1))[1] * log(dim(data)[1])
which.min(bics)  # Find quantile with lowest BIC



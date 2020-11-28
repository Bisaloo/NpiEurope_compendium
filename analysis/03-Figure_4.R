#-------------------------------------------------
# Summary of public health responses efficiencies
#-------------------------------------------------
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(reshape)
library(lme4)
library(dplyr)
library(gridExtra)

load("matResults.RData")

dfMat <- data.frame(matResults)

# Estimate difference of efficiency from previous strategy
dfMat_ts <- dfMat
pos <- 0
dfMat_ts$dif.efficiency <- NA
for (i in unique(dfMat_ts$Country)) {
  my.db <- dfMat_ts[dfMat_ts$Country == i, ]
  for (j in 1:nrow(my.db)) {
    pos <- pos + 1
    if (j == 1) {
      dfMat_ts$dif.efficiency[pos] <- dfMat_ts$Efficiency[pos]
    }
    else {
      dfMat_ts$dif.efficiency[pos] <- dfMat_ts$Efficiency[pos] - dfMat_ts$Efficiency[pos - 1]
      for (k in 1:24) {
        if (dfMat[pos, k] == 1 & dfMat[pos - 1, k] == 1) {
          dfMat_ts[pos, k] <- 0
        }
        if (dfMat[pos, k] == 0 & dfMat[pos - 1, k] == 1) {
          dfMat_ts[pos, k] <- -1
        }
      }
    }
  }
}
dfMat_ts$GDP <- dfMat_ts$GDP / 10000
#----------------------------------------------------------
# Model change in efficiency per day from time series data
#----------------------------------------------------------
# Univariate with and without control for GDP and Nb Interventions
my.table_ts <- array(NA, c(24, 7))
for (i in 1:24) {
  my.var <- colnames(dfMat_ts)[i]
  # Univariate model
  model.uni <- lmer(as.formula(paste("dif.efficiency~", my.var, "+(1|Country)", sep = "")), data = dfMat_ts)
  my.table_ts[i, 2] <- coefficients(summary(model.uni))[2, 1]
  my.table_ts[i, 3] <- coefficients(summary(model.uni))[2, 1] - 1.96 * coefficients(summary(model.uni))[2, 2]
  my.table_ts[i, 4] <- coefficients(summary(model.uni))[2, 1] + 1.96 * coefficients(summary(model.uni))[2, 2]
  # Multivariate model with one variable at a time (controlled by GDP & nb interventions)
  model.controlled <- lmer(as.formula(paste("dif.efficiency~NbStrategies+GDP+duration+", my.var, "+(1|Country)", sep = "")), data = dfMat_ts)
  my.table_ts[i, 5] <- coefficients(summary(model.controlled))[5, 1]
  my.table_ts[i, 6] <- coefficients(summary(model.controlled))[5, 1] - 1.96 * coefficients(summary(model.controlled))[5, 2]
  my.table_ts[i, 7] <- coefficients(summary(model.controlled))[5, 1] + 1.96 * coefficients(summary(model.controlled))[5, 2]
}
my.table_ts <- as.data.frame(my.table_ts)
colnames(my.table_ts) <- c("NPI", "diff.effect.uni", "low.ci.uni", "up.ci.uni", "diff.effect.control", "low.ci.control", "up.ci.control")
my.table_ts$NPI <- colnames(matResults)[1:24]

# Check most effective interventions
my.table_ts[my.table_ts$diff.effect.control > 0.15 & my.table_ts$low.ci.control > 0, "NPI"]
my.table_ts[my.table_ts$diff.effect.control == max(my.table_ts$diff.effect.control), "NPI"]

#--------------------------------
# Figure 4A - Univariate results
#--------------------------------
npi.group <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1)
# axis.col=factor(npi.group, labels=c('black','grey40','black','grey40','black'))
axis.face <- factor(npi.group, labels = c("bold", "italic", "bold", "italic", "bold"))

changes.plot_ts <- ggplot(my.table_ts, aes(x = diff.effect.control, y = NPI)) +
  geom_vline(xintercept = 0, color = "darkred", size = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = low.ci.control, xmax = up.ci.control)) +
  geom_point(size = 3) +
  labs(x = "Change in efficiency when adding each NPI", y = "") +
  # facet_grid(variable~.) +
  theme(
    legend.position = "bottom", axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(face = as.character(axis.face))
  )
changes.plot_ts

#------------------------------
# Figure 4B - Grouped Analysis
#------------------------------
# Group them into larger groups (select the appropriate NPI group according to what we want to include)
npi.group <- c(1, 2, 3, 1, 4, 4, 4, 4, 2, 4, 5, 5, 4, 4, 5, 3, 4, 1, 4, 2, 4, 1, 5, 1) # Considering all interventions
# npi.group=c(1,2,3,1,4,4,4,4,2,4,5,5,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA)    # Excluding interventions with partial implementation

npi.group.db <- as.data.frame(array(NA, c(nrow(dfMat), 5)))
colnames(npi.group.db) <- c("StayHome", "MassGather", "Teleworking", "ClosingSchools", "Masks")
for (j in unique(npi.group)[1:5]) {
  jj <- which(npi.group == j)
  if (length(jj) > 1) {
    npi.group.db[, j] <- apply(dfMat[, jj], 1, function(x) {
      ifelse(any(x == 1), 1, 0)
    })
  }
  else {
    npi.group.db[, j] <- dfMat[, jj]
  }
}
dfMat <- data.frame(npi.group.db, dfMat[, 25:ncol(dfMat)])

# Estimate efficiency without stay home orders
length(which(dfMat$Efficiency[dfMat$StayHome == 1] > 0.66)) / length(which(dfMat$Efficiency > 0.66))
length(which(dfMat$Efficiency[dfMat$StayHome == 0] > 0.66)) / length(which(dfMat$Efficiency > 0.66))

# Estimate difference of efficiency from previous strategy
dfMat_ts <- dfMat
pos <- 0
dfMat_ts$dif.efficiency <- NA
for (i in unique(dfMat_ts$Country)) {
  my.db <- dfMat_ts[dfMat_ts$Country == i, ]
  for (j in 1:nrow(my.db)) {
    pos <- pos + 1
    if (j == 1) {
      dfMat_ts$dif.efficiency[pos] <- dfMat_ts$Efficiency[pos]
    }
    else {
      dfMat_ts$dif.efficiency[pos] <- dfMat_ts$Efficiency[pos] - dfMat_ts$Efficiency[pos - 1]
      for (k in 1:5) {
        if (dfMat[pos, k] == 1 & dfMat[pos - 1, k] == 1) {
          dfMat_ts[pos, k] <- 0
        }
        if (dfMat[pos, k] == 0 & dfMat[pos - 1, k] == 1) {
          dfMat_ts[pos, k] <- -1
        }
      }
    }
  }
}
dfMat_ts$GDP <- dfMat_ts$GDP / 10000
#----------------------------------------------------------
# Model change in efficiency per day from time series data
#----------------------------------------------------------
# Univariate with and without control for GDP and Nb Interventions
my.table_ts <- array(NA, c(5, 7))
for (i in 1:5) {
  my.var <- colnames(dfMat_ts)[i]
  # Univariate model
  model.uni <- lmer(as.formula(paste("dif.efficiency~", my.var, "+(1|Country)", sep = "")), data = dfMat_ts)
  my.table_ts[i, 2] <- coefficients(summary(model.uni))[2, 1]
  my.table_ts[i, 3] <- coefficients(summary(model.uni))[2, 1] - 1.96 * coefficients(summary(model.uni))[2, 2]
  my.table_ts[i, 4] <- coefficients(summary(model.uni))[2, 1] + 1.96 * coefficients(summary(model.uni))[2, 2]
  # Multivariate model with one variable at a time (controlled by GDP & nb interventions)
  model.controlled <- lmer(as.formula(paste("dif.efficiency~NbStrategies+GDP+duration+", my.var, "+(1|Country)", sep = "")), data = dfMat_ts)
  my.table_ts[i, 5] <- coefficients(summary(model.controlled))[5, 1]
  my.table_ts[i, 6] <- coefficients(summary(model.controlled))[5, 1] - 1.96 * coefficients(summary(model.controlled))[5, 2]
  my.table_ts[i, 7] <- coefficients(summary(model.controlled))[5, 1] + 1.96 * coefficients(summary(model.controlled))[5, 2]
}
my.table_ts <- as.data.frame(my.table_ts)
colnames(my.table_ts) <- c("NPI", "diff.effect.uni", "low.ci.uni", "up.ci.uni", "diff.effect.control", "low.ci.control", "up.ci.control")
my.table_ts$NPI <- colnames(dfMat_ts)[1:5]

# Multivariate model
model.multi <- lmer(dif.efficiency ~ NbStrategies + GDP + duration + StayHome + MassGather + Teleworking + ClosingSchools + Masks + (1 | Country), data = dfMat_ts)

my.table_ts$diff.effect.multi <- coefficients(summary(model.multi))[5:9, 1]
my.table_ts$low.ci.multi <- coefficients(summary(model.multi))[5:9, 1] - 1.96 * coefficients(summary(model.multi))[5:9, 2]
my.table_ts$up.ci.multi <- coefficients(summary(model.multi))[5:9, 1] + 1.96 * coefficients(summary(model.multi))[5:9, 2]

#------------------
# Plot the results
#------------------
my.table_ts$NPI.axis <- c(21, 16, 25, 4, 12)
changes.plot_ts_grouped <- ggplot(my.table_ts, aes(x = diff.effect.control, y = NPI.axis)) +
  geom_vline(xintercept = 0, color = "darkred", size = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = low.ci.control, xmax = up.ci.control), size = 1) +
  geom_point(size = 4) +
  labs(x = "Change in efficiency when adding each NPI", y = "") +
  scale_y_continuous(breaks = my.table_ts$NPI.axis, labels = my.table_ts$NPI, limits = c(-1, 25)) +
  theme(
    plot.margin = margin(0.2, 3, 0.2, 0, "cm"), axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(face = c("italic", "bold", "bold", "bold", "italic"))
  ) #
changes.plot_ts_grouped


# Save into a high resolution image
jpeg("Figure 4.jpeg", units = "in", width = 14, height = 8, res = 300)
grid.arrange(changes.plot_ts, changes.plot_ts_grouped, ncol = 2)
dev.off()

#-------------------------------------------------
# Summary of public health responses efficiencies
#-------------------------------------------------
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(reshape)
library(grid)
library(gridExtra)
dfMat <- readRDS("matResults.rds")
stratUn <- colnames(dfMat)[1:26]

hist.plot <- ggplot(dfMat, aes(x = Efficiency)) +
  geom_histogram(binwidth = 0.05, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  geom_vline(xintercept = 0.66, color = "darkred", lty = "dashed", size = 2) +
  coord_flip() +
  theme_classic() +
  labs(y = "Public Health Responses") +
  theme(
    axis.text = element_text(size = 16, face = "bold"), axis.title = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )
hist.plot

correlation.plot <- ggplot(dfMat, aes(x = NbStrategies, y = Efficiency)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0.66, color = "darkred", lty = "dashed", size = 2) +
  theme_classic() +
  labs(x = "NPIs implemented") +
  scale_x_continuous(breaks = seq(1, 11, 2)) +
  theme(
    axis.text = element_text(size = 16, face = "bold"), axis.title = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )
correlation.plot
# Save into a high resolution image
jpeg("Figure 3_inset.jpeg", units = "in", width = 4, height = 12, res = 300)
grid.arrange(hist.plot, correlation.plot, nrow = 2)
dev.off()

# Estimate effect by number of strategies
my.table <- array(NA, c(26, 12))
my.table.se <- array(NA, c(26, 12))
for (i in 1:26) {
  my.var <- colnames(matResults)[i]
  for (j in 1:11) {
    # Univariate model
    model.uni <- glm(data = subset(dfMat, NbStrategies == j), as.formula(paste("Efficiency~", my.var, sep = "")))
    if (nrow(coefficients(summary(model.uni))) > 1) {
      my.table[i, j + 1] <- coefficients(summary(model.uni))[2, 1]
      my.table.se[i, j + 1] <- coefficients(summary(model.uni))[2, 2]
    }
    else {
      my.table[i, j + 1] <- NA
      my.table.se[i, j + 1] <- NA
    }
  }
}
my.table <- as.data.frame(my.table)
my.table.se <- as.data.frame(my.table.se)
colnames(my.table) <- c("NPI", paste("effect.uni", 1:11, sep = "."))
colnames(my.table.se) <- c("NPI", paste("stderror.uni", 1:11, sep = "."))
my.table$NPI <- colnames(matResults)[1:26]
my.table.se$NPI <- colnames(matResults)[1:26]

library(tidyr)
my.table_long <- gather(my.table, NbStrategies, efficiency, effect.uni.1:effect.uni.11, factor_key = TRUE)
my.table.se_long <- gather(my.table.se, NbStrategies, stderror, stderror.uni.1:stderror.uni.11, factor_key = TRUE)

num.pos <- gregexpr("[0-9]+", my.table_long$NbStrategies)
strategies <- regmatches(my.table_long$NbStrategies, num.pos)
strategies <- lapply(strategies, `[[`, 1)
my.table_long$NbStrategies <- as.numeric(unlist(strategies))

num.pos <- gregexpr("[0-9]+", my.table.se_long$NbStrategies)
strategies <- regmatches(my.table.se_long$NbStrategies, num.pos)
strategies <- lapply(strategies, `[[`, 1)
my.table.se_long$NbStrategies <- as.numeric(unlist(strategies))

# Plot
npi.group <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1)
# axis.col=factor(npi.group, labels=c('black','grey40','black','grey40','black'))
axis.face <- factor(npi.group, labels = c("bold", "italic", "bold", "italic", "bold"))

nb.strategies.plot <- ggplot(data = subset(my.table_long, NbStrategies > 3), mapping = aes(x = NbStrategies, y = NPI, fill = efficiency)) +
  geom_tile() +
  geom_text(aes(label = round(efficiency, 2)), size = 2.5) +
  theme(strip.placement = "outside") +
  scale_fill_gradient2(high = "darkgreen") +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 10, face = "bold"), legend.position = "none",
    axis.text.y = element_text(face = as.character(axis.face))
  ) +
  scale_x_continuous(breaks = seq(4, 11, 1)) +
  labs(x = "NPIs implemented", y = "")
nb.strategies.plot

nb.strategies.se.plot <- ggplot(data = subset(my.table.se_long, NbStrategies > 3), mapping = aes(x = NbStrategies, y = NPI, fill = stderror)) +
  geom_tile() +
  theme(strip.placement = "outside") +
  scale_fill_gradient2(high = "darkred") +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 10, face = "bold"), legend.position = "none",
    axis.text.y = element_text(face = as.character(axis.face))
  ) +
  scale_x_continuous(breaks = seq(4, 11, 1)) +
  labs(x = "NPIs implemented", y = "")
nb.strategies.se.plot

# NPI in the highest efficiency responses
resNb <- c()
for (i in 1:8) {
  id <- which(dfMat$NbStrategies == 3 + i)
  id1 <- which(dfMat$Efficiency[dfMat$NbStrategies == 3 + i] == max(dfMat$Efficiency[dfMat$NbStrategies == 3 + i]))
  # id1<-which( dfMat$Efficiency[dfMat$NbStrategies==i]>0.66)
  resNb <- rbind(resNb, matResults[id[id1], ])
}
tempDf <- melt(resNb[, 1:26], id.var = "indv")
tempDf$X1 <- tempDf$X1 + 3
# Plot
max.efficiency.plot <- ggplot(tempDf, aes(X1, X2)) +
  geom_tile(aes(fill = value), colour = "white") +
  labs(x = "NPIs implemented", y = "") +
  scale_x_continuous(breaks = seq(4, 11, 1)) +
  theme(
    plot.margin = margin(0.2, 5, 0.2, 0, "cm"), legend.position = "none", axis.text.y = element_blank(),
    axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 12, face = "bold")
  )
max.efficiency.plot

# Save into a high resolution image
jpeg("Figure 3.jpeg", units = "in", width = 14, height = 8, res = 300)
grid.arrange(nb.strategies.plot, max.efficiency.plot, ncol = 2)
dev.off()


# Load required library (ggplot2 only plots will use base R)
has_ggplot <- require(ggplot2, quietly = TRUE)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Read the dataset
data <- read.csv("World University Rankings 2023.csv", stringsAsFactors = FALSE)

# Display basic information about the dataset
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:\n")
print(colnames(data))

# =============================================================================
# 2. DATA CLEANING
# =============================================================================

# Extract country from Location column
data$Country <- sapply(strsplit(as.character(data$Location), ","), function(x) {
  if (length(x) > 0) {
    trimws(x[length(x)])
  } else {
    NA
  }
})

# Clean Overall Score - remove any non-numeric characters and convert to numeric
# First, handle the OverAll.Score column properly
data$OverAll.Score.Clean <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(data$OverAll.Score))))

# Check unique country values to find exact names
cat("\nChecking country values in Location field...\n")
country_counts <- sort(table(data$Country), decreasing = TRUE)
cat("Top 10 countries:\n")
print(head(country_counts, 10))

# Find the exact string for US and UK
us_pattern <- grep("United States|USA|US$", unique(data$Country), value = TRUE, ignore.case = TRUE)
uk_pattern <- grep("United Kingdom|UK$|Britain", unique(data$Country), value = TRUE, ignore.case = TRUE)

cat("\nUS patterns found:", us_pattern, "\n")
cat("UK patterns found:", uk_pattern, "\n")

# Filter for US and UK universities only - using the actual country names found
data_filtered <- data[!is.na(data$Country) & 
                      !is.na(data$OverAll.Score.Clean) & 
                      data$OverAll.Score.Clean > 0 &
                      (data$Country %in% us_pattern | data$Country %in% uk_pattern), ]

# Standardize country names for clarity
data_filtered$Country <- ifelse(data_filtered$Country %in% us_pattern, "United States",
                                ifelse(data_filtered$Country %in% uk_pattern, "United Kingdom", 
                                       data_filtered$Country))

# Convert Country to factor for proper plotting
data_filtered$Country <- factor(data_filtered$Country, levels = c("United Kingdom", "United States"))

# Display sample sizes
cat("\n=============================================================================\n")
cat("SAMPLE SIZES\n")
cat("=============================================================================\n")
us_count <- sum(data_filtered$Country == "United States")
uk_count <- sum(data_filtered$Country == "United Kingdom")
cat("US Universities:", us_count, "\n")
cat("UK Universities:", uk_count, "\n")
cat("Total:", nrow(data_filtered), "\n")

# =============================================================================
# 3. DESCRIPTIVE STATISTICS
# =============================================================================

cat("\n=============================================================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("=============================================================================\n")

# Count frequencies
table(uni$University.Rank)

# Proportions
prop.table(table(uni$University.Rank))

# Calculate statistics for US
us_data <- data_filtered$OverAll.Score.Clean[data_filtered$Country == "United States"]
us_stats <- c(
  N = length(us_data),
  Mean = mean(us_data, na.rm = TRUE),
  Median = median(us_data, na.rm = TRUE),
  SD = sd(us_data, na.rm = TRUE),
  Min = min(us_data, na.rm = TRUE),
  Max = max(us_data, na.rm = TRUE),
  Q1 = quantile(us_data, 0.25, na.rm = TRUE),
  Q3 = quantile(us_data, 0.75, na.rm = TRUE)
)

# Calculate statistics for UK
uk_data <- data_filtered$OverAll.Score.Clean[data_filtered$Country == "United Kingdom"]
uk_stats <- c(
  N = length(uk_data),
  Mean = mean(uk_data, na.rm = TRUE),
  Median = median(uk_data, na.rm = TRUE),
  SD = sd(uk_data, na.rm = TRUE),
  Min = min(uk_data, na.rm = TRUE),
  Max = max(uk_data, na.rm = TRUE),
  Q1 = quantile(uk_data, 0.25, na.rm = TRUE),
  Q3 = quantile(uk_data, 0.75, na.rm = TRUE)
)

# Create descriptive statistics table
descriptive_stats <- data.frame(
  Statistic = names(us_stats),
  United_States = us_stats,
  United_Kingdom = uk_stats
)
rownames(descriptive_stats) <- NULL

print(descriptive_stats)

# =============================================================================
# 4. VISUALIZATIONS
# =============================================================================

# Create output directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

if (has_ggplot) {
  # --- VISUALIZATION 1: Boxplot (Main plot for comparison) ---
  cat("\nCreating boxplot with ggplot2...\n")
  p1 <- ggplot(data_filtered, aes(x = Country, y = OverAll.Score.Clean, fill = Country)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      title = "Comparison of Overall Scores: US vs UK Universities",
      subtitle = "World University Rankings 2023",
      x = "Country",
      y = "Overall Score",
      caption = "Diamond shape indicates mean value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 11),
      legend.position = "none"
    ) +
    scale_fill_manual(values = c("United Kingdom" = "#0051BA", "United States" = "#B31942"))
  
  ggsave("plots/boxplot_comparison.png", p1, width = 8, height = 6, dpi = 300)
  print(p1)
  
  # --- VISUALIZATION 2: Histograms (Required for comparison of means) ---
  cat("\nCreating histograms with ggplot2...\n")
  p2 <- ggplot(data_filtered, aes(x = OverAll.Score.Clean, fill = Country)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~Country, ncol = 1, scales = "free_y") +
    labs(
      title = "Distribution of Overall Scores by Country",
      subtitle = "World University Rankings 2023",
      x = "Overall Score",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 11),
      legend.position = "none"
    ) +
    scale_fill_manual(values = c("United Kingdom" = "#0051BA", "United States" = "#B31942"))
  
  ggsave("plots/histogram_distributions.png", p2, width = 8, height = 8, dpi = 300)
  print(p2)
  
  # --- VISUALIZATION 3: Density plot (Additional visualization) ---
  cat("\nCreating density plot with ggplot2...\n")
  
  # Calculate means for vertical lines
  mean_us <- mean(us_data)
  mean_uk <- mean(uk_data)
  
  p3 <- ggplot(data_filtered, aes(x = OverAll.Score.Clean, fill = Country)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = mean_us, color = "#B31942", linetype = "dashed", size = 1) +
    geom_vline(xintercept = mean_uk, color = "#0051BA", linetype = "dashed", size = 1) +
    labs(
      title = "Density Distribution of Overall Scores",
      subtitle = "Dashed lines indicate mean values",
      x = "Overall Score",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 11)
    ) +
    scale_fill_manual(values = c("United Kingdom" = "purple", "United States" = "pink"))
  
  ggsave("plots/density_plot.png", p3, width = 8, height = 6, dpi = 300)
  print(p3)
  
} else {
  # Use base R plotting if ggplot2 is not available
  cat("\nggplot2 not available. Creating plots with base R...\n")
  
  # --- VISUALIZATION 1: Boxplot ---
  png("plots/boxplot_comparison.png", width = 800, height = 600, res = 100)
  par(mar = c(5, 5, 4, 2))
  boxplot(OverAll.Score.Clean ~ Country, data = data_filtered,
          main = "Comparison of Overall Scores: US vs UK Universities",
          xlab = "Country",
          ylab = "Overall Score",
          col = c("#0051BA", "#B31942"),
          border = "black",
          notch = FALSE)
  
  # Add means as points
  means <- tapply(data_filtered$OverAll.Score.Clean, data_filtered$Country, mean)
  points(1:2, means, pch = 23, bg = "white", cex = 1.5)
  legend("topright", legend = "Diamond = Mean", pch = 23, pt.bg = "white", pt.cex = 1.5)
  dev.off()
  
  # --- VISUALIZATION 2: Histograms ---
  png("plots/histogram_distributions.png", width = 800, height = 800, res = 100)
  par(mfrow = c(2, 1), mar = c(5, 5, 4, 2))
  
  # UK histogram
  hist(uk_data, 
       breaks = 30, 
       col = "#0051BA",
       main = "Distribution of Overall Scores - United Kingdom",
       xlab = "Overall Score",
       ylab = "Frequency",
       xlim = range(c(us_data, uk_data)))
  
  # US histogram
  hist(us_data, 
       breaks = 30, 
       col = "#B31942",
       main = "Distribution of Overall Scores - United States",
       xlab = "Overall Score",
       ylab = "Frequency",
       xlim = range(c(us_data, uk_data)))
  
  dev.off()
  
  # --- VISUALIZATION 3: Density plot ---
  png("plots/density_plot.png", width = 800, height = 600, res = 100)
  par(mar = c(5, 5, 4, 2))
  
  # Calculate densities
  dens_uk <- density(uk_data)
  dens_us <- density(us_data)
  
  # Plot
  plot(dens_uk, col = "#0051BA", lwd = 2, 
       main = "Density Distribution of Overall Scores",
       xlab = "Overall Score",
       ylab = "Density",
       xlim = range(c(us_data, uk_data)),
       ylim = c(0, max(c(dens_uk$y, dens_us$y))))
  lines(dens_us, col = "#B31942", lwd = 2)
  
  # Add mean lines
  abline(v = mean(uk_data), col = "#0051BA", lty = 2, lwd = 2)
  abline(v = mean(us_data), col = "#B31942", lty = 2, lwd = 2)
  
  legend("topright", 
         legend = c("United Kingdom", "United States"),
         col = c("#0051BA", "#B31942"),
         lwd = 2,
         bty = "n")
  
  dev.off()
}

cat("\nPlots saved successfully in 'plots/' directory.\n")

# =============================================================================
# 5. NORMALITY TESTS
# =============================================================================

cat("\n=============================================================================\n")
cat("NORMALITY TESTS (Shapiro-Wilk Test)\n")
cat("=============================================================================\n")

# Shapiro-Wilk test for US
shapiro_us <- shapiro.test(us_data)
cat("US Universities:\n")
cat("  W statistic:", shapiro_us$statistic, "\n")
cat("  p-value:", shapiro_us$p.value, "\n")
cat("  Interpretation:", ifelse(shapiro_us$p.value > 0.05, 
                                 "Data appears normally distributed (p > 0.05)",
                                 "Data deviates from normal distribution (p < 0.05)"), "\n\n")

# Shapiro-Wilk test for UK
shapiro_uk <- shapiro.test(uk_data)
cat("UK Universities:\n")
cat("  W statistic:", shapiro_uk$statistic, "\n")
cat("  p-value:", shapiro_uk$p.value, "\n")
cat("  Interpretation:", ifelse(shapiro_uk$p.value > 0.05, 
                                 "Data appears normally distributed (p > 0.05)",
                                 "Data deviates from normal distribution (p < 0.05)"), "\n")

# =============================================================================
# 6. VARIANCE HOMOGENEITY TEST (Levene's Test)
# =============================================================================

cat("\n=============================================================================\n")
cat("VARIANCE HOMOGENEITY TEST (Levene's Test - Manual Calculation)\n")
cat("=============================================================================\n")

# Manual Levene's test calculation (using median)
# Calculate deviations from group medians
us_median <- median(us_data)
uk_median <- median(uk_data)

us_deviations <- abs(us_data - us_median)
uk_deviations <- abs(uk_data - uk_median)

# Perform ANOVA on the deviations
all_deviations <- c(us_deviations, uk_deviations)
group_labels <- factor(c(rep("US", length(us_deviations)), 
                         rep("UK", length(uk_deviations))))

levene_result <- summary(aov(all_deviations ~ group_labels))
levene_pvalue <- levene_result[[1]]$"Pr(>F)"[1]

cat("Levene's Test Results:\n")
print(levene_result)
cat("\nInterpretation:", ifelse(levene_pvalue > 0.05,
                                "Variances are equal (p > 0.05) - assumption met",
                                "Variances are unequal (p < 0.05) - use Welch's t-test"), "\n")

# =============================================================================
# 7. HYPOTHESIS TESTING: TWO-SAMPLE T-TEST
# =============================================================================

cat("\n=============================================================================\n")
cat("HYPOTHESIS TESTING: INDEPENDENT TWO-SAMPLE T-TEST\n")
cat("=============================================================================\n")

cat("\nNull Hypothesis (H0): There is no difference in mean Overall Score between\n")
cat("                      US and UK universities (μUS = μUK)\n")
cat("\nAlternative Hypothesis (H1): There is a significant difference in mean\n")
cat("                             Overall Score between US and UK universities (μUS ≠ μUK)\n")
cat("\nSignificance level: α = 0.05\n")

# Perform Welch's t-test (doesn't assume equal variances)
t_test_result <- t.test(us_data, uk_data, 
                        var.equal = FALSE, 
                        alternative = "two.sided")

cat("\n--- TEST RESULTS ---\n")
print(t_test_result)

# Effect size (Cohen's d)
mean_diff <- mean(us_data) - mean(uk_data)
pooled_sd <- sqrt(((length(us_data) - 1) * var(us_data) + 
                    (length(uk_data) - 1) * var(uk_data)) / 
                   (length(us_data) + length(uk_data) - 2))
cohens_d <- mean_diff / pooled_sd

cat("\n--- EFFECT SIZE ---\n")
cat("Mean difference (US - UK):", round(mean_diff, 3), "\n")
cat("Cohen's d:", round(cohens_d, 3), "\n")
cat("Interpretation:", 
    ifelse(abs(cohens_d) < 0.2, "Negligible effect",
           ifelse(abs(cohens_d) < 0.5, "Small effect",
                  ifelse(abs(cohens_d) < 0.8, "Medium effect", "Large effect"))), "\n")

# =============================================================================
# 8. CONCLUSION
# =============================================================================

cat("\n=============================================================================\n")
cat("STATISTICAL CONCLUSION\n")
cat("=============================================================================\n")

if (t_test_result$p.value < 0.05) {
  cat("Decision: REJECT the null hypothesis (p < 0.05)\n")
  cat("\nConclusion: There IS a statistically significant difference in mean\n")
  cat("Overall Score between US and UK universities.\n")
  cat("\nUS Mean:", round(mean(us_data), 2), "\n")
  cat("UK Mean:", round(mean(uk_data), 2), "\n")
  cat("Difference:", round(mean_diff, 2), "\n")
  cat("p-value:", format(t_test_result$p.value, scientific = TRUE), "\n")
} else {
  cat("Decision: FAIL TO REJECT the null hypothesis (p >= 0.05)\n")
  cat("\nConclusion: There is NO statistically significant difference in mean\n")
  cat("Overall Score between US and UK universities.\n")
  cat("\nUS Mean:", round(mean(us_data), 2), "\n")
  cat("UK Mean:", round(mean(uk_data), 2), "\n")
  cat("Difference:", round(mean_diff, 2), "\n")
  cat("p-value:", round(t_test_result$p.value, 4), "\n")
}

cat("\n=============================================================================\n")
cat("Analysis complete. Plots saved in 'plots/' directory.\n")
cat("=============================================================================\n")


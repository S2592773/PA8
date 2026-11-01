# PA8
# Andrew Zachman
# 2026.11.01
# Dr. Horrible's zombie cookies experiments ANOVA process

# Recipe vectors
Recipe_A = c(0,4,4,5,4,1,2,3,3,5)
Recipe_B = c(7,6,4,2,3,0,8,6,2,3)
Recipe_C = c(1,0,1,0,NA,1,1,NA,1,0)

# Recipe data frames
df <- data.frame(
  Recipe_A,
  Recipe_B,
  Recipe_C
)
df

# Stack data frames, remove NA values
newdf <- na.omit(stack(df))
newdf

# Summary of newdf
initial_summary <- summary(newdf)
initial_summary

# One-way analysis of ANOVA
anova_test <- aov(
  values ~ ind,
  data = newdf
)

# Summary of ANOVA test
anova_summary <- summary(anova_test)
anova_summary

# Hypothesis and variables definition:
# Response variable: values
# Predictor variable: ind
#
# H0: The mean zombie transformation significance values are equal across 
#   all three cookie recipes.
#
# Ha: At least one cookie recipe has a different mean zombie transformation
#   significance value.

# Extract the p-value and determine if the null hypothesis can be rejected
pval <- anova_summary[[1]]$'Pr(>F)'[1]
pval

if(pval < 0.05) {
  cat("Sufficient evidence to reject the null hypothesis. \n")
} else {
  cat("Ingredient X may have an effect on the results. \n")
}

### Kruskal-Wallis one-way ANOVA plus Mann-Whitney post hoc

library(dplyr)
library(ggpubr)
library(readxl) ## Substantially faster than XLConnect

df <- read_excel("A:\\Projects\\SalesMarketing\\Direct to Consumer\\Ambassadors Program\\Q2 Ambassadors Campaign Test (2019-05-20).xlsx"
  , sheet = 'CampaignData', col_names = TRUE)

df <- df %>%
  select(PolicyNumber, TestFlag, TestName, SignUp, Share)

df <- as.data.frame(df)

### Summary stats SignUp
group_by(df, TestName) %>%
  summarise(
    Count = n(),
    mean = mean(SignUp, na.rm = TRUE),
    sd = sd(SignUp, na.rm = TRUE),
    median = median(SignUp, na.rm = TRUE),
    IQR = IQR(SignUp, na.rm = TRUE)
  )
### Summary stats Share
group_by(df, TestName) %>%
  summarise(
    Count = n(),
    mean = mean(Share, na.rm = TRUE),
    sd = sd(Share, na.rm = TRUE),
    median = median(Share, na.rm = TRUE),
    IQR = IQR(Share, na.rm = TRUE)
  )
### Some box and whiskers - Not relevant here
#ggboxplot(df, x = "TestName", y = "SignUp",
 #         color = "TestName", 
  #        order = c("Control", "Email", "Direct Mail", "Both"),
   #       ylab = "SignUps", xlab = "Test Group")
### Geom jitter type plot - Hardly relevant here
ggline(df, x = "TestName", y = "SignUp", 
       add = c("mean_se", "jitter"), 
       order = c("Control", "Email", "Direct Mail", "Both"),
       ylab = "SignUps", xlab = "Test Group")
### Stupid list error, even though table(df$TestFlag) is finite
df$TestFlag <- as.factor(df$TestFlag)

# Kruskal Wallis Test - SignUps
kruskal.test(SignUp ~ TestFlag, data = df)
# Pairwise Wilcoxon/Mann Whitney, Benjamini Hochberg p value adjustment
pairwise.wilcox.test(df$SignUp, df$TestFlag,
                     p.adjust.method = "BH")

# Kruskal Wallis Test - Shares
kruskal.test(Share ~ TestFlag, data = df)
# Pairwise Wilcoxon/Mann Whitney, Benjamini Hochberg p value adjustment
pairwise.wilcox.test(df$Share, df$TestFlag,
                     p.adjust.method = "BH")

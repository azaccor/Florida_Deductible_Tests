### Kruskal-Wallis one-way ANOVA plus Mann-Whitney post hoc

library(dplyr)
library(ggpubr)
library(XLConnect)
library(rbind)

df <- readWorksheetFromFile("A:\\Projects\\SalesMarketing\\FL Meeting\\Florida Deductible Test (2018-12-14).xlsx", sheet = 'EnrollData', header = TRUE)
df <- df %>%
  select(ppid, Prem, PostalCode, partner, Path, EnrollDateOnly, TestFlag)

typeof(df)
df <- as.data.frame(df)

### Total Group

### Summary stats
group_by(df, TestFlag) %>%
  summarise(
    Count = n(),
    mean = mean(Prem, na.rm = TRUE),
    sd = sd(Prem, na.rm = TRUE),
    median = median(Prem, na.rm = TRUE),
    IQR = IQR(Prem, na.rm = TRUE)
  )
### Some box and whiskers
ggboxplot(df, x = "TestFlag", y = "Prem",
          color = "TestFlag", 
          order = c("Control", "Test1", "Test2"),
          ylab = "Premium", xlab = "Test Group")
### Geom jitter type plot
ggline(df, x = "TestFlag", y = "Prem", 
       add = c("mean_se", "jitter"), 
       order = c("Control", "Test1", "Test2"),
       ylab = "Premium", xlab = "Test Group")
### Stupid list error, even though table(df$TestFlag) is finite
df$TestFlag <- as.factor(df$TestFlag)
### Kruskal Wallis Test
kruskal.test(Prem ~ TestFlag, data = df)


### Separate Web and Phone Enrolls
dfweb <- filter(df, Path == "Web")
dfphone <- filter(df, Path == "Phone")


### Web ONLY

### Summary stats (Web)
group_by(dfweb, TestFlag) %>%
  summarise(
    Count = n(),
    mean = mean(Prem, na.rm = TRUE),
    sd = sd(Prem, na.rm = TRUE),
    median = median(Prem, na.rm = TRUE),
    IQR = IQR(Prem, na.rm = TRUE)
  )
### Some box and whiskers (Web)
ggboxplot(dfweb, x = "TestFlag", y = "Prem",
          color = "TestFlag", 
          order = c("Control", "Test1", "Test2"),
          ylab = "Premium", xlab = "Test Group")
### Geom jitter type plot (Web)
ggline(dfweb, x = "TestFlag", y = "Prem", 
       add = c("mean_se", "jitter"), 
       order = c("Control", "Test1", "Test2"),
       ylab = "Premium", xlab = "Test Group")
dfweb$TestFlag <- as.factor(dfweb$TestFlag)

### Kruskal Wallis Test (Web)
kruskal.test(Prem ~ TestFlag, data = dfweb)

### Pairwise Wilcoxon/Mann Whitney, Benjimani Hochberg p value adjustment
pairwise.wilcox.test(dfweb$Prem, dfweb$TestFlag,
                     p.adjust.method = "BH")









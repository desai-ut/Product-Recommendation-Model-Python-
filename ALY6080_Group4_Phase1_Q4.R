library(dplyr)
library(lubridate)
library(broom)
library(tidyr)

Awan <- read.csv(file.choose())
head(Awan, n=5)
str(Awan)
summary(Awan)
length(unique(Awan$sku_id))

is.na(Awan)
colSums(is.na(Awan))
which(colSums(is.na(Awan))>0)
names(which(colSums(is.na(Awan))>0))

hist(Awan$sku_id, col="pink")
hist(Awan$top_cat_id, col="blue")

# remove NAs
new_Awan <- Awan[complete.cases(Awan), ]        # Apply complete.cases function
head(new_Awan, 5)                              
summary(new_Awan)
length(unique(new_Awan$sku_id))

# group by sku_id
by_sku_id <- new_Awan_1 %>%
  group_by(sku_id) %>%
  summarize(total=n())
head(by_sku_id,5)
tail(by_sku_id,5)
max(by_sku_id$total, na.rm=TRUE)
by_sku_id[rev(order(by_sku_id$total)),]
by_sku_id[order(by_sku_id$total),]

lowSales_sku_id <- by_sku_id %>%
  filter(total > 1)
head(lowSales_sku_id)
lowSales_sku_id[order(lowSales_sku_id$total),]

by_date_id <- new_Awan_1 %>%
  group_by(sku_id,date)%>%
  summarise(total=n())
head(by_date_id, 5)
tail(by_date_id,5)
by_date_id %>%
  arrange(desc(total))

by_date_id %>%
  arrange(total)


hist(by_month_id$sku_id,breaks = sqrt(nrow(by_month_id)),col="light blue")
plot(total~sku_id, data=by_month_id, col=sku_id)
barplot(by_month_id$sku_id,by_month_id$total)

by_date_id$diff <- unlist(tapply(as.Date(test$date), INDEX = test$sku_id,
                                 FUN = function(x) c(0, `units<-`(diff(x), "days"))))
head(by_date_id)

by_date_id_group <- by_date_id %>%
  group_by(sku_id) %>%
  summarise(Avg_diff = mean(diff))
diff_days = as.numeric(diff, units = 'days')
head(by_date_id_group)

#checking the maximum value       
max(by_date_id_group$Avg_diff)
which.max(by_date_id_group$Avg_diff)

# Slowest moving item = highest average difference
by_date_id_group %>%
  arrange(desc(Avg_diff))

# fastest moving sku_ids
str(by_date_id_group)
Fastest_sku_id <- by_date_id_group %>%
  filter(Avg_diff >= 2)
Fastest_sku_id %>%
  arrange(Avg_diff)

fast_sku_id <- by_date_id_group %>%
  filter(Avg_diff >= 1)

fast_sku_id %>%
  arrange(Avg_diff)

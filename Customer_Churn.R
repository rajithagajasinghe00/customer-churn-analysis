library(dplyr)
library(ggplot2)

churn <- read_csv("E:/Projects/Telco_Customer_Churn.csv")
str(churn)

attach(churn)

churn <- churn[-1]
churn
churn$TotalCharges = as.numeric(churn$TotalCharges)
churn = na.omit(churn)

churn$Churn <- factor(churn$Churn)
churn$Contract <- factor(churn$Contract)
churn$InternetService <- factor(churn$InternetService)


# Churn rate
str(churn)
table(churn$Churn)
prop.table(table(churn$Churn))

# Mean monthly charges by churn

summarise(
  group_by(churn,Churn),
  Mean_Monthly_Charges = mean(MonthlyCharges)
)

# Mean tenure by churn
summarise(
  group_by(churn, Churn),
  Mean_Tenure = mean(tenure)
)


# Churn distribution

ggplot(churn, aes(x=Churn))+geom_bar(fill = "steelblue")+
  labs(title = "Customer Churn Distribution")

# Monthly charges vs churn

ggplot(churn, aes(x = Churn, y = MonthlyCharges)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Monthly Charges by Churn Status")

# Tenure vs churn

ggplot(churn, aes(x = tenure, fill = Churn)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Tenure Distribution by Churn")



grouped_data <- group_by(churn, Contract,Churn)
grouped_data

summary_data <- summarise(
  grouped_data,
  Count = n()
)

final_data <- mutate(
  summary_data,
  Churn_Rate = Count / sum(Count)
)
final_data

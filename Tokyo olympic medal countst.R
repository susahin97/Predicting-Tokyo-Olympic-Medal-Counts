############################################################################################################
# APM Assingment
############################################################################################################

#### Load the data #########################################################################################
df <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/tokyoolympics.csv"))

# Load Necessary packagges ---------------------------------------------------------------------------------
library(missForest)
library(stats)
library(MASS)
library(MuMIn)
library(ggcorrplot)
library(pscl)
library(ggplot2)
install.packages("logistf")
library(logistf)

## view data -----------------------------------------------------------------------------------------------
head(df)
str(df)
summary(df)

## check for missing data ----------------------------------------------------------------------------------
missing_counts <- colSums(is.na(df))
print(missing_counts)



## Convert non-numeric values to NA ------------------------------------------------------------------------
df$gdp00[df$gdp00 == "#N/A"] <- NA
df$gdp16[df$gdp16 == "#N/A"] <- NA
df$gdp20[df$gdp20 == "#N/A"] <- NA

# Convert columns to numeric
df$gdp00 <- as.numeric(df$gdp00)
df$gdp16 <- as.numeric(df$gdp16)
df$gdp20 <- as.numeric(df$gdp20)

missing_countries <- df$country[is.na(df$gdp16)]
countries <- c("Afghanistan")
gdp_table <- df[df$country %in% countries, c("country", "gdp00", "gdp04", "gdp08", "gdp12", "gdp16", "gdp20")]
gdp_table

###### Handle Missing Values ###############################################################################

# Impute missing GDP values using missForest
imputed_data <- missForest(df[, 3:8])
df[, 3:8] <- round(imputed_data$ximp)

# Impute missing population values using missForest
imputed_data_pop <- missForest(df[, 9:15])
df[, 9:15] <- round(imputed_data_pop$ximp)
 
str(df)

# Transform the data #######################################################################################

# Specify the years to iterate over
years <- c("00", "04", "08", "12", "16", "20")

# Create an empty data frame to store the transformed values
df1 <- data.frame()

# Iterate over each year and extract the corresponding values
for (year in years) {
  # Create column names for the specific year
  gdp_col <- paste0("gdp", year)
  pop_col <- paste0("pop", year)
  athletes_col <- paste0("athletes", year)
  gold_col <- paste0("gold", year)
  tot_col <- paste0("tot", year)
  tot_gold_col <- ifelse(year == "20", "totgold", paste0("totgold", year))
  avmedals_col <- paste0("avmedals", year)
  
  # Extract the values for the specific year
  year_values <- data.frame(
    year = rep(as.numeric(paste0("20", year)), nrow(df)),
    country = df$country,
    gdp = df[, gdp_col],
    pop = df[, pop_col],
    athletes = df[, athletes_col],
    gold = df[, gold_col],
    tot = df[, tot_col],
    tot_gold = ifelse(year == "20", NA, df[, tot_gold_col]),
    avmedals = df[, avmedals_col],
    muslim = df$muslim,
    host = df$host,
    comm = df$comm,
    soviet = df$soviet,
    oneparty = df$oneparty,
    altitude = df$altitude
  )
  
  # Append the year values to the transformed data frame
  df1 <- rbind(df1, year_values)
}



df2 <- df1 %>%
  group_by(year, country) %>%
  mutate(athletes_avmedals = avmedals / athletes)

df2 <- df2 %>%
  group_by(year, country) %>%
  mutate(athletes_avmedals = round(avmedals * athletes))

df2$GDPpercapita <- df2$gdp * 10^6 / df2$pop

##### Explanatory Data Analysis #####################################################
summary(df2$tot)
variance <- summary(df2[df2$year >= 2000 & df2$year <= 2012, "tot"], na.rm = TRUE)
sqrt(variance)

# Create a scatter plot for each predictor variable
for (var in names(train_data)) {
  if (var != "tot") {
    p <- ggplot(train_data, aes(x = !!as.name(var), y = tot)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(x = var, y = "tot") +
      ggtitle(paste("Scatter plot of", var, "vs. tot"))
    
    print(p)
  }
}

train_data <- df2[, c("gdp", "pop", "athletes", "tot", "avmedals", "muslim", "host", "comm", "soviet", "oneparty", "altitude", "prev_tot")]

ggpairs(train_data,
        upper = list(continuous = wrap("points", alpha = 0.4, color = "#d73027")),
        lower = list(continuous = wrap("cor", size = 3, digits=2)),
        axisLabels = "none")





# Calculate the correlation matrix

cor_matrix <- round(cor(df2[, c("gold", "tot", "gdp","athletes_avmedals", "pop", "athletes",
                          "muslim", "host", "comm", "soviet",
                          "oneparty","prev_tot", "GDPpercapita" ,"altitude")]),2)


corr <- round(cor(df2[, c("tot","prev_tot", "gdp", "pop", "athletes",
                           "muslim", "host", "comm", "soviet","avmedals",
                          "oneparty", "altitude")]), 2)

ggcorrplot(corr,
           type = "lower",
           lab = TRUE,
           lab_size = 5,
           colors = c("red", "white", "darkgreen"),
           title = "Correlogram of Dataset",
           ggtheme = theme_bw)

# Filter the dataset for the year 2016
df_2016 <- df2[df2$year == 2012, c("tot", "prev_tot", "gdp", "pop", "athletes",
                                   "muslim", "host", "comm", "soviet", "avmedals",
                                   "oneparty", "altitude")]

# Compute the correlation matrix
corr_2016 <- round(cor(df_2016), 2)

# Create the correlation plot
ggcorrplot(corr_2016,
           type = "lower",
           lab = TRUE,
           lab_size = 5,
           colors = c("red", "white", "darkgreen"),
           title = "Correlogram of 2012 Data",
           ggtheme = theme_bw)




# Compute the correlation matrix

# Create the line plot
ggplot(df_filtered, aes(x = year, y = gold, color = country, group = country)) +
  geom_line() +
  geom_text(data = subset(df_filtered, year == 2012), aes(label = country), vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "Year", y = "Number of Gold Medals",title = "Gold Medals by Country Over Time") +) +
  scale_x_continuous(breaks = seq(2000, 2020, 4), labels = as.character(seq(2000, 2020, 4))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5))


## Total medal won

ggplot(df_filtered, aes(x = year, y = tot, color = country, group = country)) +
  geom_line() +
  geom_text(data = subset(df_filtered, year == 2012), aes(label = country), vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "Year", y = "Number of Total Medals", title = "Total Medals by Country Over Time") +
  scale_x_continuous(breaks = seq(2000, 2020, 4), labels = as.character(seq(2000, 2020, 4))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5))

library(ggplot2)

df_filtered <- subset(df2, year >=2000)

ggplot(df_filtered, aes(x =gdp, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "GDP", y = "Total Medals", title = "GDP vs. Total Medals (Year 2012)") +
  theme_bw() +
  theme(legend.position = "none")

library(ggplot2)

df_filtered <- subset(df2, year == 2012)
df_filered2 <- subset(df2, year== 2016)

gdp2012 <- ggplot(df_filtered, aes(x = athletes, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Athletes", y = "Total Medals", title = "Number of Athletes vs. Total Medals 2012") +
  theme_bw() +
  theme(legend.position = "none")

gdp2016 <- ggplot(df_filered2, aes(x = athletes, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Athletes", y = "Total Medals", title = "Number of Athletes vs. Total Medals 2016") +
  theme_bw() +
  theme(legend.position = "none")

combined_plot <- grid.arrange(gdp2012, gdp2016, ncol = 2)

# Display the combined plot
print(combined_plot)

### Boxplots
library(ggplot2)
library(gridExtra)

# Filter the data for year 2012
df_2012 <- subset(df2, year == 2012)
# Create a new column with the logarithm of total medals for 2012
df_2012$log_tot <- log(df_2012$tot)

# Filter the data for year 2016
df_2016 <- subset(df2, year == 2016)
# Create a new column with the logarithm of total medals for 2016
df_2016$log_tot <- log(df_2016$tot)

# Plot the boxplots separately for 2012 and 2016
plot_2012 <- ggplot(df_2012, aes(x = host, y = log_tot, fill = factor(host))) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(x = "Host", y = "Logarithm of Total Medals", title = "Boxplot of Host Countries (2012)") +
  theme_bw()

plot_2016 <- ggplot(df_2016, aes(x = host, y = log_tot, fill = factor(host))) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(x = "Host", y = "Logarithm of Total Medals", title = "Boxplot of Host Countries (2016)") +
  theme_bw()

# Arrange the plots side by side
grid.arrange(plot_2012, plot_2016, ncol = 2)






# Filter the data for the relevant countries and years
host_countries <- c("Australia", "Greece", "China", "United Kingdom" , "Brazil")
host_years <- c(2000, 2004, 2008, 2012, 2016)
df_host <- df1[df1$country %in% host_countries & df1$year %in% host_years, ]

# Create the line plot
ggplot(df_host, aes(x = year, y = tot, color = country)) +
  geom_line() +
  labs(x = "Year", y = "Total Medals", title = "Number of Medals per Country for Host Years") +
  scale_color_manual(values = c("Australia" = "red", "Greece" = "blue", "China" = "green", "United Kingdom" = "purple", "Brazil"="orange")) +
  scale_x_continuous(breaks = seq(2000, 2020, 4), labels = as.character(seq(2000, 2020, 4))) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5))

ggplot(df_host, aes(x = gdp, y = tot, color = country)) +
  geom_line() +
  labs(x = "GDP", y = "Total Medals", title = "Number of Medals vs GDP") +
  scale_color_manual(values = c("Australia" = "red", "Greece" = "blue", "China" = "green", "United Kingdom" = "purple", "Brazil"="orange")) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5))

# Histogram of GDP
ggplot(df2, aes(x = gdp)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(x = "GDP", y = "Frequency") +
  ggtitle("Distribution of GDP")

ggplot(df2, aes(x = gdp, y = tot, color = country)) +
  geom_point() +
  geom_text(aes(label = country), size = 3, hjust = 0, vjust = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Scatter Plot: Total Medal Count vs. GDP", x = "GDP", y = "Total Medal Count") +
  guides(color = FALSE)

ggplot(df2, aes(x = gdp, y = tot, color = country)) +
  geom_point() +
  geom_text(aes(label = country), size = 3, hjust = 0, vjust = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Scatter Plot: Total Medal Count vs. GDP", x = "GDP", y = "Total Medal Count") +
  guides(color = FALSE) +
  xlim(0, 200000) +  # Adjust the limits according to your data
  ylim(0, 50)       # Adjust the limits according to your data




plot(df1$gold, df1$tot, xlab = "Gold Medals", ylab = "Total Medals", main = "Gold Medals vs Total Medals")

hist(df1$gdp, xlab = "GDP", main = "Distribution of GDP")
hist(df1$athletes, xlab = "Number of Athletes", main = "Distribution of Number of Athletes")

boxplot(df1$gold ~ df1$host, xlab = "Host", ylab = "Gold Medals", main = "Gold Medals by Hosting Status")

host_countries <- c("Australia", "Greece", "China", "UK")
avg_gold <- tapply(df1$gold, df1$country, mean)
host_avg_gold <- avg_gold[host_countries]
barplot(host_avg_gold, names.arg = host_countries, xlab = "Hosting Country", ylab = "Average Gold Medals",
        main = "Average Gold Medals for Host Countries")


df_2012 <- df1[df1$year == 2012, ]
tot <- ggplot(df_2012, aes(x = tot)) +
  geom_histogram(bins = 20, fill = "grey", color = "black") +
  labs(x = "Total Medals", y = "Frequency") +
  ggtitle("Distribution of Total Medals in 2012")

gold <- ggplot(df_2012, aes(x = gold)) +
  geom_histogram(bins = 20, fill = "grey", color = "black") +
  labs(x = "Total Medals", y = "Frequency") +
  ggtitle("Distribution of Gold Medals in 2012")

grid.arrange(tot, gold, ncol = 2)


library(ggplot2)

dd <- ggplot(df2, aes(x = prev_tot, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "Previous Year's Total Medals", y = "Total Medals", title = "Total Medals vs. Previous Year's Total Medals") +
  theme_bw() +
  theme(legend.position = "none")

library(ggplot2)
library(cowplot)

# Create plots for 2012 and 2016
 plot_2012 <- ggplot(df_2012, aes(x = prev_tot, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "Previous Year's Total Medals", y = "Total Medals", title = "Total Medals vs. Previous Year's Total Medals 2012") +
  theme_bw() +
  theme(legend.position = "none")

plot_2016 <- ggplot(df_2016, aes(x = prev_tot, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  labs(x = "Previous Year's Total Medals", y = "Total Medals", title = "Total Medals vs. Previous Year's Total Medals (2016)") +
  theme_bw() +
  theme(legend.position = "none")

grid.arrange(plot_2012, plot_2016, ncol=2)


################# MAP #######################################################################################
install.packages("rnaturalearth")
library(rnaturalearth)

# Aggregate medal points by country
country <- aggregate(df1$tot, by = list(df1$country), sum)
colnames(country) <- c("Country", "Medals")

# Merge with GDP data
country2 <- merge(country, df1, by.x = "Country", by.y = "country", all.x = TRUE)

# Define colors
palette <- colorRampPalette(brewer.pal(n = 7, name = "Oranges"))(7)
palette <- c("white", palette)

# Create map
world <- ne_countries(scale = "medium", returnclass = "sf")
country3 <- merge(world, country2, by.x = "name", by.y = "Country", all.x = TRUE)

# Map of Medal Points
worldmedalsmap <- ggplot() +
  geom_sf(data = country3, aes(fill = Medals)) +
  labs(fill = "Medal Points") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Map of GDP per Capita
worldgdpmap <- ggplot() +
  geom_sf(data = country3, aes(fill = gdp)) +
  labs(fill = "GDP per Capita") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

worldmedalsmap
worldgdpmap

# Select the columns for boxplot
box <- as.data.frame(scale(df1[, c("gold", "tot", "gdp", "pop", "athletes", "avmedals",
                                   "muslim", "host", "comm", "soviet", "oneparty", "altitude")]))

# Create boxplots
ggplot(stack(box), aes(x = ind, y = values)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) +
  labs(title = "Boxplots of Scaled Variables", x = "Variable", y = "Scaled Value")

ggplot(df2[df2$year != 2020, ], aes(x = year, y = tot, color = country, label = country)) +
  geom_point() +
  geom_text(vjust = -0.7, hjust = 0.5, size = 3) +
  geom_line(aes(group = country), alpha = 0.5) +
  labs(x = "Year", y = "Total Medals", title = "Total Medals by Year") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016))




##### MODEL DEVELOPMENT ###########################################################################################

## Split the data for training ####################################################################################
# Set the seed for reproducibility
set.seed(123)

# Filter the data for the desired years
train_data <- df2[df2$year >= 2000 & df2$year <= 2012, ]
validation_data <- df2[df2$year ==2016,]
test_data <- df2[df2$year == 2020, ]

# Create the training, validation, and test sets
train_set <- train_data[, !(colnames(train_data) %in% c("year", "country", "tot_gold"))]
validation_set <-validation_data[, !(colnames(train_data) %in% c("year", "country", "tot_gold"))] 
test_set <- test_data[, !(colnames(test_data) %in% c("year", "country", "tot_gold"))]


#### Poisson Model ############################################################################################
library(GGally)

# Select the variables for plotting, excluding 'country', 'year', 'tot_gold', and 'avegold'
variables <- setdiff(names(df1), c("country", "year", "tot_gold", "avegold"))

# Subset the dataframe with the selected variables
df_subset <- df1[, variables]

# Create the pairwise scatter plots with custom style
ggpairs(df_subset,
        upper = list(continuous = wrap("points", alpha = 0.4, color = "#d73027")),
        lower = "blank",
        axisLabels = "none")


# Fit a Poisson GLM for total medals
model <- glm(tot ~ log(gdp) + log(pop) + athletes + gold + muslim + host +
               comm + oneparty, data = train_data, family = poisson)

model <- glm(tot ~ soviet +comm + muslim + oneparty + host + soviet + gold +
               athletes + log(gdp) + pop, data=train_data, family=poisson)

model <- glm(tot ~ log(gdp) + athletes + muslim + pop + gold +prev_tot +
                 host +comm, data = train_data, family = poisson)


# Perform stepwise model selection
step_model <- stepAIC(model, direction = "both")

# View the final selected model
summary(step_model)
summary(model)


# Test data
test_predictions <- predict(model, newdata = test_set, type = "response")
# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - test_predictions)^2))

# Summarize the findings
cat("Test RMSE:", test_rmse, "\n")

# Make predictions for the training set
predictions <- predict(model, newdata = train_set, type = "response")

# Plot fitted vs actual values
plot(predictions, train_set$tot, xlab = "Fitted Values", ylab = "Actual Values", 
     main = "Fitted vs Actuals (Poisson Model)")
abline(0, 1, col = "red")


# Create the ggplot plot
ggplot(model_poisson, aes(x = log(fitted(model_poisson)), y = log((tot - fitted(model_poisson))^2))) +
  geom_point(col = "#f46d43") +
  geom_abline(slope = 1, intercept = 0, col = "#a6d96a", size = 1) +
  ylab(expression((y-hat(mu))^2)) +
  xlab(expression(hat(mu)))

## Quasi-Poisson Model######

# dispersion parameter 
X2 <- sum(resid(step_model, type = "pearson")^2) 
dp <- X2 / step_model$df.res
dp # dispersion is present

summary(step_model, dispersion=dp)

drop1(step_model, test = "F")





# Test data
test_predictions <- predict(model, newdata = test_set, type = "response")
# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - test_predictions)^2))

# Summarize the findings
cat("Test RMSE:", test_rmse, "\n")


##### 
model2 <- glm(formula = tot ~ log(gdp) + pop + athletes + gold + muslim + 
                host + comm + oneparty, family = poisson, data = train_set)

summary(model2)

ggplot(model2, aes(x = log(fitted(model2)), y = log((tot - fitted(model2))^2))) +
  geom_point(col = "#f46d43") +
  geom_abline(slope = 1, intercept = 0, col = "#a6d96a", size = 1) +
  ylab(expression((y-hat(mu))^2)) +
  xlab(expression(hat(mu)))

X2 <- sum(resid(model2, type = "pearson")^2) 
dp <- X2 / model2$df.res
dp # dispersion is present

summary(model2, dispersion=dp)


# Validation
valid_predictions <- predict(model2, newdata = validation_set, type = "response")

# Test data
test_predictions <- predict(model2, newdata = test_set, type = "response")

# Validation data
valid_actual <- validation_set$tot
valid_rmse <- sqrt(mean((valid_actual - valid_predictions)^2))

# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - test_predictions)^2))

# Summarize the findings
cat("Validation RMSE:", valid_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

## Negative Binomial ##################


summary(model_nb)
model_nb <- glm.nb(tot ~ log(gdp) + prev_tot + athletes +  comm +, data = train_set)

model3 <- glm.nb(tot ~ log(gdp) + pop + prev_tot + athletes + muslim +comm, data = train_set)
model3 <- 

summary(model3)


model_nb <- glm.nb(tot ~ log(gdp) + log(pop) +athletes + prev_tot+ muslim +comm , data = train_data)
summary(model3.1)








# Plot fitted vs actual values
plot(predictions, train_data$tot, xlab = "Fitted Values", ylab = "Actual Values", main = "Fitted vs Actuals (Negative Binomial Model)")
abline(0, 1, col = "red")

# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - predictions)^2))

# Summarize the findings

cat("Test RMSE:", test_rmse, "\n")


# Plot fitted vs actual values
plot(predictions, train_data$tot, xlab = "Fitted Values", ylab = "Actual Values",
     main = "Fitted vs Actuals (Negative Binomial Model)")
abline(0, 1, col = "red")

# Calculate AIC
aic <- AIC(model3.1)

# Print the AIC value
print(paste("AIC:", aic))



X2 <- sum(resid(model3, type = "pearson")^2) 
dp <- X2 / model3$df.res
dp 

### Compare poisson and negative binomial
# poisson
c(model2$deviance, model2$aic)
# NB
c(model3.1$deviance, model3.1$aic)
c(model3$deviance, model3$aic)

# Validation
valid_predictions <- predict(model3, newdata = validation_set, type = "response")

# Test data
test_predictions <- predict(model3, newdata = test_set, type = "response")

# Validation data
valid_actual <- validation_set$tot
valid_rmse <- sqrt(mean((valid_actual - valid_predictions)^2))

# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - test_predictions)^2))

# Summarize the findings
cat("Validation RMSE:", valid_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")



## zero inflated model #################
ggplot(data = df1, aes(x = tot)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "tot", y = "Counts") +
  ggtitle("Histogram of tot versus Counts") +
  theme_minimal()



zip_mod <- zeroinfl(tot ~ log(gdp) +soviet, data = train_set, dist = "poisson")
summary(zip_mod)

# Test data
test_predictions <- predict(zip_mod, newdata = test_set, type = "response")
# Test data
test_actual <- test_set$tot
test_rmse <- sqrt(mean((test_actual - test_predictions)^2))

# Summarize the findings
cat("Test RMSE:", test_rmse, "\n")





## Normal distibution #####################################################################################

# Fit linear regression model
model <- lm(tot ~ ., data = train_data)

step_model <- lm(tot~ gdp + athletes + prev_tot + soviet, data= train_data)
summary(step_model)
AIC(step)
# Predict with test data
test_predictions <- predict(step_model, newdata = test_set)

# Calculate RMSE for test data
test_rmse <- sqrt(mean((test_set$tot - test_predictions)^2))

# Print RMSE values

cat("Test RMSE:", test_rmse, "\n")

### Normal Linear Regression ###################################################
mod.lm <- lm(tot ~ log(gdp) + pop + muslim + host + comm + oneparty+ athletes + gold + soviet, data = train_set)
summary(mod.lm)



step_lm <- stepAIC(mod.lm, direction = "both")

summary(step_lm)

# Predict with validation data
valid_predictions <- predict(step_lm, newdata = validation_set)

# Calculate RMSE for validation data
valid_rmse <- sqrt(mean((validation_set$tot - valid_predictions)^2))

# Predict with test data
test_predictions <- predict(step_lm, newdata = test_set)

# Calculate RMSE for test data
test_rmse <- sqrt(mean((test_set$tot - test_predictions)^2))

# Print RMSE values
cat("Validation RMSE:", valid_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

str(df1)

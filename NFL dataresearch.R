#SCIENTIFIC PROGRAMMING
#Melle Klaver

#Using the packages

library(dplyr)
library(tidyr)
library(ggplot2)

#Setting up the data and importing the files
setwd("C:/Users/MK122/OneDrive/Bureaublad/School/Systems Biology/Scientific programming/Final skills session/Git/Scientific-programming-project-Melle")
Quarterback_gamelogs = read.csv("Game_Logs_Quarterback.csv")
Basic_stats = read.csv("Basic_stats.csv")
Runningback_gamelogs = read.csv("Game_Logs_Runningback.csv")
WideReceiver_gamelogs = read.csv("Game_Logs_Wide_Receiver_and_Tight_End.csv")
Kicker_gamelogs = read.csv("Game_Logs_Kickers.csv")

#Setting all numbers in the data to numeric
Numeric_Columns_QB_Gamelogs = c(4,6,12:28) 
Quarterback_gamelogs[Numeric_Columns_QB_Gamelogs] = lapply(Quarterback_gamelogs[Numeric_Columns_QB_Gamelogs], as.numeric)
Numeric_Columns_RB_Gamelogs = c(4,6,12:25)
Runningback_gamelogs[Numeric_Columns_RB_Gamelogs] = lapply(Runningback_gamelogs[Numeric_Columns_RB_Gamelogs], as.numeric)
Numeric_Columns_WR_Gamelogs = c(4,6,12:25)
WideReceiver_gamelogs[Numeric_Columns_RB_Gamelogs] = lapply(WideReceiver_gamelogs[Numeric_Columns_WR_Gamelogs], as.numeric)
Numeric_Columns_K_Gamelogs = c(4,6,12:27)
Kicker_gamelogs[Numeric_Columns_RB_Gamelogs] = lapply(Kicker_gamelogs[Numeric_Columns_K_Gamelogs], as.numeric)



#Adding the age of the Quarterback during each game
Basic_stats$Birthyear = substring(Basic_stats$Birthday, regexpr("/", Basic_stats$Birthday) + 1)
Basic_stats$Birthyear = as.numeric(substring(Basic_stats$Birthyear, regexpr("/", Basic_stats$Birthyear) + 1))
Quarterback_gamelogs = left_join(Quarterback_gamelogs, Basic_stats %>% select(Player.Id, Birthyear), by = "Player.Id")
Quarterback_gamelogs$Age =  Quarterback_gamelogs$Year - Quarterback_gamelogs$Birthyear
Runningback_gamelogs = left_join(Runningback_gamelogs, Basic_stats %>% select(Player.Id, Birthyear), by = "Player.Id")
Runningback_gamelogs$Age =  Runningback_gamelogs$Year - Runningback_gamelogs$Birthyear
WideReceiver_gamelogs = left_join(WideReceiver_gamelogs, Basic_stats %>% select(Player.Id, Birthyear), by = "Player.Id")
WideReceiver_gamelogs$Age =  WideReceiver_gamelogs$Year - WideReceiver_gamelogs$Birthyear
Kicker_gamelogs = left_join(Kicker_gamelogs, Basic_stats %>% select(Player.Id, Birthyear), by = "Player.Id")
Kicker_gamelogs$Age =  Kicker_gamelogs$Year - Kicker_gamelogs$Birthyear

#Removes games where the QB didnt actually play and calculate Offensive points
Quarterback_gamelogs = filter(Quarterback_gamelogs, Passes.Attempted>0)
Quarterback_gamelogs = filter(Quarterback_gamelogs, Season=="Regular Season")
Quarterback_gamelogs$Offensive.Points <- as.numeric(sub(" .*", "", Quarterback_gamelogs$Score))
Quarterback_gamelogs <- Quarterback_gamelogs %>%
  mutate_if(~!is.list(.), ~ifelse(is.na(.), 0, .))

#Removes games where the RB didnt actually play
Runningback_gamelogs = filter(Runningback_gamelogs, Rushing.Attempts>0)
Runningback_gamelogs = filter(Runningback_gamelogs, Season=="Regular Season")

#Removes games where the WR didnt actually play
WideReceiver_gamelogs = filter(WideReceiver_gamelogs, Receptions>0)
WideReceiver_gamelogs = filter(WideReceiver_gamelogs, Season=="Regular Season")

#Removes games where the K didnt actually play
Kicker_gamelogs = filter(Kicker_gamelogs, FGs.Attempted>0 | Extra.Points.Attempted>0)
Kicker_gamelogs = filter(Kicker_gamelogs, Season=="Regular Season")


# Creates gamelog of every offensive play
Sum_Columns_QB = c(14,15,17,19:24,26:28)
Column_names_QB = colnames(Quarterback_gamelogs[Sum_Columns_QB])

Sum_Columns_RB = c(14,15,17:20,22:25)
Column_names_RB = colnames(Runningback_gamelogs[Sum_Columns_RB])

Sum_Columns_WR = c(14,15,17:20,22:25)
Column_names_WR = colnames(WideReceiver_gamelogs[Sum_Columns_WR])

Sum_Columns_K = c(14:17,19,20)
Column_names_K = colnames(Kicker_gamelogs[Sum_Columns_K])

Offensive_gamelog_QB = Quarterback_gamelogs %>%
  group_by(Year, Week, Opponent, Outcome, Home.or.Away, Score, Offensive.Points) %>%
  summarize(
    Player.ID = paste(Player.Id, collapse = " + "),
    across(Column_names_QB,sum),
  ) %>%
  ungroup()
Column_names_QB = paste("QB_", names(Offensive_gamelog_QB)[8:20], sep = "")
names(Offensive_gamelog_QB)[8:20] = Column_names_QB


Offensive_gamelog_RB = Runningback_gamelogs %>%
  group_by(Year, Week, Opponent) %>%
  summarize(
    Player.ID = paste(Player.Id, collapse = " + "),
    across(Column_names_RB,sum),
  ) %>%
  ungroup()
Column_names_RB = paste("RB_", names(Offensive_gamelog_RB)[4:14], sep = "")
names(Offensive_gamelog_RB)[4:14] = Column_names_RB


Offensive_gamelog_WR = WideReceiver_gamelogs %>%
  group_by(Year, Week, Opponent) %>%
  summarize(
    Player.ID = paste(Player.Id, collapse = " + "),
    across(Column_names_WR,sum),
  ) %>%
  ungroup()
Column_names_WR = paste("WR_", names(Offensive_gamelog_WR)[4:14], sep = "")
names(Offensive_gamelog_WR)[4:14] = Column_names_WR

Offensive_gamelog_K = Kicker_gamelogs %>%
  group_by(Year, Week, Opponent) %>%
  summarize(
    Player.ID = paste(Player.Id, collapse = " + "),
    across(Column_names_K,sum),
  ) %>%
  ungroup()
Column_names_K = paste("K_", names(Offensive_gamelog_K)[4:10], sep = "")
names(Offensive_gamelog_K)[4:10] = Column_names_K


#Combining the offensive_gamelogs
Offensive_gamelog_Combined = left_join(Offensive_gamelog_QB, Offensive_gamelog_RB %>% select(RB_Player.ID,Column_names_RB,Year,Week,Opponent), by = c("Week", "Year", "Opponent"))
Offensive_gamelog_Combined = left_join(Offensive_gamelog_Combined, Offensive_gamelog_WR %>% select(WR_Player.ID,Column_names_WR,Year,Week,Opponent), by = c("Week", "Year", "Opponent"))
Offensive_gamelog_Combined = left_join(Offensive_gamelog_Combined, Offensive_gamelog_K %>% select(K_Player.ID,Column_names_K,Year,Week,Opponent), by = c("Week", "Year", "Opponent"))
Offensive_gamelog_Combined <- Offensive_gamelog_Combined %>%
  mutate_if(~!is.list(.), ~ifelse(is.na(.), 0, .))

#EVERYTHING ABOVE IS SETUP AND DATA MANIPULATION

#Create original passer rating function
calculate_passer_rating <- function(COMP, ATT, YDS, TD, INT) {
  
  # Calculate the four components of the passer rating
  a = ((COMP / ATT) - 0.3) * 5
  b = ((YDS / ATT) - 3) * 0.25
  c = (TD / ATT) * 20
  d = 2.375 - ((INT / ATT) * 25)
  
  # Ensure that a,b,c&d are below 2.375
  a = pmin(a, 2.375)
  a = pmax(a, 0)
  b = pmin(b, 2.375)
  b = pmax(b,0)
  c = pmin(c, 2.375)
  c = pmax(c,0)
  d = pmax(d, 0)
  
  # Calculate the passer rating
  passer_rating = ((a+b+c+d)/6) * 100

  
  return(passer_rating)
}

Quarterback_gamelogs$Passer.Rating = calculate_passer_rating(Quarterback_gamelogs$Passes.Completed,Quarterback_gamelogs$Passes.Attempted, Quarterback_gamelogs$Passing.Yards, Quarterback_gamelogs$TD.Passes, Quarterback_gamelogs$Ints)
Offensive_gamelog_Combined$QB_Passer.Rating = calculate_passer_rating(Offensive_gamelog_Combined$QB_Passes.Completed,Offensive_gamelog_Combined$QB_Passes.Attempted, Offensive_gamelog_Combined$QB_Passing.Yards, Offensive_gamelog_Combined$QB_TD.Passes, Offensive_gamelog_Combined$QB_Ints)




#Create new passer rating function
calculate_passer_rating2.0 <- function(COMP, ATT, YDS, TD, INT, SACK, S.YDS, RUSH, R.YDS, FUMB, FUMB.L) {
  
  # Calculate the four components of the passer rating
  a = ((COMP / ATT) - 0.3) * 5
  b = ((YDS / ATT) - 3) * 0.25
  c = (TD / ATT) * 20
  d = 2.375 - ((INT / ATT) * 25)
  e = ifelse(SACK == 0, 1, 1.000 - ((SACK / ATT) * ((S.YDS / SACK) + 3)))
  f = ifelse(RUSH == 0, 0, ((R.YDS / RUSH) - 3) * 0.25)
  g = 2.375 - ((FUMB/ATT)*3 + (FUMB.L/ATT)*22)
  
  # Ensure that a,b,c&d are below 2.375
  a = pmin(a, 2.375)
  a = pmax(a, 0)
  b = pmin(b, 2.375)
  b = pmax(b,0)
  c = pmin(c, 2.375)
  c = pmax(c,0)
  d = pmax(d, 0)
  e = pmax(e, 0)
  f = pmin(f,1)
  f = pmax(f,0)
  g = pmax(g,0)
  
  # Calculate the passer rating
  passer_rating2.0 = ((a + b + c + d + e + f + g) / 8.5) * 100
  
  return(passer_rating2.0)
}

Quarterback_gamelogs$Passer.Rating2.0 = calculate_passer_rating2.0(Quarterback_gamelogs$Passes.Completed,Quarterback_gamelogs$Passes.Attempted, Quarterback_gamelogs$Passing.Yards, Quarterback_gamelogs$TD.Passes, Quarterback_gamelogs$Ints, Quarterback_gamelogs$Sacks, Quarterback_gamelogs$Sacked.Yards.Lost, Quarterback_gamelogs$Rushing.Attempts, Quarterback_gamelogs$Rushing.Yards, Quarterback_gamelogs$Fumbles, Quarterback_gamelogs$Fumbles.Lost)
Offensive_gamelog_Combined$QB_Passer.Rating2.0 = calculate_passer_rating2.0(Offensive_gamelog_Combined$QB_Passes.Completed,Offensive_gamelog_Combined$QB_Passes.Attempted, Offensive_gamelog_Combined$QB_Passing.Yards, Offensive_gamelog_Combined$QB_TD.Passes, Offensive_gamelog_Combined$QB_Ints, Offensive_gamelog_Combined$QB_Sacks, Offensive_gamelog_Combined$QB_Sacked.Yards.Lost, Offensive_gamelog_Combined$QB_Rushing.Attempts, Offensive_gamelog_Combined$QB_Rushing.Yards, Offensive_gamelog_Combined$QB_Fumbles, Offensive_gamelog_Combined$QB_Fumbles.Lost)

#Explorative statistics

Stats_of_Interest = c(9:20,22:31,33:42,44:50)
correlations <- cor(Offensive_gamelog_Combined$Offensive.Points, Offensive_gamelog_Combined[Stats_of_Interest])
print(correlations)


#Regression results offense
regression_results_Offense = data.frame()
# Create a loop for all statistics in the offense that might have an effect on offensive production
Stats_of_Interest = c(9:20,22:31,33:42,44:51)
for (i in Stats_of_Interest) {  
  DV = "Offensive.Points"    
  IV = names(Offensive_gamelog_Combined)[i] 
  
  # Fit the regression model
  model <- lm(paste(DV, "~", IV), data = Offensive_gamelog_Combined)
  
  # Extract relevant information
  intercept <- coef(model)[1]
  coefficient <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Add results to the dataframe
  result_row <- data.frame(IV, intercept, coefficient, p_value, adj_r_squared)
  regression_results_Offense <- rbind(regression_results_Offense, result_row)
}

# Round, order and print the regression results
regression_results_Offense$intercept = round(regression_results_Offense$intercept, digits = 3)
regression_results_Offense$coefficient = round(regression_results_Offense$coefficient, digits = 3)
regression_results_Offense$adj_r_squared = round(regression_results_Offense$adj_r_squared, digits = 3)
regression_results_Offense = regression_results_Offense[order(-regression_results_Offense$adj_r_squared), ]
print(regression_results_Offense)

# Create a bar chart using ggplot2
ggplot(head(regression_results_Offense, 10), aes(x = reorder(IV, -adj_r_squared), y = adj_r_squared)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Statistics", y = "adj_r_squared", title = "What stat has the biggest effect on offensive output") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

















#The following chooses the stats that we want to compare across ages 
Age_stats_Columns_QB = c(15,17,20,21,23,31,32)
Age_stats_Columns_QB = colnames(Quarterback_gamelogs[Age_stats_Columns_QB])

#This code creates a dataframe in which the amount of average stats of each quarterback of a certain age is represented. Age groups with less than 100 games are excluded as that is a to small sample size
Age_stats_QB = Quarterback_gamelogs %>%
  group_by(Age) %>%
  summarize(
    Games = n(),
    across(Age_stats_Columns_QB,mean),
  ) %>%
  ungroup() %>%
  filter(Games >= 100)

#Create a plot showing the Ints, Sacks, Rushing attempts, Passes attempted and Offensive points
plot = ggplot(data = Age_stats_QB, aes(x = Age)) +
  geom_line(aes(y = Ints, color = "Ints"), linetype = "solid") +
  geom_line(aes(y = Sacks, color = "Sacks"), linetype = "solid") +
  geom_line(aes(y = Rushing.Attempts, color = "Rushing Attempts"), linetype = "solid") +
  geom_line(aes(y = Passes.Attempted / 10, color = "Passes Attempted"), linetype = "solid") +
  geom_line(aes(y = Offensive.Points / 10, color = "Offensive Points"), linetype = "solid") +
  geom_line(aes(y = Offensive.Points / 10, color = "Offensive Points"), linetype = "solid") +
  labs(x = "Age", title = "Quarterback Statistics")

#Adds a y-axis on both the left and the right. The left Y-axis corresponds with the amount of Ints, Sacks and Rushing attempts. The right corresponds with the passes attempted and the offensive points.
plot = plot + scale_y_continuous(
  name = "Amount of Ints, Sacks & Rushing attempts",
  sec.axis = sec_axis(trans = ~ . * 10, name = "Amount of Offensive points & Passes attempted"),
  breaks = seq(0.5, 3.5, by = 0.5),
  limits = c(0.5, 3.5)
)

#Adds the x-axis and shows the amount of games played on each age
plot <- plot + scale_x_continuous(breaks = Age_stats_QB$Age[seq(1, nrow(Age_stats_QB), by = 4)], 
                                  labels = paste(Age_stats_QB$Age[seq(1, nrow(Age_stats_QB), by = 4)], "(", Age_stats_QB$Games[seq(1, nrow(Age_stats_QB), by = 5)], " games)"))

#Creates a functioning legend
custom_colors <- c("Ints" = "blue", "Rushing Attempts" = "red", "Sacks" = "green", "Passes Attempted" = "purple", "Offensive Points" = "orange")
plot <- plot + scale_color_manual(values = custom_colors)

plot = plot + theme(legend.position="top")
# Print the plot
print(plot)


regression_results_Age <- data.frame()
# Loop through variable pairs
for (i in 3:8) {  
  DV <- names(Age_stats_QB)[i]  
  IV <- "Age"  
  
  # Fit the regression model
  model <- lm(paste(DV, "~", IV), data = Age_stats_QB)
  
  # Extract relevant information
  intercept <- coef(model)[1]
  coefficient <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Add results to the dataframe
  result_row <- data.frame(DV, IV, intercept, coefficient, p_value, adj_r_squared)
  regression_results_Age <- rbind(regression_results_Age, result_row)
}

# Print or analyze the regression results
print(regression_results_Age)


#Create Career stats
Career_stats <- Quarterback_gamelogs %>%
  group_by(Name, Birthyear) %>%
  summarise(
    Wins = sum(Outcome == "W"),
    Amount_of_games = n(),
    Passing.Yards = sum(Passing.Yards),
    Passes.Completed = sum(Passes.Completed),
    Passes.Attempted = sum(Passes.Attempted),
    Sacks = sum(Sacks),
    Sacked.Yards.Lost = sum(Sacked.Yards.Lost),
    Rushing.Attempts = sum(Rushing.Attempts),
    Rushing.Yards = sum(Rushing.Yards),
    Fumbles = sum(Fumbles),
    Fumbles.Lost = sum(Fumbles.Lost),
    Ints = sum(Ints),
    TD.Passes = sum(TD.Passes),
    Offensive.Points = mean(Offensive.Points)
  ) %>%
  filter(Amount_of_games>=5)

Career_stats$Wins[is.na(Career_stats$Wins)] = 0
Career_stats$WinPercentage = floor(Career_stats$Wins/Career_stats$Amount_of_games *100)
Career_stats$Passer.Rating = calculate_passer_rating(Career_stats$Passes.Completed,Career_stats$Passes.Attempted, Career_stats$Passing.Yards, Career_stats$TD.Passes, Career_stats$Ints, Career_stats$Sacks, Career_stats$Sacked.Yards.Lost, Career_stats$Rushing.Attempts, Career_stats$Rushing.Yards, Career_stats$Fumbles, Career_stats$Fumbles.Lost)

Career_stats$games_group = cut(Career_stats$Amount_of_games, 
                        breaks = c(-Inf, 10, 150, Inf),
                        labels = c("Below 10", "10-150", "Above 150"))
color_mapping = c("Below 10" = "black", "10-150" = "blue", "Above 150" = "red")

ggplot(Career_stats, aes(x = WinPercentage, y = Passer.Rating, color = games_group)) +
  geom_point() +
  scale_color_manual(values = color_mapping) +
  labs(x = "WinPercentage", y = "Passer.Rating") +
  ggtitle("Scatter Plot of Passer Rating vs. Win Percentage")



# Divide the variables of interest by Amount_of_games
Career_stats_average <- Career_stats %>%
  mutate(
    Passing.Yards = Passing.Yards / Amount_of_games,
    Passes.Completed = Passes.Completed / Amount_of_games,
    Passes.Attempted = Passes.Attempted / Amount_of_games,
    Sacks = Sacks / Amount_of_games,
    Sacked.Yards.Lost = Sacked.Yards.Lost / Amount_of_games,
    Rushing.Attempts = Rushing.Attempts / Amount_of_games,
    Rushing.Yards = Rushing.Yards / Amount_of_games,
    Fumbles = Fumbles / Amount_of_games,
    Fumbles.Lost = Fumbles.Lost / Amount_of_games,
    Ints = Ints / Amount_of_games,
    TD.Passes = TD.Passes / Amount_of_games,
    Generation = Birthyear + 28
  )

Career_stats_average$Generation_Category <- cut(
  Career_stats_average$Generation,
  breaks = c(1960, 1980, 2000, Inf),
  labels = c("1960-1980", "1980-2000", "> 2000")
)


## Select the numeric variables for PCA (excluding non-numeric and response variables)
variables_of_interest <- Career_stats_average[, c(
  "Passes.Completed", "Passes.Attempted",
  "Sacks", "Rushing.Attempts", "Fumbles", "Ints", "TD.Passes",
  "Offensive.Points"
)]

# Standardize the data (mean center and scale)
standardized_data <- scale(variables_of_interest)

# Perform PCA
pca_result <- prcomp(standardized_data)

# Summary of PCA results
summary(pca_result)

# Variance explained by each principal component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Scree plot to visualize variance explained
scree_plot <- ggplot() +
  geom_bar(aes(x = 1:length(var_explained), y = var_explained), stat = "identity") +
  labs(x = "Principal Component", y = "Variance Explained") +
  ggtitle("Scree Plot")


color_mapping <- c(             
  "1960-1980" = "blue",         # Between 1960 and 1980
  "1980-2000" = "green",        # Between 1980 and 2000
  "> 2000" = "purple"           # Bigger than 2000
)

pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], Generation = Career_stats_average$Generation)


pc1_pc2_plot <- ggplot(as.data.frame(pca_result$x), aes(x = PC1, y = PC2, color = Career_stats_average$Generation_Category)) +
  geom_point() +
  labs(x = "PC1", y = "PC2") +
  ggtitle("PCA: PC1 vs. PC2") +
  scale_color_manual(values = color_mapping)  # This line should be inside ggplot

# Print or visualize the PCA results and plots
print(pca_result)
print(scree_plot)
print(pc1_pc2_plot)


# Create a biplot
biplot(pca_result, scale = 0, col = c("white", "black"))
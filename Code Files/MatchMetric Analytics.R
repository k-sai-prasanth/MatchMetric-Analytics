library(dplyr)
library(caTools)  # For data splitting

# Data reading
atp <- read.csv("atp_matches_2023.csv")

#Checking for missing values
sum(is.na(atp))

# Data Structure
str(atp)

# Replace all NA values in the entire dataframe 'atp' with 0
atp[is.na(atp)] <- 0

# Remove extra part from 'tourney_name' column at Davis Cup
atp$tourney_name <- gsub("^Davis Cup.*", "Davis Cup", atp$tourney_name)

# Trimming white spaces
atp <- data.frame(lapply(atp, trimws))

# Data Formatting
atp$tourney_id <- as.factor(atp$tourney_id)
atp$tourney_name <- as.factor(atp$tourney_name)
atp$surface <- as.factor(atp$surface)
atp$draw_size <- as.factor(atp$draw_size)
atp$tourney_level <- as.factor(atp$tourney_level)
atp$match_num <- as.integer(atp$match_num)
atp$winner_id <- as.factor(atp$winner_id)
atp$winner_seed <- as.factor(atp$winner_seed)
atp$winner_entry <- as.factor(atp$winner_entry)
atp$loser_id <- as.factor(atp$loser_id)
atp$loser_seed <- as.factor(atp$loser_seed)
atp$loser_entry <- as.factor(atp$loser_entry)
atp$winner_name <- as.factor(atp$winner_name)
atp$winner_hand <- as.factor(atp$winner_hand)
atp$winner_ht <- as.factor(atp$winner_ht)
atp$winner_ioc <- as.factor(atp$winner_ioc)
atp$winner_age <- as.factor(atp$winner_age)
atp$loser_name <- as.factor(atp$loser_name)
atp$loser_hand <- as.factor(atp$loser_hand)
atp$loser_ht <- as.factor(atp$loser_ht)
atp$loser_ioc <- as.factor(atp$loser_ioc)
atp$loser_age <- as.factor(atp$loser_age)
atp$best_of <- as.factor(atp$best_of)
atp$round <- as.factor(atp$round)
atp$minutes <- as.integer(atp$minutes)
atp$w_ace <- as.integer(atp$w_ace)
atp$w_df <- as.integer(atp$w_df)
atp$w_svpt <- as.integer(atp$w_svpt)
atp$w_1stIn <- as.integer(atp$w_1stIn)
atp$w_1stWon <- as.integer(atp$w_1stWon)
atp$w_2ndWon <- as.integer(atp$w_2ndWon)
atp$w_SvGms <- as.integer(atp$w_SvGms)
atp$w_bpSaved <- as.integer(atp$w_bpSaved)
atp$w_bpFaced <- as.integer(atp$w_bpFaced)
atp$l_ace <- as.integer(atp$l_ace)
atp$l_df <- as.integer(atp$l_df)
atp$l_svpt <- as.integer(atp$l_svpt)
atp$l_1stIn <- as.integer(atp$l_1stIn)
atp$l_1stWon <- as.integer(atp$l_1stWon)
atp$l_2ndWon <- as.integer(atp$l_2ndWon)
atp$l_SvGms <- as.integer(atp$l_SvGms)
atp$l_bpSaved <- as.integer(atp$l_bpSaved)
atp$l_bpFaced <- as.integer(atp$l_bpFaced)
atp$winner_rank <- as.factor(atp$winner_rank)
atp$winner_rank_points <- as.integer(atp$winner_rank_points)
atp$loser_rank <- as.factor(atp$loser_rank)
atp$loser_rank_points <- as.integer(atp$loser_rank_points)

# Date formatting
atp$tourney_date <- as.Date(as.character(atp$tourney_date), format = "%Y%m%d")

# New month column
atp$tourney_month <- as.factor(format(atp$tourney_date, "%m"))

# Write the modified dataframe to a CSV file
write.csv(atp, "atp_data_preprocessed.csv", row.names = FALSE)


# Question 1
#Can we predict the player winning chance based on the player and match parameters?

#Data conversion
winner_specific_columns <- c("winner_id", "winner_seed", "winner_entry", "winner_hand", "winner_ht", "winner_ioc", "winner_age", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "winner_rank", "winner_rank_points", "surface","draw_size","tourney_level","minutes" )
winner_df <- atp[, winner_specific_columns]
winner_df$Result <- "Won"


loser_specific_columns <- c("loser_id", "loser_seed", "loser_entry", "loser_hand", "loser_ht", "loser_ioc", "loser_age", "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced","loser_rank", "loser_rank_points", "surface","draw_size","tourney_level","minutes")
loser_df <- atp[, loser_specific_columns]
loser_df$Result <- "Lost"

colnames(winner_df) <- sub("^winner_", "", colnames(winner_df))
colnames(winner_df) <- sub("^w_", "", colnames(winner_df))

colnames(loser_df) <- sub("^loser_", "", colnames(loser_df))
colnames(loser_df) <- sub("^l_", "", colnames(loser_df))


# Combine winner_df and loser_df into a new DataFrame
atp2 <- rbind(winner_df, loser_df)

# View the combined DataFrame
head(atp2)  # View the first few rows

str(atp2)

atp2$Result <- ifelse(atp2$Result == "Won", 1, 0)
atp2$age <- round(as.numeric(as.character(atp2$age)))

atp2$Result <- as.factor(atp2$Result)
atp2$rank <- as.integer(atp2$rank)
atp2$seed <- as.integer(atp2$seed)

# Model building

set.seed(123)  # For reproducibility
split <- sample.split(atp2$Result, SplitRatio = 0.7)  # 70% train, 30% test
train_data <- subset(atp2, split == TRUE)
test_data <- subset(atp2, split == FALSE)

train_data <- subset(train_data, select = -c(id))
test_data <- subset(test_data, select = -c(id))

# Build logistic regression model
model <- glm(Result ~ ., data = train_data, family = binomial)

# Summary of the model
summary(model)

test_data <- subset(test_data, !(ioc %in% c("JAM", "MAR", "MEX")))
# Predict on test set
predictions <- predict(model, newdata = test_data, type = "response")

# Evaluate model performance (for example, accuracy)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_classes == test_data$Result)
accuracy


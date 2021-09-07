### Homework 3
# Barbara Schweitzer

# Load Data & Data Exploration

chessGames <- read.csv('games.csv', header = TRUE, stringsAsFactors = FALSE)
str(chessGames)

numberOfColumns <- length(chessGames)
numberOfRows <- nrow(chessGames)

(numberOfColumns*4)*(numberOfRows/100)

## Single Dimension Plots

# Histogram
hist(chessGames$turns, main = 'Histogram of Chess Game Turns'
     , xlab = 'Number of Turns', col = 'lightblue3')

# Barplots
barplot(table(chessGames$rated), main = 'Rated vs. Unrated Games'
        , xlab = 'Rated or Not', col = 'midnightblue')

barplot(sort(table(chessGames$victory_status)), main = 'Game Ending'
        , xlab = "Type of Ending", col = 'seashell2')

# Boxplot
ratingsDF <- data.frame(chessGames$white_rating, chessGames$black_rating)
colnames(ratingsDF) <- c('White', 'Black')
boxplot(ratingsDF, main = 'Boxplots of Player Ratings'
        , xlab = 'Player`s Piece Color', col = c('azure1', 'azure4'))
summary(ratingsDF)

df <- as.vector(as.matrix(ratingsDF[,c('White', 'Black')]))

boxplot(df, main = 'Boxplot of Player Ratings'
        , col = c('azure3'), horizontal = TRUE)

write.csv(chessGames,"chessGames.csv", row.names = TRUE)


## Multi-Dimension Plots

# Scatterplot
plot(chessGames$white_rating, chessGames$black_rating, pch = 16, cex = .2
     , main = 'White vs. Black Rating', xlab = 'White`s Rating'
     , ylab = 'Black`s Rating', col = 'lemonchiffon4')

## Boxplots
chessGames$firstMove = substr(chessGames$moves,1,3)
table(chessGames$firstMove)

firstMoveTable <- table(chessGames$firstMove)
firstMoveTable
fmDF <- data.frame(firstMoveTable)
fmDF <- fmDF[-c(7,9,12,16),]
barplots <- barplot(fmDF$Freq, ylim = c(0, 14000))
text(barplots, fmDF$Freq+1000, fmDF$Freq, cex=1)

boxplot(a3$white_rating, a4$white_rating, b3$white_rating, b4$white_rating
        , c3$white_rating, c4$white_rating, d3$white_rating, d4$white_rating
        , e3$white_rating, e4$white_rating, f3$white_rating, f4$white_rating
        , g3$white_rating, g4$white_rating, h3$white_rating, h4$white_rating
        , Na3$white_rating, Nc3$white_rating, Nf3$white_rating, Nh3$white_rating
        , col = '#EDEBE9')


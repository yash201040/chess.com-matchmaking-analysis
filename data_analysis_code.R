#0. PREREQUISITE INFO
# Direct link to source data set -> https://www.kaggle.com/datasets/adityajha1504/chesscom-user-games-60000-games
# Data set file: "club_games_data.csv" (166.67 MB) raw
# API command to download file -> kaggle datasets download -d adityajha1504/chesscom-user-games-60000-games
# If error in fetching data set, kindly edit to appropriate local path in line13


#1. LOADING REQUISITES
require(magrittr)
require(tidyverse)
require(knitr)
require(lessR)
df = read.csv('/Users/guts/Documents/Uni Files/MTHM501/Week 5 Assessment/chessdotcom_games_data.csv')
# Chess.com data set with 65k+ obs, 14 features.


#2. DATASET EXAMINATION: By Perceiving Categorical Features
gameVariantsList = unique(df$rules) # 5 unique game variants
resultsList = unique(df$white_result) # 13 unique game outcomes
timeClassesList = unique(df$time_class) # 4 unique time classes
timeControlsList = unique(df$time_control) # 62 unique time controls


#3. DATA WRANGLING & TINKERING: To Prepare For Some Data Science!
df = df %>% filter(rated=='True' & rules=='chess' & # filtered to rated chess
                   white_result != 'abandoned' & 
                   black_result != 'abandoned') %>% # excluded abandoned games
  dplyr::rename(white_elo = white_rating,
         black_elo = black_rating) %>% # renamed cols
  select(c(white_elo, black_elo, white_result, black_result,
           time_control)) %>% # selected required features
  mutate(white_elo = as.numeric(white_elo),
         black_elo = as.numeric(black_elo)) # mutated all elos to numeric


#4. MISSING VALUES: Considering 2 Essential Columns Below As Missing Features
result = c() # to minimize 2 complex result features into 1 simplified feature
white_outcome = c() # to create numeric feature that quantifies result feature

for(i in 1:nrow(df)) { # Categorizing result data; given white's perspective
  resultValue = switch(df$white_result[i],
                  'win'                 ='white-won',
                  'checkmated'          ='black-won',
                  'timeout'             ='black-won',
                  'resigned'            ='black-won',
                  'timevsinsufficient'  ='draw',
                  'repetition'          ='draw',
                  'insufficient'        ='draw',
                  'stalemate'           ='draw',
                  'agreed'              ='draw',
                  '50move'              ='draw')
                                            # draw = 0,
  outcomeValue = switch(df$white_result[i], # white-win = +1, black-win = -1
                  'win'                 = +1,
                  'checkmated'          = -1,
                  'timeout'             = -1,
                  'resigned'            = -1,
                  'timevsinsufficient'  = 0,
                  'repetition'          = 0,
                  'insufficient'        = 0,
                  'stalemate'           = 0,
                  'agreed'              = 0,
                  '50move'              = 0)
  result = append(result, resultValue) # inserting values into new features
  white_outcome = append(white_outcome, outcomeValue)
}

df = df %>% mutate(match_rating = (df$white_elo + df$black_elo)/2, #mean\median
                   delta_elo = df$white_elo - df$black_elo, #difference in elo
                   result = result, # added derived result feature to data set
                   white_outcome = white_outcome) %>% # quantified result col
  select(-c(white_result, black_result)) # removed 2 cols, no longer needed

timeControlsDf = data.frame(table(df$time_control)) %>% # time control freq df
  arrange(-Freq)  # sorting by frequency to filter a single best time control

timeControlTable = kable(timeControlsDf) # tabulated time controls frequency
timeControlTable # 2nd most popular time control selected as 1st is a 1min game

df = filter(df, time_control=='600') %>% # Filters to 10 min time control
  select(-time_control) # Removing time_control as all values are 600 sec


#5. GRAPHICS: Scatterplot, LinearModel, Histograms, PieCharts
# plotting PvP elo from rated matchmaking 
matchmakingScatterplot =
  ggplot(df, aes(x=black_elo, y=white_elo, col=delta_elo)) +
  ggtitle('Match-Making Elo Scatterplot') + geom_point(size=0.25) +
  labs(colour='Elo Difference') + xlab('Black Elo') + ylab('White Elo') +
  scale_colour_gradient2(low = 'black', mid = 'red', high='white') +
  theme_classic() + theme(panel.background = element_rect(fill='darksalmon'))

matchmakingLinearModel =
  ggplot(df, aes(x=black_elo, y=white_elo, col=result)) +
  geom_point(size=0.5) + ggtitle('Match-Making Result Linear Regression') +
  xlab('Black Elo') + ylab('White Elo') + theme_classic() +
  scale_color_manual(values = c('black', 'red', 'azure')) +
  theme(panel.background = element_rect(fill = 'bisque')) +
  stat_smooth(method='lm', col='green') # linear regression on matchmaking

# histograms will help visualize games played across elo
matchRatingDensity = density(df$match_rating) # kernel density estimate value
plotGameVolumesHistogram = function(kde=FALSE, d=matchRatingDensity) {
  hist(df$match_rating, breaks = 15, freq = !kde,
       main = 'Volume of Games Played Across Ratings',
       xlab = 'Match Ratings (Average of 2-player elo in versus)',
       ylab = 'Volume of Games Played',
       col='aquamarine')
  if(kde) {lines(matchRatingDensity)}
  axis(side = 1, at = 100)
}

matchmakingScatterplot # running scatterplot
matchmakingLinearModel # running linear regression on scatterplot

plotGameVolumesHistogram()
plotGameVolumesHistogram(kde = TRUE, d=matchRatingDensity)
# histograms indicate peak player volumes in 1200-1600 elo range

# filtering for maximum game volumes for players matched at closer elo
highGameVolumesDf = filter(df, match_rating>1200 & match_rating<1600 &
                             delta_elo>=-10 & delta_elo<=10)
                                                              #1065 games found

# plotting PieChart to visualize outcome distribution in high volume games
highGameVolResults = highGameVolumesDf$result # extracted results
plotOutcomePiechart_atHighGameVolumes = function() {
  style(panel_fill = 'skyblue')
  PieChart(highGameVolResults, hole=0, density = 75, clockwise = TRUE,
           main='Outcome from Games Rated 1200-1600',
           fill = c('black','red','azure'), color = c('black'),
           values_color = c('white', 'white', 'black'))
}

plotOutcomePiechart_atHighGameVolumes() # on total 1065 games

#6. HYPOTHESIS TEST: Successfully Rejected Null Hypothesis
# Null Hypothesis -> "Actual outcome of a game can be predicted by
# (+\-)delta_elo/match_rating."

# (+\-)delta_elo is signed difference in elo of 2 players matched together:
# (white_elo - black_elo) & match_rating is mean of both players' elos.

expectedOutcome = highGameVolumesDf$delta_elo/
                    highGameVolumesDf$match_rating
                                              # lies in a range of (-1 to 1)
actualOutcome = highGameVolumesDf$white_outcome # 3 discrete values {-1,0,1}
# -1 is perceived as black-won
# 0 is perceived as draw
# +1 is perceived as white-won
predictiveOutcome_HypothesisTest = t.test(expectedOutcome,
                                          actualOutcome,
                                          alternative = 'two.sided')
predictiveOutcome_HypothesisTest # runs test; outputs p.value = 0.000644
# Since p.value observed << 0.05, null hypothesis can be successfully rejected!
# Alternate hypothesis is supported by analysis of 1065 games, that outcome of
# a game CANNOT be predicted based on (difference in elo) / (match rating).

# Therefore, each rating range can be analyzed in sub-groups to
# conclude any possible advantage white can have in higher rated games
# --- E N D ---

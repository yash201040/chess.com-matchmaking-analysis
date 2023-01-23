# chess.com-matchmaking-analysis
Analysing chess.com's matchmaking of rated games for fairness across different elo ranges.
Involves black-&-white pieces scatter plot and matched pair's elo mid-point prediction through linear regression.
Follow-up with analysis of different volumes of matches played at different elo mid-points and focusing on the range where maximum matches occur (1200-1600). Concluding with hypothesis testing for any advantage to white or black pieces due to matchmaking bias.
_________________________________

The rendered output of the .qmd file is in the pdf document (13 pages). Download and view locally if text-font seems distorted on github.

The data file used can be found here https://www.kaggle.com/datasets/adityajha1504/chesscom-user-games-60000-games
The .csv file was too large to be compressed under 25 MB for uploading here.

You can run the .r file or .qmd file in RStudio.
Double-check csv file path (after downloading the data from kaggle) in both the .r and .qmd files' lines where csv file is being read.

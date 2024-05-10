instructions <- function() {
  HTML(
    "This app was designed to analyse match data from war games (or any game for that matter) within a Bayesian framework.
    Analysing the data in this way allows us to regularise the results by using a sensible prior distribution on what kind of win rates we'd expect to see in reality. Our inferences therefore won't be skewed by unexpectedly high or low win rates from only a few games.
    The model used in the app makes use of a moderately informative prior distribution (plotted in the adjacent window), which effectively states that no team/player/faction is likely to win more or less than 75% or 25% of the time, respectively, in the long run.
    If there's a lot of data that suggests that a team/player/faction does win or lose more than this, then this will override the prior's influence on the results; however, there will have to be a lot of consistent data to do this.
    The model doesn't currently take into account any nested/hierarchical data structures (e.g. several independent players within a faction).
    To use the app, navigate to the <b>[See who wins]</b> tab in the navbar on the left-hand side of the window and follow the instructions below:<br>
    <br>

    <b>Data input tab:</b><br>

       <p style='margin-left: 20px'>
       <b>Data input:</b><br>
       - Using the <b>[Browse]</b> button, find and select the .csv/.xls/.xlsx file containing your data<br>
       - Indicate with the checkbox whether the first row of your data contains the column names<br>
       - The app expects three columns in your data: (i) the team/player/faction name, (ii) the number of wins, and (iii) the total number of games played<br>
       <br>

       <b>Your data:</b><br>
       - You can view and search through your data once it has loaded<br>
       <br>

       <b>Column selection:</b><br>
       - Select the columns within your data as indicated<br>
       - Press the <b>[Fit model]</b> button when you are ready<br>
       - Note that, depending on the size of your data set, this may take a few minutes to run as indicated by the loading bar<br>
       </p>

    <b>Results tab:</b><br>

       <p style='margin-left: 20px'>
       <b>Model fit:</b><br>
       - The posterior distributions of the win rates for each team/player/faction are plotted against the input data<br>
       <br>

      <b>Model checks:</b><br>
      - The success or failure of some simple checks of model validity are indicated in this box<br>
      <br>

      <b>Results summary:</b><br>
      - The posterior distributions of the win rates are summarised for each team/player/faction<br>
      <br>
       </p>

   "
  )
}

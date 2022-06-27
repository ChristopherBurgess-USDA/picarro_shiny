# Picarro Shiny App

Our lab uses our Picarro (Model G2101-i) regularly; however, in order to make everyone's lives just a little bit easier I decided to write a shiny app which takes the picarro data off the machine and parses it into a format that is useful for them to use for their downstream analysis.

## [Link to App](https://cjburgess.shinyapps.io/picarro_shiny/)

Please contact me or open an issue on the repository if you have any errors or questions. 

## How to use the App

This app is set up to parse the raw data coming off our Picarro (model G2101-i) with a 16 port sample reader. The problem is that the data coming off the Picarro is a continuous stream of readings taken each second. This app is able to filter out all the unwanted data and only keep readings from the actual samples--by removing readings taken from ports we do not use (port 16) and ensuring the there is 2 minutes of consecutive readings from a sample port. Since the readings from a sample take a while to stabilize, we average the last 30seconds on each sample readings.

These are the key features on how our Picarro records data that may differ from your set up.

* **Samples are only recorded on valves 1-15** with 16 being a blank valve set to run for 30min to give us a little buffer to change samples between sets of 15.
* **2min of gas is drawn from each sample**. The readings for each sample take around a minutes to stabilize in the Picarro's output; therefore, we only take the last 30seconds from each sample's readings. The script will identify which sample's readings were less than 2min (Quality column of the output).




**NOTE:** This app has been tailored to our lab's picarro output format and needs. If this is app doesn't work for your machine please contact me and we'll see if we can add additional additional features or at least a new app to parse your files!


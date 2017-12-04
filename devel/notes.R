#### NOTES ####

# formatStations should only return things that are formatted data. No calculations,
# no calibrations. Everything like applyCalibration, driftShit, should be taking
# the formatStation output as their input and then doin shizz.

# Shit dont work if there arent the right number of effort records.
# The 'last effort is not off' warnings dont work right.

# Summary stuff: Map of all stations, then map for each species. X mark if none.
# Just use coastlines for simple automatic one, can create satellite option

# Table with Station #, lat/long, time, recording length, # species breakdown,
# some kind of estimate of buoy angle error, drift calibration if we did it

# For plotting of a station we want to be able to show localizations and possibly
# the location of the ship

# Possible we want to check 'DifarModuleAction == deployed' in HydrophoneStreamers table for buoy position
# Seems like it would avoid random entries. Maybe only if it exists? It doesnt in calcurceas

# Probably want warnings about multiple deployment position records for buoys. Station 2 and 3 have a bunch of weird
# shit (lasker) in the streamers table. Looks like stuff got split between two dbs over the days.

# Species summary provides detection for each buoy, but isnt trying to match them up to say 'these two calls on
# these buoys are the same call' using MatchedAngles or whatever Eric has it called.

# For species maps may want to also show histogram of counts by species or something. If there are a few big
# detections (there are), then the color scale will always be shit. Also might want to bin things, not
# sure how we would decide the bin sizes. Have it going this way now.

# Species maps - Need to deal with buoys. Should we show all factor levels when they arent present (the legend)

# Individual station maps - Need to be able to plot many detections at same time (arrows)? Coloring and legend
# gets a little stupid. Better to be in a manipulate where you just pick one?

# Correct for deployment position - need two different corrections. One a fixed distance based on location of
# ship GPS compared to drop location, one for time delay.

# Drift shit - try and get guidelines from our run around in circles data. Or simulated. Need some kind of
# half assed guidelines.

# Drift simulations? Just make a track of ship locations as a circle or some shit around a fixed deployment point
# can then change the time or someting for each one. Need to work first under assumption that our angles are good
# then we can go back and say 'what if they are bad' or just say make sure your angles are good.

# Bits with variation changes based on bearing angle will be kind of a separate problem. Wont necessarily try and
# deal with it now, but say 'hey look at this shit'

# read difar module action for buoy deployment. Default is say you have to have that. If it isnt there we need
# a warning to go fix it.
#   1) Take the first. 2) Provide a file??? That either says which line to use or actual lat long

# Buoy calibration summary. Are there buoys I dont trust? Use manipulate to go through histograms of calibration values
# Loop through all buoy calibrations - have user mark ABC before looking at next one. Add flag to buoy info list

# Matching calls will need more flexibility built in

# Sound source estimation. Needs a system calibration and a buoy calibration. Need localization to get a distance
# Basic spherical spreading ok. Get papers from SR.

# Single station plot. Will want to add localization to each, and have ability to quickly tab through. Should
# switch to base plot, then have option to make pretty version. Probably want to still load the map so we can
# get same bounding box? or maybe a better way. Want same frame for each one, actually prob just bound on the
# extremes of localization or whatever we have.

#### ORDER OF WORK ####
# 1) Buoy calibration. 2) Buoy drift. 3) Error checking? 4) Format output for models
# 5) sound source estimation? 6) localization?

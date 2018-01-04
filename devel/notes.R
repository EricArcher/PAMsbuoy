#### NOTES ####

# formatStations should only return things that are formatted data. No calculations,
# no calibrations. Everything like applyCalibration, driftShit, should be taking
# the formatStation output as their input and then doin shizz.

# Table with Station #, lat/long, time, recording length, # species breakdown,
# some kind of estimate of buoy angle error, drift calibration if we did it
# DONT KNOW RECORDING LENGTH

# For plotting of a station we want to be able to show localizations and possibly
# the location of the ship

# Probably want warnings about multiple deployment position records for buoys. Station 2 and 3 have a bunch of weird
# shit (lasker) in the streamers table. Looks like stuff got split between two dbs over the days.

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

# Matching calls will need more flexibility built in

# Sound source estimation. Needs a system calibration and a buoy calibration. Need localization to get a distance
# Basic spherical spreading ok. Get papers from SR.

# Single station plot. Will want to add localization to each, and have ability to quickly tab through. Should
# switch to base plot, then have option to make pretty version. Probably want to still load the map so we can
# get same bounding box? or maybe a better way. Want same frame for each one, actually prob just bound on the
# extremes of localization or whatever we have.

# Functionality to combine _P1 and such things automatically? Is this common, or just DIY?

# labelDetection breaks if we have Ids in MatchedAngles that aren't in Id column. No idea how this happened.
# temp fix for now, will just remove bad Ids from MA and warn you where it happened.

# drift needs to check inputs are proper form otherwise you get weird errors

# Check buoy position stuff (loading PAST_20160607 data). Doesnt seem like error is happening if its missing

#### ORDER OF WORK ####
# 1) Buoy calibration. 2) Buoy drift. 3) Error checking? 4) Format output for models
# 5) sound source estimation? 6) localization?

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

# Progress updates on loadStations - calcurceas taking a long time, would be good to know how far along we are.

# Species summary provides detection for each buoy, but isnt trying to match them up to say 'these two calls on
# these buoys are the same call' using MatchedAngles or whatever Eric has it called.

# For species maps may want to also show histogram of counts by species or something. If there are a few big
# detections (there are), then the color scale will always be shit. Also might want to bin things, not
# sure how we would decide the bin sizes. Have it going this way now.
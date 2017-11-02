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
# Seems like it would avoid random entries
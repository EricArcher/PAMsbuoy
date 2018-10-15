#### NOTES ####

# formatStations should only return things that are formatted data. No calculations,
# no calibrations. Everything like applyCalibration, driftShit, should be taking
# the formatStation output as their input and then doin shizz.

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
#####################################
# BIAS FUCKS IT, BUT IF YOU HAVE A BIG ENOUGH RANGE YOU CAN SORT OF OVERCOME IT.
# AT LEAST TO THE POINT OF 'ITS BETTER THAN NOT DOING IT'
# IF YOU HAVE GOOD ANGLES, RANDOM VARIATION DOESNT MATTER
# CHECK DRIFTSIMS AND SIMDIAGNOSTIC IN DRIFTSIM.R
##########

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

# Check buoy position stuff (loading PAST_20160607 data). Doesnt seem like error is happening if its missing

# UIDS not loaded ?

# Adjust makeLines to be able to be iterative - just call again on last point to turn again?
# Bias makes it fuuuuuuucked

#### ORDER OF WORK ####
# 1) Buoy calibration. 2) Buoy drift. 3) Error checking? 4) Format output for models
# 5) sound source estimation? 6) localization?

###### POTENTIAL PROBLEM####
# NO INTERNET MEANS NO MAPS ####
# Change getMap - first suppress warning messages when it cant download, then after it fails
# however many times have it spit out some blank bullshit to plot on


# Report Notes:
# Captions: have both for figures, none for tables.

# PG version: lookup in table (later once it's added), if can't be found then ask. Probably
# create an info part of station (like buoy, det) to store this.

# For now no recording length. Need to figure out better way.

# In future connect difar species to lookup table. For now we can say: I found these species, what are
# their names.

# Table info: want also number of calls only heard on 1 buoy?
# Collapsing will break if only 1 species or other shit

# Thoughts on drift - it isn't as important to say "is this better" for 1 buoy,
# we need the relative positions for our secr shit

####
# Eric Qs
# Offline mapping
## 3 options: download bases google map first?
### People can supply coord bounds, then get map at start
### see if get_map returns something when offline
# Drift shenanigans confidence measure
## Convert rate/bear to points, then distance from max like point after an hour or whatever
# Install phantom_js on package load

###
# Drift problems
# Angle error is a worse problem when you are further away

## Offline - have a preparatory function to download all offline stuff before casting off
# Also create folder with default info? Like cruise number, ship name, coord bounds, saved in a text file
# Possibly wincruise style formatting. Save as an rdata object.

# We may need to rewrite shit with S4 methods. New github branch to start working on this. Will finish
# this version first, then try again later.

## LATERRRRR
# Shiny for the layman

# Drift - normalize each distance bin because each bin really corresponds to a ring on concecntric circles
# further ones have much more area.
## This isnt really true. Simulated with normals on x,y and it works out the way it should without
# trying to normalize them. My expectations were wrong - in my sims that didnt make sense the buoy had
# only been moving for 9 minutes @ .7 km/hour. It isn't long enough to see much movement (under assumption
# of error of 7). If the boat is slower it works out just fine.
# i think i'm happy with drift. Should be a good enough measure of 'do we have the data to measure drift'
# Can we reduce the cap from 3 to 2? What is an unreasonable drift rate?
# Will be tricky to come up with 'when is this good.' probably based on relation to predicted rate.
# Just dividing by rate estimate is sketchy. When we have a small estimate it explodes and looks bad.
## But is that really bad? That's all we have to go on. It is harder to detect a small rate, so it
# should be worse when it is msall.
## Yes it is still bad. If you happen to guess high it looks a lot better than it should

#### PROBLEM ###
# Need to put in a check for deployment times. First calstation the second deploy time is matched
# to the first set of calibrations so everything doesnt make sense

## calibrate stations:
# 1) check calibrations
# 2) apply calibrations    make sure we check OK. AND report how many had nothing. esp if 'do same all'
# 3) calculate drift     make sure we check applied. Do we want to have option to just run through,
# then check later? Takes a long

# detsummary is broken if zero detections. Fixed, but createSummaryReport(sette[1:10]) my tables are all fucked
# up. Colors dont alternate, need to fix the column squishing thing.

# 90% score rev

# Multiple species codes on a single vis id what to do? Add progress. The fuck?
# Dont label by SightingId, just by ship for opp with no wincruz?

## Anything without unid italicize species. For now, if multiple species detected save as 1 / 2 with
# separate color for it and such.


# Calibration summary function to call

# Localization is in greene.etal sonobuoy paper with gain imbalance. max like with von mises and some
# extra fanciness for robustlihood.
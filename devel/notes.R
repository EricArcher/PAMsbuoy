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

# drift needs to check inputs are proper form otherwise you get weird errors

# Check buoy position stuff (loading PAST_20160607 data). Doesnt seem like error is happening if its missing

# UIDS not loaded ?

# Adjust makeLines to be able to be iterative - just call again on last point to turn again?
# Bias makes it fuuuuuuucked

#### ORDER OF WORK ####
# 1) Buoy calibration. 2) Buoy drift. 3) Error checking? 4) Format output for models
# 5) sound source estimation? 6) localization?

#################
# Can you save a pretty html table as in image, then load into word doc?
#
# Create small report on drift stuff for Shannon showing results of simulations

###### POTENTIAL PROBLEM####
# NO INTERNET MEANS NO MAPS ####
# Change getMap - first suppress warning messages when it cant download, then after it fails
# however many times have it spit out some blank bullshit to plot on


# Report Notes:
# Try assuming it's in own page, then limit size. Need to do about 3/4 page size so there is
# lots of room for table header
# Captions: have both for figures, none for tables.
# Table bottom is cutoff - add pixels to end

# Ask for survey full text name for first [Cruise Name], then also short-hand for later in report.
# then question For 1705_whatver, what is the vessel name

# PG version: lookup in table (later once it's added), if can't be found then ask. Probably
# create an info part of station (like buoy, det) to store this.

# For now no recording length. Need to figure out better way.

# In future connect difar species to lookup table. For now we can say: I found these species, what are
# their names.

# Table info: want also number of calls only heard on 1 buoy?
# Collapsing will break if only 1 species or other shit

# Paste pictures and tables into google docs so we can adjust text around it

# Offline maps - ask Eric. Problem - my zooming relies on google bounds. Could have people
# download some coastfiles or whatever before going out to sea.

#### Drift sim pictures
# Some examples good and bad - bad error, fewer points
# Thoughts on drift - it isn't as important to say "is this better" for 1 buoy,
# we need the relative positions for our secr shit

#####
# Burstpulseshenanigans
#####
# WM detector in PG. Some for actualy whistles, some for BPs. Need to have analytical
# and detailed method for making one that is good for BP vs WM. High and low BPs

####
# Eric Qs
# Offline mapping
# Drift shenanigans confidence measure

###
# Drift problems
# Angle error is a worse problem when you are further away

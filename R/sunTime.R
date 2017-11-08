
# Function to convert clock time to "sun time", where sunrise is mapped to pi/2 and
#   sunset to 3*pi/2 (or 6am and 6pm if you convert from radians to hours).

# See Nouvellet et al (2012) "Noisy clocks and silent sunrises:
#   measurement methods of daily activity pattern", J Zoology 286(3) 179-184

# On 21 June in Scotland, day length (sunrise to sunset) is approx. 18 hours and 
#   night length 6 hours.
# The 18 "clock hours" of day are squeezed into the interval pi/2 to 3*pi/2, while
#   the 6 "clock hours" of night are stretched to fill 3*pi/2 to pi/2. There is 
#   an abrupt change from stretching to squeezing at sunrise and back at sunset.
# .............................................................................

# Arguments:
# clockTime : the time of the observation in radians, 0 = 2*pi = midnight
# Dates : the dates of the observations as a POSIXct object with the time zone set
#  to the time zone used for 'clockTime'
# Coords : a SpatialPoints object with the locations of the observations
#   (can be a single point giving a approximation).
#
# Output:
# a vector of sun times in radians, where sunrise = pi/2 and sunset = 3*pi/2

sunTime <- function(clockTime, Dates, Coords) {
  # Find sunset/sunrise times at location
  sr <- maptools::sunriset(Coords, Dates, direct="sunrise") * 2 * pi  # radians
  ss <- maptools::sunriset(Coords, Dates, direct="sunset") * 2 * pi
  # Which observations are 'day', get relevant values for day vs night
  day <- clockTime > sr & clockTime < ss
  startClock <- ifelse(day, sr, ss)  # either sunrise or sunset
  dayLength <- ss - sr
  span <- ifelse(day, dayLength, (2*pi) - dayLength)  # length of day or night
  startSun <- ifelse(day, pi/2, 3*pi/2) # start of day or night in sun-time units
  # Get time since startClock, convert to sun time
  timeSince <- (clockTime - startClock + 2*pi) %% (2*pi)
  out <- startSun + timeSince / span * pi
  return(out %% (2*pi))
}





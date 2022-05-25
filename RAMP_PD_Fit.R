
# This script will compute Kp (drive) and Kd (damping) parameters using the PD control model for a single trial from the rapid assessment of motor processing (RAMP) paradigm

library("jsonlite")

input <- jsonlite::fromJSON("path/data.json")

frame <- as.data.frame(input$Moves)

names(frame) <- as.matrix(frame[1, ])

frame <- frame[-1, ]

frame[] <- lapply(frame, function(x) type.convert(as.character(x)))

# Convert time units

frame$times=frame$times/1000

# Start from when subject begins to move car

start = min(which(abs(frame$slider) > .001))

framerem = frame[start:nrow(frame), ]

# Remove anomalously short sampling windows to prevent acceleration outliers

framerem = framerem[diff(framerem$times) > .001, ]

# Convert velocity units

vel = framerem$carVel * 1000000

# Calculate acceleration (dependent variable for PD model fitting)

acc = c(diff(vel) / diff(framerem$times), 0)

# Error (proportion term) is final position minus current position

prop = framerem$carPos[nrow(framerem)] - framerem$carPos

# Derivative of error is negative of recorded velocity

der = -1 * vel

# Linear regression to fit PD model (removing acceleration outliers)

model = lm(acc[acc < 10 & acc > -10] ~ 0 + prop[acc < 10 & acc > -10] + der[acc < 10 & acc > -10])

Kp = model$coefficients[1]

Kd = model$coefficients[2]


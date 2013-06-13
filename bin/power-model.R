#!/usr/bin/Rscript
#
# Usage: pweor-model.R FILENAME
#  build a linear power model from the csv data in FILENAME
#
#  model used is
#
#     kwh = C_cycles × M_cycles +
#           C_instructions × M_instructions +
#           C_fops × M_fops +
#           C_mem × M_mem
#  where
#    fops = r533f00 or (r532010 + r538010)
#     mem = cache.misses + cache.references
#
library( boot )

args <- commandArgs(trailingOnly = TRUE);

if (length(args) < 1) # Check that required arguments exist
{ stop("Pass the path to benchmarks.csv as first argument.") }

metrics <- read.csv(args[1]);

if('r533f00' %in% colnames(metrics)) {
    fops <- metrics$r533f00;
} else {
    fops <- metrics$r532010 + metrics$r538010;
}

data <- data.frame(
    energy = metrics$watts * metrics$time,
    kwh = metrics$kwh,
    cyc = metrics$cycles,
    ins = metrics$instructions,
    fops = fops,
    cch = metrics$cache.references,
    mem = metrics$cache.misses,

    watts = metrics$watts,
    ipc = metrics$instructions / metrics$cycles,
    fpc = fops / metrics$cycles,
    cpc = metrics$cache.references / metrics$cycles,
    mpc = metrics$cache.misses / metrics$cycles
)

#model <- with( data, glm( kwh ~ cyc + ins + fops + cch + mem ) )
model <- with( data, glm( watts ~ ipc + fpc + cpc + mpc ) )
summary( model )

# cv.glm uses average squared error by default: mean( delta * delta )

error <- function( observations, predictions ) {
    delta <- observations - predictions
    #mean( delta * delta )
    #sqrt( mean( delta * delta ) )
    mean( abs( delta ) )
}

cat( "performing cross-validation...\n" )
model.error <- error( model$residuals, 0 )
xval.error  <- cv.glm( data, model, cost = error, K = 10 )$delta[ 2 ]

cat( paste( "model error:  ", model.error, "\n" ) )
cat( paste( "10x cross-val:", signif( xval.error / model.error, 3 ), "x\n" ) )


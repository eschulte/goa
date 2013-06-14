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
{
    cat( "Usage: power-model.R benchmarks.csv [error-mode]\n" )
    stop("Pass the path to benchmarks.csv as first argument.")
}

metrics <- read.csv(args[1]);
err_mode <- if (length(args) == 1) "RMS" else args[2]

error <- function( observations, predictions ) {
    # cv.glm uses average squared error by default: mean( delta * delta )
    delta <- observations - predictions
    if ( err_mode == "RMS" )
        sqrt( mean( delta * delta ) )
    else if ( err_mode == "average square" )
        mean( delta * delta )
    else if ( err_mode == "average absolute" )
        mean( abs( delta ) )
    else
        stop( paste( "unknown error mode:", err_mode ) )
}
x <- error( 0, 0 )

if('r533f00' %in% colnames(metrics)) {
    fops <- metrics$r533f00;
} else {
    fops <- metrics$r532010 + metrics$r538010;
}
if ( 'time' %in% colnames( metrics ) ) {
    secs <- metrics$time
} else {
    secs <- metrics$seconds
}

data <- data.frame(
    energy = metrics$watts * secs,
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

cat( "performing cross-validation...\n" )
cat( paste( "using", err_mode, "error\n" ) )
model.error <- error( model$residuals, 0 )
xval.error  <- cv.glm( data, model, cost = error, K = 10 )$delta[ 2 ]

cat( paste( "model error:  ", model.error, "\n" ) )
cat( paste( "10x cross-val:", signif( xval.error / model.error, 3 ), "x\n" ) )


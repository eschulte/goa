#!/usr/bin/Rscript
#
# Usage: energy-corr.R FILENAME
#  print correlations between energy counters
#
options(width=160)
args <- commandArgs(trailingOnly = TRUE);

if (length(args) < 1) # Check that required arguments exist
{ stop("Pass the path to benchmarks.csv as first argument.") }

metrics <- read.csv(args[1]);

if('r533f00' %in% colnames(metrics))
{ metrics$fops <- metrics$r533f00; } else
{ metrics$fops <- rowSums(data.frame(metrics$r532010, metrics$r538010)); }

metrics$mem <- rowSums(data.frame(metrics$cache.references, metrics$cache.misses));

## summary(lm(metrics$kwh ~ metrics$cycles + metrics$instructions + fops + mem))
cor(metrics[sapply(metrics, is.numeric)])

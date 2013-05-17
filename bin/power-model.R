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
args <- commandArgs(trailingOnly = TRUE);

if (length(args) < 1) # Check that required arguments exist
{ stop("Pass the path to benchmarks.csv as first argument.") }

metrics <- read.csv(args[1]);

if('r533f00' %in% colnames(metrics))
{ fops <- metrics$r533f00; } else
{ fops <- rowSums(data.frame(metrics$r532010, metrics$r538010)); }

mem <- rowSums(data.frame(metrics$cache.references, metrics$cache.misses));

## summary(lm(metrics$kwh ~ metrics$cycles + metrics$instructions + fops + mem))
summary(lm(metrics$kwh ~ metrics$cycles + metrics$instructions + fops + metrics$cache.references + metrics$cache.misses))

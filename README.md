# Do I know what I want to say? Modeling meaning uncertainty in RSA
Models using the Rational Speech Act (RSA) framework typically assume that speakers are certain about the meaning being communicated. In this work we note that there are contexts in which this assumption need not hold, and propose a method (um-RSA) to incorporate this meaning uncertainty within the RSA framework.

## Experiment
1. Create a new project in PCIbex
2. Upload `experiment/{text, image}/main.js` to scripts
3. Upload files in `experiment/{text, image}/chunk_includes` to resources
4. Run experiment!

Here's a [demo](https://farm.pcibex.net/r/SHOtya/experiment.html?test=true) of the image experiment and a [demo](https://farm.pcibex.net/r/cusQEo/experiment.html?test=true) of the text experiment.

## Model
We proposed two ways of implementing meaning uncertainty in RSA, which can be found in `model/rsa_committed.Rmd` and `model/rsa_uncommitted.Rmd`

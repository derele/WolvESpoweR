# WolvESpoweR - Power calculations for the WolvES application


## General setup 

We simulated data for 20 wolves at 14 different locations. Sample size
is uniformly distributed over these (potentially) 200 animals (some
will be unsampled at low overall sampling sizes).

## Sex data

We simulated predation on male or female prey and sampling of the
resulting droppings. We set the variation of sex preferences between
wolf individuals and locations to 1.3-fold (rate ratio) each, meaning
that locations and wolf individuals are biased in their choice as much
as 1.3-fold.

We ran the appropriate generalized linear mixed effect models
(including a random term for wolf individuals and locations) and
tabulated how often sex differences would be detected in 1000 simulated
data sets. 

The results (Figure 1) show that 2000 samples of a particular prey
species would allow us to detect a 1.2-fold increase in female
prey. We propose 2000 samples in the application. If a quarter of
those samples would have a focal prey species represented we could
still detect a deviation from 1.35-fold female increase with
sufficient power.

![Figure 1](https://github.com/derele/WolvESpoweR/blob/master/figures/Sex_power.png)

## Location data 

We assigned 7 of the 14 locations to a "low-prevalence wolf" category,
the other 7 to a high wolf prevalence category. We used the same
statistical models to analyze the power to detect a wolf density
effect. We found that sample size has a lower impact on this, as,
quite obviously, replication across different locations is more
important for this factor. In a more sophisticated analysis we foresee
for the actual data analysis it should be possible to use wolf density
as a continuous variable allowing for more power. The analysis
preliminary as performed here indicated adequate power to detect
1.8-fold increase in female prey in low-density wolf areas.

![Figure 2](https://github.com/derele/WolvESpoweR/blob/master/figures/Loc_power.png)

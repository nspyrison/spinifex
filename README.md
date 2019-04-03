[![Travis build status](https://travis-ci.org/nspyrison/spinifex.svg?branch=master)](https://travis-ci.org/nspyrison/spinifex)

# spinifex

Generates the path for manual tours. Tours are generally available in the tourr package. The grand tour is an algorithm that shows all possible projections given sufficient time. Guided uses projection pursuit to steer the tour towards interesting projections. The spinifex package implements manual of control, where the contribution of a selected variable can be adjusted between -1 to 1, to examine the sensitivity of structure in the data to that variable. The result is an animation where the variable is toured into and out of the projection completely, which can be rendered using the 'gganimate' and 'plotly' packages.


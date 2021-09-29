# Fortran77-Projects
A collection of my computational Fortran77 projects

Exponential_decay - decay.f uses Euler's method to solve a exponential decay probblem for two nuclei where the decay rates are dependent on the populations of each type of nuclei. It compares the results of applying Euler's method to the analytical solutions from solving the differential equations modeling the exponential decay.

Infinte_series_summation - series.f calculates the infinte series sum S = 1 + x + x^2 + x^3 ... using a finite number of terms N for different x values, -1 < x < 1. It then, compares the the summation to the closed form expression T = 1/(1-x).


Monte_Carlo - monte.f estimates the value of pi using the Monte Carlo method. 
monte.f uses a multiplicative linear congruential generator to generate sets of random coordinates.
Pi is then estimated by counting the proportion of random points that land inside a circle that is inscribed in square.

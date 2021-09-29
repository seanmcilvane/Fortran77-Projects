	program monte 
      	implicit none

!	monte.f uses the Monte Carlo method to estimate pi by plotting 
!	random points inside of a square with a circle inscribed in it.
!	A Multiplicative Linear Congruential Generator is used to generate
!	random points between 0 < x < 1 and 0 < y < 1. Points that are one 
!	unit of length from the origin are counted inside the circle. Pi is 
!	estimated by multiplying the proportion of points inside the circle
!	by 4. 10 samples of 100,000 points are used to make the estimate. 

!	Initialize variables
	real c, x, y, r,  m, a,  meanx, meany, g, h, pi
	real pi_mean, total_pi, experimental_pi, variance, standard_dev
	real actual_pi, percent_error, x_total, y_total 

!	Initializes a list of size 10 to put sample means into	
	real pi_sample_mean(10)

!	Iteration variables for loops and size of the samples
	integer i, j, f, sample_size
	sample_size = 100000
	
!	Parameters for the MCLG. A is the coefficient, m is the modulus.
	a = 16807	
	m = (2**30)-35

	y_total = 0
	x_total = 0
	total_pi = 0
	actual_pi = 3.141592

!	Open and label data file
	open(1, file = 'monte.txt')
	write(1,100) 'Trial#', 'x-mean', 'y-mean', 'pi est.'
100     format(T1,A6 ,T10, A6, T20, A6, T30, A6)

! 	Runs do loop 20 for each sample	
	do 10 i = 1, 10, 1

!	Generates seeds for x and y for each sample
       		h = rand()*100
                g = rand()*100		
		
!	Resets counting variables for each sample
		c = 0
		x_total = 0
		y_total = 0  
		pi_sample_mean(i) = 0

!	Runs the MLCG and counts the number of points that land inside 
!	the circle. Calculates the sample mean of pi.

		do 20 j = 1, sample_size, 1 
		
!	Use the MLCG to generate random numbers h and g.
!	Dividing h and g yields normalized  x and y coordinates
			h = mod(a*h, m)
			g = mod(a*g, m)
			x = h/m
			y = g/m

!	Use x and y to calculate the distance between the point and
!	the origin, r. If r < 1, then the point is inside the ciricle 

			r = SQRT(x**2+y**2)
			IF ( r .LT. 1.0 ) c = c + 1

!	Used to calculate sample means later
			x_total = x_total + x
			y_total = y_total + y
20		continue 

!	Calculates the average x and y for each sample. A value 
!	close to .5 means the random number generator has a 
!	uniform distribution from 0 to 1.	

		meanx = x_total/sample_size
		meany = y_total/sample_size
	
!	Estimates pi
 
		pi_sample_mean(i) = 4*(c/sample_size)		
		
!	Sums the sample means for pi to calculate the mean of all 
!	samples later

		total_pi= total_pi + pi_sample_mean(i)

		write (1,200) i,  meanx, meany, pi_sample_mean(i)
200		format(T1, I2, T10, F6.5, T20, F8.6, T30, F8.6)
	
10	continue
	
! 	Calculate the mean and standard deviation of the sample means.

	i = 10
	experimental_pi = (total_pi)/i

	do 30 f = 1, 10, 1 

		variance = ((pi_sample_mean(f)-experimental_pi)**2) 
     +		+ variance
		standard_dev = SQRT(variance/(i))

30	continue

! 	Write the mean and standard deviation of the samples
 	
 	write (1,300)  'Experimental pi = ', experimental_pi, 
     +  'Std Dev = ', standard_dev
300	format(/,T1, A18, T20 F8.6, T30, A10, T40, F6.5)

!	Calculate and write the percent error

	percent_error=100*((ABS(actual_pi - experimental_pi))/(actual_pi))
	
	write (1,400) 'Actual Pi = ', actual_pi, 'Percent Error = ',
     +	percent_error, '%'
400	format(T1, A12, T15, F8.6,/, T1, A16, T20, F4.3, T25, A1)	

	close(1)
	Stop
	end program Monte 

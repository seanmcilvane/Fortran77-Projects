!	This program uses the Euler Method to calculate radioactive decay
        program decay
        implicit none

!	Initialize variables. A and B are the amount of Nuclei of A and B calculated using Euler's Method.
!	C and D are the amount of Nuclei of A and B, respectively, calculated using the analytic soultions.
!	EA and EB are the percent errors between the two methods of type A and type B respectively.
!	SEA and SEB are used to calculate AAE and ABE, respectively.
!	AAE and ABE are the average errors for Euler's Method for type A and type B, respectively
! 	TA and TB are the time constants of type A and type B, respectively.
!	t represents time and TS is the time step used for Euler's Method. TMAX is how far in time we will
!	calculate values for. 

        real A, B, C, D, EA, EB, SEA, SEB, AAE, ABE, TA, TB, t, TS,
     +	TMAX, N
	N = 100
        A = 100
	B = 0
	C = N
	D = 0
	TA = 1
	TB = 2
	TS = .005
	TMAX = 10
	EA = 0
	EB = 0
	SEA = 0
	SEB = 0
	AAE = 0
	ABE = 0
	t = 0


!	Open an output file 'decay.txt' to store output

        open(1, file='decay.txt')

!	Label columns
        write(1,100) 't','Euler A','Euler B','Analytic A',
     +	'Analytic B', 'AError(%)', 'BError(%)'
100     format(T1, A1, T10, A7, T20, A7, T30, A10, T45,
     +	A10, T60, A9, T70, A9)
             

!	The do while loop performs calculations at time t and then increases t by the time step TS every loop
!	and keeps going until time is greater than TMAX. 
	
	do 10 while(t .LE. TMAX)

!		Write calculations to the output file               
		write(1,200) t, A, B, C, D, EA, EB
200             format(T1, F5.3, T10, F8.3, T20, F8.3, T30,
     +          F8.3, T45, F8.3, T60, F5.2, T70, F5.2)

!		Time step
		t = t + TS

!		Euler Calculations for type A and type B
        	A = A+((-A/TA)*TS)                       
		B = B+((A/TA)-(B/TB))*TS
	
!		Anayltical calculatios of type A and type B
		C = N*exp(-t/(TA))
		D = N*(TB/(TA-TB))*(exp(-t/(TA))-exp(-t/(TB)))

!		Percent error for type A and type B
		EA = ((C-A)/C)*100
		EB = ((D-B)/D)*100
	
!		Sums up the percent error to calculate average error later on
		SEA = SEA + EA
		SEB = SEB + EB


10      enddo

!	Calculations average error for type A and type B
	AAE = SEA/(TMAX/TS)
	ABE = SEB/(TMAX/TS)

!	Writes average errors to output file
        write(1,300)'#', 'Average Error of A = ', AAE  
300     format(T1, A1, T3, A20, T24, F5.2)
	write(1,400)'#', 'Average Error of B = ', ABE 
400     format(T1, A1, T3, A21, T24, F5.2)

        close(1)
        Stop
	end program decay



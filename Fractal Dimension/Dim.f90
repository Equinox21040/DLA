program FractalDim 
    implicit none 

    real :: r,dr,rmin,rmax,t,sumx,sumy,sumxy,sumxx,slope,intercept
    integer :: i,x,y,n,rbin,ibin
    real,allocatable,dimension(:) ::radii, NP           !The arrays radii, and containing the amount of points enclosed    
    n = 3000                                            !The number of points
    dr = 2.0                                            !The change in the radius of the circles 
    rmin = 0                                            !The smallest possible radius
    rmax = 60                                           !The biggest possible radius
    sumx = 0                                            !
    sumy = 0                                            !   The constants used for 
    sumxy = 0                                           !   calculating slope and intercept
    sumxx = 0                                           !


    !Calculating and allocating bin size
    rbin=int((rmax-rmin)/real(dr))
    allocate(radii(rbin))
    allocate(Np(rbin))

    radii = 0                                          !Initiating the arrays
    NP = 0

    !Generating classmarks
    t = rmin + dr  
    do i=1,rbin
        radii(i)=t
        t=t+dr
    enddo



    open(1,file="Output.dat",status = "old")                           !The file that contains the coordinates of the points 

    do i = 1,n 
        read(1,*)x,y                                    !Collecting Coordinates
        r = sqrt((real(x)**2) + real(y)**2)             !Calculating the radius 

        !Sorting Datapoints
        ibin=int((r-rmin)/real(dr)) 
        NP(ibin)=NP(ibin) + 1

    end do 

    NP(1) = NP(1) + 1                                   !The (0,0) point isn't recorded hence the following
    
    open(2,file="Result.dat")                           !New output file
    write(2,*)radii(1),NP(1)
    do i=2,rbin
        !Cumulative points 
        NP(i) = NP(i) + NP(i-1)

        !Output
        write(2,*)radii(i),NP(i)
    enddo

    radii = log(radii)
    NP = log(NP)

    do i = 1,rbin                                 !Buffer are the points at the end we want to exclude
        sumx = radii(i) + sumx
        sumy = NP(i) + sumy
        sumxy = radii(i)*NP(i) + sumxy 
        sumxx = radii(i)**2 + sumxx
    end do

    slope = ((rbin*sumxy) - (sumx*sumy)) / real(rbin*(sumxx) - (sumx)**2)
    intercept = (sumy - slope*sumx)/real(rbin)

    write(*,*)slope
    write(*,*)intercept


end program FractalDim 
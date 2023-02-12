program D2rw  
    implicit none 

    !Variables 
    integer :: x,y,flag,count,counter,n,lattice
    real :: r1,r,rp,pi,r2,r3,bias
    integer,dimension(:,:),allocatable :: pos


    !Inputs
    n = 3000            !Required number of particles     
    lattice = 500       !The side length of the underlying lattice 
    r =  10             !Production boundary 
    rp = 5 * r          !Elimination boundary radius
    flag = 0            !flag if particle enters target area 
    count = 0           !how many particles entered the target area
    bias = 1.0          !The bias alloted to random walker
    pi = 2*asin(1.0)

    allocate(pos(-int(lattice):int(lattice),-int(lattice):int(lattice))) !Creating a square lattice 

    !Setting every lattice as 0 
    pos = 0

    pos(0,0) = 1                    !Central lattice is 1 

    open(1,file = "Output.dat")     !Output file 
    write(1,*)0,0
    do                              !Loop ends when n particles enter target zone 
        counter = 0 
        if (flag == 1) then         !particle counter
            count = count + 1
            write(1,*)x,y

            !Resizing the radius if the max distance is 80% of the radius
            if ((x**2) + (y**2) > (r**2)*0.64) then           
                r = int(sqrt(real((x**2) + (y**2)))) + 3  
                rp = r * 5          !Resizing Elimination Boundary
            end if 


        end if

        flag = 0                    !flag reset 

        if (count >= n) then        !exit condition 
            write(*,*)"We got the required number of particles"
            exit
        end if 
        
        !Generate a random angle 
        call random_number(r1)
        r1 = 2*pi*r1 

        !Generate x,y coordinates of a circle for walker 
        x = nint(r*cos(r1))
        y = nint(r*sin(r1))


    do      
        
        call random_number(r2)

        if (r2 > bias) then 
         
            !Loop for unbiased random walk
            call random_number(r1) 
            

            if (r1<=(0.250d0)) then              !Move to right
                x = x + 1
            else if (r1 <= (0.50d0)) then        !Move to left
                x = x - 1 
            else if (r1 <= (0.750d0)) then       !Move Up
                y = y + 1
            else                                 !Move down
                y = y - 1
            end if

        else 
            
            !Loop fo biased random walk 
            call random_number(r3)

            if (r3 >= 0.5) then 
                if (x > 0) then 
                    x = x - 1
                else if (x < 0) then
                    x = x + 1 
                else if (x == 0 .and. y > 0) then 
                    y = y - 1
                else if (x==0 .and. y < 0) then 
                    y = y + 1
                end if 
            else 
                if (y > 0) then 
                    y = y - 1
                else if (y < 0) then
                    y = y + 1 
                else if (y == 0 .and. x > 0) then 
                    x = x - 1
                else if (y==0 .and. x < 0) then 
                    x = x + 1
                end if 
            end if 


        end if 



        if (((x**2) + (y**2)) > (rp**2)) then !Checking for out of bounds condition 
            exit
        end if

        !Checking for Neighbours when the particle enters the radius 
        if (((x**2) + (y**2)) < (r**2)) then

            if (pos(x + 1,y) == 1) then 
                flag = 1 
                pos(x,y) = 1
                exit 
            else if (pos(x - 1,y) == 1) then 
                flag = 1 
                pos(x,y) = 1
                exit 
            else if (pos(x,y + 1) == 1) then 
                flag = 1 
                pos(x,y) = 1
                exit 
            else if (pos(x,y - 1) == 1) then 
                flag = 1 
                pos(x,y) = 1
                exit 
            end if 
        end if
        
        !Extra exit condition in case of an infinite loop 
        if (counter >= 1000000) then 
            write(*,*)"infinite loop"
            flag = 1 
            exit 
        end if

        counter = counter + 1 
    end do 
    

    end do



    



end program D2rw
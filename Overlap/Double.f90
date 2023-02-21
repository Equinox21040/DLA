program Overlap 
    implicit none  

    integer :: x,y,i,j,count 
    integer,allocatable,dimension(:) :: coords_x,coords_y

    open(1, file = 'Output.dat')

    allocate(coords_x(3001),coords_y(3001))

    do i = 1,3001 
        read(1,*)x,y 
        coords_x(i) = x 
        coords_y(i) = y 
    end do

    count = 0

    do i = 1,3000 
        do j = 1,3000
            if (coords_x(i) == coords_x(j) .and. coords_y(i) == coords_y(j) .and. i /= j) then 
                write(*,*)i,j
                count = count + 1

            end if 
        end do 
    end do
    
    write(*,*)count/2


end program Overlap 
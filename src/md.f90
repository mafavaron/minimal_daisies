! Module "md" - Contains all important computing functions used by 'minimal_daisies'
!
! Copyright 2020 by Patrizia Favaron
!
! This is open-source softeare, covered by the MIT license
!
module md

    implicit none
    
    private
    
    ! Public interface
    public  :: generate_admissible
    public  :: advance_time
    
contains

    subroutine generate_admissible(n, rvX, rvY, rmD)
    
        ! Routine arguments
        integer, intent(in)                             :: n    ! Positive by parameter check in calling program
        real, dimension(:), allocatable, intent(out)    :: rvX  ! Vector of buds' X coordinates
        real, dimension(:), allocatable, intent(out)    :: rvY  ! Vector of buds' Y coordinates
        real, dimension(:,:), allocatable, intent(out)  :: rmD  ! Distances matrix
        
        ! Locals
        integer :: i, j
        real    :: rX, rY
        
        ! Reserve workspace
        allocate(rvX(n))
        allocate(rvY(n))
        allocate(rmD(n,n))
        
        ! Main loop
        do i = 1, n
            do
                call random_number(rX)
                rX = rX*2. - 1.
                call random_number(rY)
                rY = rY*2. - 1.
                if(sqrt(rX**2 + rY**2) < 1.) exit
            end do
            rvX(i) = rX
            rvY(i) = rY
        end do
        
        ! Compute distance matrix
        do i = 1, n
            rmD(i,i) = 0.
            do j = i+1, n
                rmD(i,j) = sqrt((rvX(j) - rvX(i))**2 + (rvY(j) - rvY(i))**2)
                rmD(j,i) = rmD(i,j)
            end do
        end do
        
    end subroutine generate_admissible
    
    
    subroutine advance_time(rvX, rvY, rDeltaT)
    
        ! Routine arguments
        real, dimension(:), intent(inout)   :: rvX
        real, dimension(:), intent(inout)   :: rvY
        real, intent(in)                    :: rDeltaT
        
        ! Locals
        
        
    end subroutine advance_time

end module md

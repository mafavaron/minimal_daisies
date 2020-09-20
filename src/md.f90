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
    
contains

    subroutine generate_admissible(n, rvX, rvY)
    
        ! Routine arguments
        integer, intent(in)                             :: n    ! Positive by parameter check in calling program
        real, dimension(:), allocatable, intent(out)    :: rvX
        real, dimension(:), allocatable, intent(out)    :: rvY
        
        ! Locals
        integer :: i
        real    :: rX, rY
        
        ! Reserve workspace
        allocate(rvX(n))
        allocate(rvY(n))
        
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
        
    end subroutine generate_admissible

end module md

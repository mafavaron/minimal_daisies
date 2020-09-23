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

    subroutine generate_admissible(n, rvX, rvY, rmD, rvC)
    
        ! Routine arguments
        integer, intent(in)                             :: n    ! Positive by parameter check in calling program
        real, dimension(:), allocatable, intent(out)    :: rvX  ! Vector of buds' X coordinates
        real, dimension(:), allocatable, intent(out)    :: rvY  ! Vector of buds' Y coordinates
        real, dimension(:,:), allocatable, intent(out)  :: rmD  ! Distances between any two points
        real, dimension(:), allocatable, intent(out)    :: rvC  ! Distances between any point and center
        
        ! Locals
        integer :: i, j
        real    :: rX, rY
        
        ! Reserve workspace
        allocate(rvX(n))
        allocate(rvY(n))
        allocate(rmD(n,n))
        allocate(rvC(n))
        
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
            rvC(i)   = sqrt(rvX(i)**2 + rvY(i)**2)
            rmD(i,i) = 0.
            do j = i+1, n
                rmD(i,j) = sqrt((rvX(j) - rvX(i))**2 + (rvY(j) - rvY(i))**2)
                rmD(j,i) = rmD(i,j)
            end do
        end do
        
    end subroutine generate_admissible
    
    
    subroutine advance_time(rvX, rvY, rmD, rvC, rDeltaT, rAlpha, rBeta)
    
        ! Routine arguments
        real, dimension(:), intent(inout)   :: rvX
        real, dimension(:), intent(inout)   :: rvY
        real, dimension(:,:), intent(in)    :: rmD
        real, dimension(:), intent(in)      :: rvC
        real, intent(in)                    :: rDeltaT
        real, intent(in)                    :: rAlpha
        real, intent(in)                    :: rBeta
        
        ! Locals
        real, dimension(size(rvX))              :: rvFx
        real, dimension(size(rvX))              :: rvFy
        real, dimension(size(rvX), size(rvY))   :: rmGx
        real, dimension(size(rvX), size(rvY))   :: rmGy
        integer                                 :: n
        integer                                 :: i
        integer                                 :: j
        
        ! Constants
        real, parameter :: K1 = 1.e-1
        real, parameter :: K2 = 1.e-1
        
        ! Initialize
        n = size(rvX)
        
        ! Compute "peripheral strength" components
        rvFx = K1 * rvX * rvC**(rAlpha-1.)
        rvFy = K1 * rvY * rvC**(rAlpha-1.)
        
        ! Compute "reciprocal strength" components
        do i = 1, n
            do j = 1, n
                if(i /= j) then
                    rmGx(i,j) = rvX(i) - rvX(j) + (rvX(j) - rvX(i)) * (K2*rmD(i,j)**(-rBeta-1.) + 1.)
                    rmGy(i,j) = rvY(i) - rvY(j) + (rvY(j) - rvY(i)) * (K2*rmD(i,j)**(-rBeta-1.) + 1.)
                else
                    rmGx(i,j) = 0.
                    rmGy(i,j) = 0.
                end if
            end do
        end do
        
    end subroutine advance_time

end module md

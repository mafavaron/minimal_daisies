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
    public  :: PointType
    
    ! Data types
    
    type PointType
        real    :: rX
        real    :: rY
    contains
        procedure   :: GenerateRandom
        procedure   :: ForceFromUnitCircle
    end type PointType
    
contains

    function GenerateRandom(this) result(iNumIterations)
    
        ! Routine arguments
        class(PointType), intent(out)   :: this
        integer                         :: iNumIterations
        
        ! Locals
        integer :: iIteration
        
        ! Repeat point generation until its coordinates lie inside the unit circle
        ! (OK, maybe not the most clever way to do; but, this step is done only
        ! once per point on a whole simulation, so the possible extra-cost may be
        ! safely neglected)
        iIteration = 0
        do
            call random_number(this % rX)
            call random_number(this % rY)
            this % rX = 2. * this % rX - 1.
            this % rY = 2. * this % rY - 1.
            iIteration = iIteration + 1
            if(this % rX ** 2 + this % rY ** 2 < 1.) exit
        end do
        iNumIterations = iIteration
        
    end function GenerateRandom
    
    
    function ForceFromUnitCircle(this) result(tPoint)
    
        ! Routine arguments
        class(PointType), intent(in)    :: this
        type(PointType)                 :: tPoint
        
        ! Locals
        real    :: rDistanceFromCenter
        
        ! Compute the information desired
        rDistanceFromCenter = sqrt(this % rX ** 2 + this % rY ** 2)
        
    end function ForceFromUnitCircle
    
end module md

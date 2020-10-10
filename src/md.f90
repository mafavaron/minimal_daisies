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

    function GenerateRandom(this) result(rRho)
    
        ! Routine arguments
        class(PointType), intent(out)   :: this
        real                            :: rRho
        
        ! Locals
        real    :: rRho
        real    :: rTheta
        
        ! Constants
        real, parameter :: PI  = 3.1415926535
        real, parameter :: PI2 = 2. * PI
        
        ! Repeat point generation until its coordinates lie inside the unit circle
        ! (OK, maybe not the most clever way to do; but, this step is done only
        ! once per point on a whole simulation, so the possible extra-cost may be
        ! safely neglected)
        call random_number(rRho)
        call random_number(rTheta)
        this % rX = rRho * cos(rTheta*PI2)
        this % rY = rRho * sin(rTheta*PI2)
    
    end function GenerateRandom
    
    
    function ForceFromUnitCircle(this) result(tPoint)
    
        ! Routine arguments
        class(PointType), intent(in)    :: this
        type(PointType)                 :: tPoint
        
        ! Locals
        real    :: rDistanceFromCenter
        real    :: rDistanceFromCircle
        real    :: rMagnitude
        real    :: rFx
        real    :: rFy
        
        ! Compute the information desired
        rDistanceFromCenter = sqrt(this % rX ** 2 + this % rY ** 2)
        rDistanceFromCircle = 1. - rDistanceFromCenter
        rMagnitude          = rDistanceFromCircle ** -2.
        rFx                 = (this % rX / rDistanceFromCenter) * rMagnitude
        rFx                 = (this % rY / rDistanceFromCenter) * rMagnitude
        
    end function ForceFromUnitCircle
    
end module md

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
        procedure   :: GenerateDeterministic
        procedure   :: ForceFromUnitCircle
    end type PointType
    
contains

    function GenerateRandom(this) result(rRho)
    
        ! Routine arguments
        class(PointType), intent(out)   :: this
        real                            :: rRho
        
        ! Locals
        real    :: rTheta
        
        ! Constants
        real, parameter :: PI  = 3.1415926535
        real, parameter :: PI2 = 2. * PI
        
        ! Generate point
        call random_number(rRho)
        call random_number(rTheta)
        this % rX = rRho * cos(rTheta*PI2)
        this % rY = rRho * sin(rTheta*PI2)
    
    end function GenerateRandom
    
    
    subroutine GenerateDeterministic(this, rRho, rTheta)
    
        ! Routine arguments
        class(PointType), intent(out)   :: this
        real, intent(in)                :: rRho
        real, intent(in)                :: rTheta
        
        ! Locals
        real    :: rAlpha
        
        ! Check input parameters
        rAlpha = abs(rRho)
        if(rAlpha > 1.) rAlpha = 1. / rAlpha
        
        ! Compute position
        this % rX = rRho * cos(rAlpha)
        this % rY = rRho * sin(rAlpha)
    
    end subroutine GenerateDeterministic
    
    
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

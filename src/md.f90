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
    public  :: VectorType
    
    ! Data types
    
    type VectorType
        real    :: rX
        real    :: rY
    contains
        procedure   :: VectorAdd
        procedure   :: Length
        procedure   :: GenerateRandom
        procedure   :: GenerateDeterministic
        procedure   :: ForceFromUnitCircle
        procedure   :: ForceFromPoint
        procedure   :: Update
        procedure   :: Anneal
    end type VectorType
    
contains
    
    function VectorAdd(this, tVectorB) result(tVector)
    
        ! Routine arguments
        class(VectorType), intent(in)   :: this
        type(VectorType), intent(in)    :: tVectorB
        type(VectorType)                :: tVector
        
        ! Locals
        ! --none--
        
        ! Compute the sum of vectors
        tVector % rX = this % rX + tVectorB % rX
        tVector % rY = this % rY + tVectorB % rY
        
    end function VectorAdd
    
    
    function Length(this) result(rLength)
    
        ! Routine arguments
        class(VectorType), intent(in)   :: this
        real                            :: rLength
        
        ! Locals
        ! --none--
        
        ! Compute the information desired
        rLength = sqrt(this % rX ** 2 + this % rY ** 2)
        
    end function Length
    

    subroutine GenerateRandom(this)
    
        ! Routine arguments
        class(VectorType), intent(out)  :: this
        
        ! Locals
        real    :: rRho
        real    :: rTheta
        
        ! Constants
        real, parameter :: PI  = 3.1415926535
        real, parameter :: PI2 = 2. * PI
        
        ! Generate point
        call random_number(rRho)
        call random_number(rTheta)
        this % rX = rRho * cos(rTheta*PI2)
        this % rY = rRho * sin(rTheta*PI2)
    
    end subroutine GenerateRandom
    
    
    subroutine GenerateDeterministic(this, rRho, rTheta)
    
        ! Routine arguments
        class(VectorType), intent(out)  :: this
        real, intent(in)                :: rRho
        real, intent(in)                :: rTheta
        
        ! Locals
        real    :: rAlpha
        
        ! Check input parameters
        rAlpha = abs(rRho)
        if(rAlpha > 1.) rAlpha = 1. / rAlpha
        
        ! Compute position
        this % rX = rAlpha * cos(rTheta)
        this % rY = rAlpha * sin(rTheta)
    
    end subroutine GenerateDeterministic
    
    
    function ForceFromUnitCircle(this) result(tForce)
    
        ! Routine arguments
        class(VectorType), intent(in)   :: this
        type(VectorType)                :: tForce
        
        ! Locals
        real    :: rDistanceFromCenter
        real    :: rDistanceFromCircle
        real    :: rMagnitude
        
        ! Compute the information desired
        rDistanceFromCenter = sqrt(this % rX ** 2 + this % rY ** 2)
        if(rDistanceFromCenter <= 1.e-6) then
            tForce % rX         = 0.0
            tForce % rY         = 0.0
        else
            rDistanceFromCircle = 1. - rDistanceFromCenter
            rMagnitude          = rDistanceFromCircle ** -2.
            tForce % rX         = -(this % rX / rDistanceFromCenter) * rMagnitude
            tForce % rY         = -(this % rY / rDistanceFromCenter) * rMagnitude
        end if
        
    end function ForceFromUnitCircle
    
    
    function ForceFromPoint(this, tOtherPoint) result(tForce)
    
        ! Routine arguments
        class(VectorType), intent(in)   :: this
        type(VectorType), intent(in)    :: tOtherPoint
        type(VectorType)                :: tForce
        
        ! Locals
        real    :: rDistance
        real    :: rMagnitude
        real    :: rDx
        real    :: rDy
        
        ! Compute distance and force magnitude
        rDistance = sqrt((this % rX - tOtherPoint % rX)**2 + (this % rY - tOtherPoint % rY)**2)
        rMagnitude = 1. / rDistance ** 2
        
        ! To express direction, we may write the position of the
        ! "main" point relative to the "other", and use this data
        ! as we made with force from circle
        rDx = this % rX - tOtherPoint % rX
        rDy = this % rY - tOtherPoint % rY
        tForce % rX = (rDx / rDistance) * rMagnitude
        tForce % rY = (rDy / rDistance) * rMagnitude
        
    end function ForceFromPoint
    
    
    subroutine Update(this, tForce, rDeltaTime)
    
        ! Routine arguments
        class(VectorType), intent(inout)    :: this
        type(VectorType), intent(in)        :: tForce
        real, intent(in)                    :: rDeltaTime
        
        ! Locals
        real    :: rVectorLength
        
        ! Contraction step (which, incidentally, has nothing to do with F = m*a)
        this % rX = rDeltaTime * tForce % rX + this % rX
        this % rY = rDeltaTime * tForce % rY + this % rY
        
        ! Check the point has reached or exceeded the unit circle
        rVectorLength = sqrt(this % rX ** 2 + this % rY ** 2)
        if(rVectorLength >= 1.) then
            this % rX = this % rX / (1.0001 * rVectorLength)
            this % rY = this % rY / (1.0001 * rVectorLength)
        end if
        
    end subroutine Update
    
    
    subroutine Anneal(tvPoint, rIntensity)
    
        ! Routine arguments
        type(VectorType), dimension(:), intent(inout)   :: tvPoint
        real, intent(inout)                             :: rIntensity
        
        ! Locals
        integer                                     :: iNumPoints
        real                                        :: rVectorLength
        type(VectorType), dimension(:), allocatable :: tvForce
        type(VectorType), dimension(:), allocatable :: tvNewPoint
        
        ! Reserve workspace
        iNumPoints = size(tvPoint)
        allocate(tvForce(iNumPoints))
        allocate(tvNewPoint(iNumPoints))
        
        ! Leave
        deallocate(tvNewPoint)
        deallocate(tvForce)
        
    end subroutine Anneal
    
end module md

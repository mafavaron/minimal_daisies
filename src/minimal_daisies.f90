! Program exploring the "minimal energy daisies" 
!
! Copyright 2020 by Patrizia Favaron
!
! This is open-source softeare, covered by the MIT license
!
program Minimal_Daisies

    use md
    
    implicit none
    
    ! Locals
    integer                                     :: iErrCode
    integer                                     :: iNumPoints
    real                                        :: rExponent
    character(len=256)                          :: sOutFile
    character(len=16)                           :: sBuffer
    integer                                     :: i
    integer                                     :: iNumIterations
    type(VectorType), dimension(:), allocatable :: tvPoint
    type(VectorType)                            :: tPoint
    type(VectorType)                            :: tOtherPoint
    type(VectorType)                            :: tCentralForce
    type(VectorType)                            :: tPointForce
    
    ! Get parameters
    if(command_argument_count() /= 3) then
        print *, 'Minimal_Daisies - Program demonstrating use of simulated annealing'
        print *, '                  for finding a minimal energy solution'
        print *
        print *, 'Usage:'
        print *
        print *, '  md <Num_Points> <Exponent> <Out_File>'
        print *
        print *, 'Copyright by Patrizia Favaron'
        print *, 'This is open-source software, covered by the MIT license'
        print *
        stop
    end if
    call get_command_argument(1, sBuffer)
    read(sBuffer, *, iostat=iErrCode) iNumPoints
    if(iErrCode /= 0 .or. iNumPoints <= 0) then
        print *, 'md:: error: Invalid number of points'
        stop
    end if
    call get_command_argument(2, sBuffer)
    read(sBuffer, *, iostat=iErrCode) rExponent
    if(iErrCode /= 0 .or. rExponent <= 0) then
        print *, 'md:: error: Invalid number of points'
        stop
    end if
    call get_command_argument(3, sOutFile)
    
    ! Generate an initial admissible configuration
    allocate(tvPoint(iNumPoints))
    do i = 1, iNumPoints
        iNumIterations = tvPoint(i) % GenerateRandom()
    end do
    
    ! Debug: Advance one time step
    print *, "*** Test 1 ***"
    
    do i = 1, 8
        call tPoint % GenerateDeterministic(0.5, ((i-1) / 8.) * 2. * 3.1415926535)
        tCentralForce = tPoint % ForceFromUnitCircle()
        print "(i3,2(1x,f6.3),2(1x,f7.3))", i, tPoint % rX, tPoint % rY, tCentralForce % rX, tCentralForce % rY
    end do
    
    print *, "*** Test 2 ***"
    
    do i = 1, 5
        call tPoint % GenerateDeterministic(0.2*(i-1), 0.0)
        tCentralForce = tPoint % ForceFromUnitCircle()
        print "(i3,2(1x,f6.3),2(1x,f7.3))", i, tPoint % rX, tPoint % rY, tCentralForce % rX, tCentralForce % rY
    end do
    
    print *, "*** Test 3 ***"
    
    tOtherPoint = VectorType(0., 0.)
    do i = 1, 8
        call tPoint % GenerateDeterministic(0.5, ((i-1) / 8.) * 2. * 3.1415926535)
        tCentralForce = tPoint % ForceFromUnitCircle()
        tPointForce   = tPoint % ForceFromPoint(tOtherPoint)
        print "(i3,2(1x,f6.3),4(1x,f7.3))", i, &
            tPoint % rX, tPoint % rY, &
            tCentralForce % rX, tCentralForce % rY, &
            tPointForce % rX, tPointForce % rY
    end do
    
    ! Leave
    deallocate(tvPoint)
    print *, "*** End Job ***"

end program Minimal_Daisies

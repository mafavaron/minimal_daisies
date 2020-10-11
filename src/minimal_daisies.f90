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
    real                                        :: rMaxForce
    real                                        :: rTimeStep
    real                                        :: rTotForce
    character(len=256)                          :: sOutFile
    character(len=16)                           :: sBuffer
    integer                                     :: i
    integer                                     :: j
    integer                                     :: iNumIterations
    type(VectorType), dimension(:), allocatable :: tvPoint
    type(VectorType), dimension(:), allocatable :: tvForce
    
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
    allocate(tvForce(iNumPoints))
    do i = 1, iNumPoints
        call tvPoint(i) % GenerateRandom()
    end do
    
    ! Main loop
    iNumIterations = 0
    do
    
        ! Generate the composed force for each point
        do i = 1, iNumPoints
            tvForce(i) = tvPoint(i) % ForceFromUnitCircle()
            do j = 1, iNumPoints
                if(i /= j) then
                    tvForce(i) = tvForce(i) % VectorAdd(tvPoint(i) % ForceFromPoint(tvPoint(j)))
                end if
            end do
        end do
        
        ! Compute the maximum force and, from it, the next time step
        rMaxForce = 0.
        do i = 1, iNumPoints
            rMaxForce = max(rMaxForce, tvForce(i) % Length())
        end do
        rTimeStep = 0.01 / max(1.,rMaxForce/10.)
        
        ! Perform one iteration
        do i = 1, iNumPoints
            call tvPoint(i) % Update(tvForce(i), rTimeStep)
        end do
        
        ! Compute the sum of absolute forces acting on points
        rTotForce = 0.
        do i = 1, iNumPoints
            rTotForce = rTotForce + tvForce(i) % Length()
        end do
        
        ! Dia print
        print *, "Total absolute force: ", rTotForce, "   Iteration = ", iNumIterations
        
        iNumIterations = iNumIterations + 1
        
        if(rTotForce <= 0.00001 .or. iNumIterations > 1000) exit
        
    end do
    
    ! Save data
    open(10, file=sOutFile, status='unknown', action='write')
    write(10, "('X, Y')")
    do i = 1, iNumPoints
        write(10, "(f8.5,',',f8.5)") tvPoint(i) % rX, tvPoint(i) % rY
    end do
    close(10)
    
    ! Leave
    deallocate(tvForce)
    deallocate(tvPoint)
    print *, "*** End Job ***"

end program Minimal_Daisies

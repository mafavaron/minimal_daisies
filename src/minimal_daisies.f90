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
    integer                             :: iErrCode
    integer                             :: iNumPoints
    real                                :: rExponent
    real, dimension(:), allocatable     :: rvX
    real, dimension(:), allocatable     :: rvY
    real, dimension(:,:), allocatable   :: rmD
    real, dimension(:), allocatable     :: rvC
    character(len=256)                  :: sOutFile
    character(len=16)                   :: sBuffer
    
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
    call generate_admissible(iNumPoints, rvX, rvY, rmD, rvC, .true.)
    
    ! Debug: Advance one time step
    call advance_time(rvX, rvY, rmD, rvC, 0.01, rExponent, rExponent)

end program Minimal_Daisies

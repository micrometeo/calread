! commands.f90 - Parse and execute computing commands
!
! =============================================================================
!
! MIT License
!
! Copyright (c) 2023 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
module commands

    use cal_get
    use calendar
    
    implicit none
    
    private

    ! Public interface
    public  :: parse
    public  :: execute
    
    ! Command tables
    integer, parameter  :: CMD_MSE      = 1
    integer, parameter  :: CMD_MAX_DIFF = 2
    integer, parameter  :: CMD_R        = 3
    integer, parameter  :: CMD_MEAN     = 4
    integer, parameter  :: CMD_VAR      = 5
    integer, parameter  :: CMD_MAX      = 6
    integer, parameter  :: CMD_SERIES   = 7
    integer, parameter  :: CMD_DIFF     = 8
    character(len=*), dimension(*), parameter   :: COMMAND = [ &
        "mse_series     ", &
        "max_diff_series", &
        "correlation_map", &
        "mean_map       ", &
        "var_map        ", &
        "max_map        ", &
        "all_series     ", &
        "difference     " &
    ]
    logical, dimension(*), parameter            :: DUAL = [ &
        .true., &
        .true., &
        .true., &
        .false., &
        .false., &
        .false., &
        .true., &
        .true. &
    ]

contains

    function parse(sCommand, iCmdNumber, lDualOnly, sFileName) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)    :: sCommand
        integer, intent(out)            :: iCmdNumber
        logical, intent(out)            :: lDualOnly
        character(len=*), intent(out)   :: sFileName
        integer                         :: iRetCode
    
        ! Locals
        character(len=512)  :: sCmdString
        character(len=256)  :: sCmdName
        integer             :: iCmdSize
        integer             :: iPos
        integer             :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Get command name part
        sCmdString = adjustl(sCommand)
        iCmdSize = len_trim(sCmdString)
        if(iCmdSize <= 0) then
            iRetCode = -1
            return
        end if
        iPos = index(sCmdString, " ")
        if(iPos >= iCmdSize) then
            ! No blanks within: surely invalid
            iRetCode = 2
            return
        end if
        sCmdName = sCmdString(1:iPos-1)
    
        ! Lookup for command name
        iCmdNumber = 0
        do i = 1, size(COMMAND)
            if(sCmdName == COMMAND(i)) then
                iCmdNumber = i
                lDualOnly  = DUAL(i)
                exit
            end if
        end do
        if(iCmdNumber <= 0) then
            iRetCode = 3
            return
        end if
    
        ! Get file name
        sCmdString = adjustl(sCmdString(iPos+1:))
        iCmdSize = len_trim(sCmdString)
        if(iCmdSize <= 0) then
            iRetCode = 4
            return
        end if
        iPos = index(sCmdString, " ")
        if(iPos < iCmdSize) then
            ! Some blanks within: surely invalid
            iRetCode = 5
            return
        end if
        sFileName = sCmdString
    
    end function parse


    function execute(iCmdNumber, lDualOnly, sFileName, lDual, ivDateTime, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        integer, intent(in)                             :: iCmdNumber
        logical, intent(in)                             :: lDualOnly
        character(len=*), intent(in)                    :: sFileName
        logical, intent(in)                             :: lDual
        integer, dimension(:), intent(in)               :: ivDateTime
        type(CalpuffType), intent(in)                   :: tRef
        real, dimension(:,:,:), intent(in)              :: raRefConc
        type(CalpuffType), intent(in), optional         :: tCmp
        real, dimension(:,:,:), intent(in), optional    :: raCmpConc
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iErrCode
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Check the "optional" passed parameters are consistent with the dual nature
        ! of run or not
        if(present(tCmp) .and. .not.present(raCmpConc)) then
            iRetCode = 1
            return
        end if
        if(lDual .and. .not.present(tCmp)) then
            iRetCode = 2
            return
        end if
        if(lDual .and. .not.lDualOnly) then
            iRetCode = 3
            return
        end if
    
        ! Check the command number to make sense
        if(iCmdNumber <= 0 .or. iCmdNumber > size(COMMAND)) then
            iRetCode = 4
            return
        end if
    
        ! Dispatch execution based on command number
        select case(iCmdNumber)
        case(CMD_MSE)
            iErrCode = GetMSE(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 10
        case(CMD_MAX_DIFF)
            iErrCode = GetMaxDiff(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 20
        case(CMD_R)
            iErrCode = GetR(sFileName, tRef, raRefConc, tCmp, raCmpConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 30
        case(CMD_MEAN)
            iErrCode = GetMean(sFileName, tRef, raRefConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 40
        case(CMD_VAR)
            iErrCode = GetVar(sFileName, tRef, raRefConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 50
        case(CMD_MAX)
            iErrCode = GetMax(sFileName, tRef, raRefConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 60
        case(CMD_SERIES)
            iErrCode = GetSeries(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 60
        case(CMD_DIFF)
            iErrCode = GetDifference(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc)
            if(iErrCode /= 0) iRetCode = iErrCode + 60
        case default
            ! Unknown command (should never happen after the inputs check)
            iRetCode = 70
        end select
    
    end function execute
    
    ! *********************
    ! * Internal Rourines *
    ! *********************
    
    ! Tier 1: Assemble results
    
    function GetDifference(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        integer, dimension(:), intent(in)       :: ivDateTime
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        type(CalpuffType), intent(in)           :: tCmp
        real, dimension(:,:,:), intent(in)      :: raCmpConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:), allocatable         :: rvResult
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: iYear
        integer                                 :: iMonth
        integer                                 :: iDay
        integer                                 :: iHour
        integer                                 :: iMinute
        integer                                 :: iSecond
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeDifference(raRefConc, raCmpConc, rvResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Date.Time, Difference')")
        do i = 1, size(ivDateTime)
            call UnpackTime(ivDateTime(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
            write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7)") &
                iYear, iMonth, iDay, iHour, iMinute, iSecond, &
                rvResult(i)
        end do
        close(iLUN)
        
    end function GetDifference
    
    
    function GetSeries(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        integer, dimension(:), intent(in)       :: ivDateTime
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        type(CalpuffType), intent(in)           :: tCmp
        real, dimension(:,:,:), intent(in)      :: raCmpConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:), allocatable         :: rvMSE
        real, dimension(:), allocatable         :: rvMaxDiff
        real, dimension(:), allocatable         :: rvTimeMeanRef
        real, dimension(:), allocatable         :: rvTimeMeanCmp
        real, dimension(:), allocatable         :: rvTimeMaxRef
        real, dimension(:), allocatable         :: rvTimeMaxCmp
        integer, dimension(:,:), allocatable    :: imTimeZeros
        integer, dimension(:,:), allocatable    :: imTimeNonZeros
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: iYear
        integer                                 :: iMonth
        integer                                 :: iDay
        integer                                 :: iHour
        integer                                 :: iMinute
        integer                                 :: iSecond
		real									:: rConcordance
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeMSE(raRefConc, raCmpConc, rvMSE)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
        iErrCode = ComputeMaxDiff(raRefConc, raCmpConc, rvMaxDiff)
        if(iErrCode /= 0) then
            iRetCode = 20 + iErrCode
            return
        end if
        iErrCode = ComputeTimeMean(raRefConc, rvTimeMeanRef)
        if(iErrCode /= 0) then
            iRetCode = 40 + iErrCode
            return
        end if
        iErrCode = ComputeTimeMean(raCmpConc, rvTimeMeanCmp)
        if(iErrCode /= 0) then
            iRetCode = 50 + iErrCode
            return
        end if
        iErrCode = ComputeTimeMax(raRefConc, rvTimeMaxRef)
        if(iErrCode /= 0) then
            iRetCode = 60 + iErrCode
            return
        end if
        iErrCode = ComputeTimeMax(raCmpConc, rvTimeMaxCmp)
        if(iErrCode /= 0) then
            iRetCode = 70 + iErrCode
            return
        end if
        iErrCode = ComputeTimeZeros(raRefConc, raCmpConc, imTimeZeros)
        if(iErrCode /= 0) then
            iRetCode = 80 + iErrCode
            return
        end if
        iErrCode = ComputeTimeNonZeros(raRefConc, raCmpConc, imTimeNonZeros)
        if(iErrCode /= 0) then
            iRetCode = 85 + iErrCode
            return
        end if
    
        ! Write result
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 90 + iErrCode
            return
        end if
        write(iLUN, "('Date.Time, " // &
            "MSE, Max.Diff, Mean.Ref, Mean.Cmp, Max.Ref, Max.Cmp, " // &
            "Zeros.Ref, Zeros.Cmp, Zeros.RefCmp, Zeros.Both, Zeros.Any, " // &
			"Non.Zeros.Ref, Non.Zeros.Cmp, Non.Zeros.Both, Non.Zeros.Any, " // &
			"Concordance')")
        do i = 1, size(ivDateTime)
            call UnpackTime(ivDateTime(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			if(imTimeNonZeros(i,4) > 0) then
				rConcordance = real(imTimeNonZeros(i,3),kind=4)/real(imTimeNonZeros(i,4),kind=4)
			else
				rConcordance = 0.0
			end if
            write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),6(',',e15.7),9(',',i5),',',f9.7)") &
                iYear, iMonth, iDay, iHour, iMinute, iSecond, &
                rvMSE(i), rvMaxDiff(i), rvTimeMeanRef(i), rvTimeMeanCmp(i), &
                rvTimeMaxRef(i), rvTimeMaxCmp(i), imTimeZeros(i,1:5), imTimeNonZeros(i,1:4), &
				rConcordance
        end do
        close(iLUN)
        
    end function GetSeries
    
    
    function GetMSE(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        integer, dimension(:), intent(in)       :: ivDateTime
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        type(CalpuffType), intent(in)           :: tCmp
        real, dimension(:,:,:), intent(in)      :: raCmpConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:), allocatable         :: rvResult
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: iYear
        integer                                 :: iMonth
        integer                                 :: iDay
        integer                                 :: iHour
        integer                                 :: iMinute
        integer                                 :: iSecond
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeMSE(raRefConc, raCmpConc, rvResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Date.Time, MSE')")
        do i = 1, size(ivDateTime)
            call UnpackTime(ivDateTime(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
            write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7)") &
                iYear, iMonth, iDay, iHour, iMinute, iSecond, &
                rvResult(i)
        end do
        close(iLUN)
        
    end function GetMSE
    
    
    function GetMaxDiff(sFileName, ivDateTime, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        integer, dimension(:), intent(in)       :: ivDateTime
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        type(CalpuffType), intent(in)           :: tCmp
        real, dimension(:,:,:), intent(in)      :: raCmpConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:), allocatable         :: rvResult
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: iYear
        integer                                 :: iMonth
        integer                                 :: iDay
        integer                                 :: iHour
        integer                                 :: iMinute
        integer                                 :: iSecond
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeMaxDiff(raRefConc, raCmpConc, rvResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Date.Time, Max.Diff')")
        do i = 1, size(ivDateTime)
            call UnpackTime(ivDateTime(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
            write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7)") &
                iYear, iMonth, iDay, iHour, iMinute, iSecond, &
                rvResult(i)
        end do
        close(iLUN)
        
    end function GetMaxDiff
    
    
    function GetR(sFileName, tRef, raRefConc, tCmp, raCmpConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        type(CalpuffType), intent(in)           :: tCmp
        real, dimension(:,:,:), intent(in)      :: raCmpConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:,:), allocatable       :: rmResult
        real(8), dimension(:), allocatable      :: rvX
        real(8), dimension(:), allocatable      :: rvY
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeR(raRefConc, raCmpConc, rmResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        iErrCode = GatherGridCoord(tRef, rvX, rvY)
        if(iErrCode /= 0) then
            iRetCode = 20 + iErrCode
            return
        end if
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Easting, Northing, R')")
        do i = 1, size(rvX)
            do j = 1, size(rvY)
                write(iLUN, "(f10.2,',',f10.2,',',e15.7)") rvX(i), rvY(j), rmResult(i,j)
            end do
        end do
        close(iLUN)
        
    end function GetR
    
    
    function GetMean(sFileName, tRef, raRefConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:,:), allocatable       :: rmResult
        real(8), dimension(:), allocatable      :: rvX
        real(8), dimension(:), allocatable      :: rvY
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeMean(raRefConc, rmResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        iErrCode = GatherGridCoord(tRef, rvX, rvY)
        if(iErrCode /= 0) then
            iRetCode = 20 + iErrCode
            return
        end if
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Easting, Northing, Mean')")
        do i = 1, size(rvX)
            do j = 1, size(rvY)
                write(iLUN, "(f10.2,',',f10.2,',',e15.7)") rvX(i), rvY(j), rmResult(i,j)
            end do
        end do
        close(iLUN)
        
    end function GetMean
    
    
    function GetVar(sFileName, tRef, raRefConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:,:), allocatable       :: rmResult
        real(8), dimension(:), allocatable      :: rvX
        real(8), dimension(:), allocatable      :: rvY
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeVar(raRefConc, rmResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        iErrCode = GatherGridCoord(tRef, rvX, rvY)
        if(iErrCode /= 0) then
            iRetCode = 20 + iErrCode
            return
        end if
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Easting, Northing, Var')")
        do i = 1, size(rvX)
            do j = 1, size(rvY)
                write(iLUN, "(f10.2,',',f10.2,',',e15.7)") rvX(i), rvY(j), rmResult(i,j)
            end do
        end do
        close(iLUN)
        
    end function GetVar
    
    
    function GetMax(sFileName, tRef, raRefConc) result(iRetCode)
    
        ! Routine arguments
        character(len=*), intent(in)            :: sFileName
        type(CalpuffType), intent(in)           :: tRef
        real, dimension(:,:,:), intent(in)      :: raRefConc
        integer                                 :: iRetCode
    
        ! Locals
        real, dimension(:,:), allocatable       :: rmResult
        real(8), dimension(:), allocatable      :: rvX
        real(8), dimension(:), allocatable      :: rvY
        integer                                 :: iErrCode
        integer                                 :: iLUN
        integer                                 :: i
        integer                                 :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Compute result
        iErrCode = ComputeMax(raRefConc, rmResult)
        if(iErrCode /= 0) then
            iRetCode = 10 + iErrCode
            return
        end if
    
        ! Write result
        iErrCode = GatherGridCoord(tRef, rvX, rvY)
        if(iErrCode /= 0) then
            iRetCode = 20 + iErrCode
            return
        end if
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 30 + iErrCode
            return
        end if
        write(iLUN, "('Easting, Northing, Max')")
        do i = 1, size(rvX)
            do j = 1, size(rvY)
                write(iLUN, "(f10.2,',',f10.2,',',e15.7)") rvX(i), rvY(j), rmResult(i,j)
            end do
        end do
        close(iLUN)
        
    end function GetMax
    
    ! Tier 2: Compute statistics
    
    function GatherGridCoord(tCalp, rvX, rvY) result(iRetCode)
    
        ! Routine arguments
        type(CalpuffType), intent(in)                   :: tCalp
        real(8), dimension(:), allocatable, intent(out) :: rvX
        real(8), dimension(:), allocatable, intent(out) :: rvY
        integer                                         :: iRetCode
    
        ! Locals
        integer :: i
        real(8) :: rX0
        real(8) :: rY0
        real(8) :: rDelta
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        if(allocated(rvX)) deallocate(rvX)
        if(allocated(rvY)) deallocate(rvY)
        allocate(rvX(tCalp % iNxSampling))
        allocate(rvY(tCalp % iNySampling))
    
        ! Get the information desired
        rDelta = 1000.d0 * tCalp % rDx / tCalp % iMeshFactor
        rX0    = 1000.d0 * (tCalp % rX0 + tCalp % rDx * (tCalp % iMinSampX - 1) + tCalp % rDx / 2.)
        rY0    = 1000.d0 * (tCalp % rY0 + tCalp % rDy * (tCalp % iMinSampY - 1) + tCalp % rDy / 2.)
        do i = 1, tCalp % iNxSampling
            rvX(i) = rX0 + (i - 1) * rDelta
        end do
        do i = 1, tCalp % iNySampling
            rvY(i) = rY0 + (i - 1) * rDelta
        end do
    
    end function GatherGridCoord
    
    
    function ComputeMean(raConc, rmResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc
        real, dimension(:,:), allocatable, intent(out)  :: rmResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
        integer :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iNx = size(raConc, dim=2)
        iNy = size(raConc, dim=3)
        if(allocated(rmResult)) deallocate(rmResult)
        allocate(rmResult(iNx,iNy))
        rmResult = 0.
    
        ! Iterate over components
        iSeriesLength = size(raConc, dim=1)
    
        ! Get the information desired
        do i = 1, iNx
            do j = 1, iNy
                rmResult(i,j) = sum(raConc(:,i,j)) / iSeriesLength
            end do
        end do
    
    end function ComputeMean
    
    
    function ComputeVar(raConc, rmResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc
        real, dimension(:,:), allocatable, intent(out)  :: rmResult
        integer                                         :: iRetCode
    
        ! Locals
        integer                             :: jErrCode
        integer                             :: iNx
        integer                             :: iNy
        integer                             :: iSeriesLength
        integer                             :: iErrCode
        integer                             :: i
        integer                             :: j
        real                                :: rMean
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iNx = size(raConc, dim=2)
        iNy = size(raConc, dim=3)
        if(allocated(rmResult)) deallocate(rmResult)
        allocate(rmResult(iNx,iNy), stat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        rmResult = 0.
    
        ! Iterate over components
        iSeriesLength = size(raConc, dim=1)
        do i = 1, iNx
            do j = 1, iNy
                rMean = sum(raConc(:,i,j)) / iSeriesLength
                rmResult(i,j) = sum((raConc(:,i,j) - rMean)**2) / iSeriesLength
            end do
        end do
    
    end function ComputeVar
    
    
    function ComputeMax(raConc, rmResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc
        real, dimension(:,:), allocatable, intent(out)  :: rmResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
        integer :: j
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iNx = size(raConc, dim=2)
        iNy = size(raConc, dim=3)
        if(allocated(rmResult)) deallocate(rmResult)
        allocate(rmResult(iNx,iNy))
        rmResult = 0.
    
        ! Iterate over components
        iSeriesLength = size(raConc, dim=1)
        do i = 1, iNx
            do j = 1, iNy
                rmResult(i,j) = maxval(raConc(:,i,j))
            end do
        end do
    
    end function ComputeMax
    
    
    function ComputeTimeMean(raConc1, rvResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:), allocatable, intent(out)    :: rvResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(rvResult)) deallocate(rvResult)
        allocate(rvResult(iSeriesLength))
        rvResult = 0.
    
        ! Get the information desired
        do i = 1, iSeriesLength
            rvResult(i) = sum(raConc1(i,:,:)) / (iNx*iNy)
        end do
    
    end function ComputeTimeMean
    
    
    function ComputeTimeMax(raConc1, rvResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:), allocatable, intent(out)    :: rvResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(rvResult)) deallocate(rvResult)
        allocate(rvResult(iSeriesLength))
        rvResult = 0.
    
        ! Get the information desired
        do i = 1, iSeriesLength
            rvResult(i) = maxval(raConc1(i,:,:))
        end do
    
    end function ComputeTimeMax
    
    
    function ComputeTimeZeros(raConc1, raConc2, imResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              	:: raConc1
        real, dimension(:,:,:), intent(in)              	:: raConc2
        integer, dimension(:,:), allocatable, intent(out)  	:: imResult
        integer                                         	:: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(imResult)) deallocate(imResult)
        allocate(imResult(iSeriesLength, 5))
        imResult = 0
    
        ! Get the information desired
        do i = 1, iSeriesLength
            imResult(i,1) = count(raConc1(i,:,:) <= 0.)
            imResult(i,2) = count(raConc2(i,:,:) <= 0.)
            imResult(i,3) = count(raConc1(i,:,:)*raConc2(i,:,:) <= 0.)
            imResult(i,4) = count(raConc1(i,:,:) <= 0. .and. raConc2(i,:,:) <= 0.)
            imResult(i,5) = count(raConc1(i,:,:) <= 0. .or. raConc2(i,:,:) <= 0.)
        end do
    
    end function ComputeTimeZeros
    
    
    function ComputeTimeNonZeros(raConc1, raConc2, imResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              	:: raConc1
        real, dimension(:,:,:), intent(in)              	:: raConc2
        integer, dimension(:,:), allocatable, intent(out)  	:: imResult
        integer                                         	:: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(imResult)) deallocate(imResult)
        allocate(imResult(iSeriesLength, 4))
        imResult = 0
    
        ! Get the information desired
        do i = 1, iSeriesLength
            imResult(i,1) = count(raConc1(i,:,:) > 0.)
            imResult(i,2) = count(raConc2(i,:,:) > 0.)
            imResult(i,3) = count(raConc1(i,:,:) > 0. .and. raConc2(i,:,:) > 0.)
            imResult(i,4) = count(raConc1(i,:,:) > 0. .or. raConc2(i,:,:) > 0.)
        end do
    
    end function ComputeTimeNonZeros
    
    
    function ComputeMSE(raConc1, raConc2, rvResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:,:,:), intent(in)              :: raConc2
        real, dimension(:), allocatable, intent(out)    :: rvResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(rvResult)) deallocate(rvResult)
        allocate(rvResult(iSeriesLength))
        rvResult = 0.
    
        ! Get the information desired
        do i = 1, iSeriesLength
            rvResult(i) = sum((raConc1(i,:,:) - raConc2(i,:,:))**2) / (iNx*iNy)
        end do
    
    end function ComputeMSE
    
    
    function ComputeDifference(raConc1, raConc2, rvResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:,:,:), intent(in)              :: raConc2
        real, dimension(:), allocatable, intent(out)    :: rvResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        if(allocated(rvResult)) deallocate(rvResult)
        allocate(rvResult(iSeriesLength))
        rvResult = 0.
    
        ! Get the information desired
        do i = 1, iSeriesLength
            rvResult(i) = sum(raConc1(i,:,:) - raConc2(i,:,:))
        end do
    
    end function ComputeDifference
    
    
    function ComputeMaxDiff(raConc1, raConc2, rvResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:,:,:), intent(in)              :: raConc2
        real, dimension(:), allocatable, intent(out)    :: rvResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(rvResult)) deallocate(rvResult)
        allocate(rvResult(iSeriesLength))
        rvResult = 0.
    
        ! Get the information desired
        do i = 1, iSeriesLength
            rvResult(i) = maxval(raConc1(i,:,:) - raConc2(i,:,:))
        end do
    
    end function ComputeMaxDiff
    
    
    function ComputeR(raConc1, raConc2, rmResult) result(iRetCode)
    
        ! Routine arguments
        real, dimension(:,:,:), intent(in)              :: raConc1
        real, dimension(:,:,:), intent(in)              :: raConc2
        real, dimension(:,:), allocatable, intent(out)  :: rmResult
        integer                                         :: iRetCode
    
        ! Locals
        integer :: iNx
        integer :: iNy
        integer :: iSeriesLength
        integer :: i
        integer :: j
        real    :: rMean1
        real    :: rMean2
        real    :: rStdDev1
        real    :: rStdDev2
        real    :: rCov
    
        ! Assume success (will falsify on failure)
        iRetCode = 0
    
        ! Reserve workspace
        iSeriesLength = size(raConc1, dim=1)
        iNx = size(raConc1, dim=2)
        iNy = size(raConc1, dim=3)
        if(allocated(rmResult)) deallocate(rmResult)
        allocate(rmResult(iNx,iNy))
        rmResult = 0.
    
        ! Get the information desired
        do i = 1, iNx
            do j = 1, iNy
                rMean1   = sum(raConc1(:,i,j)) / iSeriesLength
                rMean2   = sum(raConc2(:,i,j)) / iSeriesLength
                rStdDev1 = sqrt(sum((raConc1(:,i,j)-rMean1)**2) / iSeriesLength)
                rStdDev2 = sqrt(sum((raConc2(:,i,j)-rMean2)**2) / iSeriesLength)
                rCov     = sum( &
                    (raConc1(:,i,j)-rMean1) * &
                    (raConc2(:,i,j)-rMean2) &
                ) / iSeriesLength
                rmResult(i,j) = rCov / (rStdDev1*rStdDev2)
            end do
        end do
    
    end function ComputeR
    
end module commands

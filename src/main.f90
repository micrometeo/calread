! calread - Main program of application for comparing concentration fields
!           from CALPUFF runs made on same grid and time sequence
!
! =============================================================================
!
! MIT License
!
! Copyright (c) 2024-2025 Patrizia Favaron
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
program main

  use cal_get
  use commands
  use calendar

  implicit none

  ! Locals
  ! -1- Auxiliaries
  character(len=256)    :: sBuffer
  character(len=256)    :: sErrStr
  character(len=256)    :: sRefFile
  character(len=256)    :: sCmpFile
  character(len=256)    :: sCmdFile
  character(len=256)    :: sFileName
  type(CalpuffType)     :: tRef
  type(CalpuffType)     :: tCmp
  integer               :: iRetCode
  integer               :: iPollIdx
  integer               :: iRefLUN
  integer               :: iCmpLUN
  integer               :: iStep
  integer               :: iCpuffVersion
  integer, dimension(9) :: values
  logical               :: lDualRun
  logical               :: lDualOnly
  logical               :: lPleaseStop = .false.
  integer               :: iLUN
  integer               :: iNumCmd
  integer               :: iCmd
  integer               :: iCmdNumber
  ! -1- Commands list
  character(len=256), dimension(:), allocatable :: svCommand
  ! -1- Time series of concentration 2D fields
  real, dimension(:,:,:), allocatable :: raRefConc
  real, dimension(:,:,:), allocatable :: raCmpConc
  integer, dimension(:), allocatable  :: ivDateTime

  ! Get command line parameters
  if(command_argument_count() /= 5) then
    print *, "calread - Analytic comparer of Calpuf same-grid runs"
    print *
    print *, "Usage:"
    print *
    print *, "  ./calread <reference_file> <compare_file> <cpuff_version> <pollutant_index> <command_list>"
    print *
    print *, "where <cpuff_version> ::= 6 | 7, and <pollutant_index> is an integer from 1 to the number of"
    print *, "modelled pollutants; number of pollutants and their order are user-defined in Calpuff main"
    print *, "input file."
    print *
    print *, "If <reference_file> and <compare_file> are the same, then run is performed in 'single mode',"
    print *, "the analysis conduced on the file itself. Otherwise, the normal 'dual mode' is entered,"
    print *, "in which the two files are compared (in various ways)."
    print *
    print *, "Copyright 2025 by Patrizia Favaron"
    print *, "This is open-source code, covered by the MIT license"
    print *
    stop
  end if
  call get_command_argument(1, sRefFile)
  call get_command_argument(2, sCmpFile)
  call get_command_argument(3, sBuffer)
  read(sBuffer, *, iostat=iRetCode) iCpuffVersion
  if(iRetCode /= 0) then
    print *, "calread:: error: Invalid Calpuff version"
    stop
  end if
  if(iCpuffVersion < 6 .or. iCpuffVersion > 7) then
    print *, "calread:: error: Invalid Calpuff version; only 6 and 7 are currently supported"
    stop
  end if
  call get_command_argument(4, sBuffer)
  read(sBuffer, *, iostat=iRetCode) iPollIdx
  if(iRetCode /= 0) then
    print *, "calread:: error: Invalid pollutant index"
    stop
  end if
  if(iPollIdx <= 0) then
    print *, "calread:: error: Invalid pollutant index; only positive integers should be used"
    stop
  end if
  call get_command_argument(5, sCmdFile)
  if(sCmdFile == sRefFile .or. sCmdFile == sCmpFile) then
    print *, "calread:: error: Output file has the same name of at least one of the input files"
    stop
  end if
  lDualRun = (sRefFile /= sCmpFile)

  call date_and_time(values=values)
  print "('Begin on: ', i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", values(1:3), values(5:7)

  ! Get the two files (one in single run mode)
  iRetCode = tRef % getHeader(sRefFile, iCpuffVersion, sErrStr, iRefLUN)
  if(iRetCode /= 0) then
    print *, "calread:: error: Reference input file '", trim(sRefFile), "' missing or invalid - ", trim(sErrStr)
    stop
  end if
  if(lDualRun) then
    iRetCode = tCmp % getHeader(sCmpFile, iCpuffVersion, sErrStr, iCmpLUN)
    if(iRetCode /= 0) then
      print *, "calread:: error: Comparison input file '", trim(sCmpFile), "' missing or invalid - ", trim(sErrStr)
      stop
    end if
  end if

  ! In case of a dual run, check the two data sets refer to the same grid and time span / time step; if not, no
  ! comparison could be safely made (some form of interpolation would be, in case, necessary).
  if(lDualRun) then

    ! Check horizontal grids
    if(abs(tRef % rX0 - tCmp % rX0) >= 0.001 .or. abs(tRef % rY0 - tCmp % rY0) >= 0.001) then
      print *, "calread:: error: The grid origins of the two files differ by more than 1m"
      print *, "                 Ref: X0 = ", tRef % rX0, ", Y0 = ", tRef % rY0
      print *, "                 Cmp: X0 = ", tCmp % rX0, ", Y0 = ", tCmp % rY0
      lPleaseStop = .true.
    end if
    if(tRef % iNxSampling /= tCmp % iNxSampling .or. tRef % iNySampling /= tCmp % iNySampling) then
      print *, "calread:: error: The grid nodes of the two files differ"
      print *, "                 Ref: Nx = ", tRef % iNxSampling, ", Ny = ", tRef % iNySampling
      print *, "                 Cmp: Nx = ", tCmp % iNxSampling, ", Ny = ", tCmp % iNySampling
      lPleaseStop = .true.
    end if
    if(abs(tRef % rDx - tCmp % rDx) >= 0.001 .or. abs(tRef % rDy - tCmp % rDy) >= 0.001) then
      print *, "calread:: error: The grid spacings of the two files differ by more than 1m"
      print *, "                 Ref: Dx = ", tRef % rDx, ", Dy = ", tRef % rDy
      print *, "                 Cmp: Dx = ", tCmp % rDx, ", Dy = ", tCmp % rDy
      lPleaseStop = .true.
    end if

    ! Check time span and step
    if( &
      tRef % iBeginYear /= tCmp % iBeginYear .or. &
      tRef % iBeginJulDay /= tCmp % iBeginJulDay .or. &
      tRef % iBeginHour /= tCmp % iBeginHour .or. &
      tRef % iBeginSecond /= tCmp % iBeginSecond .or. &
      tRef % iModelSteps /= tCmp % iModelSteps &
    ) then
      print *, "calread:: error: The time spans of the two files differ"
      print *, "                 Ref: T0 = ", tRef % iBeginYear, tRef % iBeginJulDay, tRef % iBeginHour, tRef % iBeginSecond
      print *, "                 Cmp: T0 = ", tCmp % iBeginYear, tCmp % iBeginJulDay, tCmp % iBeginHour, tCmp % iBeginSecond
      lPleaseStop = .true.
    end if
    if(tRef % iAveragingTime /= tCmp % iAveragingTime) then
      print *, "calread:: error: The averaging times of the two files differ"
      print *, "                 Ref: AvgT = ", tRef % iAveragingTime
      print *, "                 Cmp: AvgT = ", tCmp % iAveragingTime
      lPleaseStop = .true.
    end if

    ! Stop, in case the two files differ
    if(lPleaseStop) stop

  end if

  ! Get commands from file
  open(newunit=iLUN, file=sCmdFile, status='old', action='read', iostat=iRetCode)
  if(iRetCode /= 0) then
    print *, "calread:: error: Command file not read - Return code = ", iRetCode
    stop
  end if
  iNumCmd = 0
  do
    read(iLUN, "(a)", iostat=iRetCode) sBuffer
    if(iRetCode /= 0) exit
    iNumCmd = iNumCmd + 1
  end do
  if(iNumCmd <= 0) then
    print *, "calread:: error: Command file is empty"
    stop
  end if
  allocate(svCommand(iNumCmd))
  rewind(iLUN)
  do iCmd = 1, iNumCmd
    read(iLUN, "(a)") svCommand(iCmd)
  end do
  close(iLUN)

  ! Reserve workspace
  allocate(raRefConc(tRef % iModelSteps, tRef % iNxSampling, tRef % iNySampling), stat=iRetCode)
  if(iRetCode /= 0) then
    print *, "calread:: error: Allocation of reference matrix failed - Return code = ", iRetCode
    stop
  end if
  allocate(ivDateTime(tRef % iModelSteps), stat=iRetCode)
  if(iRetCode /= 0) then
    print *, "calread:: error: Allocation of reference matrix failed - Return code = ", iRetCode
    stop
  end if
  if(lDualRun) then
    allocate(raCmpConc(tCmp % iModelSteps, tCmp % iNxSampling, tCmp % iNySampling), stat=iRetCode)
    if(iRetCode /= 0) then
      print *, "calread:: error: Allocation of comparison matrix failed - Return code = ", iRetCode
      stop
    end if
  end if

  ! Reading loop: gather data
  do iStep = 1, tRef % iModelSteps

    ! Read data records from Calpuff file(s)
    iRetCode = tRef % getRecord(iRefLUN)
    if(iRetCode /= 0) then
      print *, "calread:: error: Error reading reference file - Return code = ", iRetCode
      stop
    end if
    raRefConc(iStep,:,:) = tRef % rmConc(:,:,iPollIdx)
    call PackTime( &
      ivDateTime(iStep), &
      tRef % iYearDisplay, &
      tRef % iMonthDisplay, &
      tRef % iDayDisplay, &
      tRef % iHourDisplay, &
      tRef % iMinuteDisplay, &
      tRef % iSecondDisplay  &
    )
    if(lDualRun) then
      iRetCode = tCmp % getRecord(iCmpLUN)
      if(iRetCode /= 0) then
        print *, "calread:: error: Error reading comparison file - Return code = ", iRetCode
        stop
      end if
      raCmpConc(iStep,:,:) = tCmp % rmConc(:,:,iPollIdx)
      print "(i4.4,'-',i3.3,1x,i2.2,',',i4.4,' - ',e15.7,' ',e15.7)", &
        tRef % iYear, tRef % iJulDay, tRef % iHour, tRef % iSecond, &
        maxval(tRef % rmConc(:,:,iPollIdx)), &
        maxval(tCmp % rmConc(:,:,iPollIdx))
    else
      print "(i4.4,'-',i3.3,1x,i2.2,',',i4.4,' - ',e15.7)", &
        tRef % iYear, tRef % iJulDay, tRef % iHour, tRef % iSecond, &
        maxval(tRef % rmConc(:,:,iPollIdx))
    end if

  end do

  ! Process commands
  do iCmd = 1, iNumCmd
    iRetCode = parse(svCommand(iCmd), iCmdNumber, lDualOnly, sFileName)
    if(iRetCode == 0) then
      print *, "Processing command ", iCmdNumber, trim(sFileName)
    else
      print *, "warning:: Command ", trim(svCommand(iCmd)), " unknown"
      cycle
    end if
    iRetCode = execute(iCmdNumber, lDualOnly, sFileName, lDualRun, ivDateTime, tRef, raRefConc, tCmp, raCmpConc)
    if(iRetCode /= 0) then
      print *, "warning:: Command ", trim(svCommand(iCmd)), " not executed. Return code = ", iRetCode
    end if
  end do

  ! Prepare to leave
  deallocate(ivDateTime)
  deallocate(svCommand)
  deallocate(raRefConc)
  if(lDualRun) deallocate(raCmpConc)

  ! Inform user the program has terminated
  call date_and_time(values=values)
  print "('End on: ', i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", values(1:3), values(5:7)

end program main

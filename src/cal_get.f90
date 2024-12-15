! CalpuffFiles - Module supporting Calpuff 6+ file formats
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
module cal_get

    use calendar

    implicit none
    
    private
    
    ! Public interface
    public  :: CalpuffType
    
    type CalpuffType
        ! Meta-data
        integer                                         :: iCpuffMode   ! 6 or 7, as of Calpuff version used to produce data (which users should know in advance)
        ! Header
        character(len=16)                               :: sDataSet
        character(len=16)                               :: sDataVersion
        character(len=64)                               :: sDataModel
        character(len=132), dimension(:), allocatable   :: svCommentLine
        character(len=12)                               :: sModel
        character(len=12)                               :: sVersion
        character(len=12)                               :: sLevel
        integer                                         :: iBeginYear
        integer                                         :: iBeginJulday
        integer                                         :: iBeginHour
        integer                                         :: iBeginSecond
        character(len=8)                                :: sTimeZone
        integer                                         :: iLoggingTime
        integer                                         :: iModelSteps
        integer                                         :: iAveragingTime
        integer                                         :: iNx
        integer                                         :: iNy
        real                                            :: rDx
        real                                            :: rDy
        integer                                         :: iNumLevels
        real                                            :: rX0
        real                                            :: rY0
        integer                                         :: iNumSurfaceStations
        integer                                         :: iMinCompX
        integer                                         :: iMaxCompX
        integer                                         :: iMinCompY
        integer                                         :: iMaxCompY
        integer                                         :: iMinSampX
        integer                                         :: iMaxSampX
        integer                                         :: iMinSampY
        integer                                         :: iMaxSampY
        integer                                         :: iMeshFactor
        integer                                         :: iNumSourceTypes
        integer                                         :: iNumPointSources1
        integer                                         :: iNumAreaSources1
        integer                                         :: iNumLineSources1
        integer                                         :: iNumVolumeSources1
        integer                                         :: iNumPointSources2
        integer                                         :: iNumAreaSources2
        integer                                         :: iNumLineSources2
        integer                                         :: iNumVolumeSources2
        integer                                         :: iMsource
        integer                                         :: iNumFreeReceptors
        integer                                         :: iNumReceptorGroups
        integer                                         :: iNumCTSGReceptors
        logical                                         :: lSamplingGrid
        integer                                         :: iNumChemicalSpecies
        logical                                         :: lCompressed
        integer                                         :: i2Dmet
        integer                                         :: iUTMZone
        real                                            :: rFalseEasting
        real                                            :: rFalseNorthing
        real                                            :: rNlat0
        real                                            :: rElon0
        real                                            :: rXlat1
        real                                            :: rXlat2
        character(len=8)                                :: sPmap
        character(len=4)                                :: sUTMHem
        character(len=8)                                :: sDatum
        character(len=12)                               :: sDaten
        character(len=16)                               :: sClat0
        character(len=16)                               :: sClon0
        character(len=16)                               :: sClat1
        character(len=16)                               :: sClat2
        character(len=15), dimension(:), allocatable    :: svSpeciesName
        character(len=16), dimension(:), allocatable    :: svSpeciesUnit
        real, dimension(:), allocatable                 :: rvFreeX
        real, dimension(:), allocatable                 :: rvFreeY
        real, dimension(:), allocatable                 :: rvFreeZ
        real, dimension(:), allocatable                 :: rvFreeHeight
        integer, dimension(:), allocatable              :: ivFreeGroupIdx
        character(len=80), dimension(:), allocatable    :: svFreeGroup
        real, dimension(:), allocatable                 :: rvCTSGX
        real, dimension(:), allocatable                 :: rvCTSGY
        real, dimension(:), allocatable                 :: rvCTSGZ
        integer, dimension(:), allocatable              :: ivHill
        character(len=80), dimension(3)                 :: svTitle
        character(len=256), dimension(:), allocatable   :: svSeriesFile
        character(len=16), dimension(:), allocatable    :: svPointSource1
        character(len=16), dimension(:), allocatable    :: svPointSource2
        character(len=16), dimension(:), allocatable    :: svAreaSource1
        character(len=16), dimension(:), allocatable    :: svAreaSource2
        character(len=16), dimension(:), allocatable    :: svLineSource1
        character(len=16), dimension(:), allocatable    :: svLineSource2
        character(len=16), dimension(:), allocatable    :: svVolumeSource1
        character(len=16), dimension(:), allocatable    :: svVolumeSource2
        character(len=16), dimension(:,:), allocatable  :: smSourceName
        logical                                         :: lHasBeenRead
        integer, dimension(:), allocatable              :: ivNumSourcesPerType
        ! Deduced information
        integer                                         :: iNxSampling
        integer                                         :: iNySampling
        ! Record
        integer                                         :: iYearDisplay
        integer                                         :: iMonthDisplay
        integer                                         :: iDayDisplay
        integer                                         :: iHourDisplay
        integer                                         :: iMinuteDisplay
        integer                                         :: iSecondDisplay
        integer                                         :: iYear
        integer                                         :: iJulday
        integer                                         :: iHour
        integer                                         :: iSecond
        integer                                         :: iYear2
        integer                                         :: iJulday2
        integer                                         :: iHour2
        integer                                         :: iSecond2
        integer                                         :: iSrcType
        integer                                         :: iSrcNum
        character(len=16)                               :: sSrcName
        real                                            :: rSrcX, rSrcY
        real, dimension(:,:), allocatable               :: rvCTSGConc
        real, dimension(:), allocatable                 :: rvCTSGCompressedConc
        real, dimension(:,:), allocatable               :: rvFreeConc
        real, dimension(:), allocatable                 :: rvFreeCompressedConc
        real, dimension(:,:,:), allocatable             :: rmConc
        ! Gridded accumulators
        real, dimension(:), allocatable                 :: rvConc
        real, dimension(:), allocatable                 :: rvCompressedData
    contains
        procedure   :: cleanHeader      => cleanCalpuffHeader
        procedure   :: cleanRecord      => cleanCalpuffRecord
        procedure   :: getHeader        => getCalpuffHeader
        procedure   :: getRecord        => getCalpuffRecord
    end type CalpuffType
    
contains

    subroutine cleanCalpuffHeader(this)
    
        ! Routine arguments
        class(CalpuffType), intent(inout)   :: this
        
        ! Locals
        ! -none-
        
        ! Reclaim allocated space, if any
        if(allocated(this % svCommentLine)) deallocate(this % svCommentLine)
        if(allocated(this % svSpeciesName)) deallocate(this % svSpeciesName)
        if(allocated(this % svSpeciesUnit)) deallocate(this % svSpeciesUnit)
        if(allocated(this % rvFreeX)) deallocate(this % rvFreeX)
        if(allocated(this % rvFreeY)) deallocate(this % rvFreeY)
        if(allocated(this % rvFreeZ)) deallocate(this % rvFreeZ)
        if(allocated(this % rvFreeHeight)) deallocate(this % rvFreeHeight)
        if(allocated(this % ivFreeGroupIdx)) deallocate(this % ivFreeGroupIdx)
        if(allocated(this % svFreeGroup)) deallocate(this % svFreeGroup)
        if(allocated(this % rvFreeConc)) deallocate(this % rvFreeConc)
        if(allocated(this % rvFreeCompressedConc)) deallocate(this % rvFreeCompressedConc)
        if(allocated(this % rvCTSGX)) deallocate(this % rvCTSGX)
        if(allocated(this % rvCTSGY)) deallocate(this % rvCTSGY)
        if(allocated(this % rvCTSGZ)) deallocate(this % rvCTSGZ)
        if(allocated(this % ivHill)) deallocate(this % ivHill)
        if(allocated(this % rvCTSGConc)) deallocate(this % rvCTSGConc)
        if(allocated(this % rvCTSGCompressedConc)) deallocate(this % rvCTSGCompressedConc)
        if(allocated(this % rmConc)) deallocate(this % rmConc)
        if(allocated(this % rvConc)) deallocate(this % rvConc)
        if(allocated(this % rvCompressedData)) deallocate(this % rvCompressedData)
        if(allocated(this % svSeriesFile)) deallocate(this % svSeriesFile)
        if(allocated(this % svPointSource1)) deallocate(this % svPointSource1)
        if(allocated(this % svPointSource2)) deallocate(this % svPointSource2)
        if(allocated(this % svAreaSource1)) deallocate(this % svAreaSource1)
        if(allocated(this % svAreaSource2)) deallocate(this % svAreaSource2)
        if(allocated(this % svLineSource1)) deallocate(this % svLineSource1)
        if(allocated(this % svLineSource2)) deallocate(this % svLineSource2)
        if(allocated(this % svVolumeSource1)) deallocate(this % svVolumeSource1)
        if(allocated(this % svVolumeSource2)) deallocate(this % svVolumeSource2)
        if(allocated(this % smSourceName)) deallocate(this % smSourceName)
        if(allocated(this % ivNumSourcesPerType)) deallocate(this % ivNumSourcesPerType)
        
    end subroutine cleanCalpuffHeader
    
    
    ! Get header from Calpuff concentration files. Currently, version 2.2 is supported for
    ! Calpuff 6 (Calpuff 7 uses a different format altogether, which is supported as-is).
    function getCalpuffHeader(this, sInputFile, iCpuffMode, sErrStr, iLUN) result(iRetCode)
    
        ! Routine arguments
        class(CalpuffType), intent(out) :: this
        character(len=*), intent(in)    :: sInputFile
        integer, intent(in)             :: iCpuffMode
        character(len=128), intent(out) :: sErrStr
        integer, intent(out)            :: iLUN
        integer                         :: iRetCode
        
        ! Locals
        integer                     :: iErrCode
        integer                     :: iNumStrings
        integer                     :: iString
        character(len=132)          :: sCommentLine
        character(len=15)           :: sSpecies
        integer                     :: iSpecies
        integer                     :: iGroup
        integer                     :: iSource
        integer                     :: iSourceType
        integer                     :: iSourceTypeAsRead
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Connect input file, with minimal check on input variables
        open(newunit=iLUN, file=sInputFile, status='OLD', form='UNFORMATTED', action='READ', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 10
            sErrStr = "- Fatal - Input file not found"
            return
        end if
        
        ! ************************************************************
        ! * Header part, as in CALPOST's "GETHEAD()" and subroutines *
        ! ************************************************************
        
        ! Get first record, and check it corresponds to meaningful data
        read(iLUN, iostat=iErrCode) &
            this % sDataSet, &
            this % sDataVersion, &
            this % sDataModel
        if(iErrCode /= 0) then
            iRetCode = 11
            close(iLUN)
            sErrStr = "- Fatal - Record #1 (DataSet, DataVersion, DataModel) invalid"
            return
        end if
        if(this % sDataSet /= 'CONC.DAT' .AND. this % sDataSet /= 'DFLX.DAT' .AND. this % sDataSet /= 'WFLX.DAT') then
            iRetCode = 12
            close(iLUN)
            sErrStr = "- Fatal - Record #1 DataSet is not 'CONC.DAT', 'DFLX.DAT' or 'WFLX.DAT'"
            return
        end if
        ! Post-condition: Now, data file type fits one of the three possibilities expected
        
        ! Get the pre-header comment information, containing the
        ! CALPUFF input file
        read(iLUN, iostat=iErrCode) iNumStrings
        if(iErrCode /= 0) then
            iRetCode = 13
            close(iLUN)
            sErrStr = "- Fatal - Comment block: invalid number of comments"
            return
        end if
        allocate(this % svCommentLine(iNumStrings))
        do iString = 1, iNumStrings
            read(iLUN, iostat=iErrCode) sCommentLine
            this % svCommentLine(iString) = sCommentLine
            if(iErrCode /= 0) then
                iRetCode = 14
                close(iLUN)
                sErrStr = "- Fatal - Comment block: invalid comment text"
                return
            end if
        end do
        
        ! Read header record
        if(iCpuffMode == 6) then
            read(iLUN, iostat=iRetCode) &
                this % sModel, &
                this % sVersion, &
                this % sLevel, &
                this % iBeginYear, &
                this % iBeginJulday, &
                this % iBeginHour, &
                this % iBeginSecond, &
                this % sTimeZone, &
                this % iModelSteps, &
                this % iLoggingTime, &
                this % iAveragingTime, &
                this % iNx, &
                this % iNy, &
                this % rDx, &
                this % rDy, &
                this % iNumLevels, &
                this % rX0, &
                this % rY0, &
                this % iNumSurfaceStations, &
                this % iMinCompX, &
                this % iMaxCompX, &
                this % iMinCompY, &
                this % iMaxCompY, &
                this % iMinSampX, &
                this % iMinSampY, &
                this % iMaxSampX, &
                this % iMaxSampY, &
                this % iMeshFactor, &
                this % iNumPointSources1, &
                this % iNumPointSources2, &
                this % iNumAreaSources1, &
                this % iNumAreaSources2, &
                this % iNumLineSources1, &
                this % iNumLineSources2, &
                this % iNumVolumeSources1, &
                this % iNumVolumeSources2, &
                this % iMsource, &
                this % iNumFreeReceptors, &
                this % iNumCTSGReceptors, &
                this % lSamplingGrid, &
                this % iNumChemicalSpecies, &
                this % lCompressed, &
                this % i2Dmet, &
                this % iUTMZone, &
                this % rFalseEasting, &
                this % rFalseNorthing, &
                this % rNlat0, &
                this % rElon0, &
                this % rXlat1, &
                this % rXlat2, &
                this % sPmap, &
                this % sUTMHem, &
                this % sDatum, &
                this % sDaten, &
                this % sClat0, &
                this % sClon0, &
                this % sClat1, &
                this % sClat2
            if(iErrCode /= 0) then
                iErrCode = 15
                close(iLUN)
                sErrStr = "- Fatal - Invalid header"
                return
            end if
            if(this % iNumChemicalSpecies <= 0) then
                iRetCode = 16
                close(iLUN)
                sErrStr = "- Fatal - Number of chemical species modeled is <= 0"
                return
            end if
        else ! That is, iCpuffMode == 7, by construction
            read(iLUN, iostat=iRetCode) &
                this % sModel, &
                this % sVersion, &
                this % sLevel, &
                this % iBeginYear, &
                this % iBeginJulday, &
                this % iBeginHour, &
                this % iBeginSecond, &
                this % sTimeZone, &
                this % iModelSteps, &
                this % iLoggingTime, &
                this % iAveragingTime, &
                this % iNx, &
                this % iNy, &
                this % rDx, &
                this % rDy, &
                this % iNumLevels, &
                this % rX0, &
                this % rY0, &
                this % iNumSurfaceStations, &
                this % iMinCompX, &
                this % iMaxCompX, &
                this % iMinCompY, &
                this % iMaxCompY, &
                this % iMinSampX, &
                this % iMinSampY, &
                this % iMaxSampX, &
                this % iMaxSampY, &
                this % iMeshFactor, &
                this % iNumSourceTypes, &
                this % iMsource, &
                this % iNumFreeReceptors, &
                this % iNumReceptorGroups, &
                this % iNumCTSGReceptors, &
                this % lSamplingGrid, &
                this % iNumChemicalSpecies, &
                this % lCompressed, &
                this % i2Dmet, &
                this % iUTMZone, &
                this % rFalseEasting, &
                this % rFalseNorthing, &
                this % rNlat0, &
                this % rElon0, &
                this % rXlat1, &
                this % rXlat2, &
                this % sPmap, &
                this % sUTMHem, &
                this % sDatum, &
                this % sDaten, &
                this % sClat0, &
                this % sClon0, &
                this % sClat1, &
                this % sClat2
            if(iErrCode /= 0) then
                iErrCode = 15
                close(iLUN)
                sErrStr = "- Fatal - Invalid header"
                return
            end if
            if(this % iNumChemicalSpecies <= 0) then
                iRetCode = 16
                close(iLUN)
                sErrStr = "- Fatal - Number of chemical species modeled is <= 0"
                return
            end if
            if(this % iNumSourceTypes <= 0) then
                iRetCode = 16
                close(iLUN)
                sErrStr = "- Fatal - Number of pollutant sources is <= 0"
                return
            end if
            allocate(this % ivNumSourcesPerType(this % iNumSourceTypes))
            this % ivNumSourcesPerType = 0
            read(iLUN, iostat=iRetCode) &
                (this % ivNumSourcesPerType(iSourceType), iSourceType = 1, this % iNumSourceTypes)
            if(iRetCode /= 0 .or. this % iNumSourceTypes <= 0) then
                iRetCode = 16
                close(iLUN)
                sErrStr = "- Fatal - Invalid number of sources per type"
                return
            end if
        end if
        
        ! Reserve workspace for chemical species and receptor data
        allocate(this % svSpeciesName(this % iNumChemicalSpecies))
        allocate(this % svSpeciesUnit(this % iNumChemicalSpecies))
        if(this % iNumFreeReceptors > 0) then
            allocate(this % rvFreeX(this % iNumFreeReceptors))
            allocate(this % rvFreeY(this % iNumFreeReceptors))
            allocate(this % rvFreeZ(this % iNumFreeReceptors))
            allocate(this % rvFreeConc(this % iNumFreeReceptors, this % iNumChemicalSpecies))
            allocate(this % rvFreeCompressedConc(this % iNumFreeReceptors))
            if(iCpuffMode == 7) then
                allocate(this % rvFreeHeight(this % iNumFreeReceptors))
                allocate(this % ivFreeGroupIdx(this % iNumFreeReceptors))
            end if
        end if
        if(this % iNumCTSGReceptors > 0) then
            allocate(this % rvCTSGX(this % iNumCTSGReceptors))
            allocate(this % rvCTSGY(this % iNumCTSGReceptors))
            allocate(this % rvCTSGZ(this % iNumCTSGReceptors))
            allocate(this % ivHill(this % iNumCTSGReceptors))
            allocate(this % rvCTSGConc(this % iNumCTSGReceptors, this % iNumChemicalSpecies))
            allocate(this % rvCTSGCompressedConc(this % iNumCTSGReceptors))
        end if
        
        ! Computes dimension of the sampling grid, and then reserve
        ! workspace based on the information gathered
        if(this % lSamplingGrid) then
            this % iNxSampling = this % iMeshFactor * (this % iMaxSampX - this % iMinSampX) + 1
            this % iNySampling = this % iMeshFactor * (this % iMaxSampY - this % iMinSampY) + 1
        else
            this % iNxSampling = 0
            this % iNySampling = 0
        end if
        
        if(this % iNxSampling>0 .AND. this % iNySampling>0) then
            allocate(this % rmConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
            allocate(this % rvConc(this % iNxSampling * this % iNySampling))
            allocate(this % rvCompressedData(this % iNxSampling * this % iNySampling))
            allocate(this % svSeriesFile(this % iNumChemicalSpecies))
        end if
        
        ! Get run title
        read(iLUN, iostat=iErrCode) this % svTitle
        if(iErrCode /= 0) then
            iRetCode = 17
            close(iLUN)
            sErrStr = "- Fatal - Invalid run title"
            return
        end if
        
        ! Get chemical species
        if(iCpuffMode == 6) then
            read(iLUN,iostat=iErrCode) (this % svSpeciesName(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
            if(iErrCode /= 0) then
                iRetCode = 18
                sErrStr = "- Fatal - Invalid chemical specie name(s)"
                close(iLUN)
                return
            end if
            do iSpecies = 1, this % iNumChemicalSpecies
                this % svSpeciesName(iSpecies) = this % svSpeciesName(iSpecies)(1:12)   ! Ignore layer no. on chars 13:15 (always 1 in CALPUFF, usually > 1 in CALGRID)
            end do
            read(iLUN,iostat=iErrCode) (this % svSpeciesUnit(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
            if(iErrCode /= 0) then
                iRetCode = 19
                sErrStr = "- Fatal - Invalid chemical specie unit(s)"
                close(iLUN)
                return
            end if
        else
            read(iLUN,iostat=iErrCode) (this % svSpeciesName(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
            if(iErrCode /= 0) then
                iRetCode = 18
                sErrStr = "- Fatal - Invalid chemical specie name(s)"
                close(iLUN)
                return
            end if
            do iSpecies = 1, this % iNumChemicalSpecies
                this % svSpeciesName(iSpecies) = this % svSpeciesName(iSpecies)(1:12)   ! Ignore layer no. on chars 13:15 (always 1 in CALPUFF, usually > 1 in CALGRID)
            end do
            read(iLUN,iostat=iErrCode) (this % svSpeciesUnit(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
            if(iErrCode /= 0) then
                iRetCode = 19
                sErrStr = "- Fatal - Invalid chemical specie unit(s)"
                close(iLUN)
                return
            end if
        end if
        
        ! Get non-gridded receptors, if any
        if(this % iNumFreeReceptors > 0) then
            if(iCpuffMode == 6) then
                read(iLUN, iostat=iErrCode) &
                    this % rvFreeX, &
                    this % rvFreeY, &
                    this % rvFreeZ
                if(iErrCode /= 0) then
                    iRetCode = 20
                    close(iLUN)
                    sErrStr = "- Fatal - Invalid free receptors data"
                    return
                end if
            else
                read(iLUN, iostat=iErrCode) &
                    this % rvFreeX, &
                    this % rvFreeY, &
                    this % rvFreeZ, &
                    this % rvFreeHeight, &
                    this % ivFreeGroupIdx
                if(iErrCode /= 0) then
                    iRetCode = 20
                    close(iLUN)
                    sErrStr = "- Fatal - Invalid free receptors data"
                    return
                end if
                read(iLUN, iostat=iErrCode) &
                    (this % svFreeGroup(iGroup), iGroup = 1, this % iNumReceptorGroups)
                if(iErrCode /= 0) then
                    iRetCode = 20
                    close(iLUN)
                    sErrStr = "- Fatal - Invalid free receptors group names"
                    return
                end if
            end if
        end if
        
        ! Get complex-terrain receptors, if any
        if(this % iNumCTSGReceptors > 0) then
            read(iLUN, iostat=iErrCode) &
                this % rvFreeX, &
                this % rvFreeY, &
                this % rvFreeZ, &
                this % ivHill
            if(iErrCode /= 0) then
                iRetCode = 21
                close(iLUN)
                sErrStr = "- Fatal - Invalid complex-terrain receptors"
                return
            end if
        end if
        
        ! Get source names
        if(iCpuffMode == 6) then
            if(this % iNumPointSources1 > 0) then
                allocate(this % svPointSource1(this % iNumPointSources1))
                read(iLUN, iostat=iErrCode) iSourceType, this % svPointSource1
                if(iErrCode /= 0) then
                    iRetCode = 22
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading point sources 1"
                    return
                end if
            end if
            if(this % iNumPointSources2 > 0) then
                allocate(this % svPointSource2(this % iNumPointSources2))
                read(iLUN, iostat=iErrCode) iSourceType, this % svPointSource2
                if(iErrCode /= 0) then
                    iRetCode = 23
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading point sources 2"
                    return
                end if
            end if
            if(this % iNumAreaSources1 > 0) then
                allocate(this % svAreaSource1(this % iNumAreaSources1))
                read(iLUN, iostat=iErrCode) iSourceType, this % svAreaSource1
                if(iErrCode /= 0) then
                    iRetCode = 24
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading area sources 1"
                    return
                end if
            end if
            if(this % iNumAreaSources2 > 0) then
                allocate(this % svAreaSource2(this % iNumAreaSources2))
                read(iLUN, iostat=iErrCode) iSourceType, this % svAreaSource2
                if(iErrCode /= 0) then
                    iRetCode = 25
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading area sources 2"
                    return
                end if
            end if
            if(this % iNumLineSources1 > 0) then
                allocate(this % svLineSource1(this % iNumLineSources1))
                read(iLUN, iostat=iErrCode) iSourceType, this % svLineSource1
                if(iErrCode /= 0) then
                    iRetCode = 26
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading line sources 1"
                    return
                end if
            end if
            if(this % iNumLineSources2 > 0) then
                allocate(this % svLineSource2(this % iNumLineSources2))
                read(iLUN, iostat=iErrCode) iSourceType, this % svLineSource2
                if(iErrCode /= 0) then
                    iRetCode = 27
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading line sources 2"
                    return
                end if
            end if
            if(this % iNumVolumeSources1 > 0) then
                allocate(this % svVolumeSource1(this % iNumVolumeSources1))
                read(iLUN, iostat=iErrCode) iSourceType, this % svVolumeSource1
                if(iErrCode /= 0) then
                    iRetCode = 28
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading volume sources 1"
                    return
                end if
            end if
            if(this % iNumVolumeSources2 > 0) then
                allocate(this % svVolumeSource2(this % iNumVolumeSources2))
                read(iLUN, iostat=iErrCode) iSourceType, this % svVolumeSource2
                if(iErrCode /= 0) then
                    iRetCode = 29
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading volume sources 2"
                    return
                end if
            end if
        else
            allocate(this % smSourceName(maxval(this % ivNumSourcesPerType), this % iNumSourceTypes))
            do iSourceType = 1, this % iNumSourceTypes
                if(this % ivNumSourcesPerType(iSourceType) > 0) then
                    read(iLUN, iostat=iErrCode) iSourceTypeAsRead, &
                        (this % smSourceName(iSource, iSourceTypeAsRead), &
                            iSource = 1, this % ivNumSourcesPerType(iSourceTypeAsRead))
                end if
                if(iErrCode /= 0) then
                    iRetCode = 30
                    close(iLUN)
                    sErrStr = "- Fatal - Error reading source names"
                    return
                end if
            end do
        end if
        this % lHasBeenRead  = .false.
        
        ! Assign metadata
        this % iCpuffMode = iCpuffMode

    end function getCalpuffHeader


    subroutine cleanCalpuffRecord(this)
    
        ! Routine arguments
        class(CalpuffType), intent(inout)   :: this
        
        ! Locals
        ! -none-
        
        ! Do nothing: all useful work is made during header clean-up
        
    end subroutine cleanCalpuffRecord
    

    function getCalpuffRecord(this, iLUN) result(iRetCode)
    
        ! Routine arguments
        class(CalpuffType), intent(inout)       :: this
        integer, intent(in)                     :: iLUN
        integer                                 :: iRetCode
        
        ! Locals
        integer                                         :: iErrCode
        integer                                         :: iSpecies
        integer                                         :: iNumExpectedData
        integer                                         :: iNumActualWords
        character(len=15)                               :: sSpeciesName
        integer                                         :: iPos
        integer                                         :: iData
        integer                                         :: iX
        integer                                         :: iY
        integer                                         :: iZero
        integer                                         :: iNumZeros
        integer, dimension(2)                           :: ivPos
        logical                                         :: lIsARM2
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Get a new data record header, if any
        read(iLUN, iostat=iErrCode) &
            this % iYear, &
            this % iJulday, &
            this % iHour, &
            this % iSecond, &
            this % iYear2, &
            this % iJulday2, &
            this % iHour2, &
            this % iSecond2
        if(iErrCode /= 0) then
            iRetCode = -1   ! Success, with end-of-file (special case)
            close(iLUN)
            return
        end if
        call ExpandDate( &
            this % iYear, this % iJulday, this % iHour, this % iSecond, &
            this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
            this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay &
        )
        
        ! Get source info
        read(iLUN, iostat=iErrCode) &
            this % iSrcType, &
            this % iSrcNum, &
            this % sSrcName, &
            this % rSrcX, &
            this % rSrcY
        if(iErrCode /= 0) then
            iRetCode = 50
            close(iLUN)
            return
        end if
        
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
            ! Get gridded concentrations, if any
            if(this % lSamplingGrid) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNxSampling * this % iNySampling
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
                        iRetCode = 51
                        close(iLUN)
                        return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCompressedData(1:iNumActualWords)
                    if(iErrCode /= 0) then
                        iRetCode = 52
                        close(iLUN)
                        return
                    end if
                    iPos = 0
                    this % rvConc = 0.
                    do iData = 1, iNumActualWords
                        if(this % rvCompressedData(iData) > 0.) then
                            iPos = iPos + 1
                            this % rvConc(iPos) = this % rvCompressedData(iData)
                        else
                            iNumZeros = ABS(this % rvCompressedData(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvConc(iPos) = 0.
                            end do
                        end if
                    end do
                    if(iPos > iNumExpectedData) then
                        iRetCode = 53
                        close(iLUN)
                        return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvConc
                    if(iErrCode /= 0) then
                        iRetCode = 54
                        close(iLUN)
                        return
                    end if
                    
                end if
                
                if(this % iSrcType == 0) then   ! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
                    ! Transfer data from linear vector to final array
                    iPos = 0
                    do iY = 1, this % iNySampling
                        do iX = 1, this % iNxSampling
                            iPos = iPos + 1
                            this % rmConc(iX, iY, iSpecies) = this % rvConc(iPos)
                        end do
                    end do
                
                end if
            
            end if
            
            ! Get concentration from non-gridded receptors
            if(this % iNumFreeReceptors > 0) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNumFreeReceptors
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
                        iRetCode = 55
                        close(iLUN)
                        return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvFreeCompressedConc(1:iNumActualWords)
                    if(iErrCode /= 0) then
                        iRetCode = 56
                        close(iLUN)
                        return
                    end if
                    iPos = 0
                    do iData = 1, iNumActualWords
                        if(this % rvFreeCompressedConc(iData) >= 0.) then
                            iPos = iPos + 1
                            this % rvFreeConc(iPos,iSpecies) = this % rvFreeCompressedConc(iData)
                        else
                            iNumZeros = ABS(this % rvFreeCompressedConc(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvFreeConc(iPos,iSpecies) = 0.
                            end do
                        end if
                    end do
                    if(iPos /= iNumExpectedData) then
                        iRetCode = 57
                        close(iLUN)
                        return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvFreeConc(:,iSpecies)
                    if(iErrCode /= 0) then
                        iRetCode = 58
                        close(iLUN)
                        return
                    end if
                    
                end if
                
                ! Inform user
                write(*,"('FREE,',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7,',',i3,',',i3,',',a)") &
                    this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
                    this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
                    MAXVAL(this % rvFreeConc(:,iSpecies)), MAXLOC(this % rvFreeConc(:,iSpecies)), -1, &
                    TRIM(sSpeciesName(:12))
                
            end if
            
            ! Get concentrations from complex-terrain receptors
            if(this % iNumCTSGReceptors > 0) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNumFreeReceptors
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
                        iRetCode = 59
                        close(iLUN)
                        return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCTSGCompressedConc(1:iNumActualWords)
                    if(iErrCode /= 0) then
                        iRetCode = 60
                        close(iLUN)
                        return
                    end if
                    iPos = 0
                    do iData = 1, iNumActualWords
                        if(this % rvCTSGCompressedConc(iData) >= 0.) then
                            iPos = iPos + 1
                            this % rvCTSGConc(iPos,iSpecies) = this % rvCTSGCompressedConc(iData)
                        else
                            iNumZeros = ABS(this % rvCTSGCompressedConc(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvCTSGConc(iPos,iSpecies) = 0.
                            end do
                        end if
                    end do
                    if(iPos /= iNumExpectedData) then
                        iRetCode = 61
                        close(iLUN)
                        return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCTSGConc(:,iSpecies)
                    if(iErrCode /= 0) then
                        iRetCode = 62
                        close(iLUN)
                        return
                    end if
                    
                end if
                
                ! Inform user
                write(*,"('CTSG,',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7,',',i3,',',i3,',',a)") &
                    this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
                    this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
                    MAXVAL(this % rvCTSGConc(:,iSpecies)), MAXLOC(this % rvCTSGConc(:,iSpecies)), -1, &
                    TRIM(sSpeciesName(:12))
                
            end if
        
        end do
        
        this % lHasBeenRead = .TRUE.
        
    end function getCalpuffRecord
    
    ! *********************
    ! * Internal routines *
    ! *********************
    
    function DIR_WIND(rU,rV) result(rDir)
    
        ! Routine arguments
        real, intent(in)    :: rU
        real, intent(in)    :: rV
        real                :: rDir
        
        if(abs(rU)<=0.0001 .and. abs(rV)<=0.0001) then
            rDir = 0.
            return
        endif
        rDir = ATAN2(rU,rV)*57.29578
        if(rDir < 0.) rDir = rDir + 360.
        
    end function DIR_WIND

    
    subroutine ExpandDate(iYearIn, iJuldayIn, iHourIn, iSecondIn, iYear, iMonth, iDay, iHour, iMinute, iSecond)
    
        ! Routine arguments
        integer, intent(in)     :: iYearIn
        integer, intent(in)     :: iJulDayIn
        integer, intent(in)     :: iHourIn
        integer, intent(in)     :: iSecondIn
        integer, intent(out)    :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        
        ! Locals
        integer :: iCurTime
        
        ! Compute base time, then add shifts
        call PackTime(iCurTime, iYearIn, 1, 1, 0, 0, 0)
        iCurTime = iCurTime + 86400*(iJulDayIn-1) + 3600*iHourIn + iSecondIn
        
        ! Expand to fully formatted time
        call UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
        
    end subroutine ExpandDate
    
end module cal_get

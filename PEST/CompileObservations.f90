! CompileObservations.f90
! Cathy Trudinger Feb 2017
! Compile file of observations and descriptive obs names, plus extra info, for use in PEST
!---------------------------------------------------------
MODULE BIOME_MODULE
IMPLICIT NONE
CONTAINS
FUNCTION BiomeForestFrac (gridcell_biome) 
  ! For given biome type, return forest fraction
  implicit none
  integer, intent (in) :: gridcell_biome
  real :: BiomeForestFrac
  select case (gridcell_biome)
    case (3,11)
      BiomeForestFrac =  2.0/5.0
    case (12,13,15,16)
      BiomeForestFrac =  1.0/5.0
    case (7,8,9,10)
      BiomeForestFrac = 4.0/5.0
    case (5,6)
      BiomeForestFrac = 0.7
    case (4)
      BiomeForestFrac = 1.0
  end select
end function BiomeForestFrac
END MODULE BIOME_MODULE

!----------------------------------------------------------------------------------------------------------------------------------------
MODULE ArrayOps
! Functions needed for extracting model variables from cable output
use TypeDef
use UTILS
use DateFunctions
implicit none
CONTAINS
!###############################################################################
function FindTimeIndex (ObsDate,timeDMY,leapflag)
! Find time index corresponding to data ObsDate
use TypeDef
use UTILS
use DateFunctions
implicit none
integer :: FindTimeIndex
type (dmydate), intent(in) :: ObsDate
type (dmydate), intent(in) :: timeDMY(:)
logical, intent(in) :: leapflag
integer :: j
  if (leapflag) then
    FindTimeIndex = DayDifference(ObsDate,timeDMY(1)) + 1  ! only works with leap years
!   write(6,*) 'FindTimeIndex: ',ObsDate,timeDMY(FindTimeIndex),FindTimeIndex
  else
    do j = 1,size(timeDMY)
      if (timeDMY(j) .eq. ObsDate) then   ! needed if no leap years
        FindTimeIndex = j
        exit
      endif
    enddo
  endif
  if ((timeDMY(FindTimeIndex)%Day .ne. ObsDate%Day) .and. (timeDMY(FindTimeIndex)%Month .ne. ObsDate%Month)  &
           .and. (timeDMY(FindTimeIndex)%Year .ne. ObsDate%Year)) then
    write(6,*) '***********************************************************************'
    write(6,*) 'Error finding time for observable from model file in routine FindTimeIndex'
    write(6,*) 'Model run starts ',timeDMY(1)
    write(6,*) 'Daily observation is ',ObsDate
    write(6,*) 'Days since start is =',FindTimeIndex
    write(6,*) 'which gives time of ',timeDMY(FindTimeIndex)
    write(6,*) 'Program stopped'
    write(6,*) '***********************************************************************'
    STOP
  endif
end function FindTimeIndex

END MODULE ArrayOps
!----------------------------------------------------------------------------------------------------------------------------------------

program CompileObservations
use netcdf
use TypeDef
use UTILS
use BIOME_MODULE
use ArrayOps
use DateFunctions
use String_Utility
use parse
use BIOME_MODULE
implicit none
integer :: i,j,k,n,m,p,ip,ig,time_dim,land_dim,x_dim,y_dim,patch_dim,soil_dim,nlayer,ncatch,ncells,idum,pidx,pop_land_dim
integer :: FILE_ID, CLIMATE_FILE_ID, POP_FILE_ID, VARID, VARID2, dID, STATUS, iarea,iostat
integer :: numDims, numAtts
integer, dimension(nf90_max_var_dims) :: rhDimIds
integer :: ddstart,mmstart, yystart, ddend, mmend, yyend,countobs,nsites,yy,mm,dd,hh,ff,nn,jland,sitecount, countobs_newsite,ipatch
integer :: vr_flag,vr_sitenum,countfile,count_hr,count_days,Qobs_nt,Qobs_xls_nt,Qobs_xls_ncatch,idx,Qdays,bpl_undist,bpl_day,bpl_mon,bpl_yr,bpl_ltd
integer :: d1,d2,timeidx,jlandsav(1000),jlandcount,precipcount,totalprecipcount,varcount,firstidx,icount,iplus1
integer :: maxobs = 10000000
!integer :: ntan = 10
integer, allocatable :: time_days(:),biome(:),iveg(:),sortedmap(:)
real :: sitelat, sitelon, GPP,NEP,evap,Reco,SMF,Rsoil,patchmissing,patchfill,varsum,varmean
real :: modelres, rangeweight,sum_sm,sum_dep,sm,dep,rdum,prevlat,prevlon,precip,totalprecip
real :: vr_lat,vr_lon,vr_NPP,vr_agphyto,vr_aglitter,vr_soilC0,vr_precip,sm_hr, dep_hr,Q,Qsum
real :: bpl_lat,bpl_lon,bpl_lba,bpl_agd,bpl_agd_se    ! biomass plot library variables
real,allocatable :: time(:),local_lat(:),local_lon(:),latitude(:,:),longitude(:,:),patchfrac(:,:),zse(:,:,:),Qobs_xls_Q(:,:),Qobs_Q(:)
real,allocatable :: obs(:),weight(:),pop_lat(:),pop_lon(:),oznet(:),lai(:)
real,allocatable :: sortedobs(:),sortedweight(:)
real,allocatable :: nvis(:),reccap(:),Rainf(:,:,:)
character(200) FILE_NAME,OBS_PATH,flnm,OzFluxSiteFile,LAISiteFile,LAIBinFile,CosmOzSiteFile,OzNetSiteFile,CatchmentFile,ClimateFile,POPfile
character(200) :: NVISFile, RECCAPFile,OzFluxSMSiteFile,OzFluxRsoSiteFile
character(400) :: strline
character(40) :: sitename,str,timeunits,tres
character(10) :: datestr
character(8) :: output_averaging
character(6) :: depstr,catchname
character(6),allocatable :: obssitename(:)
character(5) :: timestr,bpl_SiteID
character(4) :: yystr,vr_SiteID
character(3) :: sitecode,Rsodm='day',SMCdm='day',SMOdm='day',SMFdm='day',STRdm='day'  !  default value is daily
character(3),allocatable :: obstype(:),obs_proc(:)   ! obs_proc can be 'abs','ano','cdf' for absolute, anomaly, cdf matching
character(3),allocatable :: sortedsitename(:),sortedobstype(:),sortedobs_proc(:)
character(3) :: obscode(17) = (/ 'EvT','GPP','NEP','Rec','Rso','LAI','NPP','Phy','Lit','LBA','LTD','AGD','SC0','SMC','SMO','SMF','STR' /)
character(3) :: EvTSelect='not',GPPSelect='not',NEPSelect='not',RecSelect='not',RsoSelect='not',NPPSelect='not',VRNPPSelect='not',VRSoilC0Select='not'  ! default = don't include
character(3) :: VRPhySelect='not',VRLitSelect='not',SMCSelect='not',SMOSelect='not',SMFSelect='not',STRSelect='not',LBASelect='not',LTDSelect='not',AGDSelect='not'
character(3) :: LAISelect = 'not',xls_csv
character(3) :: OzFluxRes='day'
character(3) :: EvTProcess='abs',GPPProcess='abs',NEPProcess='abs',RecProcess='abs',RsoProcess='abs',VRNPPProcess='abs',VRSoilC0Process='abs'  !default = absolute value
character(3) :: VRPhyProcess='abs',VRLitProcess='abs',SMCProcess='abs',SMOProcess='abs',SMFProcess='abs',STRProcess='abs',LBAProcess='abs',LTDProcess='abs',AGDProcess='abs'
character(3) :: LAIProcess = 'abs'
!character(3),allocatable :: ProcessArr(:),SelectArr(:),VarNameArr(:)
character :: sitepatch,ch
character(2) :: sdum2,s1,s2
character(20),allocatable :: obsname(:),obsinfo(:),grpname(:),sarray(:)
character(20),allocatable :: sortedobsname(:),sortedobsinfo(:),sortedgrpname(:)
character(100) :: bpl_sitename,sdum,prevsite
character(300) :: buffer
character(6),allocatable :: Qobs_xls_catch(:)
character(4),allocatable :: smdeps(:)  ! e.g. 0007, 3060 for oznet sm
type(dmydate) :: ModelStartDate, StartDate, EndDate  ! start, end dates
type(dmydate) :: CurrentDate,PrevDate,SiteStart,SiteEnd,PreviousDate,LAIDate,ObsDate
type(dmydate),allocatable :: Qobs_date(:),Qobs_xls_date(:), timeDMY(:)
logical :: IncludeArray(4)
logical :: found, foundp, leapflag, FileExistFlag, lastcurrent, readxls
CHARACTER(LEN=10) :: calendar 

! Read in the user-supplied parameters from a namelist file
namelist /CompileObservations_Namelist/ OBS_PATH, FILE_NAME, modelres, &
  POPfile,  &
  OzFluxSiteFile, CosmOzSiteFile, OzNetSiteFile, OzFluxSMSiteFile, CatchmentFile, LAISiteFile, LAIBinFile, &
  NVISFile, RECCAPFile, ClimateFile, OzFluxRsoSiteFile, &
  EvTSelect, GPPSelect, NEPSelect, RecSelect, OzFluxRes, LAISelect, &
  VRNPPSelect, VRSoilC0Select, VRPhySelect, VRLitSelect,  &
  EvTProcess, GPPProcess, NEPProcess, RecProcess, LAIProcess, &
  VRNPPProcess, VRSoilC0Process, VRPhyProcess, VRLitProcess,  &
  RsoSelect, RsoProcess, Rsodm, &
  SMCSelect, SMCProcess, SMCdm, &
  SMOSelect, SMOProcess, SMOdm, &
  SMFSelect, SMFProcess, SMFdm, &
  STRSelect, STRProcess, STRdm, &
  LBAProcess, LTDProcess, AGDProcess, & 
  LBASelect, LTDSelect, AGDSelect, FILE_NAME, modelres

open (979, file='CompileObservations.nml')
read (979, nml=CompileObservations_Namelist)
close (979)

write(6,*) 'Variables chosen for inclusion are:'
if (EvTSelect .ne. 'not') write(6,*) 'EvTSelect is "',EvTSelect,'" with processing "',EvTProcess,'" and time resolution "',OzFluxRes,'"'
if (GPPSelect .ne. 'not') write(6,*) 'GPPSelect is "',GPPSelect,'" with processing "',GPPProcess,'" and time resolution "',OzFluxRes,'"'
if (NEPSelect .ne. 'not') write(6,*) 'NEPSelect is "',NEPSelect,'" with processing "',NEPProcess,'" and time resolution "',OzFluxRes,'"'
if (RecSelect .ne. 'not') write(6,*) 'RecSelect is "',RecSelect,'" with processing "',RecProcess,'" and time resolution "',OzFluxRes,'"'
if (RsoSelect .ne. 'not') write(6,*) 'RsoSelect is "',RsoSelect,'" with processing "',RsoProcess,'" and time resolution "',Rsodm,'"'
if (LAISelect .ne. 'not') write(6,*) 'LAISelect is "',LAISelect,'" with processing "',LAIProcess,'"'
if (SMCSelect .ne. 'not') write(6,*) 'SMCSelect is "',SMCSelect,'" with processing "',SMCProcess,'" and time resolution "',SMCdm,'"'
if (SMOSelect .ne. 'not') write(6,*) 'SMOSelect is "',SMOSelect,'" with processing "',SMOProcess,'" and time resolution "',SMOdm,'"'
if (SMFSelect .ne. 'not') write(6,*) 'SMFSelect is "',SMFSelect,'" with processing "',SMFProcess,'" and time resolution "',SMFdm,'"'
if (STRSelect .ne. 'not') write(6,*) 'STRSelect is "',STRSelect,'" with processing "',STRProcess,'" and time resolution "',STRdm,'"'
if (VRNPPSelect .ne. 'not') write(6,*) 'VRNPPSelect is "',VRNPPSelect,'" with processing "',VRNPPProcess,'"'
if (VRSoilC0Select .ne. 'not') write(6,*) 'VRSoilC0Select is "',VRSoilC0Select,'" with processing "',VRSoilC0Process,'"'
if (VRPhySelect .ne. 'not') write(6,*) 'VRPhySelect is "',VRPhySelect,'" with processing "',VRPhyProcess,'"'
if (VRLitSelect .ne. 'not') write(6,*) 'VRLitSelect is "',VRLitSelect,'" with processing "',VRLitProcess,'"'
if (LBASelect .ne. 'not') write(6,*) 'LBASelect is "',LBASelect,'" with processing "',LBAProcess,'"'
if (LTDSelect .ne. 'not') write(6,*) 'LTDSelect is "',LTDSelect,'" with processing "',LTDProcess,'"'
if (AGDSelect .ne. 'not') write(6,*) 'AGDSelect is "',AGDSelect,'" with processing "',AGDProcess,'"'

 ! Check for valiid selection of observation types to include in optimisation
  if (((EvTSelect .ne. 'opt') .and. (EvTSelect .ne. 'com') .and. (EvTSelect .ne. 'not')) .or.   &
      ((GPPSelect .ne. 'opt') .and. (GPPSelect .ne. 'com') .and. (GPPSelect .ne. 'not')) .or.   &
      ((NEPSelect .ne. 'opt') .and. (NEPSelect .ne. 'com') .and. (NEPSelect .ne. 'not')) .or.   &
      ((RecSelect .ne. 'opt') .and. (RecSelect .ne. 'com') .and. (RecSelect .ne. 'not')) .or.   &
      ((RsoSelect .ne. 'opt') .and. (RsoSelect .ne. 'com') .and. (RsoSelect .ne. 'not')) .or.   &
      ((LAISelect .ne. 'opt') .and. (LAISelect .ne. 'com') .and. (LAISelect .ne. 'not')) .or.   &
      ((SMCSelect .ne. 'opt') .and. (SMCSelect .ne. 'com') .and. (SMCSelect .ne. 'not')) .or.   &
      ((SMOSelect .ne. 'opt') .and. (SMOSelect .ne. 'com') .and. (SMOSelect .ne. 'not')) .or.   &
      ((SMFSelect .ne. 'opt') .and. (SMFSelect .ne. 'com') .and. (SMFSelect .ne. 'not')) .or.   &
      ((STRSelect .ne. 'opt') .and. (STRSelect .ne. 'com') .and. (STRSelect .ne. 'not')) .or.   &
      ((VRNPPSelect .ne. 'opt') .and. (VRNPPSelect .ne. 'com') .and. (VRNPPSelect .ne. 'not')) .or.   &
      ((VRSoilC0Select .ne. 'opt') .and. (VRSoilC0Select .ne. 'com') .and. (VRSoilC0Select .ne. 'not')) .or.   &
      ((VRPhySelect .ne. 'opt') .and. (VRPhySelect .ne. 'com') .and. (VRPhySelect .ne. 'not')) .or.   &
      ((VRLitSelect .ne. 'opt') .and. (VRLitSelect .ne. 'com') .and. (VRLitSelect .ne. 'not')) .or.   &
      ((LBASelect .ne. 'opt') .and. (LBASelect .ne. 'com') .and. (LBASelect .ne. 'not')) .or.   &
      ((LTDSelect .ne. 'opt') .and. (LTDSelect .ne. 'com') .and. (LTDSelect .ne. 'not')) .or.   &
      ((AGDSelect .ne. 'opt') .and. (AGDSelect .ne. 'com') .and. (AGDSelect .ne. 'not'))) then 
        write(6,*) '*******************************************************************'
        write(6,*) 'Error in selection choices for EvTSelect, GPPSelect, NEPSelect or RecSelect etc'
        write(6,*) 'Valid choices are ''opt'', ''com'' or ''not'''
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
  endif

  ! Check for valid selection of processing options
  if ((VRNPPProcess .eq. 'tan') .or. (VRSoilC0Process .eq. 'tan') .or. (VRPhyProcess .eq. 'tan') .or. (VRLitProcess .eq. 'tan') .or.  &
      (LBAProcess .eq. 'tan') .or. (LTDProcess .eq. 'tan') .or. (AGDProcess .eq. 'tan')) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Error in processing choices for some run-averaged variables, cannot be temporal average (tan)'
        write(6,*) 'VRNPPProcess, VRSoilC0Process, VRPhyProcess, VRLitProcess, LBAProcess, LTDProcess, AGDProcess ='
        write(6,*) VRNPPProcess,' ',VRSoilC0Process,' ',VRPhyProcess,' ',VRLitProcess,' ',LBAProcess,' ',LTDProcess,' ',AGDProcess
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
  endif

! First read a model output netcdf file to get time range and gridcell locations
  ! NOW INPUT FORM NAMELIST
  ! Open netcdf file to read variables
  !FILE_NAME = "/flush1/tru034/CABLE/mdf/Ozmdf/plume_out_cable_2000_2016.nc"
  !FILE_NAME = "/flush1/tru034/PEST/PEST1/CABLE1/plume_out_cable.nc"
  !FILE_NAME = "/flush1/tru034/CABLE/mdf_test/bios_out_cable_2000_2016.nc"
  !modelres = 0.05
  !FILE_NAME = "/flush1/tru034/CABLE/mdf_test_global/plume_out_cable_2000_2016.nc"
  !modelres = 0.5

  write(6,*) 'Open model output file ', TRIM(FILE_NAME)
  STATUS = NF90_OPEN( TRIM(FILE_NAME), NF90_NOWRITE, FILE_ID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  ! Get time and land dimensions 
  STATUS = NF90_INQ_DIMID( FILE_ID, 'time', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=time_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'land', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=land_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'x', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=x_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'y', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=y_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'patch', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=patch_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'soil', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=soil_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  write(6,*) 'time_dim, land_dim, soil_dim = ',time_dim, land_dim, soil_dim

  ! Read lat and lon at sites in mask
  allocate(local_lat(land_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'local_lat', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, local_lat )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  write(6,*) 'latitude=',local_lat

  allocate(local_lon(land_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'local_lon', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, local_lon )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  write(6,*) 'longitude=',local_lon

  ! Read patchfrac
  allocate(patchfrac(land_dim,patch_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'patchfrac', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, patchfrac )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  ! Read missing and fill values
  STATUS = NF90_GET_ATT(FILE_ID, VARID,'_FillValue',patchfill)
  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
  STATUS = NF90_GET_ATT(FILE_ID, VARID,'missing_value',patchmissing)
  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
  write(6,*) 'Patch missing and fill values are ',patchmissing,' ',patchfill


  ! Read latitude and longitude of model grid (calculate model res for leapflag)
  allocate(latitude(x_dim,y_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'latitude', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, latitude )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  allocate(longitude(x_dim,y_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'longitude', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, longitude )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  ! Determine model resolution from latitude and longitude
  !modelres = (latitude(1,1)-latitude(1,2))
  !if ((longitude(2,1)-longitude(1,1)) .ne. modelres) then
  !  write(6,*) '*******************************************************************'
  !  write(6,*) 'Model resolution is different for latitude and longitude',modelres,(longitude(2,1)-longitude(1,1))
  !  write(6,*) 'Check calculation of gridcell location for obs (assumes same)'
  !  write(6,*) 'Program stopped'
  !  write(6,*) '*******************************************************************'
  !  STOP
  !endif
  write(6,*) 'Model resolution is ',modelres

  ! Determine whether model output is daily or monthly
  STATUS = NF90_INQUIRE_ATTRIBUTE ( FILE_ID, nf90_global, 'Output_averaging')
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_ATT( FILE_ID, nf90_global, 'Output_averaging', output_averaging )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  write(6,*) 'Output averaging is ',output_averaging

  ! Read soil layer thicknesses, calculate depths, give information for soil moisture obs
  if ((SMCSelect .ne. 'not') .or. (SMOSelect .ne. 'not') .or. (SMFSelect .ne. 'not')) then
    allocate(zse(land_dim,patch_dim,soil_dim))
    STATUS = NF90_INQ_VARID( FILE_ID, 'zse', VARID )   
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_GET_VAR( FILE_ID, VARID, zse )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  endif   
 
  ! Read time in cable netcdf file, convert time units, get time range
  allocate(time(time_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'time', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, time )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  ! Model time is in seconds since start date, convert to days since start date
  allocate(time_days(time_dim))  ! time in days since start of run
  do i = 1, time_dim
    time_days(i) = time(i)/(3600*24)
  enddo
  ! Check whether leap years are used or not
!  write(6,*) 'read calendar attribute'
!  STATUS = NF90_GET_ATT(FILE_ID, VARID,'calendar',calendar)
!  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
!  write(6,*) 'calendar from cable netcdf file is ',calendar
!  if (calendar .eq. '') then
!    write(6,*) 'setting calendar to "standard" and disabling check'
!    calendar = 'standard'                                                         !    ********** FIX THIS WITH PROPER CALENDAR VALUE FROM FILE
!  endif
  calendar = 'standard'                                                         !    ********** FIX THIS WITH PROPER CALENDAR VALUE FROM FILE
  select case (calendar)
    case ('standard')
      leapflag = .true.
    case ('noleap')
      leapflag = .false.
    case default
      write(6,*) '*******************************************************************'
      write(6,*) 'Problem with time attribute calendar from netcdf file'
      write(6,*) 'Expect either "standard" or "noleap"'
      write(6,*) 'calendar is ',calendar
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
  end select
  write(6,*) 'calendar = ',calendar
  ! Read time units
  STATUS = NF90_GET_ATT(FILE_ID, VARID,'units',timeunits)
  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
  write(6,*) 'Time in CABLE is in ',trim(timeunits)
  ! Check to make sure have right place in timeunits string and start at 1 Jan
  if (timeunits(19:22) .ne. '-1-1') then 
    write(6,*) '*******************************************************************'
    write(6,*) 'Problem reading time units from netcdf file'
    write(6,*) 'Expect timeunits(19:21) to be ''-1-1'' for start-of-year but it is ',timeunits(19:22)
    write(6,*) 'Program stopped'
    write(6,*) '*******************************************************************'
    STOP
  endif
  yystr = timeunits(15:18)
  read(yystr,'(i4)') ModelStartDate%Year   
  ModelStartDate%Month = 1
  ModelStartDate%Day = 1
 ! do i = 1,time_dim  ! just checking data functions
 !   if (LeapFlag) then
 !     EndDate = AddDay(StartDate,time_days(i))
 !   else
 !     EndDate = AddDayNoLeap(StartDate,time_days(i))
 !   endif
 !   write(6,*) EndDate
 !   if (mod(i,100) .eq. 0) then
 !     read(5,*)
 !   endif
 ! enddo
  ! Calculate EndDate for model run so know which measurements to include
  if (LeapFlag) then
    StartDate = AddDay(ModelStartDate,time_days(1))
    EndDate = AddDay(ModelStartDate,time_days(time_dim))
  else 
    StartDate = AddDayNoLeap(ModelStartDate,time_days(1))
    EndDate = AddDayNoLeap(ModelStartDate,time_days(time_dim))
  endif
  write(6,201) StartDate%Year,StartDate%Month,StartDate%Day,EndDate%Year,EndDate%Month,EndDate%Day
201 format('Model output is available between ',i4,'-',i2,'-',i2,' and ',i4,'-',i2,'-',i2)
  if ((output_averaging .eq. 'Monthly') .and. (EndDate%Month .ne. 12) .and. (EndDate%Day .ne. 16)) then
    write(6,*) '*******************************************************************'
    write(6,*) 'Calculated EndDate not mid-December, check whether have correct leap year flag for driving met'
    write(6,*) 'Program stopped'
    write(6,*) '*******************************************************************'
    STOP
  endif

  ! Check that required model variables are written to netcdf file by CABLE
  ! Variables required are Evap, GPP, NEE, AutoResp+HeteroResp, SoilMoist, above-ground drymass
  if (EvTSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'Evap', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'ET is output by the model, VARID= ',VARID
  endif
  if (GPPSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'GPP', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'GPP is output by the model, VARID= ',VARID
  endif
  if (NEPSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'NEE', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'NEE is output by the model, VARID= ',VARID
  endif
  if (RecSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'AutoResp', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQ_VARID( FILE_ID, 'HeteroResp', VARID2 )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'AutoResp and HeteroResp are output by the model, VARID= ',VARID,VARID2
  endif
  if (RsoSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'HeteroResp', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQ_VARID( FILE_ID, 'RootResp', VARID2 )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'HeteroResp and RootResp are output by the model, VARID= ',VARID,VARID2
  endif
  if (LAISelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'LAI', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'LAI is output by the model, VARID= ',VARID
  endif
  if ((SMCSelect .ne. 'not') .or. (SMOSelect .ne. 'not') .or. (SMFSelect .ne. 'not')) then
    STATUS = NF90_INQ_VARID( FILE_ID, 'SoilMoist', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'SoilMoist is output by the model, VARID= ',VARID
  endif
  if (AGDSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'TotLivBiomass', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'TotLivBiomass is output by the model, VARID= ',VARID
  endif
  if (STRSelect .ne. 'not') then
    STATUS = NF90_INQ_VARID( FILE_ID, 'Qs', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'Surface runoff is output by the model, VARID= ',VARID
    STATUS = NF90_INQ_VARID( FILE_ID, 'Qsb', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'Deep runoff is output by the model, VARID= ',VARID
    ! Later calculate run-averaged precip to write to file with streamflow info
    allocate(Rainf(land_dim,patch_dim,time_dim))
    STATUS = NF90_INQ_VARID( FILE_ID, 'Rainf', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'Rainfall is output by the model, VARID= ',VARID
    STATUS = NF90_GET_VAR( FILE_ID, VARID, Rainf)
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    Rainf = Rainf*60.0*60.0*24.0    ! convert precip kg/m2/s = mm/s to mm/d

    ! Calculate date for all model times, used in checking date for precip ave
    allocate(timeDMY(time_dim))
    do i = 1, time_dim
      time_days(i) = time(i)/(3600*24)
      if (LeapFlag) then
        timeDMY(i) = AddDay(ModelStartDate,time_days(i))
      else
        timeDMY(i) = AddDayNoLeap(ModelStartDate,time_days(i))
      endif
  enddo

  endif


  call check( nf90_close(FILE_ID) )  ! close CABLE netcdf file

  ! Read bin file for NVIS classification
  allocate(nvis(land_dim))
  INQUIRE (file=trim(NVISFile), exist=FileExistFlag)
  if (FileExistFlag) then
    OPEN (unit=200, file=trim(NVISFile), status='old', form='binary', action='read')
    REWIND (unit=200)
    read (200) nvis 
    CLOSE (unit=200)
  else
    write(*,"('File not found:',/,a)") trim(NVISFile)
  end if

  ! Read bin file for RECCAP ID
  allocate(reccap(land_dim))
  INQUIRE (file=trim(RECCAPFile), exist=FileExistFlag)
  if (FileExistFlag) then
    OPEN (unit=200, file=trim(RECCAPFile), status='old', form='binary', action='read')
    REWIND (unit=200)
    read (200) reccap
    CLOSE (unit=200)
  else
    write(*,"('File not found:',/,a)") trim(RECCAPFile)
  end if
  
  write(6,*) 'NVIS classification and RECCAP ID'
  do i = 1,land_dim
    write(6,*) i,' ',nvis(i),' ',reccap(i) 
  end do 

  allocate(biome(land_dim))
  allocate(iveg(land_dim))
  if (trim(ClimateFile) .eq. 'nil') then
    write(6,*) 'No climate file read'
    biome(:) = 0
    iveg(:) = 0
  else
    write(6,*) 'Read climate file to find out biomes'
    write(6,*) 'Climate file is ',ClimateFile
    STATUS = NF90_OPEN( TRIM(ClimateFile), NF90_NOWRITE, CLIMATE_FILE_ID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQ_VARID( CLIMATE_FILE_ID, 'biome', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS) 
    STATUS = NF90_GET_VAR( CLIMATE_FILE_ID, VARID, biome )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'Using POP output, so biome read from climate file'
    STATUS = NF90_INQ_VARID( CLIMATE_FILE_ID, 'iveg', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS) 
    STATUS = NF90_GET_VAR( CLIMATE_FILE_ID, VARID, iveg )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  endif
  ! Read pop_land_dim, latitude and longitude from pop ini file
  if (trim(POPFile) .eq. 'nil') then
    write(6,*) 'No POP file read'
    if ((LBASelect .ne. 'not') .or. (LTDSelect .ne. 'not')) then
      write(6,*) '*****************************************************************'
      write(6,*) 'ERROR: POP file not specified, but LBA or LTD obs are selected'
      write(6,*) 'These obs require POP output file'
      write(6,*) 'Program stopped'
      write(6,*) '*****************************************************************'
      STOP
    endif
  else
    STATUS = NF90_OPEN( TRIM(POPFile), NF90_NOWRITE, POP_FILE_ID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQ_DIMID( POP_FILE_ID, 'land', dID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQUIRE_DIMENSION( POP_FILE_ID, dID, LEN=pop_land_dim )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    allocate(pop_lat(pop_land_dim))
    allocate(pop_lon(pop_land_dim))
    STATUS = NF90_INQ_VARID( POP_FILE_ID, 'latitude', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_GET_VAR( POP_FILE_ID, VARID, pop_lat )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_INQ_VARID( POP_FILE_ID, 'longitude', VARID )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    STATUS = NF90_GET_VAR( POP_FILE_ID, VARID, pop_lon )
    IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
    write(6,*) 'POP latitude and longitude read to enable checking'
    if (LBASelect .ne. 'not') then
      STATUS = NF90_INQ_VARID( POP_FILE_ID, 'basal_area', VARID )
      IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
      write(6,*) 'Basal area is output by the model (POP output), VARID= ',VARID
    endif
    if (LTDSelect .ne. 'not') then
      STATUS = NF90_INQ_VARID( POP_FILE_ID, 'densindiv', VARID )
      IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
      write(6,*) 'Tree density is output by the model (POP output), VARID= ',VARID
    endif
  endif

  allocate(obsname(maxobs))
  allocate(obsinfo(maxobs))
  allocate(grpname(maxobs))
  allocate(obs(maxobs))
  allocate(weight(maxobs))
  allocate(obssitename(maxobs))
  allocate(obstype(maxobs))
  allocate(obs_proc(maxobs))
  allocate(sortedmap(maxobs))
  countobs = 0
  write(6,*) ' '

!-------------------------------------------------------------------------------------------------------------------------------------------------------
  ! Now read OzFlux observations

  if ((EvTSelect .ne. 'not') .or. (GPPSelect .ne. 'not') .or. (NEPSelect .ne. 'not') .or.  (RecSelect .ne. 'not')) then 

    if ((OzFluxRes .ne. 'mon') .and. (OzFluxRes .ne. 'day') .and. (OzFluxRes .ne. 'hho') .and. (OzFluxRes .ne. 'hou')) then 
      write(6,*) 'ERROR: OzFluxRes = ',OzFluxRes
      write(6,*) 'OzFluxRes only set up for mon, day, hou and hho'
      write(6,*) ' Program stopped'
      STOP
    endif
    ! Read file describing observations to use, and obs themselves
    OPEN(UNIT=21,FILE=trim(OzFluxSiteFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) 'Missing file mentioned in CompileObservations.nml:',trim(OzFluxSiteFile)
      write(6,*) 'Program stopped'
      STOP
    endif
    read(21,*) nsites
    read(21,*)
    do i = 1, nsites
      read(21,*) sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) 'OzFlux site ',trim(sitecode), ' =  ',trim(sitename)
      write(6,*) mmstart, yystart, mmend, yyend,sitelat,sitelon,trim(sitepatch)
      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell for ',trim(sitename),' is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        do j = 1,land_dim 
          write(6,*) local_lat(j),local_lon(j)
        end do
        write(6,*) '*******************************************************************'
        write(6,*) 'Model lat, lon = '
        write(6,*) 'Error finding model gridcell corresponding to OzFlux observation ',sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      ! Check patch info is valid
      write(6,*) 'sitepatch, patchfrac=',sitepatch, patchfrac(jland,:)
      if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
        read(sitepatch,*) ipatch
        if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
          write(6,*) '*******************************************************************'
          write(6,*) 'ERROR: zero or missing patchfrac for patch specified for OzFlux site',sitename
          write(6,*) 'Patch for site is ',sitepatch
          write(6,*) 'patchfrac = ',patchfrac(jland,:)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      endif
      ! read data file
      if (OzFluxRes .eq. 'mon') tres = 'Monthly'
      if (OzFluxRes .eq. 'day') tres = 'Daily'
      if (OzFluxRes .eq. 'hou') tres = 'Hourly'
      if (OzFluxRes .eq. 'hho') tres = 'HalfHourly'
      flnm = trim(OBS_PATH) // 'OzFlux/' // trim(sitename)//'_'//trim(tres)//'.csv'
      write(6,*) 'Read OzFlux file ',trim(flnm)
      open(unit=22,file=flnm,status='OLD',iostat=iostat)
      if (iostat .ne. 0) then
        write(6,*) 'Missing OzFlux file:',trim(flnm)
        write(6,*) 'Program stopped'
        STOP
      endif

      read(22,*) 
      sitecount = 0  ! running count of obs for optimisation at that site
      countobs_newsite = countobs+1
      dd = 1
      hh = 1
      ff = 0
      do 
        if (OzFluxRes .eq. 'mon') read(22,*,end=200) yy, mm, GPP,NEP,evap,Reco
        if (OzFluxRes .eq. 'day') read(22,*,end=200) yy, mm, dd, GPP,NEP,evap,Reco
        if (OzFluxRes .eq. 'hou') read(22,*,end=200) yy, mm, dd, hh, GPP,NEP,evap,Reco
        if (OzFluxRes .eq. 'hho') read(22,*,end=200) yy, mm, dd, hh, ff, GPP,NEP,evap,Reco
        CurrentDate%Year = yy
        CurrentDate%Month = mm
        CurrentDate%Day = dd
        if ((CurrentDate .lt. StartDate) .or. (CurrentDate .ge. EndDate)) then
        !  write(6,*) 'OzFlux observations at site ',trim(sitecode),' in year ',yy,' are out of range for model'
        !  write(6,*) 'Not included in observation file'
        else
          if ((yy .lt. yystart) .or. (yy .gt. yyend)) then
            rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
          else
            rangeweight = 1.0  ! within time range given for optimisation
            sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
          endif  
          if ((GPPSelect .ne. 'not') .and. (GPP .gt. 0.0) .and. (GPP .gt. -999.0)) then  ! Exclude zero or negative GPP, especially for half-hourly 
            countobs = countobs + 1
            if (OzFluxRes .eq. 'mon') obsname(countobs) = 'GPP'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm) 
            if (OzFluxRes .eq. 'day') obsname(countobs) = 'GPP'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd) 
            if (OzFluxRes .eq. 'hou') obsname(countobs) = 'GPP'//trim(sitecode)//'H'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)
            if (OzFluxRes .eq. 'hho') obsname(countobs) = 'GPP'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)//int2str2(ff) 
            obsinfo(countobs) = trim(GPPProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = GPP
            if (GPPSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'gpp'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'GPP'
            obs_proc(countobs) = trim(GPPProcess)
          endif
          if ((NEPSelect .ne. 'not') .and. (NEP .gt. -999.0)) then
            countobs = countobs + 1
            if (OzFluxRes .eq. 'mon') obsname(countobs) = 'NEP'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm)
            if (OzFluxRes .eq. 'day') obsname(countobs) = 'NEP'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd)
            if (OzFluxRes .eq. 'hou') obsname(countobs) = 'NEP'//trim(sitecode)//'H'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)
            if (OzFluxRes .eq. 'hho') obsname(countobs) = 'NEP'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)//int2str2(ff)
            obsinfo(countobs) = trim(NEPProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = NEP
            if (NEPSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'nep'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'NEP'
            obs_proc(countobs) = trim(NEPProcess)
          endif
          if ((EvTSelect .ne. 'not')  .and. (evap .gt. -999.0)) then
            countobs = countobs + 1
            if (OzFluxRes .eq. 'mon') obsname(countobs) = 'EvT'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm)
            if (OzFluxRes .eq. 'day') obsname(countobs) = 'EvT'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd)
            if (OzFluxRes .eq. 'hou') obsname(countobs) = 'EvT'//trim(sitecode)//'H'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)
            if (OzFluxRes .eq. 'hho') obsname(countobs) = 'EvT'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)//int2str2(ff)
            obsinfo(countobs) = trim(EvTProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            if ((OzFluxRes .eq. 'mon') .or. (OzFluxRes .eq. 'day')) obs(countobs) = evap/60.0/60.0/24.0*1000.0   ! obs in m/d, convert to mm/s like model
            if (OzFluxRes .eq. 'hou') obs(countobs) = evap/60.0/60.0   ! obs in mm/hour, convert to mm/s like model
            if (OzFluxRes .eq. 'hho') obs(countobs) = evap/60.0/30.0   ! obs in mm/30mins, convert to mm/s like model
            if (EvTSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'evap'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'EvT'
            obs_proc(countobs) = trim(EvTProcess)
          endif
          if ((RecSelect .ne. 'not') .and. (Reco .gt. -999.0)) then
            countobs = countobs + 1
            if (OzFluxRes .eq. 'mon') obsname(countobs) = 'Rec'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm)
            if (OzFluxRes .eq. 'day') obsname(countobs) = 'Rec'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd)
            if (OzFluxRes .eq. 'hou') obsname(countobs) = 'Rec'//trim(sitecode)//'H'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)            
            if (OzFluxRes .eq. 'hho') obsname(countobs) = 'Rec'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)//int2str2(ff)
            obsinfo(countobs) = trim(RecProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = Reco
            if (RecSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'ecoresp'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'Rec'
            obs_proc(countobs) = trim(RecProcess)
          endif
        endif
      end do
200   continue
      close(22)
      if (sitecount .gt. 0) then
        do j = countobs_newsite, countobs
          weight(j) = weight(j) * 1.0/sitecount ! weight obs by number of obs at each site if otherwise selected
        enddo
      endif
      write(6,*) 'Finished OzFlux site ',trim(sitecode)
    end do
    CLOSE(21)
    write(6,*) ' '
  endif  ! OzFlux observations
  ! end of OzFlux Obs
!-------------------------------------------------------------------------------------------------------------------------------------------------------
  ! Now read OzFlux soil respiration observations

  if (RsoSelect .ne. 'not') then 

    if ((Rsodm .ne. 'mon') .and. (Rsodm .ne. 'day') .and. (Rsodm .ne. 'hho')) then 
      write(6,*) 'ERROR: Rsodm = ',Rsodm
      write(6,*) 'Soil respiration time resolution Rsodm only set up for mon, day and hho'
      write(6,*) ' Program stopped'
      STOP
    endif
    ! Read file describing observations to use, and obs themselves
    OPEN(UNIT=21,FILE=trim(OzFluxRsoSiteFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) 'Missing file mentioned in CompileObservations.nml:',trim(OzFluxRsoSiteFile)
      write(6,*) 'Program stopped'
      STOP
    endif
    read(21,*) nsites
    read(21,*)
    do i = 1, nsites
      read(21,*) sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) 'OzFlux site for soil resp ',trim(sitecode), ' =  ',trim(sitename)
      write(6,*) mmstart, yystart, mmend, yyend,sitelat,sitelon,trim(sitepatch)
      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        do j = 1,land_dim 
          write(6,*) 'lat, long = ',local_lat(j),local_lon(j)
        end do
        write(6,*) '*******************************************************************'
        write(6,*) 'Model lat, lon = '
        write(6,*) 'Error finding model gridcell corresponding to OzFlux soil resp observation ',sitecode,sitename,mmstart,yystart,mmend,yyend,sitelat,sitelon
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      ! Check patch info is valid
      write(6,*) 'sitepatch, patchfrac=',sitepatch, patchfrac(jland,:)
      if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
        read(sitepatch,*) ipatch
        if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
          write(6,*) '*******************************************************************'
          write(6,*) 'ERROR: zero or missing patchfrac for patch specified for OzFlux site',sitename
          write(6,*) 'Patch for site is ',sitepatch
          write(6,*) 'patchfrac = ',patchfrac(jland,:)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      endif
      ! read data file
      if (Rsodm .eq. 'mon') tres = 'Monthly'
      if (Rsodm .eq. 'day') tres = 'Daily'
      if (Rsodm .eq. 'hho') tres = 'HalfHourly'
      flnm = trim(OBS_PATH) // 'OzFlux/' // trim(sitename)//'_Rsoil_'//trim(tres)//'.csv'
      write(6,*) 'Reading OzFlux soil respiration file ',trim(flnm)
      open(unit=22,file=flnm,status='OLD',iostat=iostat)
      if (iostat .ne. 0) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Missing OzFlux soil respiration file:',trim(flnm)
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif

      read(22,*) 
      sitecount = 0  ! running count of obs for optimisation at that site
      countobs_newsite = countobs+1
      dd = 1
      hh = 1
      ff = 0
      do 
        if (Rsodm .eq. 'mon') read(22,*,end=206) yy, mm, Rsoil
        if (Rsodm .eq. 'day') read(22,*,end=206) yy, mm, dd, Rsoil
        if (Rsodm .eq. 'hho') read(22,*,end=206) yy, mm, dd, hh, ff, Rsoil
        CurrentDate%Year = yy
        CurrentDate%Month = mm
        CurrentDate%Day = dd
        if ((CurrentDate .lt. StartDate) .or. (CurrentDate .ge. EndDate) .or.   &
                ((.not.(leapflag)) .and. (CurrentDate%Day .eq. 29) .and. (CurrentDate%Month .eq. 2))) then
        !  write(6,*) 'OzFlux soil respiration observations at site ',trim(sitecode),' in year ',yy,' are out of range for model or 29 Feb'
        !  write(6,*) 'Not included in observation file'
        else
          if ((yy .lt. yystart) .or. (yy .gt. yyend)) then
            rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
          else
            rangeweight = 1.0  ! within time range given for optimisation
            sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
          endif  
          if ((RsoSelect .ne. 'not')  .and. (Rsoil .gt. 0.0)) then
            countobs = countobs + 1
            if (Rsodm .eq. 'mon') obsname(countobs) = 'Rso'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm)
            if (Rsodm .eq. 'day') obsname(countobs) = 'Rso'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd)
            if (Rsodm .eq. 'hho') obsname(countobs) = 'Rso'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)//int2str2(ff)
            obsinfo(countobs) = trim(RsoProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = Rsoil
            if (RsoSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'soilresp'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'Rso'
            obs_proc(countobs) = trim(RsoProcess)
          endif
        endif
      end do
206   continue
      close(22)
      if (sitecount .gt. 0) then
        do j = countobs_newsite, countobs
          weight(j) = weight(j) * 1.0/sitecount ! weight obs by number of obs at each site if otherwise selected
        enddo
      endif
      write(6,*) 'Finished ',sitecode
    end do
    write(6,*) 'Finished soil respiration'
    write(6,*) ' '
    CLOSE(21)
  endif  ! OzFlux soil repiration observations
  ! end of OzFlux soil resp obs
!-------------------------------------------------------------------------------------------------------------------------------------------------------
  ! Now read LAI observations

  if (LAISelect .ne. 'not') then 

    write(6,*) 'LAI observations'
    allocate(lai(land_dim))
    ! Read file of lats and lons where LAI will be compared with model
    open(unit=21,file=trim(LAISiteFile),status='old',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing file mentioned in CompileObservations.nml:',trim(LAISiteFile)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(21,*) nsites
    read(21,*)
    do i = 1, nsites
      read(21,*) sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon,sitepatch
      SiteStart%Day = 1
      SiteStart%Month = mmstart
      SiteStart%Year = yystart
      SiteEnd%Month = mmend
      SiteEnd%Year = yyend
      SiteEnd%Day = 1
      SiteEnd%Day = DaysInMonth(SiteEnd)
      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        do j = 1,land_dim 
          write(6,*) 'lat, long = ',local_lat(j),local_lon(j)
        end do
        write(6,*) '*******************************************************************'
        write(6,*) 'Model lat, lon = '
        write(6,*) 'Error finding model gridcell corresponding to LAI observation ',sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      ! Check patch info is valid
      write(6,*) 'sitepatch, patchfrac=',sitepatch, patchfrac(jland,:)
      if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
        read(sitepatch,*) ipatch
        if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
          write(6,*) '*******************************************************************'
          write(6,*) 'ERROR: zero or missing patchfrac for patch specified for OzFlux site',sitename
          write(6,*) 'Patch for site is ',sitepatch
          write(6,*) 'patchfrac = ',patchfrac(jland,:)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      endif
      ! Read LAI bin file
      flnm = trim(LAIBinFile) 
      write(6,*) 'Reading LAI bin file ',trim(flnm)
      open(unit=22, file=flnm, form='binary', status='old', action='read',iostat=iostat)
      if (iostat .ne. 0) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Missing LAI bin file:',trim(LAIBinFile)
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      rewind(22)
      sitecount = 0  ! running count of obs for optimisation at that site
      countobs_newsite = countobs+1
      do 
        read(22,end=202) LAIDate,LAI
        if ((LAIDate .gt. StartDate) .and. (LAIDate .lt. EndDate) .and. (LAI(jland) .gt. -998.0)) then
          if ((LAIDate .lt. SiteStart) .or. (LAIDate .gt. SiteEnd)) then
            rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
          else
            rangeweight = 1.0  ! within time range given for optimisation
            sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
          endif  
          if (LAISelect .ne. 'not') then
            countobs = countobs + 1
            obsname(countobs) = 'LAI'//trim(sitecode)//'B'//int2str4(LAIDate%Year)//int2str2(LAIDate%Month)//int2str2(LAIDate%Day) 
            obsinfo(countobs) = trim(LAIProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = LAI(jland)
            if (LAISelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'lai'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'LAI'
            obs_proc(countobs) = trim(LAIProcess)
!            write(6,*) countobs,obsname(countobs),obsinfo(countobs),obs(countobs),weight(countobs)
          endif
        endif
      end do
202   continue
      close(22)
      if (sitecount .gt. 0) then
        do j = countobs_newsite, countobs
          weight(j) = weight(j) * 1.0/sitecount ! weight obs by number of obs at each site if otherwise selected
        enddo
      endif
      write(6,*) 'Finished site ',sitecode
    end do
    close(21)
    write(6,*) 'Finished variable LAI'
    write(6,*) ' '
  endif  ! LAI observations
  ! end of LAI Obs
!------------------------------------------------------------------------------------------------------------------
!  VAST-Raison measurements (only include if in mask)
  if ((VRNPPSelect .ne. 'not') .or. (VRSoilC0Select .ne. 'not') .or. (VRPhySelect .ne. 'not') .or.  (VRLitSelect .ne. 'not')) then 
    write(6,*) 'VAST-Raison measurements:'
    flnm = trim(OBS_PATH) // 'VASTRaison/' // 'VastRaisonCombined.txt'
    open(unit=217,file=flnm,iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing VAST-Raison file',trim(flnm)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(217,*)
    read(217,*)
    do
      read(217,*,end=218) vr_lat,vr_lon,vr_sitenum,vr_NPP,vr_agphyto,vr_aglitter,vr_soilC0,vr_precip,vr_flag
      if ((vr_flag .eq. 1) .or. (vr_flag .eq. 2)) then   ! 1 for weight=1, 2 for weight=0 but include, 0 for don't include 
        ! Find index of corresponding gridcell
        found = .false.
        do j = 1,land_dim
          if ((abs(vr_lat-local_lat(j)) .le. modelres/2.0) .and. (abs(vr_lon-local_lon(j)) .le. modelres/2.0)) then
            jland = j
            found = .true.
            write(6,*) 'Gridcell is ',j
          endif
          if (found) exit
        end do
        if (.not.(found)) then
          write(6,*) 'Did not find model gridcell corresponding to observation ',vr_sitenum, vr_lat,vr_lon
        else
          write(vr_siteID,'(I0.4)') vr_sitenum   ! convert integer site code to 4-char string
          if ((vr_NPP .ne. -9999) .and. (VRNPPSelect .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'NPP'//trim(vr_siteID)//'R'
            obsinfo(countobs) = trim(VRNPPProcess)//int2str4(jland)//'PpB'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = vr_NPP
            if (vr_flag .eq. 2) then 
              weight(countobs) = 0
            else
              weight(countobs) = 1
            endif
            grpname(countobs) = 'leafnpp'
            obssitename(countobs) = trim(vr_siteID)
            obstype(countobs) = 'NPP'
            obs_proc(countobs) = trim(VRNPPProcess)
          endif
          if ((vr_agphyto .ne. -9999) .and. (VRPhySelect .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'Phy'//trim(vr_siteID)//'R'
            obsinfo(countobs) = trim(VRPhyProcess)//int2str4(jland)//'PpB'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = vr_agphyto
            if (vr_flag .eq. 2) then
              weight(countobs) = 0
            else
              weight(countobs) = 1
            endif
            grpname(countobs) = 'agphyto'
            obssitename(countobs) = trim(vr_siteID)
            obstype(countobs) = 'Phy'
            obs_proc(countobs) = trim(VRPhyProcess)
          endif
          if ((vr_aglitter .ne. -9999) .and. (VRLitSelect .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'Lit'//trim(vr_siteID)//'R'
            obsinfo(countobs) = trim(VRLitProcess)//int2str4(jland)//'PpB'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = vr_aglitter
            if (vr_flag .eq. 2) then
              weight(countobs) = 0
            else
              weight(countobs) = 1
            endif
            grpname(countobs) = 'aglitter'
            obssitename(countobs) = trim(vr_siteID)
            obstype(countobs) = 'Lit'
            obs_proc(countobs) = trim(VRLitProcess)
          endif
          if ((vr_SoilC0 .ne. -9999) .and. (VRSoilC0Select .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'SC0'//trim(vr_siteID)//'R'
            obsinfo(countobs) = trim(VRSoilC0Process)//int2str4(jland)//'PpB'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = vr_SoilC0
            if (vr_flag .eq. 2) then
              weight(countobs) = 0
            else
              weight(countobs) = 1
            endif
            grpname(countobs) = 'soilc0'
            obssitename(countobs) = trim(vr_siteID)
            obstype(countobs) = 'SC0'
            obs_proc(countobs) = trim(VRSoilC0Process)
          endif          
        endif
      endif
    end do
    write(6,*) 'Finished VAST_Raison measurements'
    write(6,*) ' '
218 continue  
  endif
!------------------------------------------------------------------------------------------------------------------
  if ((LBASelect .ne. 'not') .or. (LTDSelect .ne. 'not') .or. (AGDSelect .ne. 'not')) then 
  !  Auscover Biomass Plot Library measurements (only include if in mask)
    write(6,*) 'Read Auscover Biomass Plot Library observations'
    flnm = trim(OBS_PATH) // 'Auscover/' // 'biolib_sitelist_auscover_bios3.csv'
    open(unit=219,file=flnm,iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing Auscover BPL file',trim(flnm)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(219,*)
    sitecount = 0  ! used to generate standard sitename
    prevsite = 'dum'
    prevlat = 90.0
    prevlon = 0.0
    open(unit=220,file='Auscover_biolob_site_key.txt')  ! Create a file linking my sitename with the plot libraries
    write(220,*) 'Sitename_for_PEST  Auscover_Sitename'
    do
221   continue   
      read(219,'(A400)',end=219) line
      bpl_lat = GetArg(1)
      bpl_lon = GetArg(2)
      bpl_day = ifix(GetArg(3))
      bpl_mon = ifix(GetArg(4))
      bpl_yr = ifix(GetArg(5))
      bpl_sitename = GetStr(6)
      bpl_lba = GetArg(9)
      bpl_ltd = ifix(GetArg(10))
      bpl_agd = GetArg(11)
      bpl_agd_se = GetArg(15)
      bpl_undist = ifix(GetArg(24)) 
!write(6,*) 'bpl_lat=',bpl_lat,'*'
!write(6,*) 'bpl_lon=',bpl_lon,'*'
!write(6,*) 'bpl_day=',bpl_day,'*'
!write(6,*) 'bpl_mon=',bpl_mon,'*'
!write(6,*) 'bpl_yr=',bpl_yr,'*'
!write(6,*) 'bpl_sitename=',trim(bpl_sitename),'*'
!write(6,*) 'bpl_lba=',bpl_lba,'*'
!write(6,*) 'bpl_ltd=',bpl_ltd,'*'
!write(6,*) 'bpl_agd=',bpl_agd,'*'
!write(6,*) 'bpl_agd_se=',bpl_agd_se,'*'
!write(6,*) 'bpl_undist=',bpl_undist,'*'
      ! some lines do not have obs present, so would have bpl_undist = -999 so check for this and exclude
      if ((bpl_undist .ne. -999) .and. (bpl_undist .eq. 1)) then   ! in undisturbed gridcell 
        ! Find cable index of corresponding gridcell
        found = .false.
        do j = 1,land_dim
          if ((abs(bpl_lat-local_lat(j)) .le. modelres/2.0) .and. (abs(bpl_lon-local_lon(j)) .le. modelres/2.0)) then
            jland = j
            found = .true.
            write(6,*) 'Auscover Plot Library obs ',trim(bpl_sitename),' is in model gridcell ',jland,' with biome ',biome(jland),' and iveg=',iveg(jland)
            write(6,*) 'frac=',BiomeForestFrac(biome(jland))
          endif
          if (found) exit
        enddo
        if (found) then
          ! Find pop index for first patch of that gridcell
          foundp = .false.
          do j = 1,pop_land_dim   
            if ((abs(local_lat(jland)-pop_lat(j)) .le. modelres/10.0) .and. (abs(local_lon(jland)-pop_lon(j)) .le. modelres/10.0)) then
              pidx = j
              foundp = .true.
            endif
            if (foundp) exit
          enddo
          if (.not.(foundp)) then
            write(6,*) 'ERROR - Cannot find POP gridcell corresponding to observation at ',bpl_lat,' ',bpl_lon
            write(6,*) 'CABLE gridcell coords are ',local_lat(jland),local_lon(jland)
            write(6,*) 'Enter to continue'
           ! write(6,*) 'Program stopped'
           ! STOP
           found = .false.
          else 
            write(6,*) 'POP array index is ',pidx
            write(6,*) pop_lat(pidx),pop_lon(pidx)
            if ((abs(local_lat(jland)-pop_lat(pidx)) .gt. 0.001) .or. (abs(local_lon(jland)-pop_lon(pidx)) .gt. 0.001)) then 
              write(6,*) 'ERROR - mismatch in coordinates for cable and pop output'
              write(6,*) 'CABLE gridcell coords are ',local_lat(jland),local_lon(jland)
              write(6,*) 'POP gridcell coords are ',pop_lat(pidx),pop_lon(pidx)
              write(6,*) 'Program stopped'
              STOP
            endif
          endif
        endif
        if (.not.(found)) then
          write(6,*) 'Did not find model gridcell corresponding to observation ',trim(bpl_sitename), bpl_lat,bpl_lon
        else
          ! Check whether site is the same as previous obs or new
          if ((trim(bpl_sitename) .ne. trim(prevsite)) .or. ((bpl_lat-prevlat) .lt. 1.0e4) .or. ((bpl_lon-prevlon) .lt. 1.0e4)) then  ! new unique site
            sitecount = sitecount + 1  ! for sitename in PEST
            write(bpl_siteID,'(I0.5)') sitecount   ! convert sitecount to 5-char string
            write(220,*) bpl_siteID,' ',trim(bpl_sitename) 
          endif
          if ((bpl_lba .ne. 0) .and. (LBASelect .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'LBA'//trim(bpl_siteID)//'I'
            obsinfo(countobs) = trim(LBAProcess)//int2str5(pidx)//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
!write(6,*) BiomeForestFrac(biome(jland)),biome(jland)
!read(5,*)
            obs(countobs) = bpl_lba
            weight(countobs) = 1.0
            grpname(countobs) = 'bpllba'
            obssitename(countobs) = trim(bpl_siteID)
            obstype(countobs) = 'LBA'
            obs_proc(countobs) = trim(LBAProcess)
!            write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
          endif
          if ((bpl_ltd .ne. 0) .and. (LTDSelect .ne. 'not')) then
            countobs = countobs + 1
            obsname(countobs) = 'LTD'//trim(bpl_siteID)//'I'
            obsinfo(countobs) = trim(LTDProcess)//int2str5(pidx)//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = bpl_ltd
            weight(countobs) = 1.0
            grpname(countobs) = 'bplltd'
            obssitename(countobs) = trim(bpl_siteID)
            obstype(countobs) = 'LTD'
            obs_proc(countobs) = trim(LTDProcess)
          !  write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
          endif
          if ((bpl_agd .ne. 0) .and. (AGDSelect .ne. 'not')) then
            countobs = countobs + 1
            if (bpl_yr .lt. ModelStartDate%Year) then
              write(6,*) 'AGD obs ',trim(bpl_sitename),' for year ',bpl_yr,' set to model start year', ModelStartDate%Year
              bpl_yr = ModelStartDate%Year
            endif
            if (output_averaging .eq. 'day') then
              obsname(countobs) = 'AGD'//trim(bpl_siteID)//'D'//int2str4(bpl_yr)//int2str2(bpl_mon)//int2str2(bpl_day)
            else
              obsname(countobs) = 'AGD'//trim(bpl_siteID)//'M'//int2str4(bpl_yr)//int2str2(bpl_mon)
            endif
            obsinfo(countobs) = trim(AGDProcess)//int2str4(jland)//'PpB'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = bpl_agd
            weight(countobs) = 1.0
            grpname(countobs) = 'bplagd'
            obssitename(countobs) = trim(bpl_siteID)
            obstype(countobs) = 'AGD'
            obs_proc(countobs) = trim(AGDProcess)
          !  write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
          endif
        endif  ! found gridcell containing observation
      endif  ! if undisturbed gridcell
    end do  ! loop over lines in the file
219   continue    ! end reading Auscover Biomass Plot Library measurements 
    write(6,*) 'Finished Auscover Biomass Plot Library measurements'
    write(6,*) 
    close(220)
  endif
!------------------------------------------------------------------------------------------------------------------
! CosmOz soil moisture obs, daily means 9am-9am or monthly means of daily means
  if (SMCSelect .ne. 'not') then

    write(6,*) 'CosmOz soil moisture observations:'
    ! Read file describing observations to use, and the obs themselves
    OPEN(UNIT=21,FILE=trim(CosmOzSiteFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing file mentioned in Compileobservations.nml:',trim(CosmOzSiteFile)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(21,*) nsites
    read(21,*)
    do i = 1, nsites
      read(21,*) sitecode, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) 'CosmOz site for soil moisture ',trim(sitecode), ' =  ',trim(sitename)
      write(6,*) ddstart, mmstart, yystart, ddend, mmend, yyend,sitelat,sitelon,trim(sitepatch)
      write(6,*) sitecode, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,sitelat,sitelon,sitepatch
      SiteStart%Year = yystart
      SiteStart%Month = mmstart
      SiteStart%Day = ddstart
      SiteEnd%Year = yyend
      SiteEnd%Month = mmend
      SiteEnd%Day = ddend

      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Error finding model gridcell corresponding to observation ',sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      ! Check patch info is valid
      write(6,*) 'sitepatch, patchfrac =',sitepatch, patchfrac(jland,:)
      if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
        read(sitepatch,*) ipatch
        if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
          write(6,*) '*******************************************************************'
          write(6,*) 'ERROR: zero or missing patchfrac for patch specified for CosmOz site',sitename
          write(6,*) 'Patch for site is ',sitepatch
          write(6,*) 'patchfrac = ',patchfrac(jland,:)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      endif
      if (sitepatch .eq. 'a') ipatch = 1  ! used later for indexing zse
      ! read data file
      flnm = trim(OBS_PATH) // 'CosmOz/' // trim(sitename)//'_Level4Data.csv'
      write(6,*) 'Read CosmOz file ',trim(flnm)
      open(unit=22,file=flnm,status='OLD',iostat=iostat)
      if (iostat .ne. 0) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Missing CosmOz file:',trim(flnm)
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      read(22,*)  ! column descriptions
      countfile = 0
      do
        read(22,*,end=250) datestr, timestr, sm_hr, dep_hr
        countfile = countfile +1
   !     if (countfile .eq. 1) firstdatestr = datestr
      enddo
  250 continue
   !   lastdatestr = datestr
  !    read(firstdatestr,'(i4,a,i2,a,i2)') firstyy,ch,firstmm,ch,firstdd
  !    read(lastdatestr,'(i4,a,i2,a,i2)') lastyy,ch,lastmm,ch,lastdd
    !  allocate(sm(countfile))
     ! allocate(dep(countfile))
      close(22)
      open(unit=22,file=flnm,status='OLD')
      read(22,*)  ! column descriptions
      sitecount = 0
      countfile = 0
      sum_sm = 0.0
      sum_dep = 0.0
      count_hr = 0
      PrevDate%Year = 1900
      PrevDate%Month = 1
      PrevDate%Day = 1 
      do
        countfile = countfile + 1
        read(22,*,end=251) datestr, timestr, sm_hr, dep_hr
        read(datestr,'(i4,a,i2,a,i2)') yy,ch,mm,ch,dd
        read(timestr,'(i2,a,i2)') hh,ch,nn
        CurrentDate%Year = yy
        CurrentDate%Month = mm
        CurrentDate%Day = dd
        if (hh .lt. 9) then ! time is before 9am, so include in previous day (need to calculate yesterday's date)
          CurrentDate%Day = dd-1
          if (dd-1 .lt. 1) then
            CurrentDate%Month = mm-1  ! previous day is in previous month
            if (mm-1 .lt. 1) then
              CurrentDate%Year = yy-1  ! previous day is in previous year
              CurrentDate%Month = 12
              CurrentDate%Day = 31
            else
              CurrentDate%Day = DaysInMonth(CurrentDate) 
            endif
          endif
        endif
        if ((PrevDate .ne. CurrentDate) .and. (countfile .ne. 1)) then
          ! if more than 12 measurements in a daily average, and if not 29 Feb with leapflag=false, then write daily value to file
          if ((count_hr .gt. 12) .and. (.not.(.not.(leapflag) .and. (PrevDate%Day .eq. 29) .and. (PrevDate%Month .eq. 2))))then  
            sm = sum_sm/count_hr
            dep = sum_dep/count_hr
            count_hr = 0
            sum_sm = 0.0
            sum_dep = 0.0
            if (SMCdm .eq. 'day') then ! write to observations file
              countobs = countobs + 1
              found = .false.
              if (dep/100.0 .le. 0.5*zse(jland,ipatch,1)) then
                nlayer = 0   ! nlayer is shallowest later used for interpolation of soil layers for sm (zero for depth > 0.5*zse(1)
                found = .true.
              else
                do j = 1,soil_dim-1
                  if ((sum(zse(jland,ipatch,1:j-1))+0.5*zse(jland,ipatch,j) .lt. dep/100.0) .and.   &  ! find nlayer
                           (sum(zse(jland,ipatch,1:j))+0.5*zse(jland,ipatch,j+1) .ge. dep/100.0)) then
                    nlayer = j
                    found = .true.
                  endif
                  if (found) exit
                end do
              endif
              if (.not.(found)) then
                write(6,*) '*******************************************************************'
                write(6,*) 'Error finding model soil layer corresponding to CosmOz observation ',sitecode,' ',sitename,' ',PrevDate,' ',dep
                write(6,*) 'Program stopped'
                write(6,*) '*******************************************************************'
                STOP
              endif
              obsname(countobs) = 'SMC'//trim(sitecode)//'D'//int2str4(PrevDate.Year)//int2str2(PrevDate.Month)//int2str2(PrevDate.Day) &
                    //'L'//int2str1(nlayer)
              if (dep .lt. 10.0) then
                write(depstr,'(f5.3)') dep
              else 
                write(depstr,'(f5.2)') dep
              endif
              obsinfo(countobs) = trim(SMCProcess)//int2str4(jland)//'D'//trim(depstr)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
              obs(countobs) = sm/100.0  ! convert % to fraction
              if ((PrevDate .ge. SiteStart) .and. (PrevDate .le. SiteEnd)) then
                weight(countobs) = 1.0
              else
                 weight(countobs) = 0.0
              endif
              grpname(countobs) = 'smcosmoz'
              obssitename(countobs) = trim(sitecode)
              obstype(countobs) = 'SMC'
              obs_proc(countobs) = trim(SMCProcess)
            !  write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
            else  ! save for monthly obs
              count_days = count_days + 1   ! **** fill this in for monthly ave....
            endif
          endif  
        endif
        if (CurrentDate .ge. EndDate) goto 251 ! all remaining data is beyond range of model output, so ignore
        count_hr = count_hr + 1
        sum_sm = sum_sm + sm_hr
        sum_dep = sum_dep + dep_hr  ! depths are in cm (leave as works well in obsinfo)
        PrevDate = CurrentDate
      enddo
  251 continue
    enddo
    write(6,*) 'Finished CosmOz measurements'
    write(6,*) ' '
  endif
 

!------------------------------------------------------------------------------------------------------------------
! OzNet soil moisture obs, daily means 
  if (SMOSelect .ne. 'not') then

    write(6,*) 'OzNet soil moisture observations:'
    ! Read file describing observations to use, and the obs themselves
    OPEN(UNIT=21,FILE=trim(OzNetSiteFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing file mentioned in Compileobservations.nml:',trim(OzNetSiteFile)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(21,*) nsites
    do i = 1, nsites
      read(21,*) sitecode, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) 'OzNet site for soil moisture ',trim(sitecode), ' =  ',trim(sitename)
      write(6,*) ddstart, mmstart, yystart, ddend, mmend, yyend,sitelat,sitelon,trim(sitepatch)
      SiteStart%Year = yystart
      SiteStart%Month = mmstart
      SiteStart%Day = ddstart
      SiteEnd%Year = yyend
      SiteEnd%Month = mmend
      SiteEnd%Day = ddend

      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        write(6,*) 'No model gridcell corresponding to observation ',sitecode, sitename,sitelat,sitelon
      else
        ! Check patch info is valid
        write(6,*) 'sitepatch, patchfrac = ',sitepatch, patchfrac(jland,:)
        if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
          read(sitepatch,*) ipatch
          if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
            write(6,*) '*******************************************************************'
            write(6,*) 'ERROR: zero or missing patchfrac for patch specified for OzNet site',sitename
            write(6,*) 'Patch for site is ',sitepatch
            write(6,*) 'patchfrac = ',patchfrac(jland,:)
            write(6,*) 'Program stopped'
            write(6,*) '*******************************************************************'
            STOP
          endif
        endif
        if (sitepatch .eq. 'a') ipatch = 1  ! used later for indexing zse
        ! read data file
        flnm = trim(OBS_PATH) // 'OzNet/' // trim(StrLowCase(sitename))//'_OzNet_daily_sm.csv'
        write(6,*) 'Read OzNet file ',trim(flnm)
        open(unit=22,file=flnm,status='OLD',iostat=iostat)
        if (iostat .ne. 0) then
          write(6,*) '*******************************************************************'
          write(6,*) 'Missing Oznet file:',trim(flnm)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
        ! read column headings into string array sarray
        read(22,'(a)') strline
        n = count( (/ (strline(i:i), i=1, len_trim(strline)) /) == ",")
        allocate(sarray(n))
        allocate(smdeps(n))
        allocate(oznet(n))
        smdeps(:) = 'xxxx'
        oznet(:) = -999.0
        strline = strline(2:len(strline)) ! remove first comma (no column title for date)
        do j = 1,n-1
          k = index(strline,',')         ! position of first comma
          sarray(j) = strline(1:k-1)     
          strline = strline(k+1:len(strline))
        enddo
        sarray(5) = strline
        write(6,*) 'Column headings are '
        do j = 1,n
          str = trim(sarray(j))
          if (StrLowCase(str(1:2)) .eq. 'sm') then
            k = index(str,'-')         ! position of dash
            m = index(str,'c')         ! position of 'c'
            if (str(3:3) .eq. ' ') then
              p = 4
            else 
              p = 3
            endif
            if (str(p:p) .ne. '(') then
              s1 = str(p:k-1)
            else  
              s1 = str(p+1:k-1)
            endif
            if (len(trim(s1)) .eq. 1) then
              s1(2:2) = s1(1:1)
              s1(1:1) = '0'
            endif
            s2 = str(k+1:m-1)
            if (len(trim(s2)) .eq. 1) then
              s2(2:2) = s2(1:1)
              s2(1:1) = '0'
            endif
            smdeps(j) = s1//s2
            write(6,*)  '"',trim(sarray(j)),'" -> ',smdeps(j)
          endif
        enddo 
        ! read data
        do j = 1,n  ! loop over columns in data file, so obs from same depth range are together
          close(22)
          open(unit=22,file=flnm,status='OLD')
          read(22,*)  ! column descriptions
          do
            read(22,*,end=351) datestr,oznet 
            read(datestr,'(i4,a,i2,a,i2)') yy,ch,mm,ch,dd
            CurrentDate%Year = yy
            CurrentDate%Month = mm
            CurrentDate%Day = dd
            ! ignore Feb 29 if not using leap years
            if ((.not.(.not.(leapflag) .and. (CurrentDate%Day .eq. 29) .and. (CurrentDate%Month .eq. 2))) .and. (CurrentDate .lt. EndDate)) then  
              if ((smdeps(j) .ne. 'xxxx') .and. (oznet(j) .ne. -999.0)) then
                countobs = countobs + 1
                obsname(countobs) = 'SMO'//trim(sitecode)//'D'//int2str4(CurrentDate.Year)//int2str2(CurrentDate.Month)//int2str2(CurrentDate.Day) &
                  //'L'//smdeps(j)
                obsinfo(countobs) = trim(SMOProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                  //'R'//int2str1(ifix(reccap(jland)))
                obs(countobs) = oznet(j)/100.0  ! convert % to fraction
                if ((CurrentDate .ge. SiteStart) .and. (CurrentDate .le. SiteEnd)) then
                  weight(countobs) = 1.0
                else
                   weight(countobs) = 0.0
                endif
                grpname(countobs) = 'smoznet'
                obssitename(countobs) = trim(sitecode)
                obstype(countobs) = 'SMO'
                obs_proc(countobs) = trim(SMOProcess)
            !    write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
              endif
            endif
            if (CurrentDate .ge. EndDate) goto 351 ! all remaining data is beyond range of model output, so ignore
          enddo
  351     continue
        enddo  ! loop over columns in datafile
        deallocate(sarray)
        deallocate(smdeps)
        deallocate(oznet)
      endif
    enddo
    write(6,*) 'Finished OzNet soil moisture measurements'
    write(6,*) ' '
  endif   ! end of OzNet soil moisture

!------------------------------------------------------------------------------------------------------------------
! OzFlux soil moisture obs, daily means 
  if (SMFSelect .ne. 'not') then

    write(6,*) 'OzFlux soil moisture observations:'
    ! Read file describing observations to use, and the obs themselves
    OPEN(UNIT=21,FILE=trim(OzFluxSMSiteFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing file mentioned in CompileObservations.nml:',trim(OzFluxSMSiteFile)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(21,*) nsites
    read(21,*)
    do i = 1, nsites
      read(21,*) sitecode, sitename, mmstart, yystart, mmend, yyend,sitelat,sitelon,sitepatch
      write(6,*) 'OzFlux site for soil moisture ',trim(sitecode), ' =  ',trim(sitename)
      write(6,*) mmstart, yystart, mmend, yyend,sitelat,sitelon,trim(sitepatch)
      SiteStart%Year = yystart
      SiteStart%Month = mmstart
      SiteStart%Day = 1
      SiteEnd%Year = yyend
      SiteEnd%Month = mmend
      SiteEnd%Day = 1
      SiteEnd%Day = DaysInMonthEither(SiteEnd,LeapFlag)
      ! Find index of corresponding gridcell
      found = .false.
      do j = 1,land_dim
        if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
          jland = j
          found = .true.
          write(6,*) 'Gridcell is ',j
        endif
        if (found) exit
      end do
      if (.not.(found)) then
        write(6,*) 'No model gridcell corresponding to observation ',sitecode, sitename,sitelat,sitelon
      else
        ! Check patch info is valid
        write(6,*) 'sitepatch, patchfrac  ',sitepatch, patchfrac(jland,:)
        if ((sitepatch .eq. '1') .or. (sitepatch .eq. '3')) then
          read(sitepatch,*) ipatch
          if ((patchfrac(jland,ipatch) .eq. 0.0) .or. (patchfrac(jland,ipatch) .eq. patchfill) .or. (patchfrac(jland,ipatch) .eq. patchmissing)) then
            write(6,*) '*******************************************************************'
            write(6,*) 'ERROR: zero or missing patchfrac for patch specified for OzFlux soil moisture site',sitename
            write(6,*) 'Patch for site is ',sitepatch
            write(6,*) 'patchfrac = ',patchfrac(jland,:)
            write(6,*) 'Program stopped'
            write(6,*) '*******************************************************************'
            STOP
          endif
        endif
        if (sitepatch .eq. 'a') ipatch = 1  ! used later for indexing zse
        ! read data file
        if (SMFdm .eq. 'mon') tres = 'Monthly'
        if (SMFdm .eq. 'day') tres = 'Daily'
        if (SMFdm .eq. 'hho') tres = 'HalfHourly'
        flnm = trim(OBS_PATH) // 'OzFlux/' // trim(sitename)//'_SWC_'//trim(tres)//'.csv'
        write(6,*) 'Read OzFlux soil moisture file ',trim(flnm)
        open(unit=22,file=flnm,status='OLD',iostat=iostat)
        if (iostat .ne. 0) then
          write(6,*) '*******************************************************************'
          write(6,*) 'Missing OxFlux soil moisture file:',trim(flnm)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
        read(22,*)
        sitecount = 0  ! running count of obs for optimisation at that site
        countobs_newsite = countobs+1
        dd = 1
        hh = 1
        ff = 0
        do
          if (SMFdm .eq. 'mon') read(22,*,end=207) yy, mm, SMF
          if (SMFdm .eq. 'day') read(22,*,end=207) yy, mm, dd, SMF
          if (SMFdm .eq. 'hho') read(22,*,end=207) yy, mm, dd, hh, ff, SMF
          CurrentDate%Year = yy
          CurrentDate%Month = mm
          CurrentDate%Day = dd
          if ((CurrentDate .lt. StartDate) .or. (CurrentDate .ge. EndDate) .or.   &
                ((.not.(leapflag)) .and. (CurrentDate%Day .eq. 29) .and. (CurrentDate%Month .eq. 2))) then
         !   write(6,*) 'OzFlux observations at site ',trim(sitecode),' in year ',yy,' are out of range for model or 29 Feb'
         !   write(6,*) 'Not included in observation file'
          else
            if ((yy .lt. yystart) .or. (yy .gt. yyend)) then
              rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
            else
              rangeweight = 1.0  ! within time range given for optimisation
              sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
            endif
            countobs = countobs + 1
            if (SMFdm .eq. 'mon') obsname(countobs) = 'SMF'//trim(sitecode)//'M'//int2str4(yy)//int2str2(mm)//'L0021'
            if (SMFdm .eq. 'day') obsname(countobs) = 'SMF'//trim(sitecode)//'D'//int2str4(yy)//int2str2(mm)//int2str2(dd)//'L0021'
            if (SMFdm .eq. 'hho') obsname(countobs) = 'SMF'//trim(sitecode)//'F'//int2str4(yy)//int2str2(mm)//int2str2(dd)//int2str2(hh)  &
                    //int2str2(ff)//'L0021'
            obsinfo(countobs) = trim(SMFProcess)//int2str4(jland)//'P'//sitepatch//'B'//int2str2(biome(jland))//'N'//int2str2(ifix(nvis(jland)))  &
                    //'R'//int2str1(ifix(reccap(jland)))
            obs(countobs) = SMF
            if (SMFSelect .eq. 'opt') then
              weight(countobs) = rangeweight
            else
              weight(countobs) = 0.0
            endif
            grpname(countobs) = 'smf'
            obssitename(countobs) = trim(sitecode)
            obstype(countobs) = 'SMF'
            obs_proc(countobs) = trim(SMFProcess)
          endif
          if ((CurrentDate .ge. SiteEnd) .or. (CurrentDate .ge. EndDate)) goto 207 ! all remaining data is beyond range of model output, so ignore
        enddo
      endif 
207   continue
      close(22)
      if (sitecount .gt. 0) then
        do j = countobs_newsite, countobs
          weight(j) = weight(j) * 1.0/sitecount ! weight obs by number of obs at each site if otherwise selected
        enddo
      endif
      write(6,*) 'Finished OzFlux soil moisture for ',sitecode
    end do
    CLOSE(21)
    write(6,*) 'Finished OzFlux soil moisture'
    write(6,*) ' '
  endif  ! OzFlux soil moisture observations
  ! end of OzFlux soil moisture Obs

!-------------------------------------------------------------------------------------------------------------------------------------------------------
  ! Read streamflow observations
  ! Observations are in a csv file for all catchments together, daily data
  Qobs_xls_nt = 13880
  Qobs_xls_ncatch = 786

  if (STRSelect .ne. 'not') then 

    write(6,*) 'Streamflow observations:'
    ! Read file describing gridcells that represent the catchments, and obs themselves
    OPEN(UNIT=21,FILE=trim(CatchmentFile),STATUS='OLD',iostat=iostat)
    if (iostat .ne. 0) then
      write(6,*) '*******************************************************************'
      write(6,*) 'Missing file mentioned in Compileobservations.nml:',trim(CatchmentFile)
      write(6,*) 'Program stopped'
      write(6,*) '*******************************************************************'
      STOP
    endif
    read(21,*)
    read(21,*) ncatch
    write(6,*) ncatch,' catchments'
    ! check to see whether we need to read the large xls file
    readxls = .false.
    do i = 1, ncatch  ! loop over catchments
      read(21,*,iostat=iostat) catchname, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,ncells,iarea,xls_csv
      if (iostat .ne. 0) then
        write(6,*) 'Error reading file:',trim(flnm)
        write(6,*) 'Check that file contains the following information for each catchment:'
        write(6,*) 'catchname, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,ncells,iarea,xls_csv'
        write(6,*) 'Program stopped'
        STOP
      endif
      if (xls_csv .eq. 'xls') readxls = .true.
      do k = 1,ncells
        read(21,*)
      enddo
    enddo
    rewind(21)
    read(21,*) 
    read(21,*) 
    if ((ncatch .gt. 0) .and. readxls) then  ! read obs file
      allocate(Qobs_xls_catch(Qobs_xls_ncatch))
      allocate(Qobs_xls_date(Qobs_xls_nt))
      allocate(Qobs_xls_Q(Qobs_xls_ncatch,Qobs_xls_nt)) 
      write(6,*) 'Reading catchment Q obs from file Catchment_Qobs.csv (takes some time)'
      flnm = trim(OBS_PATH) // 'Zhang/' // 'Catchment_Qobs.csv'
      open(unit=22,file=flnm,status='old',iostat=iostat)
      if (iostat .ne. 0) then
        write(6,*) '*******************************************************************'
        write(6,*) 'Missing file:',trim(flnm)
        write(6,*) 'Program stopped'
        write(6,*) '*******************************************************************'
        STOP
      endif
      read(22,*) str,Qobs_xls_catch
      do i = 1, Qobs_xls_nt
        read(22,'(a)') line
        read(line(1:4),'(i)') CurrentDate%Year 
        read(line(6:7),'(i)') CurrentDate%Month 
        read(line(9:10),'(i)') CurrentDate%Day 
        Qobs_xls_date(i) = CurrentDate 
        line = trim(line(12:len(line)))
        do j = 1,Qobs_xls_ncatch
          Qobs_xls_Q(j,i) = GetArg(j)  ! read obs from the string read from input file (missing set to -999.0)
        enddo
        if ((.not.(LeapFlag)) .and. (CurrentDate%Month .eq. 2) .and. (CurrentDate%Day .eq. 29)) then  ! set Feb 29 to missing if not using leap years
          Qobs_xls_Q(:,i) = -999.0
        endif
      enddo
      close(22)
      write(6,*) 'Streamflow datafile read'
    endif
    write(6,*) 'Using obs from ',ncatch,' catchments'
    open(unit=26,file='Catchment_gridcells.txt')     ! details of catchment locations (which model gridcells) written to file
    write(26,'(i)') ncatch   ! number of catchments
    do i = 1, ncatch  ! loop over catchments
      totalprecip = 0.0
      totalprecipcount = 0
      jlandcount = 0
      read(21,*) catchname, sitename, ddstart, mmstart, yystart, ddend, mmend, yyend,ncells,iarea,xls_csv
      write(6,*) 'Catchment ',catchname,' (',trim(sitename),') has ',ncells,' gridcells, datafile is ',xls_csv
      write(26,*) catchname,'  ',ncells,' ',iarea
      do k = 1,ncells
        read(21,*) sitelat,sitelon
        ! Find index of corresponding gridcell
        found = .false.
        do j = 1,land_dim
          if ((abs(sitelat-local_lat(j)) .le. modelres/2.0) .and. (abs(sitelon-local_lon(j)) .le. modelres/2.0)) then
            jland = j
            found = .true.
            write(6,*) 'Gridcell is ',jland
            write(26,*) jland, ' ',biome(jland),' ',ifix(nvis(jland)),' ',ifix(reccap(jland))
            jlandcount = jlandcount + 1
            jlandsav(jlandcount) = jland  ! saved to sum up precip
          endif
          if (found) exit
        end do
        if (.not.(found)) then
          write(6,*) '*******************************************************************'
          write(6,*) 'Error finding model gridcell corresponding to catchment ',catchname,sitelat,sitelon
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      enddo
      if (xls_csv .eq. 'xls') then
        found = .false.  ! find index for current catchment in data array
        do k = 1, Qobs_xls_ncatch 
          if (trim(Qobs_xls_catch(k)) .eq. trim(catchname)) then
            found = .true.
            write(6,*) 'Index for this catchment in data file is ',k
            exit 
          endif
        enddo 
        if (found) then
          allocate(Qobs_Q(Qobs_xls_nt))
          allocate(Qobs_date(Qobs_xls_nt))
          Qobs_date = Qobs_xls_date
          Qobs_Q = Qobs_xls_Q(k,:)
          Qobs_nt = Qobs_xls_nt
        else
          write(6,*) '*******************************************************************'
          write(6,*) 'Error finding data for catchment ',trim(catchname),' in data file'
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
      else
        flnm = trim(OBS_PATH) // 'Zhang/' // trim(catchname) // '.csv'
        write(6,*) 'Read file ',trim(flnm)
        open(unit=23,file= flnm,status='old',iostat=iostat)
        if (iostat .ne. 0) then
          write(6,*) '*******************************************************************'
          write(6,*) 'Missing streamflow csv file: ',trim(flnm)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************'
          STOP
        endif
        read(23,*) 
        do    ! how many rows of data?
          k = k + 1
          read(23,*,end=230) 
        enddo      
 230    continue
        Qobs_nt = k
        allocate(Qobs_Q(Qobs_nt))
        allocate(Qobs_date(Qobs_nt))
        rewind(23)
        read(23,*)
        k = 0
        do   ! read the STR data from csv file
          k = k + 1
          read(23,*,end=231)  datestr,Q
          read(datestr(1:4),'(i)') CurrentDate%Year
          read(datestr(6:7),'(i)') CurrentDate%Month
          read(datestr(9:10),'(i)') CurrentDate%Day
          Qobs_date(k) = CurrentDate
          Qobs_Q(k) = Q
        enddo
231     continue
      endif
      if (STRdm .eq. 'day') then
        do k = 1,Qobs_nt
          if ((Qobs_Q(k) .ne. -999) .and. (Qobs_Q(k) .ne. -9999)) then  ! valid obs
            CurrentDate = Qobs_date(k)
            if ((CurrentDate .ge. StartDate) .and. (CurrentDate .lt. EndDate)) then  ! within model range
              if ((CurrentDate%Year .lt. yystart) .or. (CurrentDate%Year .gt. yyend)) then
                rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
              else
                rangeweight = 1.0  ! within time range given for optimisation
                sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
              endif
              countobs = countobs + 1
              if (len(trim(catchname)) .eq. 4) catchname = '00'//catchname
              obsname(countobs) = 'STR'//trim(catchname)//'D'//int2str4(CurrentDate%Year)//int2str2(CurrentDate%Month)//int2str2(CurrentDate%Day)
              obsinfo(countobs) = trim(STRProcess)//int2str3(ncells)//'Pa'//int2str5(iarea)
! if we ever choose to have patch 'p' to reweight patch ave, then need to specify it in the catchment details file.
              obs(countobs) = Qobs_Q(k)
              if (STRSelect .eq. 'opt') then
                weight(countobs) = rangeweight
              else
                weight(countobs) = 0.0
              endif
              grpname(countobs) = 'strmflow'
              obssitename(countobs) = trim(catchname)
              obstype(countobs) = 'STR'
              obs_proc(countobs) = trim(STRProcess)
              !write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
              timeidx = FindTimeIndex (CurrentDate,timeDMY,leapflag) 
              do ig = 1,jlandcount  ! loop over gridcells in catchment
                totalprecip = totalprecip + (Rainf(jlandsav(ig),1,timeidx)*patchfrac(jlandsav(ig),1)  &
                            + Rainf(jlandsav(ig),2,timeidx)*patchfrac(jlandsav(ig),2) + Rainf(jlandsav(ig),3,timeidx)*patchfrac(jlandsav(ig),3)) ! mm/d
                totalprecipcount = totalprecipcount + 1
              enddo
            endif
          endif
        enddo
      endif   ! using daily streamflow obs
      if (STRdm .eq. 'mon') then
        Qsum = 0.0
        Qdays = 0
        precip = 0.0
        precipcount = 0
        PreviousDate%Year = 1800
        PreviousDate%Month = 1
        PreviousDate%Day = 1 
        do k = 1,Qobs_nt   ! loop over daily obs at current catchment
          if ((Qobs_Q(k) .ne. -999) .and. (Qobs_Q(k) .ne. -9999)) then  ! valid obs
            CurrentDate = Qobs_date(k)
            if ((CurrentDate%Year .ge. StartDate%Year) .and. (CurrentDate%Year .lt. EndDate%Year)) then  ! within model range
              if ((CurrentDate%Day .eq. 1) .or. ((CurrentDate%Month .eq. PreviousDate%Month) .and. (CurrentDate%Year .eq. PreviousDate%Year) .and. &
                                                (Qdays .eq. CurrentDate%Day-1))) then  ! keep adding to running sum, as there is no missing data
                Qsum = Qsum + Qobs_Q(k)
                Qdays = Qdays + 1
                timeidx = FindTimeIndex (CurrentDate,timeDMY,leapflag)  ! calculate long-term mean precip corresp to obs
                do ig = 1,jlandcount  ! loop over gridcells in catchment
                  precip = precip + Rainf(jlandsav(ig),1,timeidx)  ! mm/d   
                  precipcount = precipcount + 1
                enddo
                if ((CurrentDate%Day .eq. DaysInMonthEither(CurrentDate,LeapFlag)) .and. (Qdays .eq. CurrentDate%Day)) then ! end of month, all days exist
                  if ((CurrentDate%Year .lt. yystart) .or. (CurrentDate%Year .gt. yyend)) then   ! 
                    rangeweight = 0.0  ! outside time range given for optimisation (include in file with zero weight)
                  else
                    rangeweight = 1.0  ! within time range given for optimisation
                    sitecount = sitecount + 1 ! counting number of obs for each site within time range to calculate obs weight
                  endif
                  countobs = countobs + 1
                  if (len(trim(catchname)) .eq. 4) catchname = '00'//catchname
                  obsname(countobs) = 'STR'//trim(catchname)//'M'//int2str4(CurrentDate%Year)//int2str2(CurrentDate%Month)
                  obsinfo(countobs) = trim(STRProcess)//int2str3(ncells)//'Pa'//int2str5(iarea)
! if we ever choose to have patch 'p' to reweight patch ave, then need to specify it in the catchment details file.
                  obs(countobs) = Qsum/Qdays  ! monthly mean in mm/d
                  if (STRSelect .eq. 'opt') then
                    weight(countobs) = rangeweight
                  else
                    weight(countobs) = 0.0
                  endif
                  grpname(countobs) = 'strmflow'
                  obssitename(countobs) = trim(catchname)
                  obstype(countobs) = 'STR'
                  obs_proc(countobs) = trim(STRProcess)
               !   write(6,*) obsname(countobs),' ',obsinfo(countobs),' ',obs(countobs)
                  Qsum = 0.0
                  Qdays = 0
                  totalprecip = totalprecip + precip
                  totalprecipcount = totalprecipcount + precipcount
                  precip = 0.0
                  precipcount = 0
                endif
                PreviousDate = CurrentDate
              else
                Qsum = 0.0
                Qdays = 0
                precip = 0.0
                precipcount = 0
              endif ! end of month - calculate monthly value if all days are present
            endif   ! time is within model time range
          endif     ! valid observation
        enddo       ! loop over obs at this catchment
      endif         ! if using monthly streamflow obs
      if (totalprecipcount .gt. 0) then
        write(26,*) totalprecip/totalprecipcount
      else
        write(26,*) '0.0'
      endif
      deallocate(Qobs_Q,Qobs_date)
    end do          ! loop over catchments
    close(21)
    close(26)
    write(6,*) 'Finished streamflow measurements'
    write(6,*) ' '
  endif  ! streamflow observations

!------------------------------------------------------------------------------------------------------------------
  ! Sort observations so that different observation types are together (needed because OzFlux obs alternate GPP, NEP, Rec, EvT, and 
  ! processing and ExtractObservables need obs types to be together)
  icount = 0
  allocate(sortedobs(countobs))
  allocate(sortedsitename(countobs))
  allocate(sortedobstype(countobs))
  allocate(sortedobs_proc(countobs))
  allocate(sortedweight(countobs))
  allocate(sortedobsname(countobs))
  allocate(sortedobsinfo(countobs))
  allocate(sortedgrpname(countobs))
  do j = 1,size(obscode)
    do i = 1,countobs
       str = obsname(i)
       if (str(1:3) .eq. obscode(j)) then
         icount = icount + 1
         sortedobs(icount) = obs(i)
         sortedsitename(icount) = obssitename(i)
         sortedobstype(icount) = obstype(i)
         sortedobs_proc(icount) = obs_proc(i)
         sortedobsname(icount) = obsname(i)
         sortedgrpname(icount) = grpname(i)
         sortedobsinfo(icount) = obsinfo(i)
         sortedweight(icount) = weight(i)
       endif
    end do
  end do

  ! Calculate temporal anomalies for any variables that require it
  varsum = 0.0
  varcount = 0
  firstidx = 0
  lastcurrent = .false.
  do i = 1,countobs
    if (sortedobs_proc(i) .eq. 'tan') then
      varsum = varsum + sortedobs(i)
      varcount = varcount + 1
      if (firstidx .eq. 0) firstidx = i
      ! check to see whether last obs of current site and observation type, in which case calculate and subtract mean
      if (i .eq. countobs) lastcurrent = .true.
      if (.not.(lastcurrent)) then
        if ((sortedsitename(i) .ne. sortedsitename(i+1)) .or. (sortedobstype(i) .ne. sortedobstype(i+1))) lastcurrent = .true.
      endif
      if (lastcurrent) then
        varmean = varsum/varcount
        do j = firstidx,i
          sortedobs(j) = sortedobs(j) - varmean
        enddo
        write(6,*) 'Subtract temporal anomaly for variable ',trim(obstype(i)),' at site ',trim(obssitename(i))
        write(6,*) '         -> anomaly is ',varmean
        varsum = 0.0
        varcount = 0
        firstidx = 0
        lastcurrent = .false.
      endif
    endif
  enddo

!------------------------------------------------------------------------------------------------------------------
  ! Write obs to file, sorted by observation type
  ! Create three files, one with obsinfo, one without and one with obsnames for *.ins file
  open(unit=23,file='ObsSpecs.txt')   ! gives obsname, value, unc, group and obsinfo
  open(unit=24,file='Obs4pst.txt')    ! gives info for pst file (obsname, value, unc, group)
  open(unit=25,file='Obs.ins')  ! gives obsnames with formatting info
  write(23,*) countobs
  write(24,*) countobs
  write(25,*) 'pif #'
  write(25,*) '#output#'
  do i = 1,countobs
    write(23,'(a20,a,e,a,e,a,a10,a,a20)') trim(sortedobsname(i)),' ',sortedobs(i),' ',sortedweight(i),' ',trim(sortedgrpname(i)),' ', trim(sortedobsinfo(i))
    write(24,*) sortedobsname(i),  sortedobs(i), sortedweight(i), sortedgrpname(i)
    write(25,*) ' l1 [', trim(sortedobsname(i)),']1:14'
  end do
  close(23)
  close(24)
  close(25)

  write(6,*) 'CompileObservations finished successfully '
  write(6,*) countobs,' observations written for:'
  if (EvTSelect .ne. 'not') write(6,*) 'EvTSelect is "',EvTSelect,'" with processing "',EvTProcess,'" and time resolution "',OzFluxRes,'"'
  if (GPPSelect .ne. 'not') write(6,*) 'GPPSelect is "',GPPSelect,'" with processing "',GPPProcess,'" and time resolution "',OzFluxRes,'"'
  if (NEPSelect .ne. 'not') write(6,*) 'NEPSelect is "',NEPSelect,'" with processing "',NEPProcess,'" and time resolution "',OzFluxRes,'"'
  if (RecSelect .ne. 'not') write(6,*) 'RecSelect is "',RecSelect,'" with processing "',RecProcess,'" and time resolution "',OzFluxRes,'"'
  if (RsoSelect .ne. 'not') write(6,*) 'RsoSelect is "',RsoSelect,'" with processing "',RsoProcess,'" and time resolution "',Rsodm,'"'
  if (LAISelect .ne. 'not') write(6,*) 'LAISelect is "',LAISelect,'" with processing "',LAIProcess,'"'
  if (SMCSelect .ne. 'not') write(6,*) 'SMCSelect is "',SMCSelect,'" with processing "',SMCProcess,'" and time resolution "',SMCdm,'"'
  if (SMOSelect .ne. 'not') write(6,*) 'SMOSelect is "',SMOSelect,'" with processing "',SMOProcess,'" and time resolution "',SMOdm,'"'
  if (SMFSelect .ne. 'not') write(6,*) 'SMFSelect is "',SMFSelect,'" with processing "',SMFProcess,'" and time resolution "',SMFdm,'"'
  if (STRSelect .ne. 'not') write(6,*) 'STRSelect is "',STRSelect,'" with processing "',STRProcess,'" and time resolution "',STRdm,'"'
  if (VRNPPSelect .ne. 'not') write(6,*) 'VRNPPSelect is "',VRNPPSelect,'" with processing "',VRNPPProcess,'"'
  if (VRSoilC0Select .ne. 'not') write(6,*) 'VRSoilC0Select is "',VRSoilC0Select,'" with processing "',VRSoilC0Process,'"'
  if (VRPhySelect .ne. 'not') write(6,*) 'VRPhySelect is "',VRPhySelect,'" with processing "',VRPhyProcess,'"'
  if (VRLitSelect .ne. 'not') write(6,*) 'VRLitSelect is "',VRLitSelect,'" with processing "',VRLitProcess,'"'
  if (LBASelect .ne. 'not') write(6,*) 'LBASelect is "',LBASelect,'" with processing "',LBAProcess,'"'
  if (LTDSelect .ne. 'not') write(6,*) 'LTDSelect is "',LTDSelect,'" with processing "',LTDProcess,'"'
  if (AGDSelect .ne. 'not') write(6,*) 'AGDSelect is "',AGDSelect,'" with processing "',AGDProcess,'"'

end program CompileObservations

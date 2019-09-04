! Cathy Trudinger Feb 2017
! Extract observables from cable's nc files, for use in PEST
!----------------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------------
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

FUNCTION NVISMVGForestFrac (gridcell_MVG)
! For given NVIS Major Vegetation Group, return forest fraction
  implicit none
  integer, intent (in) :: gridcell_MVG
  real :: NVISMVGForestFrac,CPC,projection_factor
  projection_factor = 0.71
  select case (gridcell_MVG)
    case(1)
      CPC = 0.89
    case(2)
      CPC = 0.81
    case(3)
      CPC = 0.79
    case(4)
      CPC = 0.50
    case(5)
      CPC = 0.31
    case(6)
      CPC = 0.15
    case(7)
      CPC = 0.37
    case(8)
      CPC = 0.27
    case(9)
      CPC = 0.23
    case(10)
      CPC = 0.24
    case(11)
      CPC = 0.19
    case(12)
      CPC = 0.25
    case(13)
      CPC = 0.14
    case(14)
      CPC = 0.33
    case(15)
      CPC = 0.29
    case(16)
      CPC = 0.13
    case(17)
      CPC = 0.21
    case(18)
      CPC = 0.34
    case(19)
      CPC = 0.05
    case(20)
      CPC = 0.16
    case(21)
      CPC = 0.11
    case(22)
      CPC = 0.06
    case(23)
      CPC = 1.0
    case(24)
      CPC = 0.04
    case(25)
      CPC= 0.1
    case(26)
      CPC= 0.1
    case(27)
      CPC = 0.02
    case(28)
      CPC = 0.1
    case(29)
      CPC = 0.1
    case(30)
      CPC = 0.5
    case(31)
      CPC= 0.20
    case(32)
      CPC = 0.24
    case default
      CPC= 0.1
  end select
  CPC = min(CPC/projection_factor, 1.0)
  NVISMVGForestFrac = CPC
END FUNCTION NVISMVGForestFrac 

END MODULE BIOME_MODULE
!----------------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------------
MODULE ArrayOps
! Functions needed for extracting model variables from cable output
use TypeDef
use UTILS
use DateFunctions
implicit none
CONTAINS
!###############################################################################
function FindTimeIndex (ObsDate,ObsTime,StartTimeSecs,OutputDt,ModelDMY,ModelHM,leapflag,strname)
! Find time index for model output corresponding to date ObsDate and time ObsTime
! Calculate number of days since start of run using date functions
use TypeDef    
use UTILS
use DateFunctions
implicit none
integer :: FindTimeIndex 
type (dmydate), intent(in) :: ObsDate 
type (hmtime), intent(in) :: ObsTime
type (dmydate), intent(in) :: ModelDMY(:)
type (hmtime), intent(in) :: ModelHM(:)
integer, intent(in) :: OutputDt,StartTimeSecs
logical, intent(in) :: leapflag
character(20) :: strname
integer :: j,NumberOfDays,NumberOfSeconds
  if (leapflag) then
    NumberOfDays = DayDifference(ObsDate,ModelDMY(1))  ! only works with leap years
    NumberOfSeconds = NumberOfDays*60*60*24 + ObsTime%Hour*60*60 + ObsTime%Minute*60  ! number of second since start for obs
    FindTimeIndex = (NumberOfSeconds - StartTimeSecs)/ OutputDt + 1
    !write(6,*) 'ObsDate,ObsTime,ModelDMY(1),ModelHM(1),FindTimeIndex,ModelDMY(FindTimeIndex),ModelHM(FindTimeIndex)='
    !write(6,*) ObsDate,ObsTime,ModelDMY(1),ModelHM(1),FindTimeIndex,ModelDMY(FindTimeIndex),ModelHM(FindTimeIndex)
    !read(5,*)
  else
    do j = 1,size(ModelDMY)
      if ((ModelDMY(j) .eq. ObsDate) .and. (ModelHM(j) .eq. ObsTime)) then   ! needed if no leap years
        FindTimeIndex = j
        exit
      endif
    enddo
  endif
  if ((ModelDMY(FindTimeIndex)%Day .ne. ObsDate%Day) .and. (ModelDMY(FindTimeIndex)%Month .ne. ObsDate%Month)  &  
           .and. (ModelDMY(FindTimeIndex)%Year .ne. ObsDate%Year) .and. (ModelHM(FindTimeIndex)%Hour .ne. ObsTime%Hour)  &
           .and. (ModelHM(FindTimeIndex)%Minute .ne. ObsTime%Minute)) then
    write(6,*) '***********************************************************************'
    write(6,*) 'Error finding time for observable from model file in routine FindTimeIndex'
    write(6,*) 'Model run starts ',ModelDMY(1),ModelHM(1)
    write(6,*) 'Observation name is ',strname
    write(6,*) 'Daily observation is ',ObsDate,' ',ObsTime
    write(6,*) 'Days since start is =',FindTimeIndex
    write(6,*) 'which gives time of ',ModelDMY(FindTimeIndex),' ',ModelHM(FindTimeIndex)
    write(6,*) 'Program stopped'
    write(6,*) '***********************************************************************'
    STOP
  endif
end function FindTimeIndex

!###############################################################################
function TimeAve (oldvar,ndim,d1,d2,d3,d4)
implicit none
real,allocatable :: TimeAve(:,:,:,:)
real,intent(in) :: oldvar(:,:,:,:)
integer,intent(in) :: ndim,d1,d2,d3,d4
integer i1,i2,i3
  allocate(TimeAve(d1,d2,d3,1))
  do i3 = 1,d3
    do i2 = 1,d2
      do i1 = 1,d1
        TimeAve(i1,i2,i3,1) = sum(oldvar(i1,i2,i3,:))/d4
      enddo
    enddo
  enddo
end function TimeAve

!###############################################################################

function PatchAve(oldvar,patchfrac,d1,patchfill,patchmissing,patch_dim)
! Average of patches, using the patchfrac as a weight for each patch
implicit none
real,allocatable :: PatchAve(:,:,:,:)
real,intent(in) :: oldvar(:,:,:,:)   ! (nsave,npave,1,1)
real,intent(in) :: patchfrac(:,:)    ! (d1,patch_dim)
integer,intent(in) :: d1,patch_dim   ! d1 is number of points in spatial dimension
real,intent(in) :: patchmissing,patchfill
real :: temp
integer :: is,ip
allocate(PatchAve(d1,1,1,1))
do is = 1,d1  ! loop over gridcells
  temp = 0.0
  do ip = 1,patch_dim
    if ((patchfrac(is,ip) .gt. 1.0e-4) .and. (patchfrac(is,ip) .ne. patchfill) .and. (patchfrac(is,ip) .ne. patchmissing))  &
                         temp = temp + patchfrac(is,ip) * oldvar(is,ip,1,1)
  enddo
  PatchAve(is,1,1,1) = temp
enddo
end function PatchAve

!###############################################################################
function SoilInterp (twodepths,depth,layerabove,layerbelow)
implicit none
real :: SoilInterp  
real,intent(in) :: twodepths(2)
real,intent(in) :: depth,layerabove,layerbelow
SoilInterp = twodepths(1) + (twodepths(2)-twodepths(1))/(layerbelow-layerabove)*(depth-layerabove)
end function SoilInterp

END MODULE ArrayOps

!###############################################################################
!---------------------------------------------------------
! MAIN PROGRAM
!---------------------------------------------------------
program ExtractObservables
use netcdf
use TypeDef
use UTILS
use DateFunctions
use ArrayOps
use BIOME_MODULE
implicit none
integer, dimension(nf90_max_var_dims) :: dimIDs
integer :: i,j,i1,i2,i3,time_dim,land_dim,x_dim,y_dim,patch_dim,soil_dim,casa_dim,pop_dim       ! dimensions of model arrays
integer :: FILE_ID,VARID,dID,STATUS,FILE_ID_CASA,FILE_ID_POP,OutputDt
integer :: countobs,nobs,IOstatus,nobs_included,nCellsInCatch,ncells,ncatch,firstidx,varcount
integer :: siteidx,yy,mm,dd,hh,ff,layer,lpos,ppos,dpos,spos,bpos,npos,ipatch,biomesav,nvissav,mplant,mlitter,msoil,casa_time
integer :: titlelength,onl,noop,ios,NDims,NAtts,num1,num2,num3
integer :: patchidx,npave,timeidx,ntave,nlint,nsave,idx,is,ip,ig
integer,allocatable :: time_days(:),time_seconds(:),sitearr(:),biome(:),nvis(:)
real,allocatable :: observable(:)      ! observable calculated from model output
real :: patchmissing,patchfill,smdep,layerabove,layerbelow,ForestFrac
real :: soilc0_frac,dep1,dep2,upper,lower,varsum,varmean,obssum,obsmean,prevlat,prevlon,mulby
real :: mdm  ! model-data mismatch
real,allocatable :: time(:),lat(:),lon(:),var1(:),var2(:,:),var3(:,:,:),var3b(:,:,:),var3c(:,:,:),var3d(:,:,:),var4(:,:,:,:),longitude(:,:),latitude(:,:),patchfrac(:,:) ! model arrays
real,allocatable :: zse(:,:,:),OOP(:),casa_lat(:),casa_lon(:)
real,allocatable :: obs(:),weight(:),workingvar(:,:,:,:),temp(:,:,:,:),smweight(:),pf(:,:)
character(200) FILE_NAME,FILE_NAME_CASA,FILE_NAME_POP
character(40) :: timeunits
character(10) :: calendar ! 'noleap' for no leap years, 'standard' for leap years
character(20) :: strname,strinfo
character(20),allocatable :: obsname(:),obsinfo(:),grpname(:),OOPnames(:)
character(3),allocatable :: obs_proc(:)   ! obs_proc can be 'abs','ano','cdf' for absolute, anomaly, cdf matching
character(40) :: output_averaging
character(6) :: catchname,prevcatch,prevsitename,str
character(6),allocatable :: sitename(:)
character(4) :: siteyy,yystr              ! characters read from obsname
character(4) :: strdeps, prevstrdeps     ! for OzNet sm
character(5) :: sitenum,PatchReweight='nvis'
character(3) :: prevobstype,CableCasaFile
character(3),allocatable :: obstype(:)
character(2) :: sitedd,sitemm,mmstr,ddstr            ! characters read from obsname
character    :: siteDMYR,sitepatch                 ! character read from obsname or obsinfo
type(dmydate) :: StartDate, EndDate, ModelStartDate  ! start, end dates of obs; model start date
type(dmydate) :: ObsDate
type(dmydate),allocatable :: ModelDMY(:)  ! day, month, year in model output
type(hmtime) :: ObsTime
type(hmtime),allocatable :: ModelHM(:)  ! hours and seconds in model output
logical :: found, catchfileexists, obsopparamfileexists, leapflag  ! leapflag = .false. for no leap years
logical :: CASAFileOpen,POPFileOpen,lastcurrent
logical :: verbose = .true. !.false.  !.true.

  ! Read in the user-supplied parameters from a namelist file
  namelist /ExtractObservables_Namelist/ FILE_NAME, FILE_NAME_CASA, FILE_NAME_POP, PatchReweight

  write(6,*) 'Read namelist file ExtractObservables.nml'
  open (979, file='ExtractObservables.nml')
  read (979, nml=ExtractObservables_Namelist)
  close (979)

  ! Open netcdf file to read variables - name now in nml file
!  FILE_NAME = "/flush1/tru034/CABLE/mdf/Ozmdf/plume_out_cable_2000_2016.nc"
  !FILE_NAME = "plume_out_cable.nc"
 ! FILE_NAME = "/flush1/tru034/CABLE/mdf_test/bios_out_cable_2000_2016.nc"
 ! FILE_NAME = "bios_out_cable_2000_2016.nc"
 
  write(6,*) 'Read model output file ',TRIM(FILE_NAME)

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

  STATUS = NF90_INQ_DIMID( FILE_ID, 'patch', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=patch_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'soil', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=soil_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  if (verbose) write(6,*) 'time_dim, land_dim, patch_dim, soil_dim  = ',time_dim, land_dim, patch_dim, soil_dim

  ! Read grid latitude and longitude, to determine model resolution and therefore
  ! whether it is a global run using NCEP met that has not leap years
  ! There may be a more efficient way to set leapflag
  STATUS = NF90_INQ_DIMID( FILE_ID, 'x', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=x_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  STATUS = NF90_INQ_DIMID( FILE_ID, 'y', dID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_INQUIRE_DIMENSION( FILE_ID, dID, LEN=y_dim )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

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
  !  write(6,*) 'Model resolution is different for latitude and longitude',modelres,(longitude(2,1)-longitude(1,1))
  !  write(6,*) 'Check calculation of gridcell location for obs (assumes same)'
  !  write(6,*) 'Program stopped'
  !  STOP
  !endif
  !if (verbose) write(6,*) 'Model resolution is ',modelres
  !modelres = 0.05
  !modelres = 0.5

  ! Determine whether model output is daily or monthly
  STATUS = NF90_INQUIRE_ATTRIBUTE ( FILE_ID, nf90_global, 'Output_averaging')
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)  
  STATUS = NF90_GET_ATT( FILE_ID, nf90_global, 'Output_averaging', output_averaging )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  if (verbose) write(6,*) 'Output averaging (from model output ) is ',output_averaging

  ! Read soil layer thicknesses, calculate depths, give information for soil moisture obs
  allocate(zse(land_dim,patch_dim,soil_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'zse', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, zse )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)

  ! Check whether leap years are used
!  STATUS = NF90_INQ_VARID( FILE_ID, 'time', VARID )
!  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
!  STATUS = NF90_GET_ATT(FILE_ID, VARID,'calendar',calendar)
!  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
calendar = 'standard'
write(6,*) 'Calendar overwritten to "standard"'
  if (calendar .eq. 'noleap') then
    leapflag = .false.
    if (verbose) write(6,*) 'Leap years are not used' 
  else
    leapflag = .true.
    if (verbose) write(6,*) 'Leap years are used' 
  endif

  ! Read time, convert units, calculate time range
  allocate(time(time_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'time', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, time )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_ATT(FILE_ID, VARID,'units',timeunits)
  IF (STATUS /= NF90_noerr) CALL HANDLE_ERR(STATUS)
  if (verbose) write(6,*) 'Time in CABLE is in ',trim(timeunits)
  OutputDt = time(2)-time(1)  ! Model output time interval in seconds
  write(6,*) 'Model output interval in secs = ',OutputDt
  if (trim(output_averaging) .eq. 'all timesteps recorded') then
    if (OutputDt .eq. 60*30) output_averaging = 'HalfHourly'
    if (OutputDt .eq. 60*60) output_averaging = 'Hourly'
  endif
  yystr = timeunits(15:18)
  read(yystr,'(i4)') ModelStartDate%Year
  ModelStartDate%Month = 1
  ModelStartDate%Day = 1
  if (verbose) write(6,*) 'Model start date =',ModelStartDate
  write(6,*) 'Model output resolution is ',output_averaging
  write(6,*) OutputDt
  ! time is in seconds since start date, convert to days since start date (time_days)
  ! Also calculate day, month, year of each time to check against obs (ModelDMY)
  allocate(time_days(time_dim))    ! days since start date
  allocate(time_seconds(time_dim)) ! seconds since start of THAT day
  allocate(ModelDMY(time_dim))
  allocate(ModelHM(time_dim))
  do i = 1, time_dim
    time_days(i) = floor(time(i)/(3600.0*24.0))
    time_seconds(i) = time(i)-time_days(i)*3600*24
    if (LeapFlag) then
      ModelDMY(i) = AddDay(ModelStartDate,time_days(i))
    else 
      ModelDMY(i) = AddDayNoLeap(ModelStartDate,time_days(i))
    endif
    ModelHM(i)%Hour = time_seconds(i)/3600
    ModelHM(i)%Minute = (time_seconds(i)-ModelHM(i)%Hour*3600)/60
    !write(6,*)' i, time_days(i),time_seconds(i),ModelDMY(i),ModelHM(i)='
    !write(6,*) i, time_days(i),time_seconds(i),ModelDMY(i),ModelHM(i)
    !read(5,*)
  enddo
  if (LeapFlag) then
    StartDate = AddDay(ModelStartDate,time_days(1))
    EndDate = AddDay(ModelStartDate,time_days(time_dim))
  else
    StartDate = AddDayNoLeap(ModelStartDate,time_days(1))
    EndDate = AddDayNoLeap(ModelStartDate,time_days(time_dim))
  endif
  if (verbose) write(6,201) StartDate%Year,StartDate%Month,StartDate%Day,EndDate%Year,EndDate%Month,EndDate%Day
201 format('Model output is available between ',i4,'-',i2,'-',i2,' and ',i4,'-',i2,'-',i2)
  if ((output_averaging .eq. 'Monthly') .and. (EndDate%Month .ne. 12) .and. (EndDate%Day .ne. 16)) then
    write(6,*) 'Calculated EndDate not mid-December, check whether have correct leap year flag for driving met'
    write(6,*) 'Program stopped'
    STOP
  endif

 ! Read lat and lon
  allocate(lat(land_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'local_lat', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, lat )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  if (verbose) write(6,*) 'latitude=',lat

  allocate(lon(land_dim))
  STATUS = NF90_INQ_VARID( FILE_ID, 'local_lon', VARID )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  STATUS = NF90_GET_VAR( FILE_ID, VARID, lon )
  IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
  if (verbose) write(6,*) 'longitude=',lon

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
  if (verbose) write(6,*) 'Patch missing and fill values are ',patchmissing,' ',patchfill
  ! Set patchfrac to zero for missing and fill values - now done in functon PatchAve
  !where ((patchfrac .eq. patchfill) .or. (patchfrac .eq. patchmissing) .or. (patchfrac .lt. 1.0e-4)) 
  !  patchfrac = 0.0
  !end where

  ! Open file that specifies which gridcells represent catchments, if it exists
  inquire(file='Catchment_gridcells.txt', exist=catchfileexists)
  if (catchfileexists) then
    write(6,*) 'Read catchment file Catchment_gridcells.txt'
    open(unit=35,file='Catchment_gridcells.txt') 
    read(35,*) ncatch 
  endif

  ! Read parameters required for observation operators (i.e. parameters outside CABLE
  !  that are required to calculate the model equivalents of the observations)
  inquire(file='ObsOperatorParams.txt',exist=obsopparamfileexists)
  if (obsopparamfileexists) then
    write(6,*) 'Read file with observation operators ObsOperatorParams.txt'
    open(unit=34,file='ObsOperatorParams.txt')
    read(34,*)                 ! comment line
    read(34,*) noop            ! number of observation operator parameters  
    allocate(OOP(noop))        ! values of observation operator parameters
    allocate(OOPnames(noop))   ! names of observation operator parameters
    do i = 1,noop
      read(34,*,IOSTAT=IOstatus) OOP(i),OOPnames(i)
      if (IOstatus .ne. 0) then
        write(6,*) '*********************************************************************************'
        write(6,*) ' ERROR reading observation operator parameters from file ObsOperatorParams.txt'
        write(6,*) ' noop = ',noop
        write(6,*) 'OOP = ',OOP
        write(6,*) 'OPPnames = ',OOPnames
        write(6,*) 'Program stopped'
        write(6,*) '*********************************************************************************'
        STOP
      endif
    enddo
    close(34)
  endif

  ! Read model variables corresponding to observations
  ! Assumes observations are sorted by variable type, otherwise would be inefficient
  ! Put model variable into array var3 (if it is 3-dim) or var4 (4-dim, including depth)
  ! or var1 (1-dim POP field)
  ! This is often just one model variable, but can be a combination of model variables 
  ! (e.g. Rec is a combination of autotrophic and heterotrophic resp, and SC0 is a 
  !  combination of csoil1, csoil2 and csoil3 with parameter soilc0_frac)
  write(6,*) 'Read ObsSpecs.txt'
  open(unit=25,file='ObsSpecs.txt',status='old')
  read(25,*) nobs
  allocate(obsname(nobs))
  allocate(obsinfo(nobs))
  allocate(grpname(nobs))
  allocate(obs(nobs))
  allocate(weight(nobs))
  allocate(obstype(nobs))
  allocate(sitename(nobs))
  allocate(obs_proc(nobs))
  allocate(observable(nobs))
  allocate(var2(land_dim,patch_dim))
  allocate(var3(land_dim,patch_dim,time_dim))
  allocate(var3b(land_dim,patch_dim,time_dim))
  allocate(var4(land_dim,patch_dim,soil_dim,time_dim))
  allocate(smweight(soil_dim))
  prevstrdeps = 'xxxx'
  prevsitename = 'xxx' 
  mdm = 0.0  ! model-data mismatch
  do i = 1,nobs  ! read all obs
    read(25,*) obsname(i),obs(i),weight(i),grpname(i),obsinfo(i)
   ! if (verbose) write(6,*) obsname(i),obs(i),weight(i),grpname(i),obsinfo(i)
  enddo
  if (verbose) write(6,*) nobs,' observations read'
  countobs = 0
  write(6,*) 'Read file Observables.txt'
  open(unit=26,file='Observables.txt')
  write(26,*) '  output'
  nobs_included = 0
  prevcatch = 'nil'
  prevobstype = 'nil'
  CASAFileOpen = .false.
  POPFileOpen = .false.
  ! Most observables are created with one or sometimes two model variables
  ! First read one model variable into var3 (if 3-dimensions) or var4 (if 4 dims)
  ! Next do what is needed to make this into the observable (either already is, or
  !  needs scaling, or needs combining with another variable)
  do i = 1,nobs
    strname = obsname(i)
    strinfo = obsinfo(i)
    obstype(i) = strname(1:3)
    if ((obstype(i) .eq. 'Phy') .or. (obstype(i) .eq. 'AGD') .or. (obstype(i) .eq. 'ABM') .or. (obstype(i) .eq. 'SC0') .or. (obstype(i) .eq. 'ABM') .or. (obstype(i) .eq. 'SC0') .or. (obstype(i) .eq. 'NPP')) then
      CableCasaFile = strinfo(9:11)
    else 
      CableCasaFile = 'cab'
    endif
    if (verbose) write(6,*) 'Obsname ',strname,', Obsinfo ',strinfo
    if (strname(1:3) .ne. prevobstype) then  ! read new variable
      if (verbose) write(6,*) 'Reading variable for ',obstype(i)
      ! Open CASA file if necessary
      if ((CableCasaFile .eq. 'cas') .and. (.not.(CASAFileOpen))) then ! need to open CASA file
        write(6,*) 'Open CASA file for variables Phy, AGD, ABM, CWD, FLW, NPP and/or SC0'
        write(6,*) 'Opening ',trim(FILE_NAME_CASA)
        STATUS = NF90_OPEN( TRIM(FILE_NAME_CASA), NF90_NOWRITE, FILE_ID_CASA )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_CASA, 'land', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_CASA, dID, LEN=casa_dim )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_CASA, 'mplant', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_CASA, dID, LEN=mplant )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_CASA, 'mlitter', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_CASA, dID, LEN=mlitter )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_CASA, 'msoil', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_CASA, dID, LEN=msoil )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_CASA, 'time', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_CASA, dID, LEN=casa_time )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        if (casa_time .ne. time_dim) then
          write(6,*) '****************************************************'
          write(6,*) 'time_dim = ',time_dim
          write(6,*) 'casa_time = ',casa_time
          write(6,*) 'THEY ARE DIFFERENT!!'
          write(6,*) 'Program stopped'
          write(6,*) '****************************************************'
          STOP
        endif
        allocate(casa_lat(casa_dim))
        STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'latitude', VARID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID_CASA, VARID, casa_lat )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        if (verbose) write(6,*) 'casa latitude=',casa_lat
        allocate(casa_lon(casa_dim))
        STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'longitude', VARID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID_CASA, VARID, casa_lon )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        if (verbose) write(6,*) 'casa longitude=',casa_lon

        write(6,*) 'CASA land, mplant, mlitter, msoil, time =',casa_dim,mplant,mlitter, msoil, casa_time
        if (allocated(var3c)) deallocate(var3c)
        allocate(var3c(casa_dim,mplant,casa_time))
        CASAFileOpen = .true.
        write(6,*) 'CASA file opened successfully'
      endif
      ! Open POP file if necessary
      if (((obstype(i) .eq. 'LBA') .or. (obstype(i) .eq. 'LTD')) .and. (.not.(POPFileOpen))) then ! need to open POP file
        write(6,*) 'Open POP ini file for variables LBA and/or LTD'
      !  FILE_NAME_POP = "pop_bios_ini_1900_1999.nc"  	! comes from nml file now
        write(6,*) 'Opening ',trim(FILE_NAME_POP)
        STATUS = NF90_OPEN( TRIM(FILE_NAME_POP), NF90_NOWRITE, FILE_ID_POP )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQ_DIMID( FILE_ID_POP, 'land', dID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_INQUIRE_DIMENSION( FILE_ID_POP, dID, LEN=pop_dim )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        allocate(var1(pop_dim))
        POPFileOpen = .true.
        write(6,*) 'POP file opened successfully'
      endif
      select case (obstype(i))
        case ('GPP')   ! GPP
          STATUS = NF90_INQ_VARID( FILE_ID, 'GPP', VARID )
        case ('NEP')   ! NEP
          STATUS = NF90_INQ_VARID( FILE_ID, 'NEE', VARID )
        case ('EvT')   ! evapotranspiration
          STATUS = NF90_INQ_VARID( FILE_ID, 'Evap', VARID )
        case ('Rec','Rso')   ! ecosystem respiration or soil respiration
          STATUS = NF90_INQ_VARID( FILE_ID, 'HeteroResp', VARID )
        case ('LAI')   ! leaf area index
          STATUS = NF90_INQ_VARID( FILE_ID, 'LAI', VARID )
        case ('SMO','SMC','SMF')   ! In situ soil moisture
          STATUS = NF90_INQ_VARID( FILE_ID, 'SoilMoist', VARID )
        case ('SC0')   ! soil carbon density in top 15cm
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'csoil', VARID )  ! csoil from CASA file
          else
            STATUS = NF90_INQ_VARID( FILE_ID, 'TotSoilCarb', VARID )
          endif
    !    case ('Phy')   ! above-ground phytomass
    !      STATUS = NF90_INQ_VARID( FILE_ID, 'PlantCarbLeaf', VARID )
        case ('Lit','FLW')   ! above ground litter; fine litter 
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'clitter', VARID )  ! clitter from CASA file
          else
            STATUS = NF90_INQ_VARID( FILE_ID, 'LittCarbStructural', VARID ) 
          endif
        case ('CWD')   ! coarse woody debris
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'clitter', VARID )  ! clitter from CASA file
          else
            STATUS = NF90_INQ_VARID( FILE_ID, 'LittCarbCWD', VARID )
          endif
        case ('NPP')   ! leaf NPP
          STATUS = NF90_INQ_VARID( FILE_ID, 'NPP', VARID )
        case ('STR')   ! streamflow
          STATUS = NF90_INQ_VARID( FILE_ID, 'Qs', VARID )
        case ('LBA')   ! live basal area
          STATUS = NF90_INQ_VARID( FILE_ID_POP, 'basal_area', VARID )
        case ('LTD')   ! live tree density
          STATUS = NF90_INQ_VARID( FILE_ID_POP, 'densindiv', VARID )
        case ('AGD','Phy','ABM')   ! above-ground dry biomass
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'cplant', VARID )  ! cplant from CASA file
          else
            STATUS = NF90_INQ_VARID( FILE_ID, 'TotLivBiomass', VARID )  ! TotLivBiomass from CABLE file
          endif
      end select
      IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
      ! Read required variable (3-d, except for soil moisture variable that has extra dimension for depth; basal area and tree density are 2-d)
      select case (obstype(i))
        case ('LBA','LTD')   !  basal area and tree density are 1-d from POP ini file
          STATUS = NF90_GET_VAR( FILE_ID_POP, VARID, var1 )
        case ('SMO','SMC','SMF')   ! soil moisture has depth dimension
          STATUS = NF90_GET_VAR( FILE_ID, VARID, var4 )
        case ('SC0') ! soil carbon density in top 15cm
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_GET_VAR( FILE_ID_CASA, VARID, var3c ) 
          else
            STATUS = NF90_GET_VAR( FILE_ID, VARID, var3 )
          endif
        case ('AGD','Phy','ABM','CWD','FLW')   ! 3-d variables that could come from casa or cable
          if (CableCasaFile .eq. 'cas') then
            STATUS = NF90_GET_VAR( FILE_ID_CASA, VARID, var3c )
          else 
            STATUS = NF90_GET_VAR( FILE_ID, VARID, var3 )
          endif
        case default         ! all others are 3-d
          STATUS = NF90_GET_VAR( FILE_ID, VARID, var3 )
      end select
      IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
      ! Read any additional variables required
      ! Soil C0 from casa output file is sum of csoil and clitter
      if ((obstype(i) .eq. 'SC0') .and. (CableCasaFile .eq. 'cas')) then
        write(6,*) 'Read additional variable clitter from casa file for SC0 observation'
        allocate(var3d(casa_dim,mplant,casa_time))
        STATUS = NF90_INQ_VARID( FILE_ID_CASA, 'clitter', VARID )  ! clitter from CASA file
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID_CASA, VARID, var3d )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        var3c = var3c + var3d 
        deallocate(var3d)
      endif
      ! Ecosystem respiration is sum of autotrophic and heterotrophic resp ---------------------------------------------------------------
      if (obstype(i) .eq. 'Rec') then  
        STATUS = NF90_INQ_VARID( FILE_ID, 'AutoResp', VARID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID, VARID, var3b )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        var3 = var3 + var3b 
      endif  
      ! Soil respiration is sum of root and heterotrophic resp ---------------------------------------------------------------
      if (obstype(i) .eq. 'Rso') then  
        STATUS = NF90_INQ_VARID( FILE_ID, 'RootResp', VARID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID, VARID, var3b )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        var3 = var3 + var3b 
      endif  
      ! Streamflow is sum of Qs and Qsb ---------------------------------------------------------------
      if (obstype(i) .eq. 'STR') then
        STATUS = NF90_INQ_VARID( FILE_ID, 'Qsb', VARID )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID, VARID, var3b )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        var3 = (var3 + var3b)*60.0*60.0*24.0    ! var3 will then be Qs + Qsb - convert mm/s to mm/d
      endif
      ! Calculate soil carbon density in top 15cm using parameter soilc0_frac ------------------------------------------------------------
      ! Observations are in mgC/g = gC/kg
      if (obstype(i) .eq. 'SC0') then  ! calculate soilC from casa output
        if (CableCasaFile .eq. 'cas') then     ! CASA output
          ! convert casa array (var3c=csoil+clitter(casa_dim,mplant,casa_time)) to same as cable array (var3=TotSoilCarb(land_dim,patch_dim,time_dim))
          var3(:,:,:) = 0.0
          i1 = 0
          prevlat = 0.0
          prevlon = 0.0
          ig = 0
          ip = 0
          do while (i1 .lt. casa_dim)
            i1 = i1 + 1
            ip = ip + 1
            if ((casa_lat(i1) .ne. prevlat) .or. (casa_lon(i1) .ne. prevlon)) then
              ig = ig + 1
              ip = 1
              prevlat = casa_lat(i1)
              prevlon = casa_lon(i1)
            endif
            do i3 = 1,casa_time
                var3(ig,ip,i3) = var3c(i1,1,i3) + var3c(i1,2,i3) + var3c(i1,3,i3)   ! add over soil pools
            enddo
          enddo
        endif
        STATUS = NF90_INQ_VARID( FILE_ID, 'rhosoil', VARID )  ! soil density (kg/m3)
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        STATUS = NF90_GET_VAR( FILE_ID, VARID, var2 )
        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
        found = .false.
        do j = 1,noop
          if (OOPnames(j) .eq. 'soilc0_frac') then   ! find 
            found = .true.
            soilc0_frac = OOP(j)
          endif
        enddo
        if (.not.(found)) then
          write(6,*) '*********************************************************************************'
          write(6,*) ' ERROR finding observation operator parameter soilc0_frac'
          if (obsopparamfileexists) then
            write(6,*) ' noop = ',noop
            write(6,*) 'OOP = ',OOP
            write(6,*) 'OPPnames = ',OOPnames
          else
            write(6,*) 'No ObsOperatorParams.txt file was found in directory'
          endif
          write(6,*) 'Program stopped'
          write(6,*) '*********************************************************************************'
          STOP
        endif
        if (verbose) write(6,*) 'Parameter soilc0_frac = ',soilc0_frac
        if (CableCasaFile .eq. 'cas') then
          mulby = 1.0
        else
          mulby = 1000.0
        endif
        do i3 = 1,time_dim
          do i2 = 1,patch_dim
            do i1 = 1,land_dim
              var3(i1,i2,i3) = mulby * soilc0_frac * var3(i1,i2,i3) / 0.15 / var2(i1,i2)  ! 0.001 * fraction * kg C m-2 /m /kg m-3 = gC /kg 
            enddo
          enddo
        enddo
      endif
      ! Calculate above-ground phytomass -------------------------------------------------------------------------------------------------------
      ! Observations are in tC/ha = 0.1 kg/m2 
   !   if (obstype(i) .eq. 'Phy') then 
   !     STATUS = NF90_INQ_VARID( FILE_ID, 'PlantCarbWood', VARID )
  !      IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
 !       STATUS = NF90_GET_VAR( FILE_ID, VARID, var3b )
!        IF (STATUS /= NF90_noerr) CALL handle_err(STATUS)
!        ! Observation model for above-ground phytomass
!        var3 = (var3 + 0.7*var3b) * 10.0    ! 10.0 for units (kgC/m2 to tC/ha)
!      endif
      ! Calculate above-ground litter/fine litter ----------------------------------------------------------------------------------------------------------
      ! VAST-Raison observations are in tC/ha = 0.1 kg/m2; VW_fine_litter were in tDM/ha but were converted in CompileObs to tC/ha = 0.1 kg/m2/y
      if ((obstype(i) .eq. 'Lit') .or. (obstype(i) .eq. 'FLW')) then  ! calculate above-ground litter
      ! Observation model for above-ground litter
        if (CableCasaFile .eq. 'cas') then     ! CASA output
          ! convert casa array (var3c=clitter(casa_dim,mlitter,casa_time)) to same as cable array (var3=LittCarbStructural(land_dim,patch_dim,time_dim))
          var3(:,:,:) = 0.0
          i1 = 0
          prevlat = 0.0
          prevlon = 0.0
          ig = 0
          ip = 0
          do while (i1 .lt. casa_dim)
            i1 = i1 + 1
            ip = ip + 1
            if ((casa_lat(i1) .ne. prevlat) .or. (casa_lon(i1) .ne. prevlon)) then
              ig = ig + 1
              ip = 1
              prevlat = casa_lat(i1)
              prevlon = casa_lon(i1)
            endif
            do i3 = 1,casa_time
                var3(ig,ip,i3) = var3c(i1,2,i3)  ! clitter(casa_dim,2,casa_time)
            enddo
          enddo
          var3 = 10.0*0.4*var3/1000.0
        else   ! using cable output
          var3 = 10.0*0.4*var3     ! 10 for units, 0.4 for above-ground
        endif
      endif
      ! Calculate coarse woody debris  ----------------------------------------------------------------------------------------------------------
      ! Observations were in tDM/ha but were converted in CompileObs to tC/ha = 0.1 kg/m2
      if (obstype(i) .eq. 'CWD') then  ! calculate coarse woody debris
      ! Observation model for coarse woody debris
        if (CableCasaFile .eq. 'cas') then     ! CASA output
          ! convert casa array (var3c=clitter(casa_dim,mlitter,casa_time)) to same as cable array (var3=LittCarbStructural(land_dim,patch_dim,time_dim))
          var3(:,:,:) = 0.0
          i1 = 0
          prevlat = 0.0
          prevlon = 0.0
          ig = 0
          ip = 0
          do while (i1 .lt. casa_dim)
            i1 = i1 + 1
            ip = ip + 1
            if ((casa_lat(i1) .ne. prevlat) .or. (casa_lon(i1) .ne. prevlon)) then
              ig = ig + 1
              ip = 1
              prevlat = casa_lat(i1)
              prevlon = casa_lon(i1)
            endif
            do i3 = 1,casa_time
                var3(ig,ip,i3) = var3c(i1,3,i3)  ! clitter(casa_dim,3,casa_time)
            enddo
          enddo
          var3 = 10.0*0.4*var3/1000.0
        else   ! using cable output
          var3 = 10.0*0.4*var3       ! 10 for units, 0.4 for above-ground 
        endif
      endif
      ! Calculate leafNPP ---------------------------------------------------------------------------------------------------------------------
      ! Observation in tC/ha/yr
      if (obstype(i) .eq. 'NPP') then 
      ! Observation model for leaf NPP    (*(1.0e8/(12.0107*60.0*60.0*24.0*365.25)) for units umolC/m2/s to tC/ha/yr
        var3 = 0.3*var3 /(1.0e8/(12.0107*60.0*60.0*24.0*365.25))     ! THIS NEEDS REVISITING ******************************************  placeholder only
      endif
      ! Calculate above-ground drymass ----------------------------------------------------------------------------------------------------------
      ! Observations are in Mg/ha = 0.1 kg/m2
      if ((obstype(i) .eq. 'AGD') .or. (obstype(i) .eq. 'Phy') .or. (obstype(i) .eq. 'ABM')) then  ! calculate above-ground drymass from casa output
      ! Observation model for above-ground drymass
        if (CableCasaFile .eq. 'cas') then     ! CASA output
          ! convert casa array (var3c=cplant(casa_dim,mplant,casa_time)) to same as cable array (var3=TotLivBiomass(land_dim,patch_dim,time_dim))
          var3(:,:,:) = 0.0
          i1 = 0
          prevlat = 0.0
          prevlon = 0.0
          ig = 0
          ip = 0
          do while (i1 .lt. casa_dim)
            i1 = i1 + 1
            ip = ip + 1
            if ((casa_lat(i1) .ne. prevlat) .or. (casa_lon(i1) .ne. prevlon)) then
              ig = ig + 1
              ip = 1
              prevlat = casa_lat(i1)
              prevlon = casa_lon(i1)
            endif
            do i3 = 1,casa_time
                var3(ig,ip,i3) = var3c(i1,1,i3) + var3c(i1,2,i3) + var3c(i1,3,i3)   ! add over mplant (leaf,wood,froot) 
            enddo
          enddo
          var3 = 0.78*10.0*var3/1000.0 
        else 
          var3 = 0.78*10.0*var3     ! CABLE output; Assumes 78% drymass is aboveground, 10 is for unit conversion from kgC/m2 to Mg/ha
        endif
      endif
      ! Calculate live tree number density ----------------------------------------------------------------------------------------------------------
      ! Observations are in n/ha = 10000 x n/m2
      if (obstype(i) .eq. 'LTD') then  ! calculate live tree density
      ! Observation model for live tree density
        var1 = 10000.0*var1     ! Unit conversion from n/m2 to n/ha
      endif
      ! NEP is negative of observation -------------------------------------------------------------------------------------------------------
      if (obstype(i) .eq. 'NEP') var3 = - var3   
      if (verbose) write(6,*) 'Variable successfully read'
    endif  ! read new variable
    ! Extract processing type
    obs_proc(i) = strinfo(1:3)   ! observation can be absolute, anomaly, cdf matching etc
    ! Extract sitename from obsname - length of sitename varies by observation type
    select case (obstype(i))
      case ('GPP','NEP','Rec','Rso','EvT','LAI','SMC','SMO','SMF')
        onl = 3
      case ('SC0','Lit','Phy','NPP','CWD','FLW')
        onl = 4
      case ('LBA','LTD','AGD','ABM')
        onl = 5
      case ('STR')
        onl = 6
    end select 
    str = strname(4:4+onl-1)
    !if ((obstype(i) .eq. 'STR') .and. (str(1:2) .eq. '00')) str = str(3:6)  ! 00 used to pad 4-char catchment names in obsname
    sitename(i) = str   ! character string for site
    ! Extract model gridcell for observation (or for STR, number of gridcells in catchment) from obsinfo
    select case (obstype(i))
      case ('STR') 
        sitenum = strinfo(4:6)    ! character string of number of gridcells representing catchment
        read(sitenum,*) nCellsInCatch   ! number of gridcells representing catchment
      case default
        sitenum = strinfo(4:8)    ! character string of index of observation location in spatial array
        read(sitenum,*) siteidx   ! index of observation location in spatial array
    end select
    ! Extract time information
    siteDMYR = strname(4+onl:4+onl)   ! 'F','H','D','B','M','Y','R','I' for half-hour, hour, day, bi-monthly, month, annual, run ave and ini observation
    if ((siteDMYR .eq. 'F').or.(siteDMYR .eq. 'H').or.(siteDMYR .eq. 'Y').or.(siteDMYR .eq. 'M').or.(siteDMYR .eq. 'D').or.(siteDMYR .eq. 'B')) then
      siteyy = strname(onl+5:onl+8)
      read(siteyy,*) yy  ! year of observation
    endif
    if ((siteDMYR .eq. 'F').or.(siteDMYR .eq. 'H').or.(siteDMYR .eq. 'M') .or. (siteDMYR .eq. 'D') .or. (siteDMYR .eq. 'B')) then 
      sitemm = strname(onl+9:onl+10)
      read(sitemm,*) mm  ! month of observation
    endif
    if ((siteDMYR .eq. 'F').or.(siteDMYR .eq. 'H').or.(siteDMYR .eq. 'D') .or. (siteDMYR .eq. 'B')) then
      sitedd = strname(onl+11:onl+12)
      read(sitedd,*) dd  ! day of observation
    endif
    if ((siteDMYR .eq. 'F').or.(siteDMYR .eq. 'H')) then
      sitedd = strname(onl+13:onl+14)
      read(sitedd,*) hh  ! hour of observation
    endif
    if (siteDMYR .eq. 'F') then
      sitedd = strname(onl+15:onl+16)
      read(sitedd,*) ff  ! half-hour (minutes) of observation
    endif
    ppos = scan(strinfo,'P',.true.)
    if (ppos .ne. 0) read(strinfo(ppos+1:ppos+1),*) sitepatch
    npos = scan(strinfo,'N',.true.)  ! NVIS MVG
    if (npos .ne. 0) read(strinfo(npos+1:npos+2),*) nvissav  ! save to put into nvis(:) later once allocated
    bpos = scan(strinfo,'B',.true.)  ! biome
    if (bpos .ne. 0) read(strinfo(bpos+1:bpos+2),*) biomesav  ! save to put into biome(:) later once allocated
  !  spos = scan(strinfo,'S',.true.)  ! LBA and LTD don't have patch info, instead scale factor for forest patch to gridcell
  !  if (spos .ne. 0) read(strinfo(spos+1:len_trim(strinfo)),*) popscale
    if (obstype(i) .eq. 'SMC') then  ! CosmOz soil moisture has layer associated
      if (verbose) write(6,*) strinfo
      lpos = scan(strname, 'L', .true.)  ! find position of 'L' near end of obsname
      read(strname(lpos+1:lpos+1),*) layer ! next character gives layer corresponding to obs
      dpos = scan(strinfo, 'D', .true.) ! find position of 'D' near end of obsinfo
      read(strinfo(dpos+1:dpos+5),*) smdep
      smdep = smdep/100.0  ! change cm to metres
    endif
    if ((obstype(i) .eq. 'SMO') .or. (obstype(i) .eq. 'SMF')) then    ! OzNet and OzFlux soil moisture has depth range
      lpos = scan(strname, 'L', .true.)    ! find position of 'L' near end of obsname
      read(strname(lpos+1:lpos+4),*) strdeps ! next 4 characters give depth range 
      if ((strdeps .ne. prevstrdeps) .or. (sitename(i) .ne. prevsitename)) then  
        read(strdeps(1:2),*) dep1
        read(strdeps(3:4),*) dep2
        dep1 = dep1/100.0  ! change cm to metres
        dep2 = dep2/100.0
        if (verbose) write(6,*) 'OzNet or OzFlux sm observation depth range (m) ',dep1,'-',dep2
        smweight(:) = 0.0
        do j = 1,soil_dim
          upper = 0.0 + sum(zse(siteidx,1,1:j-1))
          lower = sum(zse(siteidx,1,1:j))
          if ((lower .ge. dep1) .and. (upper .le. dep1) .and. (lower .lt. dep2)) smweight(j) = (lower-dep1) /(dep2-dep1) ! shallow part of obs layer sits within model layer
          if ((upper .ge. dep1) .and. (lower .le. dep2)) smweight(j) = (lower - upper)  /(dep2-dep1)                     ! obs range encompases entire model layer
          if ((upper .le. dep1) .and. (lower .ge. dep2)) smweight(j) = 1.0                                               ! obs range entirely within model layer
          if ((upper .le. dep2) .and. (lower .ge. dep2) .and. (upper .gt. dep1)) smweight(j) = (dep2-upper) /(dep2-dep1) ! deep part of obs layer sits within model layer
        enddo
        write(6,*) 'Weights for model layers in sm observable = ',smweight
        prevstrdeps = strdeps
        prevsitename = sitename(i)
      endif
    endif

    !if (verbose) then
    !  write(6,*) 'Daily/monthly/yearly/run = ',siteDMYR
    !  write(6,*) 'Observation processing = ',obs_proc(i)
    !  if ((obstype(i) .eq. 'SMO') .or. (obstype(i) .eq. 'SMC')) then 
    !    write(6,*) 'Depth for sm =',smdep
    !    write(6,*) 'Interpolate between layers ',layer,' and ',layer+1
    !  endif
    !  write(6,*) 'Patch is ',sitepatch
    !endif
    ! Allow for daily, monthly, annual or run-averaged observations, and monthly or daily model output
    ! 1. Half-hourly observations and half-hourly model output - no averaging required
    timeidx = -1
    ntave = -1
    if ((siteDMYR .eq. 'F') .and. (output_averaging .ne. 'HalfHourly')) then
      write(6,*) '*****************************************************************************'
      write(6,*) 'ERROR: Observation is half-hourly but model output is ',output_averaging
      write(6,*) 'Program stopped'
      write(6,*) '*****************************************************************************'
      STOP
    endif
    if ((siteDMYR .eq. 'F') .and. (output_averaging .eq. 'HalfHourly')) then
       ObsDate%Day = dd
       ObsDate%Month = mm
       ObsDate%Year = yy
       ObsTime%Hour = hh
       ObsTime%Minute = ff
       timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
       ntave = 1
     !  if (leapflag) then 
     !    timeidx = DayDifference(ObsDate,ModelStartDate) + 1  ! only works with leap years
     !  else
     !    do j = 1,time_dim            
     !      if (ModelDMY(j) .eq. ObsDate) then   ! needed if no leap years
     !        timeidx = j
     !        exit 
     !      endif
     !    enddo
     !  endif
     !  if ((ModelDMY(timeidx)%Day .ne. dd) .and. (ModelDMY(timeidx)%Month .ne. mm) .and. (ModelDMY(timeidx)%Year .ne. yy)) then
     !    write(6,*) 'Error finding time for observable from model file'
     !    write(6,*) 'Daily observation is ',dd,mm,yy
     !    write(6,*) 'Days since start is =',timeidx
     !    write(6,*) 'which gives time of ',ModelDMY(timeidx)
     !    write(6,*) 'Program stopped'
     !    STOP
     !  endif
    endif
    if ((siteDMYR .eq. 'H') .and. (output_averaging .eq. 'HalfHourly')) then
       ObsDate%Day = dd
       ObsDate%Month = mm
       ObsDate%Year = yy
       ObsTime%Hour = hh
       timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)
       ntave = 2
    endif
    if ((siteDMYR .eq. 'H') .and. (output_averaging .eq. 'Hourly')) then
       ObsDate%Day = dd
       ObsDate%Month = mm
       ObsDate%Year = yy
       ObsTime%Hour = hh
       timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
       ntave = 1
     !  if (leapflag) then 
     !    timeidx = DayDifference(ObsDate,ModelStartDate) + 1  ! only works with leap years
     !  else
     !    do j = 1,time_dim            
     !      if (ModelDMY(j) .eq. ObsDate) then   ! needed if no leap years
     !        timeidx = j
     !        exit 
     !      endif
     !    enddo
     !  endif
     !  if ((ModelDMY(timeidx)%Day .ne. dd) .and. (ModelDMY(timeidx)%Month .ne. mm) .and. (ModelDMY(timeidx)%Year .ne. yy)) then
     !    write(6,*) 'Error finding time for observable from model file'
     !    write(6,*) 'Daily observation is ',dd,mm,yy
     !    write(6,*) 'Days since start is =',timeidx
     !    write(6,*) 'which gives time of ',ModelDMY(timeidx)
     !    write(6,*) 'Program stopped'
     !    STOP
     !  endif
    endif
    if ((siteDMYR .eq. 'D') .and. ((output_averaging .eq. 'Monthly') .or. (output_averaging .eq. 'Annual'))) then
      write(6,*) '*****************************************************************************'
      write(6,*) 'ERROR: Observation is daily but model output is ',output_averaging
      write(6,*) 'Program stopped'
      write(6,*) '*****************************************************************************'
      STOP
    endif
    ! 1. Daily observations and daily, hourly or halfhourly model output - no averaging required
    if ((siteDMYR .eq. 'D') .and. ((output_averaging .eq. 'Daily') .or. (output_averaging .eq. 'Hourly') .or. (output_averaging .eq. 'HalfHourly'))) then
     ! if (verbose) then
     !   if (output_averaging .eq. 'Daily') write(6,*) 'Daily observations and daily model output'
     !   if (output_averaging .eq. 'Hourly') write(6,*) 'Daily observations and hourly model output'
     !   if (output_averaging .eq. 'HalfHourly') write(6,*) 'Daily observations and half-hourly model output'
     ! endif
      ObsDate%Day = dd
      ObsDate%Month = mm
      ObsDate%Year = yy
      select case (output_averaging)
        case ('Daily')     
          ObsTime%Hour = 12 
          ObsTime%Minute = 0
          ntave = 1
        case ('Hourly')   
          ObsTime%Hour = 1
          ObsTime%Minute = 0
          ntave = 24
        case ('HalfHourly')   
          ObsTime%Hour = 0
          ObsTime%Minute = 30
          ntave = 48
      end select
      timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
    endif
    ! 2. Monthly observations and daily model output - need to average daily output to monthly
    if ((siteDMYR .eq. 'M') .and. (output_averaging .eq. 'daily')) then
      if (verbose) write(6,*) 'Monthly observations and daily model output'
      ObsDate%Day = 1  ! want index of start of month, to determine range for monthly mean
      ObsDate%Month = mm
      ObsDate%Year = yy
      ObsTime%Hour = 12 
      ObsTime%Minute = 0
      timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
    !  write(6,*) ObsDate,ModelDMY(timeidx)
    !  write(6,*) 'timeidx =',timeidx
      if (leapflag) then
        ntave = DaysInMonth(ObsDate)
      else
        ntave = DaysInMonthNoLeap(ObsDate)
      endif
    !  write(6,*) 'ntave =',ntave
    endif
    ! 3. Monthly observations and monthly model output - no averaging required
    if ((siteDMYR .eq. 'M') .and. (Output_averaging .eq. 'monthly')) then
      timeidx = mm + (yy-ModelStartDate%Year)*12  ! time index in output arrays
      if ((ModelDMY(timeidx)%Month .ne. mm) .and. (ModelDMY(timeidx)%Year .ne. yy)) then
        write(6,*) '***********************************************************************'
        write(6,*) 'Error finding time for observable from model file'
        write(6,*) 'obsname and obsinfo ',obsname(i),' ',obsinfo(i)
        write(6,*) 'Monthly observation is ',mm,yy
        write(6,*) 'mm+yy*12=',timeidx
        write(6,*) 'which gives time of ',ModelDMY(timeidx)
        write(6,*) 'Program stopped'
        write(6,*) '***********************************************************************'
        STOP
      endif
      ntave = 1
    endif
    ! 4. Bi-monthly observations and daily model output - need to average daily output to bi-monthly
    if ((siteDMYR .eq. 'B') .and. ((output_averaging .eq. 'daily') .or. (output_averaging .eq. 'Hourly') &
                  .or. (OutputDt .eq. 60*30))) then
    !  if (verbose) then
    !    if (output_averaging .eq. 'daily') write(6,*) 'Bi-monthly observations and daily model output'
    !    if (output_averaging .eq. 'Hourly') write(6,*) 'Bi-monthly observations and hourly model output'
    !    if (OutputDt .eq. 60*30) write(6,*) 'Bi-monthly observations and half-hourly model output'
    !  endif
      ObsDate%Day = dd 
      ObsDate%Month = mm
      ObsDate%Year = yy
      ObsTime%Hour = 12 
      ObsTime%Minute = 0
      timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
    !  write(6,*) ObsDate,ModelDMY(timeidx)
    !  write(6,*) 'timeidx =',timeidx
      if (ObsDate%Day .eq. 1) then
        ntave = 15
      else
        if (leapflag) then
          ntave = DaysInMonth(ObsDate) - 15
        else
          ntave = DaysInMonthNoLeap(ObsDate) - 15
        endif
      endif
      if (output_averaging .eq. 'Hourly') ntave = ntave * 24
      if (OutputDt .eq. 60*30) ntave = ntave * 48
    !  write(6,*) 'ntave =',ntave
    !  read(5,*)
    endif
    ! 5. Annual observations and daily model output - need to average daily output to annual
    if ((siteDMYR .eq. 'Y') .and. (output_averaging .eq. 'daily')) then 
      ObsDate%Day = 1
      ObsDate%Month = 1
      ObsDate%Year = yy
      ObsTime%Hour =12 
      ObsTime%Minute = 0
      timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
      if (leapflag) then
        ntave = 365 + LeapDay(ObsDate%Year)
      else
        ntave = 365
      endif
    endif
    ! 6. Annual observations and monthly model output
    if ((siteDMYR .eq. 'Y') .and. (output_averaging .eq. 'monthly')) then ! Need to average monthly output to annual
      ObsDate%Day = 1
      ObsDate%Month = 1
      ObsDate%Year = yy
      ObsTime%Hour =12 
      ObsTime%Minute = 0
      timeidx = FindTimeIndex (ObsDate,ObsTime,int(time(1)),OutputDt,ModelDMY,ModelHM,leapflag,strname)  
      ntave = 12
    endif
    ! 7. Run-averaged observations and daily or monthly model output
    if (siteDMYR .eq. 'R') then ! Need to average output over whole run
      timeidx = 1
      ntave = time_dim
    endif 
    ! 8. Output from POP file for year 2000 used, no time, patch or space averaging needed
    if (siteDMYR .eq. 'I') then 
      timeidx = 1
      ntave = 1
      npave = 1
    endif
    ! Test to make sure found correct time index and range:
    if ((timeidx .eq. -1) .or. (ntave .eq. -1)) then
      write(6,*) '***************************************************************************************'
      write(6,*) 'ERROR: ExtractObservables.exe did not find any model time range for observation '
      write(6,*) obsname(i),' ',obsinfo(i)
      write(6,*) 'siteDMYR=',siteDMYR
      write(6,*) 'Program stopped'
      write(6,*) '***************************************************************************************'
      STOP
    endif

    ! If patch-average required, then read all patches, otherwise just read patch needed
    if ((sitepatch .eq. 'a') .or. (sitepatch .eq. 'p')) then
      patchidx = 1                ! will read patches patchidx:patchidx+npave-1
      npave = patch_dim
    else
      read(sitepatch,*) ipatch
      patchidx = ipatch           ! will read only patch patchidx
      npave = 1
    endif
    ! Soil layers
    if (obstype(i) .eq. 'SMC') then
      nlint = 2 ! usual case will be interpolation of soil layers, unless above middle of layer 1 or below middle of lowest layer
      if (layer .eq. 0) then  ! obs depth is above middle of top layer
        layer = 1
        nlint = 1  ! no interpolation required
      endif
      if (layer .eq. soil_dim) then  ! obs depth is below middle of lowest layer
        nlint = 1  ! no interpolation required
      endif
    else
      nlint = 1
      layer = 1
    endif
    if ((obstype(i) .eq. 'SMO') .or. (obstype(i) .eq. 'SMF')) then  ! Oznet soil moisture
      layer = 1
      nlint = 6
    endif
   
    ! Spatial dimension - most observation types relate to a single point in space
    ! but code allows the average of multiple points which is required for streamflow measurements
    ! where multiple points can represent a catchment
    ! nsave = number of points to average spatially; sitearr gives indices (may not be consecutive)
    if (obstype(i) .eq. 'STR') then 
      nsave = nCellsInCatch  ! averaging over this many gridcells to represent catchment
    else 
      nsave = 1              ! single gridcell, not averaging over space for this observation
    endif  
    if (.not.(allocated(sitearr))) then
      allocate(sitearr(nsave))
      allocate(biome(nsave))
      allocate(nvis(nsave))
    else
      if (nsave .ne. size(sitearr)) then
        deallocate(sitearr)   ! must have been used previously for different nsave
        allocate(sitearr(nsave))
        deallocate(nvis)
        allocate(nvis(nsave))
        deallocate(biome)
        allocate(biome(nsave))
      endif
    endif
    if (obstype(i) .eq. 'STR') then
      if (trim(sitename(i)) .ne. trim(prevcatch)) then  ! obs for a new catchment, need to read gridcells
        write(6,*) 'Read Catchment_gridcells.txt for catchment ',trim(sitename(i)),':'
        do 
          read(35,*,IOSTAT=ios) catchname, ncells
          if (ios .ne. 0) STOP "Problem reading catchment file Catchment_gridcells.txt - eof"
          if (catchname .ne. trim(sitename(i))) then
            do j = 1,ncells
              read(35,*,IOSTAT=ios) !sitearr(j)
              if (ios .ne. 0) STOP 'Problem reading catchment file Catchment_gridcells.txt'
            enddo
            read(35,*) !precip
            write(6,*) 'Details read for catchment ',trim(catchname),' but no observations (looking for ',trim(sitename(i)),')'
          else
            exit 
          endif
        enddo
        if (verbose) write(6,*) 'Read gridcells for catchment ',catchname,', ',ncells,' gridcells'
        if (ncells .ne. nCellsInCatch) then
          write(6,*) '*******************************************************************************'
          write(6,*) 'ERROR: wrong catchname or number of gridcells in file Catchment_gridcells.txt'
          write(6,*) 'Looking for sitename ', trim(sitename(i))
          write(6,*) 'Found catchname ',trim(catchname)
          write(6,*) 'Expecting ',nCellsInCatch,' gridcells'
          write(6,*) 'File has ',ncells
          write(6,*) 'obsname is ',obsname(i)
          write(6,*) 'obsinfo is ',obsinfo(i)
          write(6,*) 'Program stopped'
          write(6,*) '*******************************************************************************'
          STOP
        else
          do j = 1,ncells
            read(35,*,IOSTAT=ios) sitearr(j)
            if (ios .ne. 0) STOP 'Problem reading catchment file Catchment_gridcells.txt'
          enddo
          read(35,*) ! precip
          prevcatch = catchname
          write(6,*) 'sitearr = ',sitearr
          !write(6,*) 'biome = ',biome
        endif
      endif
    else
      sitearr(1) = siteidx
      nvis(1) = nvissav  ! was read from obsinfo earlier
      biome(1) = biomesav  ! was read from obsinfo earlier
    endif  ! if STR read gridcells, otherwise use index from obsinfo

    ! Extract required part of model variable to calculate observable; then progressively compress dimensions by averaging 
    ! or interpolation until left with a single number corresponding to the observation
    allocate(workingvar(nsave,npave,nlint,ntave))
    !if (verbose) write(6,*) 'Extract part of variable needed to create observable'
    select case (obstype(i))
      case ('LBA','LTD')   !  basal area and tree density are 1-d from POP ini file
        if (PatchReweight .eq. 'biome') then
          ForestFrac = BiomeForestFrac(biome(1))
        else
          ForestFrac = NVISMVGForestFrac(nvis(1))
        endif
        workingvar(1,1,1,1) = var1(siteidx)*ForestFrac
      case ('SMO','SMC','SMF')   ! soil moisture has depth dimension
        workingvar = var4(sitearr,patchidx:patchidx+npave-1,layer:layer+nlint-1,timeidx:timeidx+ntave-1)
      case ('Phy','AGD','ABM')
     !   if (strinfo(4:6) .eq. 'cas') then     ! CASA output
        !  workingvar(1,1,1,1) = var1(siteidx) ! what was this>??????
     !     workingvar(1,1,1,1) = var3(siteidx) 
     !   else
        workingvar(:,:,1,:) = var3(sitearr,patchidx:patchidx+npave-1,timeidx:timeidx+ntave-1)  ! CABLE file, usual 3-d array
     !   endif 
      case default         ! all others are 3-d
        workingvar(:,:,1,:) = var3(sitearr,patchidx:patchidx+npave-1,timeidx:timeidx+ntave-1)
    end select

    ! Time averaging first, if needed
    if (ntave .gt. 1) then
      allocate(temp(nsave,npave,nlint,1))
      temp = TimeAve(workingvar,4,nsave,npave,nlint,ntave)
      deallocate(workingvar)
      allocate(workingvar(nsave,npave,nlint,1))
      workingvar = temp
      deallocate(temp)
    endif
    ! Interpolation of soil layers, if needed
    if (nlint .gt. 1) then
      allocate(temp(nsave,npave,1,1))
      do is = 1,nsave
        do ip = 1,npave
          if ((obstype(i) .eq. 'SMO') .or. (obstype(i) .eq. 'SMF')) then
            temp(is,ip,1,1) = sum(workingvar(is,ip,:,1) * smweight)
          else
            idx = sitearr(is)  ! if > 1 spatial point, don't expect them to be in order
            layerabove = sum(zse(idx,patchidx+ip-1,1:layer-1))+0.5*zse(idx,patchidx+ip-1,layer)
            layerbelow = sum(zse(idx,patchidx+ip-1,1:layer))+0.5*zse(idx,patchidx+ip-1,layer+1)
            temp(is,ip,1,1) = SoilInterp(workingvar(is,ip,:,1),smdep,layerabove,layerbelow)
          endif
        enddo
      enddo
      deallocate(workingvar)
      allocate(workingvar(nsave,npave,1,1))
      workingvar = temp
      deallocate(temp)
    endif
    ! Patch averaging next, if necessary
    if ((sitepatch .eq. 'a') .or. (sitepatch .eq. 'p')) then  ! calculate patch average
      allocate(temp(nsave,1,1,1))
      allocate(pf(size(sitearr),3))
      if (sitepatch .eq. 'a') pf = patchfrac(sitearr,:)   ! patch average using model's patch weights
      if (sitepatch .eq. 'p') then  ! reweight assuming no LUC, if necessary (i.e. if 3 patches in gridcell)
        do is = 1,size(sitearr)   ! loop over gridcells in catchment if STR or one gridcell for other obs types
          if (((patchfrac(sitearr(is),2) .eq. patchfill) .or. (patchfrac(sitearr(is),2) .eq. patchmissing)) .or.   &
              ((patchfrac(sitearr(is),3) .eq. patchfill) .or. (patchfrac(sitearr(is),3) .eq. patchmissing))) then  ! no LUC, so no reweighting required
            pf(is,:) = patchfrac(sitearr(is),:)   ! 
            write(6,*) 'Patch averaging is "p" but left alone (',patchfrac(sitearr(is),:),') to (',pf(is,:),') for biome ',biome(is),' at gridcell ',sitearr(is)
          else
            if (PatchReweight .eq. 'biome') then
              ForestFrac = BiomeForestFrac(biome(is))
            else
               ForestFrac = NVISMVGForestFrac(nvis(is))
            endif
            pf(is,1) = ForestFrac
            pf(is,2) = 0.0
            pf(is,3) = 1.0 - ForestFrac
            write(6,*) 'Patch averaging is "p" so reweight patches from (',patchfrac(sitearr(is),:),') to (',pf(is,:),') for biome ',biome(is),' at gridcell ',sitearr(is)
          endif
        enddo
      endif
      temp = PatchAve(workingvar,pf,nsave,patchfill,patchmissing,patch_dim)
      deallocate(pf)
      deallocate(workingvar)
      allocate(workingvar(nsave,1,1,1))
      workingvar = temp
      deallocate(temp)
    endif
    ! Spatial average, if needed
    if (nsave .gt. 1) then
      allocate(temp(1,1,1,1))
      temp(1,1,1,1) = sum(workingvar(:,1,1,1))/nsave
      deallocate(workingvar)
      allocate(workingvar(1,1,1,1))
      workingvar = temp
      deallocate(temp)
    endif
    observable(i) = workingvar(1,1,1,1)
    deallocate(workingvar)
    prevobstype = obstype(i)
  enddo  ! loop over obs

  ! Calculate anomalies etc if necessary
  ! Temporal anomaly
  varsum = 0.0
  varcount = 0
  firstidx = 0
  lastcurrent = .false.
  do i = 1,nobs
    if (obs_proc(i) .eq. 'tan') then
      varsum = varsum + observable(i)
      varcount = varcount + 1
      if (firstidx .eq. 0) firstidx = i
      ! check to see whether last obs of current site and observation type, in which case calculate and subtract mean
      if (i .eq. nobs) lastcurrent = .true.
      if (.not.(lastcurrent)) then 
        if ((sitename(i) .ne. sitename(i+1)) .or. (obstype(i) .ne. obstype(i+1))) lastcurrent = .true.
      endif
      if (lastcurrent) then
        varmean = varsum/varcount
        do j = firstidx,i
          observable(j) = observable(j) - varmean
        enddo
        write(6,*) 'Subtract temporal anomaly for variable ',trim(obstype(i)),' at site ',trim(sitename(i))
        write(6,*) '         -> anomaly is ',varmean
        varsum = 0.0
        varcount = 0
        firstidx = 0
        lastcurrent = .false.
      endif
    endif
  enddo

  ! Apply temporal scaling if necessary
  obssum = 0.0
  varsum = 0.0
  varcount = 0
  firstidx = 0
  lastcurrent = .false.
  do i = 1,nobs
    if (obs_proc(i) .eq. 'tsc') then
      obssum = obssum + obs(i)
      varsum = varsum + observable(i)
      varcount = varcount + 1
      if (firstidx .eq. 0) firstidx = i
      ! check to see whether last obs of current site and observation type, in which case calculate and subtract mean
      if (i .eq. nobs) lastcurrent = .true.
      if (.not.(lastcurrent)) then
        if ((sitename(i) .ne. sitename(i+1)) .or. (obstype(i) .ne. obstype(i+1))) lastcurrent = .true.
      endif
      if (lastcurrent) then
        varmean = varsum/varcount
        obsmean = obssum/varcount
        do j = firstidx,i
          observable(j) = observable(j)*obsmean/varmean
        enddo
        write(6,*) 'Scale variable ',trim(obstype(i)),' at site ',trim(sitename(i)),' so model mean matches obs mean'
        write(6,*) '         -> scale factor is ',obsmean/varmean
        varsum = 0.0
        varcount = 0
        firstidx = 0
        lastcurrent = .false.
      endif
    endif
  enddo

  ! do any other processing here - spatial anom, cdf matching etc

  ! write observables to file
  do i = 1,nobs
    write(26,123) observable(i)
    if (weight(i) .gt. 0.0) then
      mdm = mdm + (observable(i)-obs(i))*(observable(i)-obs(i))*weight(i)*weight(i)
      nobs_included = nobs_included + 1
    endif
  enddo
  close(25)
  close(26)
  close(35)
123 format(1pg14.7)
  write(6,*) 'model-data-mismatch = ',mdm
  write(6,*) 'Number of obs with non-zero weight = ',nobs_included
  write(6,*) 'Total number of observations =',nobs

  call check( nf90_close(FILE_ID) )

  write(6,*) 'ExtractObservables finished successfully, file Observables.txt written'

end program ExtractObservables
 

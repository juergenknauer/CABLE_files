! Utils.f90
!---------------------------------------------------------

!*******************************************************************************

MODULE TypeDef
!-------------------------------------------------------------------------------
! PRB: 18-06-2000
! This module explicitly defines the sizes of variable types
!-------------------------------------------------------------------------------
  implicit none
  save
! Define integer kind parameters to accommodate the range of numbers usually
! associated with 4, 2, and 1 byte integers.
  integer,parameter :: i4b = selected_int_kind(9)
  integer,parameter :: i2b = selected_int_kind(4)
  integer,parameter :: i1b = selected_int_kind(2)
! Define single and double precision real kind parameters:
! * Kind(1.0)   defines sp as the machine's default size for single precision
! * Kind(1.0d0) defines dp as the machine's default size for double precision
  integer,parameter :: sp  = kind(1.0)
  integer,parameter :: dp  = kind(1.0d0)
! lgt is set to the default kind required for representing logical values.
  integer,parameter :: lgt = kind(.true.)

END MODULE TypeDef

!###############################################################################

MODULE DateFunctions
!-------------------------------------------------------------------------------
! Date arithmetic (PRB, 12-07-2000; MRR, 11-oct-05)
!-------------------------------------------------------------------------------
USE TypeDef
implicit none
public
save
! Define a type for Australian-style dates built from 3 integer fields.
! Assign as Date = dmydate(iday,imth,iyear)
! Access components as iday=Date%Day, imth=Date%Month, iyear=Date%Year
type dmydate
  integer(i4b):: Day
  integer(i4b):: Month
  integer(i4b):: Year
end type dmydate
! Define interfaces for the +, -, .eq., .ne., .lt., .gt., .le., .ge. operators to:
! * add, subtract a number of days (integer) from a dmydate, with AddDay, SubDay; 
! * logically compare two dates, with EQDates, NEDates etc.
! These explicit interfaces are required.
interface operator (+)
  module procedure AddDay
end interface
interface operator (-)
  module procedure SubDay
end interface
interface operator (.eq.)
  module procedure EQDates
end interface
interface operator (.ne.)
  module procedure NEDates
end interface
interface operator (.lt.)
  module procedure LTDates
end interface
interface operator (.gt.)
  module procedure GTDates
end interface
interface operator (.le.)
  module procedure LEDates
end interface
interface operator (.ge.)
  module procedure GEDates
end interface
!-------------------------------------------------------------------------------

CONTAINS 

!###############################################################################
  
  function AddDay(Today,Days2Add)
!-------------------------------------------------------------------------------
! Extends the '+' operator to enable adding days to a date
!-------------------------------------------------------------------------------
  implicit none
  type (dmydate)              :: AddDay    ! Date with days added (function name)
  type (dmydate), intent(in)  :: Today     ! Current date
  integer(i4b),   intent(in)  :: Days2Add  ! Days to add to current date
  real(dp)                    :: JDay      ! Julian Day
!-------------------------------------------------------------------------------
  JDay = JulianDay(Today)
  JDay = JDay + float(Days2Add)
  AddDay = GregDate(JDay)
  end function AddDay

!###############################################################################

  function AddDayNoLeap(Today,Days2Add)
!-------------------------------------------------------------------------------
! Add days to a date without leap years
!-------------------------------------------------------------------------------
  implicit none
  type (dmydate)              :: AddDayNoLeap ! Date with days added (function name)
  type (dmydate), intent(in)  :: Today        ! Current date
  integer(i4b),   intent(in)  :: Days2Add     ! Days to add to current date
  type (dmydate)              :: TempDate     ! Working variable
  integer(i4b)                :: TodayDay     ! Day of year for Today
  integer(i4b)                :: Years2Add    ! Number of whole years added
!-------------------------------------------------------------------------------
  TempDate%Day = Today%Day
  TempDate%Month = Today%Month
  TempDate%Year = 1997  ! not a leap year
  TodayDay = YearDay(TempDate)
  Years2Add = (TodayDay + Days2Add - 1)/365
  TempDate%Day = 1
  TempDate = AddDay(TempDate,mod((TodayDay+Days2Add),365)-1)
  AddDayNoLeap = TempDate
  AddDayNoLeap%Year = Today%Year + Years2Add
  end function AddDayNoLeap

!###############################################################################

  function SubDay(Today,Days2Sub)
!-------------------------------------------------------------------------------
! Extends the '-' operator to enable subtracting days from a date
!-------------------------------------------------------------------------------
  implicit none
  type (dmydate)              :: SubDay    ! Date with days added (function name)
  type (dmydate), intent(in)  :: Today     ! Current date
  integer(i4b),   intent(in)  :: Days2Sub  ! Days to add to current date
  real(dp)                    :: JDay      ! Julian Day
!-------------------------------------------------------------------------------
  JDay = JulianDay(Today)
  JDay = JDay - float(Days2Sub)
  SubDay = GregDate(JDay)
  end function SubDay

!###############################################################################

  function EQDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.eq.' operator to enable comparison of two dates for equality.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: EQDates   ! Result of equality test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!-------------------------------------------------------------------------------
  EQDates = .true.
  if (Date1%day   .ne. Date2%day   .or. &
      Date1%month .ne. Date2%month .or. &
      Date1%year  .ne. Date2%year) EQDates = .false.
  end function EQDates

!###############################################################################

  function NEDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.ne.' operator to enable comparison of two dates for inequality.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: NEDates   ! Result of inequality test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!-------------------------------------------------------------------------------  
  NEDates = .not. EQDates(Date1,Date2)
  end function NEDates

!###############################################################################

  function LTDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.lt.' operator to enable comparison of two dates for Date1 < Date2.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: LTDates   ! Result of LT test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!------------------------------------------------------------------------------- 
  LTDates = (JulianDay(Date1).lt.JulianDay(Date2))
  end function LTDates

!###############################################################################

  function GTDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.gt.' operator to enable comparison of two dates for Date1 > Date2.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: GTDates   ! Result of GT test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!-------------------------------------------------------------------------------  
  GTDates = (JulianDay(Date1).gt.JulianDay(Date2))
  end function GTDates

!###############################################################################

  function LEDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.le.' operator to enable comparison of two dates Date1 <= Date2.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: LEDates   ! Result of LE test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!-------------------------------------------------------------------------------  
  LEDates = (LTDates(Date1,Date2) .or. EQDates(Date1,Date2))
  end function LEDates

!###############################################################################

  function GEDates(Date1,Date2)
!-------------------------------------------------------------------------------
! Extends the '.ge.' operator to enable comparison of two dates Date1 >= Date2.
!-------------------------------------------------------------------------------
  implicit none
  logical                     :: GEDates   ! Result of GE test between dates
  type (dmydate), intent(in)  :: Date1     ! First date for logical comparison
  type (dmydate), intent(in)  :: Date2     ! Second date for logical comparison
!-------------------------------------------------------------------------------  
  GEDates = (GTDates(Date1,Date2) .or. EQDates(Date1,Date2))
  end function GEDates

!###############################################################################

  function JulianDay(GregDate)
!-------------------------------------------------------------------------------
! Returns a real(sp) Julian Day when given a Gregorian Date.
! Adapted from Date Algorithms of Peter Baum:
! http://vsg.cape.com/~pbaum/date/date0.htm
!-------------------------------------------------------------------------------
  implicit none
  real(dp)                   :: JulianDay
  type (dmydate), intent(in) :: GregDate
  real(dp)                   :: D,M,Y
!-------------------------------------------------------------------------------
  D = dble(GregDate%Day)
  M = dble(GregDate%Month)
  Y = dble(GregDate%Year)
  if (M.lt.3.0_8) then 
    M = M + 12.0_8 
    Y = Y - 1.0_8 
  end if 
  JulianDay = D + int((153.0_8 * M - 457.0_8)/5.0_8) + (365.0_8*Y) +     &
              floor(Y/4.0_8) - floor(Y/100.0_8) + floor(Y/400.0_8) + 1721118.5_8
  end function JulianDay

!###############################################################################

  function GregDate(JulianDay)
!-------------------------------------------------------------------------------
! Returns a Gregorian Date when given a Julian Day 
! Modified from Date Algorithms of Peter Baum 
! http://vsg.cape.com/~pbaum/date/date0.htm
!-------------------------------------------------------------------------------
  implicit none
  type (dmydate)             :: GregDate
  real(dp), intent(in)       :: JulianDay
  real(dp)                   :: D,M,Y
  real(dp)                   :: Z,R,A,G,B,C
!-------------------------------------------------------------------------------
  Z = floor(JulianDay - 1721118.5_8) 
  R = JulianDay - 1721118.5_8 - Z
  G = Z - 0.25_8 
  A = floor(G/36524.25_8) 
  B = A - floor(A/4.0_8) 
  Y = floor((B+G)/365.25_8) 
  C = B + Z - floor(365.25_8*Y) 
  M = int(((5.0_8*C) + 456.0_8) / 153.0_8) 
  D = C - int((153.0_8 * M - 457.0_8) / 5.0_8) + R  
  if (M .gt. 12.0_8) then 
    Y = Y + 1.0_8 
    M = M - 12.0_8 
  end if
  GregDate = dmydate(int(D),int(M),int(Y))  ! Keep truncated day number only
  end function GregDate

!###############################################################################

  function LeapDay(Year)
!-------------------------------------------------------------------------------
! Returns 1 if leap year, 0 if not.  Add it to the length of February.
!-------------------------------------------------------------------------------
  implicit none
  integer(i4b)               :: LeapDay  
  integer(i4b), intent(in)   :: Year
!-------------------------------------------------------------------------------
  if (mod(Year,4).ne.0) then
    LeapDay = 0
  else if (mod(Year,400).eq.0) then
    LeapDay = 1
  else if (mod(Year,100).eq.0) then
    LeapDay = 0
  else
    LeapDay = 1
  end if
  end function LeapDay

!###############################################################################

function DaysInMonth(Date)
!-------------------------------------------------------------------------------
! returns number of days in current month
! MRR, 12-oct-2005: return 0 if month not legal
!-------------------------------------------------------------------------------
type(dmydate),intent(in):: Date
integer(i4b)          :: DaysInMonth  
integer(i4b),parameter:: MonthDays(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

!-------------------------------------------------------------------------------
if (Date%Month >= 1 .and. Date%Month <= 12) then
  if (Date%Month.eq.2) then
      DaysInMonth = MonthDays(2) + LeapDay(Date%Year)
  else 
    DaysInMonth = MonthDays(Date%Month)
  end if
else
  DaysInMonth = 0
end if
end function DaysInMonth

!###############################################################################

function DaysInMonthNoLeap(Date)
!-------------------------------------------------------------------------------
! returns number of days in current month, with no leap years
! MRR, 12-oct-2005: return 0 if month not legal
!-------------------------------------------------------------------------------
type(dmydate),intent(in):: Date
integer(i4b)          :: DaysInMonthNoLeap
integer(i4b),parameter:: MonthDays(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

!-------------------------------------------------------------------------------
if (Date%Month >= 1 .and. Date%Month <= 12) then
  DaysInMonthNoLeap = MonthDays(Date%Month)
else
  DaysInMonthNoLeap = 0
end if
end function DaysInMonthNoLeap

!###############################################################################

function DaysInMonthEither(Date,LeapFlag)
!-------------------------------------------------------------------------------
! returns number of days in current month, with either leap years or not
! CMT 29-may-2017: return 0 if month not legal
!-------------------------------------------------------------------------------
type(dmydate),intent(in):: Date
integer(i4b) :: DaysInMonthEither
logical :: LeapFlag
!-------------------------------------------------------------------------------
if (LeapFlag) then
  DaysInMonthEither = DaysInMonth(Date)
else
  DaysInMonthEither = DaysInMonthNoLeap(Date)
endif
end function DaysInMonthEither

!###############################################################################

  function YearDay(Date)
!-------------------------------------------------------------------------------
  type(dmydate), intent(in)   :: Date
  integer(i4b)                :: YearDay 
  integer(i4b), dimension(12) :: MonthDays
!-------------------------------------------------------------------------------
  MonthDays = (/31,28,31,30,31,30,31,31,30,31,30,31/)
  MonthDays(2) = 28 + LeapDay(Date%Year)
  if (Date%Month.eq.1) then
    YearDay = Date%Day
  else
    YearDay = sum(MonthDays(1:Date%Month-1)) + Date%Day
  end if
  end function YearDay

!###############################################################################

FUNCTION DayDifference (Date1,Date0)
!-------------------------------------------------------------------------------
! Returns Date1 - Date0 in integer days.
! MRR, 11-oct-05
!-------------------------------------------------------------------------------
USE TypeDef
implicit none
type(dmydate),intent(in):: Date1, Date0
integer(i4b):: DayDifference
!-------------------------------------------------------------------------------
DayDifference = nint(JulianDay(Date1) - JulianDay(Date0))
END FUNCTION DayDifference

!###############################################################################

FUNCTION LegalDate (Date)
!-------------------------------------------------------------------------------
! Returns .true. if date is legal (test D,M only), otherwise .false.
! MRR, 11-oct-05
!-------------------------------------------------------------------------------
USE TypeDef
implicit none
type(dmydate),intent(in):: Date
logical(lgt):: LegalDate
!-------------------------------------------------------------------------------
LegalDate = .false.
if ( (Date%month >= 1 .and. Date%month <= 12) .and.         &   ! check month
     (Date%day >= 1 .and. Date%day <= DaysInMonth(Date)) )  &   ! check day
   LegalDate = .true.
END FUNCTION LegalDate

!###############################################################################

FUNCTION EndMonth (Date)
!-------------------------------------------------------------------------------
! Returns .true. if date is last day of month, otherwise .false.
! MRR, 11-oct-05
!-------------------------------------------------------------------------------
USE TypeDef
implicit none
type(dmydate),intent(in):: Date
logical(lgt):: EndMonth
!-------------------------------------------------------------------------------
EndMonth = .false.
if (Date%day == DaysInMonth(Date)) EndMonth = .true.
END FUNCTION EndMonth

!###############################################################################

FUNCTION EndYear (Date)
!-------------------------------------------------------------------------------
! Returns .true. if date is last day of year, otherwise .false.
! MRR, 11-oct-05
!-------------------------------------------------------------------------------
USE TypeDef
implicit none
type(dmydate),intent(in):: Date
logical(lgt):: EndYear
!-------------------------------------------------------------------------------
EndYear = .false.
if (Date%day == 31 .and. Date%month == 12) EndYear = .true.
END FUNCTION EndYear

END MODULE DateFunctions

!###############################################################################

MODULE Utils
use netcdf
implicit none
contains
  subroutine check(status)
    use netcdf
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
      write(6,*) nf90_strerror(status)
      stop "Stopped"
    end if
  end subroutine check

  SUBROUTINE HANDLE_ERR( status, msg )
    use netcdf
    implicit none
    INTEGER :: status
    CHARACTER(LEN=*), INTENT(IN),OPTIONAL :: msg
    IF(status /= NF90_noerr) THEN
       WRITE(*,*)"netCDF error:"
       IF ( PRESENT( msg ) ) WRITE(*,*)msg
       WRITE(*,*) TRIM(NF90_strerror(status))
       STOP -1
    END IF
  END SUBROUTINE HANDLE_ERR

  FUNCTION int2str1(ii) result(str)
    implicit none
    integer, intent (in) :: ii
    character(1) :: str
    write(str,'(i1)') ii
  end function int2str1

  FUNCTION int2str2(ii) result(str)
    implicit none
    integer, intent (in) :: ii
    character(2) :: str
    if (ii .le. 9) then
      write(str,'(a1,i1)') '0',ii
    else
      write(str,'(i2)') ii
    end if
  end function int2str2  

  FUNCTION int2str3(ii) result(str)
    implicit none
    integer, intent (in) :: ii
    character(3) :: str
    if (ii .le. 9) then
      write(str,'(a2,i1)') '00',ii
    else
      if (ii .le. 99) then
         write(str,'(a,i2)') '0',ii
      else
        write(str,'(i3)') ii
      endif
    endif
  end function int2str3

  FUNCTION int2str4(ii) result(str)
    implicit none
    integer, intent (in) :: ii
    character(4) :: str
    if (ii .le. 9) then
      write(str,'(a3,i1)') '000',ii
    else
      if (ii .le. 99) then 
         write(str,'(a2,i2)') '00',ii
      else
        if (ii .le. 999) then
         write(str,'(a1,i3)') '0',ii
        else
          write(str,'(i4)') ii
        endif
      endif
    endif
  end function int2str4

  FUNCTION int2str5(ii) result(str)
    implicit none
    integer, intent (in) :: ii
    character(5) :: str
    if (ii .le. 9) then
      write(str,'(a4,i1)') '0000',ii
    else
      if (ii .le. 99) then 
        write(str,'(a3,i2)') '000',ii
      else
        if (ii .le. 999) then
          write(str,'(a2,i3)') '00',ii
        else
          if (ii .le. 9999) then
            write(str,'(a1,i4)') '0',ii
          else
            write(str,'(i5)') ii
          endif
        endif
      endif
    endif
  end function int2str5

  FUNCTION flt2str4(ff) result(str)
  ! Expect number < 10.0 to fit into f4.2
    implicit none
    real, intent (in) :: ff
    character(4) :: str
    write(str,'(f4.2)') ff
  end function flt2str4

END MODULE Utils

!--------------------- 
MODULE String_Utility 
    IMPLICIT NONE 
    PRIVATE 
    PUBLIC :: StrUpCase 
    PUBLIC :: StrLowCase 


   CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz' 
    CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 


CONTAINS 


   FUNCTION StrUpCase ( Input_String ) RESULT ( Output_String ) 
      ! -- Argument and result 
      CHARACTER( * ), INTENT( IN )     :: Input_String 
      CHARACTER( LEN( Input_String ) ) :: Output_String 
      ! -- Local variables 
      INTEGER :: i, n 


     ! -- Copy input string 
      Output_String = Input_String 
      ! -- Loop over string elements 
      DO i = 1, LEN( Output_String ) 
        ! -- Find location of letter in lower case constant string 
        n = INDEX( LOWER_CASE, Output_String( i:i ) ) 
        ! -- If current substring is a lower case letter, make it upper case 
        IF ( n /= 0 ) Output_String( i:i ) = UPPER_CASE( n:n ) 
      END DO 
    END FUNCTION StrUpCase 


   FUNCTION StrLowCase ( Input_String ) RESULT ( Output_String ) 
      ! -- Argument and result 
      CHARACTER( * ), INTENT( IN )     :: Input_String 
      CHARACTER( LEN( Input_String ) ) :: Output_String 
      ! -- Local variables 
      INTEGER :: i, n 


     ! -- Copy input string 
      Output_String = Input_String 
      ! -- Loop over string elements 
      DO i = 1, LEN( Output_String ) 
        ! -- Find location of letter in upper case constant string 
        n = INDEX( UPPER_CASE, Output_String( i:i ) ) 
        ! -- If current substring is an upper case letter, make it lower case 
        IF ( n /= 0 ) Output_String( i:i ) = LOWER_CASE( n:n ) 
      END DO 
    END FUNCTION StrLowCase 
   END MODULE String_Utility

!------------------------------------------------------------------------------
module parse 
  integer, parameter :: MAX_LINE = 12000    ! you determine the size of line 
  character(MAX_LINE) :: line 
  contains 
  function getArg(n) result(arg) 
    implicit none 
    character(300) :: str
    real :: arg
    integer :: n,i,j,count 
    j = 0 
    do count=1,n 
      i = j + 1 
      j = INDEX(line(i:),',') 
      if(j == 0) exit 
      j = j + i - 1 
    end do 
    if(j == 0) then 
      if(count == n) then 
        str = line(i:) 
      else 
        str = ' ' 
      endif 
    else 
      str = line(i:j-1) 
    endif 
    if (str .eq. '') then
      arg = -999.0
    else
      read(str,*) arg
    endif
  end function getArg 
!--------------------------------------------
  function getStr(n) result(str)
    implicit none
    character(300) :: str
    integer :: n,i,j,count
    j = 0
    do count=1,n
      i = j + 1
      j = INDEX(line(i:),',')
      if(j == 0) exit
      j = j + i - 1
    end do
    if(j == 0) then
      if(count == n) then
        str = line(i:)
      else
        str = ' '
      endif
    else
      str = line(i:j-1)
    endif
  end function getStr
end module parse 



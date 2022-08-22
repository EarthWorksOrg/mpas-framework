! Various dummy type definitions and routines for the sole purpose of
! mimicking newer ESMF interface features without necessarily implementing
! them.

MODULE ESMFloc_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE ESMFloc_Grid
      INTEGER :: dummy
   END TYPE

   TYPE ESMFloc_GridComp
      INTEGER :: dummy
   END TYPE

   TYPE ESMFloc_State
      INTEGER :: dummy
   END TYPE

   TYPE ESMFloc_VM
      INTEGER :: dummy
   END TYPE

   TYPE ESMFloc_END_FLAG
      INTEGER :: dummy
   END TYPE
   TYPE(ESMFloc_END_FLAG), PARAMETER ::  &
      ESMFloc_END_ABORT   = ESMFloc_END_FLAG(1), &
      ESMFloc_END_NORMAL  = ESMFloc_END_FLAG(2), &
      ESMFloc_END_KEEPMPI = ESMFloc_END_FLAG(3)

   TYPE ESMFloc_MsgType
      INTEGER :: mtype
   END TYPE
   TYPE(ESMFloc_MsgType), PARAMETER  ::      &
      ESMFloc_LOG_INFO  =   ESMFloc_MsgType(1), &
      ESMFloc_LOG_WARNING = ESMFloc_MsgType(2), &
      ESMFloc_LOG_ERROR =   ESMFloc_MsgType(3)

   TYPE ESMFloc_LOG
      INTEGER :: dummy
   END TYPE

   LOGICAL, private, save :: initialized = .false.

   PUBLIC ESMFloc_Grid, ESMFloc_GridComp, ESMFloc_State, ESMFloc_VM
   PUBLIC ESMFloc_Initialize, ESMFloc_Finalize, ESMFloc_IsInitialized
   PUBLIC ESMFloc_LogWrite, ESMFloc_LOG, ESMFloc_MsgType, ESMFloc_END_FLAG
   PUBLIC ESMFloc_LOG_INFO, ESMFloc_LOG_WARNING, ESMFloc_LOG_ERROR
   PUBLIC ESMFloc_END_ABORT, ESMFloc_END_NORMAL, ESMFloc_END_KEEPMPI

CONTAINS


! NOOP
   SUBROUTINE ESMFloc_Initialize( vm, defaultCalendar, rc )
      USE ESMFloc_BaseMod
      USE ESMFloc_CalendarMod
!     USE ESMFloc_TimeMod,     only: defaultCal
      TYPE(ESMFloc_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(ESMFloc_CalKind_Flag), INTENT(IN   ), OPTIONAL :: defaultCalendar
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc

      TYPE(ESMFloc_CalKind_Flag) :: defaultCalType
      INTEGER :: status

      IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ! Initialize the default time manager calendar
      IF ( PRESENT(defaultCalendar) )THEN
         defaultCalType = defaultCalendar
      ELSE
         defaultCalType = ESMFloc_CALKIND_NOLEAP
      END IF
      allocate( defaultCal )
!      write(6,*) 'tcx1 ESMFloc_Stubs defcal ',defaultcaltype%caltype
!      call flush(6)
      defaultCal = ESMFloc_CalendarCreate( calkindflag=defaultCalType, &
                        rc=status)
!      write(6,*) 'tcx2 ESMFloc_Stubs defcal ',defaultcal%type%caltype
!      call flush(6)
      allocate( gregorianCal )
!      write(6,*) 'tcx1 ESMFloc_Stubs grcal ',ESMFloc_calkind_gregorian%caltype
!      call flush(6)
      gregorianCal = ESMFloc_CalendarCreate( calkindflag=ESMFloc_CALKIND_GREGORIAN, &
                        rc=status)
!      write(6,*) 'tcx2 ESMFloc_Stubs grcal ',gregoriancal%type%caltype
!      call flush(6)
      allocate( noleapCal )
!      write(6,*) 'tcx1 ESMFloc_Stubs nlcal ',ESMFloc_calkind_noleap%caltype
!      call flush(6)
      noleapCal = ESMFloc_CalendarCreate( calkindflag=ESMFloc_CALKIND_NOLEAP, &
                        rc=status)
!      write(6,*) 'tcx2 ESMFloc_Stubs nlcal ',noleapcal%type%caltype
!      call flush(6)

      ! initialize tables in time manager
      CALL initdaym

      IF (status .ne. ESMFloc_SUCCESS) THEN
          PRINT *, "Error initializing the default time manager calendar"
          RETURN
      END IF
      initialized = .true.

      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
   END SUBROUTINE ESMFloc_Initialize


   FUNCTION ESMFloc_IsInitialized()
      LOGICAL ESMFloc_IsInitialized
      ESMFloc_IsInitialized = initialized
   END FUNCTION ESMFloc_IsInitialized


! NOOP
   SUBROUTINE ESMFloc_Finalize( endflag, rc )
      USE ESMFloc_BaseMod
      type(ESMFloc_END_FLAG), intent(in), optional  :: endflag
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
#ifndef HIDE_MPI
#include <mpif.h>
#endif
      INTEGER :: ier

      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
#ifndef HIDE_MPI
      CALL MPI_Finalize( ier )
      IF ( ier .ne. mpi_success )THEN
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      END IF
#endif
   END SUBROUTINE ESMFloc_Finalize

! NOOP
   SUBROUTINE ESMFloc_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE ESMFloc_BaseMod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(ESMFloc_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(ESMFloc_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
   END SUBROUTINE ESMFloc_LogWrite


END MODULE ESMFloc_Stubs



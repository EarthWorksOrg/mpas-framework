! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Clock Module
      module ESMFloc_ClockMod
!
!==============================================================================
!
! This file contains the Clock class definition and all Clock class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMFloc_TimeMgr.inc" 

!==============================================================================
!BOPI
! !MODULE: ESMFloc_ClockMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!
! See {\tt ../include/ESMC\_Clock.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMFloc_BaseMod

      ! associated derived types
      use ESMFloc_TimeIntervalMod   ! , only : ESMFloc_TimeInterval
      use ESMFloc_TimeMod           ! , only : ESMFloc_Time
      use ESMFloc_AlarmMod,        only : ESMFloc_Alarm

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMFloc_Clock
!
!     ! F90 class type to match C++ Clock class in size only;
!     !  all dereferencing within class is performed by C++ implementation


! internals for ESMFloc_Clock
      type ESMFloc_ClockInt
        type(ESMFloc_TimeInterval) :: TimeStep
        type(ESMFloc_Time)  :: StartTime
        type(ESMFloc_Time)  :: StopTime
        type(ESMFloc_Time)  :: RefTime
        type(ESMFloc_Time)  :: CurrTime
        type(ESMFloc_Time)  :: PrevTime
        integer(ESMFloc_KIND_I8) :: AdvanceCount
        integer :: ClockMutex
        integer :: NumAlarms
        ! Note:  to mimic ESMF 2.1.0+, AlarmList is maintained
        ! within ESMFloc_Clock even though copies of each alarm are
        ! returned from ESMFloc_AlarmCreate() at the same time they
        ! are copied into the AlarmList!  This duplication is not
        ! as hideous as it might be because the ESMFloc_Alarm type
        ! has data members that are all POINTERs (thus the horrible
        ! shallow-copy-masquerading-as-reference-copy hack works).
        type(ESMFloc_Alarm), pointer, dimension(:) :: AlarmList => null()
      end type

! Actual public type:  this bit allows easy mimic of "deep" ESMFloc_ClockCreate
! in ESMF 2.1.0+
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF
!        shallow-copy-masquerading-as-reference-copy.
      type ESMFloc_Clock
        type(ESMFloc_ClockInt), pointer  :: clockint => null()
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMFloc_Clock
      public ESMFloc_ClockInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMFloc_ClockCreate
      public ESMFloc_ClockDestroy
      public ESMFloc_ClockSet
!      public ESMFloc_ClockSetOLD
      public ESMFloc_ClockGet
!      public ESMFloc_ClockGetAdvanceCount
!      public ESMFloc_ClockGetTimeStep
!      public ESMFloc_ClockSetTimeStep
!      public ESMFloc_ClockGetCurrTime
!      public ESMFloc_ClockSetCurrTime
!      public ESMFloc_ClockGetStartTime
!      public ESMFloc_ClockGetStopTime
!      public ESMFloc_ClockGetRefTime
!      public ESMFloc_ClockGetPrevTime
!      public ESMFloc_ClockGetCurrSimTime
!      public ESMFloc_ClockGetPrevSimTime
! This must be public for ESMFloc_AlarmClockMod...
      public ESMFloc_ClockAddAlarm
      public ESMFloc_ClockGetAlarmList
!      public ESMFloc_ClockGetNumAlarms
!      public ESMFloc_ClockSyncToWallClock
      public ESMFloc_ClockAdvance
      public ESMFloc_ClockIsStopTime
      public ESMFloc_ClockStopTimeDisable

! Required inherited and overridden ESMFloc_Base class methods

!      public ESMFloc_ClockRead
!      public ESMFloc_ClockWrite
      public ESMFloc_ClockValidate
      public ESMFloc_ClockPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockSetOLD - Initialize a clockint

! !INTERFACE:
      subroutine ESMFloc_ClockSetOLD(clockint, TimeStep, StartTime, &
                                  StopTime, RefTime, rc)

! !ARGUMENTS:
      type(ESMFloc_ClockInt), intent(out) :: clockint
      type(ESMFloc_TimeInterval), intent(in), optional :: TimeStep
      type(ESMFloc_Time), intent(in) :: StartTime
      type(ESMFloc_Time), intent(in) :: StopTime
      type(ESMFloc_Time), intent(in), optional :: RefTime
      integer, intent(out), optional :: rc
! Local
      integer i

! !DESCRIPTION:
!     Initialize an {\tt ESMF\_Clock}
!
!     The arguments are:
!     \begin{description}
!     \item[clockint]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      IF ( PRESENT(TimeStep) ) clockint%TimeStep = TimeStep
      IF ( PRESENT(RefTime) )THEN
         clockint%RefTime = RefTime
      ELSE
         clockint%RefTime = StartTime
      END IF
      clockint%CurrTime = StartTime
      clockint%StartTime = StartTime
      clockint%StopTime = StopTime
      clockint%NumAlarms = 0
      clockint%AdvanceCount = 0
      ALLOCATE(clockint%AlarmList(MAX_ALARMS))
      ! TBH:  This incredible hack can be removed once ESMFloc_*Validate()
      ! TBH:  can tell if a deep ESMFloc_* was created or not.
      DO i = 1, MAX_ALARMS
        NULLIFY( clockint%AlarmList( i )%alarmint )
      ENDDO
      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockSetOLD


! !IROUTINE: ESMFloc_ClockSet - Set clock properties -- for compatibility with ESMF 2.0.1

! !INTERFACE:
      subroutine ESMFloc_ClockSet(clock, TimeStep, StartTime, StopTime, &
                               RefTime, CurrTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      type(ESMFloc_TimeInterval), intent(in), optional :: TimeStep
      type(ESMFloc_Time), intent(in), optional :: StartTime
      type(ESMFloc_Time), intent(in), optional :: StopTime
      type(ESMFloc_Time), intent(in), optional :: RefTime
      type(ESMFloc_Time), intent(in), optional :: CurrTime
      integer, intent(out), optional :: rc
! Local
      integer ierr

! !DESCRIPTION:
!     Initialize an {\tt ESMF\_Clock}
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      ierr = ESMFloc_SUCCESS
      IF ( PRESENT(TimeStep) ) THEN
        CALL ESMFloc_ClockSetTimeStep ( clock, TimeStep, rc=ierr )
      ENDIF
      IF ( PRESENT(RefTime) ) clock%clockint%RefTime = RefTime
      IF ( PRESENT(StartTime) ) clock%clockint%StartTime = StartTime
      IF ( PRESENT(StopTime) ) clock%clockint%StopTime = StopTime
      IF ( PRESENT(CurrTime) ) THEN
        CALL ESMFloc_ClockSetCurrTime(clock, CurrTime, rc=ierr)
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr

      end subroutine ESMFloc_ClockSet


! Create ESMFloc_Clock using ESMF 2.1.0+ semantics
      FUNCTION ESMFloc_ClockCreate( name, TimeStep, StartTime, StopTime, &
                                 RefTime, rc )
        ! return value
        type(ESMFloc_Clock) :: ESMFloc_ClockCreate
        ! !ARGUMENTS:
        character (len=*),       intent(in),  optional :: name
        type(ESMFloc_TimeInterval), intent(in), optional :: TimeStep
        type(ESMFloc_Time), intent(in) :: StartTime
        type(ESMFloc_Time), intent(in) :: StopTime
        type(ESMFloc_Time), intent(in), optional :: RefTime
        integer, intent(out), optional :: rc
        ! locals
        type(ESMFloc_Clock) :: clocktmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( clocktmp%clockint )
        CALL ESMFloc_ClockSetOLD( clocktmp%clockint,   &
                               TimeStep= TimeStep,  &
                               StartTime=StartTime, &
                               StopTime= StopTime,  &
                               RefTime=RefTime, rc=rc )
        ESMFloc_ClockCreate = clocktmp
      END FUNCTION ESMFloc_ClockCreate

      !
      ! Deallocate memory for ESMFloc_Clock
      !
      SUBROUTINE ESMFloc_ClockDestroy( clock, rc )

         TYPE(ESMFloc_Clock), INTENT(INOUT) :: clock
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc

         if (associated(clock%clockint)) then
            if (associated(clock%clockint%AlarmList)) deallocate(clock%clockint%AlarmList)
            deallocate(clock%clockint)
         endif

         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS

      END SUBROUTINE ESMFloc_ClockDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGet - Get clock properties -- for compatibility with ESMF 2.0.1

! tcraig added alarmCount for ccsm4, consistent with ESMF3 interface

! !INTERFACE:
      subroutine ESMFloc_ClockGet(clock, StartTime, CurrTime,       &
                               AdvanceCount, StopTime, TimeStep, &
                               PrevTime, RefTime, AlarmCount, &
                               rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out), optional :: StartTime
      type(ESMFloc_Time), intent(out), optional :: CurrTime
      type(ESMFloc_Time), intent(out), optional :: StopTime
      type(ESMFloc_Time), intent(out), optional :: PrevTime
      type(ESMFloc_Time), intent(out), optional :: RefTime
      integer(ESMFloc_KIND_I8), intent(out), optional :: AdvanceCount
      integer,         intent(out), optional :: AlarmCount
      type(ESMFloc_TimeInterval), intent(out), optional :: TimeStep
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Returns the number of times the {\tt ESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[StartTime]
!          The start time
!     \item[CurrTime]
!          The current time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[{[PrevTime]}]
!          The {\tt ESMF\_Clock}'s previous current time
!     \item[{[PrevTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[AlarmCount]}]
!          The {\tt ESMF\_Clock}'s number of valid alarms
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP
      ierr = ESMFloc_SUCCESS

      IF ( PRESENT (StartTime) ) THEN
        CALL ESMFloc_ClockGetStartTime( clock, StartTime=StartTime, rc=ierr )
      ENDIF
      IF ( PRESENT (CurrTime) ) THEN
        CALL ESMFloc_ClockGetCurrTime( clock , CurrTime, ierr )
      ENDIF
      IF ( PRESENT (StopTime) ) THEN
        CALL ESMFloc_ClockGetStopTime( clock , StopTime, ierr )
      ENDIF
      IF ( PRESENT (AdvanceCount) ) THEN
        CALL ESMFloc_ClockGetAdvanceCount(clock, AdvanceCount, ierr)
      ENDIF
      IF ( PRESENT (TimeStep) ) THEN
        CALL ESMFloc_ClockGetTimeStep(clock, TimeStep, ierr)
      ENDIF
      IF ( PRESENT (PrevTime) ) THEN
        CALL ESMFloc_ClockGetPrevTime(clock, PrevTime, ierr)
      ENDIF
      IF ( PRESENT (RefTime) ) THEN
        CALL ESMFloc_ClockGetRefTime(clock, RefTime, ierr)
      ENDIF
      IF ( PRESENT (AlarmCount) ) THEN
        CALL ESMFloc_ClockGetNumAlarms(clock, AlarmCount, ierr)
      ENDIF

      IF ( PRESENT (rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine ESMFloc_ClockGet


! !IROUTINE: ESMFloc_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine ESMFloc_ClockGetAdvanceCount(clock, AdvanceCount, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      integer(ESMFloc_KIND_I8), intent(out) :: AdvanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt ESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP

      AdvanceCount = clock%clockint%AdvanceCount

      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine ESMFloc_ClockGetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_TimeInterval), intent(out) :: TimeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.2
!EOP

      TimeStep = clock%clockint%TimeStep
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine ESMFloc_ClockSetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      type(ESMFloc_TimeInterval), intent(in) :: TimeStep
      integer, intent(out), optional      :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.2
!EOP

      clock%clockint%TimeStep = TimeStep
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine ESMFloc_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

      CurrTime = clock%clockint%CurrTime
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS
      end subroutine ESMFloc_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine ESMFloc_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      type(ESMFloc_Time), intent(in) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.3
!EOP

      clock%clockint%CurrTime = CurrTime
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine ESMFloc_ClockGetStartTime(clock, StartTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out) :: StartTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s start time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from
!     \item[StartTime]
!          The start time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StartTime = clock%clockint%StartTime
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine ESMFloc_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from
!     \item[StopTime]
!          The stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StopTime = clock%clockint%StopTime
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine ESMFloc_ClockGetRefTime(clock, RefTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out) :: RefTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s reference time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from
!     \item[RefTime]
!          The reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP
      refTime = clock%clockint%RefTime
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS
      end subroutine ESMFloc_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine ESMFloc_ClockGetPrevTime(clock, PrevTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Time), intent(out) :: PrevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from
!     \item[PrevTime]
!          The previous current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

      prevTime = Clock%clockint%CurrTime - Clock%clockint%TimeStep
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS
      end subroutine ESMFloc_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine ESMFloc_ClockGetCurrSimTime(clock, CurrSimTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_TimeInterval), intent(out) :: CurrSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from
!     \item[CurrSimTime]
!          The current simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockGetCurrSimTime not supported' )
      end subroutine ESMFloc_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine ESMFloc_ClockGetPrevSimTime(clock, PrevSimTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_TimeInterval), intent(out) :: PrevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from
!     \item[PrevSimTime]
!          The previous simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockGetPrevSimTime not supported' )
      end subroutine ESMFloc_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine ESMFloc_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      type(ESMFloc_Alarm), intent(inout) :: Alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt ESMF\_Alarm} to an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt ESMF\_Alarm} to
!     \item[Alarm]
!          The {\tt ESMF\_Alarm} to add to the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
!EOP

      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
      clock%clockint%NumAlarms = clock%clockint%NumAlarms + 1
      IF ( clock%clockint%NumAlarms > SIZE (clock%clockint%AlarmList) ) THEN
        CALL wrf_error_fatal ( 'ESMFloc_ClockAddAlarm:  too many alarms' )
      ELSE IF ( .NOT. ASSOCIATED( Alarm%alarmint ) ) THEN
        CALL wrf_error_fatal ( &
               'ESMFloc_ClockAddAlarm:  alarm not created' )
      ELSE
!TBH:  why do all this initialization here?
        IF ( Alarm%alarmint%RingTimeSet ) THEN
           Alarm%alarmint%PrevRingTime = Alarm%alarmint%RingTime - &
                                         Alarm%alarmint%RingInterval
        ELSE
           Alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
        ENDIF
        Alarm%alarmint%Ringing = .FALSE.

        ! finally, load the alarm into the list
        clock%clockint%AlarmList(clock%clockint%NumAlarms) = Alarm
      ENDIF

      end subroutine ESMFloc_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine ESMFloc_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_Alarm), pointer :: AlarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} list from
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.3
!EOP

      AlarmList => clock%clockint%AlarmList
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine ESMFloc_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt ESMF\_Alarm}s in an {\tt ESMF\_Clock}'s
!       {\tt ESMF\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt ESMF\_Alarm}s from
!     \item[NumAlarms]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!            {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.3
!EOP

      NumAlarms = clock%clockint%NumAlarms
      IF ( PRESENT(rc) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockSyncToWallClock - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMFloc_ClockSyncToWallClock(clock, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time to wall clock time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.5
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockSyncToWallClock not supported' )
      end subroutine ESMFloc_ClockSyncToWallClock

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine ESMFloc_ClockAdvance(clock, RingingAlarmList, &
                                   NumRingingAlarms, rc)

use ESMFloc_TimeMod

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(inout) :: clock
      type(ESMFloc_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
      integer, intent(out), optional :: NumRingingAlarms
      integer, intent(out), optional :: rc
! Local
      logical pred1, pred2, pred3
      integer i, n
      type(ESMFloc_Alarm) :: alarm
!
! !DESCRIPTION:
!     Advance an {\tt ESMF\_Clock}'s current time by one time step
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance
!     \item[{[RingingAlarmList]}]
!          Return a list of any ringing alarms after the time step
!     \item[{[NumRingingAlarms]}]
!          The number of ringing alarms returned
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.1
!EOP
      clock%clockint%CurrTime = clock%clockint%CurrTime + &
                                clock%clockint%TimeStep

      IF ( Present(NumRingingAlarms) ) NumRingingAlarms = 0
      clock%clockint%AdvanceCount = clock%clockint%AdvanceCount + 1
      DO i = 1, MAX_ALARMS
        alarm = clock%clockint%AlarmList(i)
        ! TBH:  This is really dangerous.  We need to be able to NULLIFY
        ! TBH:  alarmint at compile-time (F95 synax) to make this safe.
!$$$TBH:  see if F95 compile-time pointer-nullification is supported by all
!$$$TBH:  compilers we support
        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              IF ( alarm%alarmint%StopTimeSet ) THEN
                PRED1 = clock%clockint%CurrTime > alarm%alarmint%StopTime
              ENDIF
              IF ( alarm%alarmint%RingTimeSet ) THEN
                 PRED2 = ( alarm%alarmint%RingTime <= clock%clockint%CurrTime     &
                        .AND. clock%clockint%CurrTime < alarm%alarmint%RingTime + &
                              clock%clockint%TimeStep )
              ENDIF
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                 PRED3 = ( alarm%alarmint%PrevRingTime + alarm%alarmint%RingInterval <= &
                           clock%clockint%CurrTime )
              ENDIF
              IF ( ( .NOT. ( pred1 ) ) .AND. &
                   ( ( pred2 ) .OR. ( pred3 ) ) ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime + &
                                                  alarm%alarmint%RingInterval
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
              IF ( alarm%alarmint%RingTime <= clock%clockint%CurrTime ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ENDIF
            ENDIF
            IF ( alarm%alarmint%StopTimeSet ) THEN
            ENDIF
          ENDIF
        ENDIF
        clock%clockint%AlarmList(i) = alarm
      ENDDO
      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockStopTimeDisable - NOOP for compatibility with ESMF 2.1.0+

! !INTERFACE:
      subroutine ESMFloc_ClockStopTimeDisable(clock, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

      rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_ClockStopTimeDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockIsStopTime - Has the clock reached its stop time ?

! !INTERFACE:
      function ESMFloc_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMFloc_ClockIsStopTime

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return true if {\tt ESMF\_Clock} has reached its stop time, false
!     otherwise
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.6
!EOP

      if ( clock%clockint%CurrTime .GE. clock%clockint%StopTime ) THEN
        ESMFloc_ClockIsStopTime = .TRUE.
      else
        ESMFloc_ClockIsStopTime = .FALSE.
      endif
      IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS

      end function ESMFloc_ClockIsStopTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMFloc_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockRead - Restores a clock

! !INTERFACE:
      subroutine ESMFloc_ClockRead(clock, TimeStep, StartTime, StopTime, &
                                RefTime, CurrTime, PrevTime, AdvanceCount, &
                                AlarmList, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(out) :: clock
      type(ESMFloc_TimeInterval), intent(in) :: TimeStep
      type(ESMFloc_Time), intent(in) :: StartTime
      type(ESMFloc_Time), intent(in) :: StopTime
      type(ESMFloc_Time), intent(in) :: RefTime
      type(ESMFloc_Time), intent(in) :: CurrTime
      type(ESMFloc_Time), intent(in) :: PrevTime
      integer(ESMFloc_KIND_I8), intent(in) :: AdvanceCount
      type(ESMFloc_Alarm), dimension(MAX_ALARMS), intent(in) :: AlarmList
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restore an {\tt ESMF\_Clock}
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to restore
!     \item[TimeStep]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt ESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockRead not supported' )
      end subroutine ESMFloc_ClockRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_ClockWrite - Saves a clock

! !INTERFACE:
      subroutine ESMFloc_ClockWrite(clock, TimeStep, StartTime, StopTime, &
                            RefTime, CurrTime, PrevTime, AdvanceCount, &
                            AlarmList, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      type(ESMFloc_TimeInterval), intent(out) :: TimeStep
      type(ESMFloc_Time), intent(out) :: StartTime
      type(ESMFloc_Time), intent(out) :: StopTime
      type(ESMFloc_Time), intent(out) :: RefTime
      type(ESMFloc_Time), intent(out) :: CurrTime
      type(ESMFloc_Time), intent(out) :: PrevTime
      integer(ESMFloc_KIND_I8), intent(out) :: AdvanceCount
      type(ESMFloc_Alarm), dimension(MAX_ALARMS), intent(out) :: AlarmList
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Save an {\tt ESMF\_Clock}
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save
!     \item[TimeStep]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt ESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockWrite not supported' )
      end subroutine ESMFloc_ClockWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMFloc_ClockValidate(clock, opts, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on an {\tt ESMF\_Clock}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMFloc_ClockValidate not supported' )
      end subroutine ESMFloc_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_ClockPrint - Print out a Clock's properties

! !INTERFACE:
      subroutine ESMFloc_ClockPrint(clock, opts, rc)

! !ARGUMENTS:
      type(ESMFloc_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_Clock}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      type(ESMFloc_Time) :: start_time
      type(ESMFloc_Time) :: stop_time
      type(ESMFloc_Time) :: curr_time
      type(ESMFloc_Time) :: ref_time
      type(ESMFloc_TimeInterval) :: timestep

      call ESMFloc_ClockGet( clock, startTime=start_time, &
                          stoptime=stop_time, currTime=curr_time, &
                          refTime=ref_time, timeStep=timestep, rc=rc )
      print *, 'Start time: '
      call ESMFloc_TimePrint( start_time )
      print *, 'Stop time: '
      call ESMFloc_TimePrint( stop_time )
      print *, 'Reference time: '
      call ESMFloc_TimePrint( ref_time )
      print *, 'Current time: '
      call ESMFloc_TimePrint( curr_time )
      print *, 'Time step: '
      call ESMFloc_TimeIntervalPrint( timestep)
      end subroutine ESMFloc_ClockPrint

!------------------------------------------------------------------------------

      end module ESMFloc_ClockMod

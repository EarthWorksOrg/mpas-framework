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
!     ESMF Alarm Module
      module ESMFloc_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMFloc_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMFloc_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMFloc_BaseMod

      ! associated derived types
      use ESMFloc_TimeIntervalMod
      use ESMFloc_TimeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------
!     ! ESMFloc_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for ESMFloc_Alarm
      type ESMFloc_AlarmInt
        character(len=256) :: name = " "
        type(ESMFloc_TimeInterval) :: RingInterval
        type(ESMFloc_Time)  :: RingTime
        type(ESMFloc_Time)  :: PrevRingTime
        type(ESMFloc_Time)  :: StopTime
        integer :: ID
        integer :: AlarmMutex
        logical :: Ringing
        logical :: Enabled
        logical :: RingTimeSet
        logical :: RingIntervalSet
        logical :: StopTimeSet
      end type

! Actual public type:  this bit allows easy mimic of "deep" ESMFloc_AlarmCreate
! in ESMF 2.1.0+.  Note that ESMFloc_AlarmCreate is in a separate module to avoid
! cyclic dependence.
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF
!        shallow-copy-masquerading-as-reference-copy insanity.
      type ESMFloc_Alarm
        type(ESMFloc_AlarmInt), pointer :: alarmint => null()
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMFloc_Alarm
      public ESMFloc_AlarmInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMFloc_AlarmDestroy
      public ESMFloc_AlarmSet
      public ESMFloc_AlarmGet
!      public ESMFloc_AlarmGetRingInterval
!      public ESMFloc_AlarmSetRingInterval
!      public ESMFloc_AlarmGetRingTime
!      public ESMFloc_AlarmSetRingTime
!      public ESMFloc_AlarmGetPrevRingTime
!      public ESMFloc_AlarmSetPrevRingTime
!      public ESMFloc_AlarmGetStopTime
!      public ESMFloc_AlarmSetStopTime
      public ESMFloc_AlarmEnable
      public ESMFloc_AlarmDisable
      public ESMFloc_AlarmRingerOn
      public ESMFloc_AlarmRingerOff
      public ESMFloc_AlarmIsRinging
!      public ESMFloc_AlarmCheckRingTime
      public operator(==)

! Required inherited and overridden ESMFloc_Base class methods

!      public ESMFloc_AlarmRead
!      public ESMFloc_AlarmWrite
      public ESMFloc_AlarmValidate
      public ESMFloc_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private ESMFloc_AlarmEQ
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMFloc_AlarmEQ

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt ESMF\_Alarm} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmSet(alarm, name, RingTime, RingInterval, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      character(len=*), intent(in), optional :: name
      type(ESMFloc_Time), intent(in), optional :: RingTime
      type(ESMFloc_TimeInterval), intent(in), optional :: RingInterval
      type(ESMFloc_Time), intent(in), optional :: StopTime
      logical, intent(in), optional :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%RingTimeSet = .FALSE.
        alarm%alarmint%RingIntervalSet = .FALSE.
        alarm%alarmint%StopTimeSet = .FALSE.
        IF ( PRESENT( name ) ) THEN
          alarm%alarmint%name = name
        END IF
        IF ( PRESENT( RingInterval ) ) THEN
          alarm%alarmint%RingInterval = RingInterval
          alarm%alarmint%RingIntervalSet = .TRUE.
        ENDIF
        IF ( PRESENT( RingTime ) ) THEN
          alarm%alarmint%RingTime = RingTime
          alarm%alarmint%RingTimeSet = .TRUE.
        ENDIF
        IF ( PRESENT( StopTime ) ) THEN
          alarm%alarmint%StopTime = StopTime
          alarm%alarmint%StopTimeSet = .TRUE.
        ENDIF
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( Enabled ) ) THEN
          alarm%alarmint%Enabled = Enabled
        ENDIF
        IF ( PRESENT( rc ) ) THEN
          rc = ESMFloc_SUCCESS
        ENDIF
        alarm%alarmint%Ringing = .FALSE.
        alarm%alarmint%Enabled = .TRUE.
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF

      end subroutine ESMFloc_AlarmSet



! Deallocate memory for ESMFloc_Alarm
      SUBROUTINE ESMFloc_AlarmDestroy( alarm, rc )
         TYPE(ESMFloc_Alarm), INTENT(INOUT) :: alarm
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           DEALLOCATE( alarm%alarmint )
         ENDIF
         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
      END SUBROUTINE ESMFloc_AlarmDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMFloc_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      type(ESMFloc_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%RingIntervalSet )THEN
           RingInterval= alarm%alarmint%RingInterval
           IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE
           IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
        END IF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmGetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMFloc_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(out) :: alarm
      type(ESMFloc_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmSetRingInterval not supported' )
      end subroutine ESMFloc_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMFloc_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      type(ESMFloc_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      type(ESMFloc_Time) :: PrevRingTime
      type(ESMFloc_TimeInterval) :: RingInterval
      integer :: ierr

      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%RingIntervalSet )THEN
           PrevRingTime = alarm%alarmint%PrevRingTime
           call ESMFloc_AlarmGetRingInterval( alarm, RingInterval, ierr)
           IF ( PRESENT( rc ) .AND. (ierr /= ESMFloc_SUCCESS) )THEN
              rc = ierr
              return
           END IF
           RingTime = PrevRingTime + RingInterval
           IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE IF ( alarm%alarmint%RingTimeSet )THEN
           RingTime = alarm%alarmint%RingTime
           IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE
           IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
        END IF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMFloc_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(out) :: alarm
      type(ESMFloc_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmSetRingTime not supported' )
      end subroutine ESMFloc_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmGet - Get an alarm's parameters -- compatibility with ESMF 2.0.1
!
! !INTERFACE:
      subroutine ESMFloc_AlarmGet(alarm, name, RingTime, PrevRingTime, RingInterval, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      character(len=*), intent(out), optional :: name
      type(ESMFloc_Time), intent(out), optional :: RingTime
      type(ESMFloc_Time), intent(out), optional :: PrevRingTime
      type(ESMFloc_TimeInterval), intent(out), optional :: RingInterval
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get
!     \item[ringTime]
!          The ring time for a one-shot alarm or the next repeating alarm.
!     \item[ringInterval]
!          The ring interval for repeating (interval) alarms.
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      ierr = ESMFloc_SUCCESS

      IF ( PRESENT(name) ) THEN
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           name = alarm%alarmint%name
         ELSE
           ierr = ESMFloc_FAILURE
         END IF
      ENDIF
      IF ( PRESENT(PrevRingTime) ) THEN
        CALL ESMFloc_AlarmGetPrevRingTime(alarm, PrevRingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingTime) ) THEN
        CALL ESMFloc_AlarmGetRingTime(alarm, RingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingInterval) ) THEN
        CALL ESMFloc_AlarmGetRingInterval(alarm, RingInterval, rc=ierr)
      ENDIF

      IF ( PRESENT(rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine ESMFloc_AlarmGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMFloc_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      type(ESMFloc_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        PrevRingTime = alarm%alarmint%PrevRingTime
        IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMFloc_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(out) :: alarm
      type(ESMFloc_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmSetPrevRingTime not supported' )
      end subroutine ESMFloc_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine ESMFloc_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      type(ESMFloc_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmGetStopTime not supported' )
      end subroutine ESMFloc_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine ESMFloc_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(out) :: alarm
      type(ESMFloc_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmSetStopTime not supported' )
      end subroutine ESMFloc_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt ESMF\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .FALSE.
        IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmRingerOn - Turn on an alarm


! !INTERFACE:
      subroutine ESMFloc_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Turn on an {\tt ESMF\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          alarm%alarmint%Ringing = .TRUE.
          IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE
          alarm%alarmint%Ringing = .FALSE.
          IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF

      end subroutine ESMFloc_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmRingerOff - Turn off an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Ringing = .FALSE.
        IF ( alarm%alarmint%Enabled ) THEN
          IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE
          IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end subroutine ESMFloc_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function ESMFloc_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMFloc_AlarmIsRinging

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          ESMFloc_AlarmIsRinging = alarm%alarmint%Ringing
          IF ( PRESENT( rc ) ) rc = ESMFloc_SUCCESS
        ELSE
          ESMFloc_AlarmIsRinging = .FALSE.
          IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMFloc_FAILURE
      ENDIF
      end function ESMFloc_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function ESMFloc_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: ESMFloc_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(inout) :: alarm
      type(ESMFloc_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt ESMF\_Clock} to check whether to trigger
!     the {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring
!     \item[ClockCurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmCheckRingTime not supported' )
      ESMFloc_AlarmCheckRingTime = .FALSE.  ! keep compilers happy
      end function ESMFloc_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
      function ESMFloc_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: ESMFloc_AlarmEQ

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm1
      type(ESMFloc_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmEQ not supported ' )
      ESMFloc_AlarmEQ = .FALSE.       ! keep compilers happy
      end function ESMFloc_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMFloc_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmRead - restores an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmRead(alarm, RingInterval, RingTime, &
                           PrevRingTime, StopTime, Ringing, &
                           Enabled, ID, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(out) :: alarm
      type(ESMFloc_TimeInterval), intent(in) :: RingInterval
      type(ESMFloc_Time), intent(in) :: RingTime
      type(ESMFloc_Time), intent(in) :: PrevRingTime
      type(ESMFloc_Time), intent(in) :: StopTime
      logical, intent(in) :: Ringing
      logical, intent(in) :: Enabled
      integer, intent(in) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmRead not supported' )
      end subroutine ESMFloc_AlarmRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMFloc_AlarmWrite - saves an alarm

! !INTERFACE:
      subroutine ESMFloc_AlarmWrite(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      type(ESMFloc_TimeInterval), intent(out) :: RingInterval
      type(ESMFloc_Time), intent(out) :: RingTime
      type(ESMFloc_Time), intent(out) :: PrevRingTime
      type(ESMFloc_Time), intent(out) :: StopTime
      logical, intent(out) :: Ringing
      logical, intent(out) :: Enabled
      integer, intent(out) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmWrite not supported' )
      end subroutine ESMFloc_AlarmWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMFloc_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMFloc_AlarmValidate not supported' )
      end subroutine ESMFloc_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMFloc_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMFloc_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      integer :: ierr
      type(ESMFloc_Time) :: ringtime
      type(ESMFloc_Time) :: prevringtime
      type(ESMFloc_TimeInterval) :: ringinterval
      character(len=256) :: name

      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%RingTimeSet )THEN
          call ESMFloc_AlarmGet( alarm, name=name, ringtime=ringtime, &
                              prevringtime=prevringtime, rc=ierr )
          IF ( PRESENT(rc) .AND. (ierr /= ESMFloc_SUCCESS) )THEN
             rc = ierr
          END IF
          print *, 'Alarm name: ', trim(name)
          print *, 'Next ring time'
          call ESMFloc_TimePrint( ringtime )
          print *, 'Previous ring time'
          call ESMFloc_TimePrint( prevringtime )
        END IF
        IF ( alarm%alarmint%RingIntervalSet )THEN
          call ESMFloc_AlarmGet( alarm, ringinterval=ringinterval, rc=ierr )
          IF ( PRESENT(rc) .AND. (ierr /= ESMFloc_SUCCESS) )THEN
             rc = ierr
          END IF
          print *, 'Ring Interval'
          call ESMFloc_TimeIntervalPrint( ringinterval )
        END IF
      END IF

      end subroutine ESMFloc_AlarmPrint

!------------------------------------------------------------------------------

      end module ESMFloc_AlarmMod

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
!     ESMF Alarm-Clock Module
      module ESMFloc_AlarmClockMod
!
!==============================================================================
!
! This file contains the AlarmCreae method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMFloc_TimeMgr.inc"

!===============================================================================
!BOPI
!
! !MODULE: ESMFloc_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both ESMFloc_AlarmMod and ESMFloc_ClockMod.
! Separation is needed to avoid cyclic dependence.
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit ESMFloc_Alarm and ESMFloc_Clock
      use ESMFloc_AlarmMod, only : ESMFloc_Alarm, ESMFloc_AlarmSet
      use ESMFloc_ClockMod, only : ESMFloc_Clock, ESMFloc_ClockAddAlarm

      ! associated derived types
      use ESMFloc_TimeIntervalMod, only : ESMFloc_TimeInterval
      use ESMFloc_TimeMod,         only : ESMFloc_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMFloc_AlarmCreate

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains

!==============================================================================


! Create ESMFloc_Alarm using ESMF 2.1.0+ semantics
      FUNCTION ESMFloc_AlarmCreate( name, clock, RingTime, RingInterval, &
                                 StopTime, Enabled, rc )

        ! return value
        type(ESMFloc_Alarm) :: ESMFloc_AlarmCreate
        ! !ARGUMENTS:
        character(len=*), intent(in) :: name
        type(ESMFloc_Clock), intent(inout) :: clock
        type(ESMFloc_Time), intent(in), optional :: RingTime
        type(ESMFloc_TimeInterval), intent(in), optional :: RingInterval
        type(ESMFloc_Time), intent(in), optional :: StopTime
        logical, intent(in), optional :: Enabled
        integer, intent(out), optional :: rc
        ! locals
        type(ESMFloc_Alarm) :: alarmtmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( alarmtmp%alarmint )
        CALL ESMFloc_AlarmSet( alarmtmp,                  &
                            name=name,                 &
                            RingTime=RingTime,         &
                            RingInterval=RingInterval, &
                            StopTime=StopTime,         &
                            Enabled=Enabled,           &
                            rc=rc )
        CALL ESMFloc_ClockAddAlarm( clock, alarmtmp, rc )
        ESMFloc_AlarmCreate = alarmtmp
      END FUNCTION ESMFloc_AlarmCreate


!------------------------------------------------------------------------------

      end module ESMFloc_AlarmClockMod

! TBH:  This version is for use with the ESMF library embedded in the WRF
! TBH:  distribution.
MODULE ESMFloc
   USE ESMFloc_AlarmMod
   USE ESMFloc_BaseMod
   USE ESMFloc_BaseTimeMod
   USE ESMFloc_CalendarMod
   USE ESMFloc_ClockMod
   USE ESMFloc_FractionMod
   USE ESMFloc_TimeIntervalMod
   USE ESMFloc_TimeMod
   USE ESMFloc_ShrTimeMod
   USE ESMFloc_AlarmClockMod
   USE ESMFloc_Stubs   ! add new dummy interfaces and typedefs here as needed
   USE MeatMod
#include "ESMFloc_TimeMgr.inc"
   INTEGER, PARAMETER :: ESMFloc_MAX_ALARMS=MAX_ALARMS
!
END MODULE ESMFloc

   integer, parameter :: MPAS_MAX_ALARMS = 40

   integer, parameter :: MPAS_NOW = 0, &
                         MPAS_START_TIME = 1, &
                         MPAS_STOP_TIME = 2

   integer, parameter :: MPAS_FORWARD = 1, &
                         MPAS_BACKWARD = -1

   integer, parameter :: MPAS_GREGORIAN = 0, &
                         MPAS_GREGORIAN_NOLEAP = 1, &
                         MPAS_360DAY = 2

   type MPAS_Time_type
#ifdef DATMFORCEDRESTORING
      type (ESMFloc_Time) :: t
#else
      type (ESMF_Time) :: t
#endif
   end type

   type MPAS_TimeInterval_type
#ifdef DATMFORCEDRESTORING
      type (ESMFloc_TimeInterval) :: ti
#else
      type (ESMF_TimeInterval) :: ti
#endif
   end type

   type MPAS_Alarm_type
      character (len=ShortStrKIND) :: alarmID
      logical :: isRecurring
      logical :: isSet
      type (MPAS_Time_type) :: ringTime
      type (MPAS_Time_type) :: prevRingTime
      type (MPAS_TimeInterval_type) :: ringTimeInterval
      type (MPAS_Alarm_type), pointer :: next => null()
   end type
   
   type MPAS_Clock_type
      integer :: direction
      integer :: nAlarms
#ifdef DATMFORCEDRESTORING
      type (ESMFloc_Clock) :: c
#else
      type (ESMF_Clock) :: c
#endif
      type (MPAS_Alarm_type), pointer :: alarmListHead => null()
   end type


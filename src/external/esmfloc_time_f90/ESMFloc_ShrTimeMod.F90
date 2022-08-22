      module ESMFloc_ShrTimeMod
!
!==============================================================================
!
! This file contains types and methods that are shared in the hierarchy
!
!------------------------------------------------------------------------------
! INCLUDES

!==============================================================================
!BOPI
! !MODULE: ESMFloc_ShrTimeMod
!
! !DESCRIPTION:
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMFloc_BaseMod

      ! inherit from base time class
      use ESMFloc_BaseTimeMod
      use ESMFloc_CalendarMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMFloc_Time
!
!     ! F90 class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

     type ESMFloc_Time
       type(ESMFloc_BaseTime) :: basetime           ! inherit base class
       ! time instant is expressed as year + basetime
       integer :: YR
       type(ESMFloc_Calendar), pointer :: calendar => null() ! associated calendar
     end type

     public ESMFloc_Time
!==============================================================================
end module ESMFloc_ShrTimeMod

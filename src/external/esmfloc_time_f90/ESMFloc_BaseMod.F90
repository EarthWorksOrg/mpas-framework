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
! ESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module ESMFloc_BaseMod

!BOP
! !MODULE: ESMFloc_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: ESMFloc_SUCCESS = 0, ESMFloc_FAILURE = -1
      integer, parameter :: ESMFloc_MAXSTR = 128
      integer, parameter :: ESMFloc_MAXDIM = 7, &
                            ESMFloc_MAXDECOMPDIM=3, &
                            ESMFloc_MAXGRIDDIM=2

      integer, parameter :: ESMFloc_MAJOR_VERSION = 2
      integer, parameter :: ESMFloc_MINOR_VERSION = 2
      integer, parameter :: ESMFloc_REVISION      = 3
      integer, parameter :: ESMFloc_PATCHLEVEL    = 0
      character(32), parameter :: ESMFloc_VERSION_STRING = "2.2.3"

!------------------------------------------------------------------------------
!
      type ESMFloc_Status
      private
          integer :: status
      end type

      type(ESMFloc_Status), parameter :: ESMFloc_STATE_UNINIT = ESMFloc_Status(1), &
                                      ESMFloc_STATE_READY = ESMFloc_Status(2), &
                                      ESMFloc_STATE_UNALLOCATED = ESMFloc_Status(3), &
                                      ESMFloc_STATE_ALLOCATED = ESMFloc_Status(4), &
                                      ESMFloc_STATE_BUSY = ESMFloc_Status(5), &
                                      ESMFloc_STATE_INVALID = ESMFloc_Status(6)

!------------------------------------------------------------------------------
!
      type ESMFloc_Pointer
      private
          integer*8 :: ptr
      end type

      type(ESMFloc_Pointer), parameter :: ESMFloc_NULL_POINTER = ESMFloc_Pointer(0), &
                                       ESMFloc_BAD_POINTER = ESMFloc_Pointer(-1)


!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      type ESMFloc_DataType
      !!private
          integer :: dtype
      end type

      type(ESMFloc_DataType), parameter :: ESMFloc_DATA_INTEGER = ESMFloc_DataType(1), &
                                        ESMFloc_DATA_REAL = ESMFloc_DataType(2), &
                                        ESMFloc_DATA_LOGICAL = ESMFloc_DataType(3), &
                                        ESMFloc_DATA_CHARACTER = ESMFloc_DataType(4)

!------------------------------------------------------------------------------

      integer, parameter :: &
                   ESMFloc_KIND_I1 = selected_int_kind(2), &
                   ESMFloc_KIND_I2 = selected_int_kind(4), &
                   ESMFloc_KIND_I4 = selected_int_kind(9), &
                   ESMFloc_KIND_I8 = selected_int_kind(18), &
                   ESMFloc_KIND_R4 = selected_real_kind(3,25), &
                   ESMFloc_KIND_R8 = selected_real_kind(6,45), &
                   ESMFloc_KIND_C8 = selected_real_kind(3,25), &
                   ESMFloc_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

      type ESMFloc_DataValue
      private
          type(ESMFloc_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=ESMFloc_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

!------------------------------------------------------------------------------
!
      type ESMFloc_Attribute
      private
          character (len=ESMFloc_MAXSTR) :: attr_name
          type (ESMFloc_DataType) :: attr_type
          type (ESMFloc_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
      type ESMFloc_AxisIndex
!     !!private
          integer :: l
          integer :: r
          integer :: max
          integer :: decomp
          integer :: gstart
      end type

      !! TODO: same comment as above.
      type ESMFloc_MemIndex
!     !!private
          integer :: l
          integer :: r
          integer :: str
          integer :: num
      end type

!------------------------------------------------------------------------------
!
      type ESMFloc_BasePointer
      private
          integer*8 :: base_ptr
      end type

      integer :: global_count = 0

!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h
      type ESMFloc_Logical
      private
          integer :: value
      end type

      type(ESMFloc_Logical), parameter :: ESMFloc_TF_UNKNOWN  = ESMFloc_Logical(1), &
                                       ESMFloc_TF_TRUE     = ESMFloc_Logical(2), &
                                       ESMFloc_TF_FALSE    = ESMFloc_Logical(3)

!------------------------------------------------------------------------------
!
      type ESMFloc_Base
      private
         integer :: ID
         integer :: ref_count
         type (ESMFloc_Status) :: base_status
         character (len=ESMFloc_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public ESMFloc_STATE_INVALID
!      public ESMFloc_STATE_UNINIT, ESMFloc_STATE_READY, &
!             ESMFloc_STATE_UNALLOCATED, ESMFloc_STATE_ALLOCATED, &
!             ESMFloc_STATE_BUSY

      public ESMFloc_DATA_INTEGER, ESMFloc_DATA_REAL, &
             ESMFloc_DATA_LOGICAL, ESMFloc_DATA_CHARACTER

      public ESMFloc_KIND_I1, ESMFloc_KIND_I2, ESMFloc_KIND_I4, ESMFloc_KIND_I8, &
             ESMFloc_KIND_R4, ESMFloc_KIND_R8, ESMFloc_KIND_C8, ESMFloc_KIND_C16

      public ESMFloc_NULL_POINTER, ESMFloc_BAD_POINTER


      public ESMFloc_FAILURE, ESMFloc_SUCCESS
      public ESMFloc_MAXSTR
      public ESMFloc_MAXDIM, ESMFloc_MAXDECOMPDIM, ESMFloc_MAXGRIDDIM

      public ESMFloc_MAJOR_VERSION, ESMFloc_MINOR_VERSION, ESMFloc_REVISION
      public ESMFloc_VERSION_STRING

      public ESMFloc_Status, ESMFloc_Pointer, ESMFloc_DataType
      public ESMFloc_DataValue, ESMFloc_Attribute
!      public ESMFloc_MemIndex
!      public ESMFloc_BasePointer
      public ESMFloc_Base

      public ESMFloc_AxisIndex, ESMFloc_AxisIndexGet
!      public ESMFloc_AxisIndexInit
      public ESMFloc_Logical
!      public ESMFloc_TF_TRUE, ESMFloc_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public ESMFloc_BaseInit

!      public ESMFloc_BaseGetConfig
!      public ESMFloc_BaseSetConfig

!      public ESMFloc_BaseGetInstCount

!      public ESMFloc_BaseSetID
!      public ESMFloc_BaseGetID

!      public ESMFloc_BaseSetRefCount
!      public ESMFloc_BaseGetRefCount

!      public ESMFloc_BaseSetStatus
!      public ESMFloc_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public ESMFloc_Read
!      public ESMFloc_Write
!      public ESMFloc_Validate
!      public ESMFloc_Print

!  Attribute methods
      public ESMFloc_AttributeSet
      public ESMFloc_AttributeGet
      public ESMFloc_AttributeGetCount
      public ESMFloc_AttributeGetbyNumber
      public ESMFloc_AttributeGetNameList
      public ESMFloc_AttributeSetList
      public ESMFloc_AttributeGetList
      public ESMFloc_AttributeSetObjectList
      public ESMFloc_AttributeGetObjectList
      public ESMFloc_AttributeCopy
      public ESMFloc_AttributeCopyAll

!  Misc methods
      public ESMFloc_SetName
      public ESMFloc_GetName
      public ESMFloc_SetPointer
      public ESMFloc_SetNullPointer
      public ESMFloc_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public ESMFloc_StatusString, ESMFloc_DataTypeString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id$'
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare
!  them as if they were simple integers.


interface operator (.eq.)
 module procedure ESMFloc_sfeq
 module procedure ESMFloc_dteq
 module procedure ESMFloc_pteq
 module procedure ESMFloc_tfeq
 module procedure ESMFloc_aieq
end interface

interface operator (.ne.)
 module procedure ESMFloc_sfne
 module procedure ESMFloc_dtne
 module procedure ESMFloc_ptne
 module procedure ESMFloc_tfne
 module procedure ESMFloc_aine
end interface

interface assignment (=)
 module procedure ESMFloc_dtas
 module procedure ESMFloc_ptas
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two ESMFloc_Status flags to see if they're the same or not

function ESMFloc_sfeq(sf1, sf2)
 logical ESMFloc_sfeq
 type(ESMFloc_Status), intent(in) :: sf1, sf2

 ESMFloc_sfeq = (sf1%status .eq. sf2%status)
end function

function ESMFloc_sfne(sf1, sf2)
 logical ESMFloc_sfne
 type(ESMFloc_Status), intent(in) :: sf1, sf2

 ESMFloc_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two ESMFloc_DataTypes to see if they're the same or not

function ESMFloc_dteq(dt1, dt2)
 logical ESMFloc_dteq
 type(ESMFloc_DataType), intent(in) :: dt1, dt2

 ESMFloc_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function ESMFloc_dtne(dt1, dt2)
 logical ESMFloc_dtne
 type(ESMFloc_DataType), intent(in) :: dt1, dt2

 ESMFloc_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine ESMFloc_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(ESMFloc_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine


!------------------------------------------------------------------------------
! function to compare two ESMFloc_Pointers to see if they're the same or not

function ESMFloc_pteq(pt1, pt2)
 logical ESMFloc_pteq
 type(ESMFloc_Pointer), intent(in) :: pt1, pt2

 ESMFloc_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function ESMFloc_ptne(pt1, pt2)
 logical ESMFloc_ptne
 type(ESMFloc_Pointer), intent(in) :: pt1, pt2

 ESMFloc_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine ESMFloc_ptas(ptval, intval)
 type(ESMFloc_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMFloc_Logicals to see if they're the same or not
! also need assignment to real f90 logical?

function ESMFloc_tfeq(tf1, tf2)
 logical ESMFloc_tfeq
 type(ESMFloc_Logical), intent(in) :: tf1, tf2

 ESMFloc_tfeq = (tf1%value .eq. tf2%value)
end function

function ESMFloc_tfne(tf1, tf2)
 logical ESMFloc_tfne
 type(ESMFloc_Logical), intent(in) :: tf1, tf2

 ESMFloc_tfne = (tf1%value .ne. tf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMFloc_AxisIndex to see if they're the same or not

function ESMFloc_aieq(ai1, ai2)
 logical ESMFloc_aieq
 type(ESMFloc_AxisIndex), intent(in) :: ai1, ai2

 ESMFloc_aieq = ((ai1%l .eq. ai2%l) .and. &
              (ai1%r .eq. ai2%r) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%decomp .eq. ai2%decomp) .and. &
              (ai1%gstart .eq. ai2%gstart))

end function

function ESMFloc_aine(ai1, ai2)
 logical ESMFloc_aine
 type(ESMFloc_AxisIndex), intent(in) :: ai1, ai2

 ESMFloc_aine = ((ai1%l .ne. ai2%l) .or. &
              (ai1%r .ne. ai2%r) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%decomp .ne. ai2%decomp) .or. &
              (ai1%gstart .ne. ai2%gstart))

end function

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_BaseInit - initialize a Base object
!
! !INTERFACE:
      subroutine ESMFloc_BaseInit(base, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base) :: base
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMFloc_FAILURE
      endif

      global_count = global_count + 1
      base%ID = global_count
      base%ref_count = 1
      base%base_status = ESMFloc_STATE_READY
      base%name = "undefined"

      if (rcpresent) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_BaseInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_SetName - set the name of this object
!
! !INTERFACE:
      subroutine ESMFloc_SetName(anytype, name, namespace, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base) :: anytype
      character (len = *), intent(in), optional :: name
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt ESMFloc_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present
      character (len = ESMFloc_MAXSTR) :: ournamespace ! Namespace if not given
      character (len = ESMFloc_MAXSTR) :: defaultname  ! Name if not given
      integer, save :: seqnum = 0       ! HACK - generate uniq names
                                        ! but not coordinated across procs

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMFloc_FAILURE
      endif

!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!
!     ! Construct a default namespace if one is not given
      if((.not. present(namespace)) .or. (namespace .eq. "")) then
          ournamespace = "global"
      else
          ournamespace = namespace
      endif
!     ! Construct a default name if one is not given
      if((.not. present(name)) .or. (name .eq. "")) then

          write(defaultname, 20) trim(ournamespace), seqnum
20        format(A,I3.3)
          seqnum = seqnum + 1
          anytype%name = defaultname
      else
          anytype%name = name
      endif

      if (rcpresent) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_GetName - get the name of this object
!
! !INTERFACE:
      subroutine ESMFloc_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
      subroutine ESMFloc_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMFloc_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine ESMFloc_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFloc_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype           ! any ESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(ESMFloc_DataType), intent(out) :: type             ! all possible data types
      type(ESMFloc_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine ESMFloc_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine ESMFloc_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeGetbyNumber - get an ESMF object's attribute by num ber
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMFloc_DataType), intent(out) :: type               ! all possible data types
      type(ESMFloc_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS:

      end subroutine ESMFloc_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMFloc_AttributeGetNameList - get an ESMF object's attribute name list
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine ESMFloc_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeSetList - set an ESMF object's attributes
!
! !INTERFACE:
      subroutine ESMFloc_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMFloc_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine ESMFloc_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeGetList - get an ESMF object's attributes
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMFloc_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMFloc_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine ESMFloc_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeSetObjectList - set an attribute on multiple ESMF objects
!
! !INTERFACE:
      subroutine ESMFloc_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMFloc_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMFloc_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  ESMFloc_AttributeGetObjectList - get an attribute from multiple ESMF objects
!
! !INTERFACE:
      subroutine ESMFloc_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMFloc_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMFloc_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMFloc_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMFloc_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine ESMFloc_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMFloc_Base), intent(in) :: source              ! any ESMF type
      type(ESMFloc_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMFloc_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine ESMFloc_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Base), intent(in) :: source              ! any ESMF type
      type(ESMFloc_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMFloc_AttributeCopyAll

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine ESMFloc_AxisIndexInit(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(ESMFloc_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      ai%l = l
      ai%r = r
      ai%max = max
      ai%decomp = decomp
      ai%gstart = gstart

      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_AxisIndexInit

!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine ESMFloc_AxisIndexGet(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(ESMFloc_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      if (present(l)) l = ai%l
      if (present(r)) r = ai%r
      if (present(max)) max = ai%max
      if (present(decomp)) decomp = ai%decomp
      if (present(gstart)) gstart = ai%gstart

      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMFloc_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMFloc_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Pointer) :: ptype
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMFloc_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMFloc_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Pointer) :: ptype
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_SetNullPointer
!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMFloc_GetPointer - get an opaque value
!
! !INTERFACE:
      function ESMFloc_GetPointer(ptype, rc)
!
! !RETURN VALUE:
      integer*8 :: ESMFloc_GetPointer

! !ARGUMENTS:
      type(ESMFloc_Pointer), intent(in) :: ptype
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ESMFloc_GetPointer = ptype%ptr
      if (present(rc)) rc = ESMFloc_SUCCESS

      end function ESMFloc_GetPointer

!-------------------------------------------------------------------------
! misc print routines
!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMFloc_StatusString - Return status as a string
!
! !INTERFACE:
      subroutine ESMFloc_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMFloc_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (status .eq. ESMFloc_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. ESMFloc_STATE_READY) string = "Ready"
      if (status .eq. ESMFloc_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMFloc_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. ESMFloc_STATE_BUSY) string = "Busy"
      if (status .eq. ESMFloc_STATE_INVALID) string = "Invalid"

      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_StatusString

!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMFloc_DataTypeString - Return DataType as a string
!
! !INTERFACE:
      subroutine ESMFloc_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(ESMFloc_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (datatype .eq. ESMFloc_DATA_INTEGER) string = "Integer"
      if (datatype .eq. ESMFloc_DATA_REAL) string = "Real"
      if (datatype .eq. ESMFloc_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. ESMFloc_DATA_CHARACTER) string = "Character"

      if (present(rc)) rc = ESMFloc_SUCCESS

      end subroutine ESMFloc_DataTypeString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMFloc_BaseMod

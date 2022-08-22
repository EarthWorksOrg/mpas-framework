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
!     ESMF BaseTime Module
      module ESMFloc_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

#include "ESMFloc_TimeMgr.inc"
!
!===============================================================================
!BOPI
! !MODULE: ESMFloc_BaseTimeMod - Base ESMF time definition
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      use ESMFloc_BaseMod    ! ESMF Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMFloc_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type ESMFloc_BaseTime
        integer(ESMFloc_KIND_I8) :: S   ! whole seconds
        integer(ESMFloc_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(ESMFloc_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMFloc_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators
      public seccmp
      public normalize_basetime
      public operator(+)
      private ESMFloc_BaseTimeSum
      public operator(-)
      private ESMFloc_BaseTimeDifference
      public operator(/)
      private ESMFloc_BaseTimeQuotI
      private ESMFloc_BaseTimeQuotI8
      public operator(.EQ.)
      private ESMFloc_BaseTimeEQ
      public operator(.NE.)
      private ESMFloc_BaseTimeNE
      public operator(.LT.)
      private ESMFloc_BaseTimeLT
      public operator(.GT.)
      private ESMFloc_BaseTimeGT
      public operator(.LE.)
      private ESMFloc_BaseTimeLE
      public operator(.GE.)
      private ESMFloc_BaseTimeGE

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
      interface operator(+)
        module procedure ESMFloc_BaseTimeSum
      end interface
      interface operator(-)
        module procedure ESMFloc_BaseTimeDifference
      end interface
      interface operator(/)
        module procedure ESMFloc_BaseTimeQuotI,ESMFloc_BaseTimeQuotI8
      end interface
      interface operator(.EQ.)
        module procedure ESMFloc_BaseTimeEQ
      end interface
      interface operator(.NE.)
        module procedure ESMFloc_BaseTimeNE
      end interface
      interface operator(.LT.)
        module procedure ESMFloc_BaseTimeLT
      end interface
      interface operator(.GT.)
        module procedure ESMFloc_BaseTimeGT
      end interface
      interface operator(.LE.)
        module procedure ESMFloc_BaseTimeLE
      end interface
      interface operator(.GE.)
        module procedure ESMFloc_BaseTimeGE
      end interface


!==============================================================================

      contains

!==============================================================================

SUBROUTINE normalize_basetime( basetime )
  ! Factor so abs(Sn) < Sd and ensure that signs of S and Sn match.
  ! Also, enforce consistency.
  ! YR and MM fields are ignored.
  IMPLICIT NONE
  TYPE(ESMFloc_BaseTime), INTENT(INOUT) :: basetime

  !PRINT *,'DEBUG:  BEGIN normalize_basetime()'
  ! Consistency check...
  IF ( basetime%Sd < 0 ) THEN
    CALL wrf_error_fatal( &
      'normalize_basetime:  denominator of seconds cannot be negative' )
  ENDIF
  IF ( ( basetime%Sd == 0 ) .AND. ( basetime%Sn .NE. 0 ) ) THEN
    CALL wrf_error_fatal( &
      'normalize_basetime:  denominator of seconds cannot be zero when numerator is non-zero' )
  ENDIF
  ! factor so abs(Sn) < Sd
  IF ( basetime%Sd > 0 ) THEN
    IF ( ABS( basetime%Sn ) .GE. basetime%Sd ) THEN
      !PRINT *,'DEBUG:  normalize_basetime() A1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S + ( basetime%Sn / basetime%Sd )
      basetime%Sn = mod( basetime%Sn, basetime%Sd )
      !PRINT *,'DEBUG:  normalize_basetime() A2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
    ! change sign of Sn if it does not match S
    IF ( ( basetime%S > 0 ) .AND. ( basetime%Sn < 0 ) ) THEN
      !PRINT *,'DEBUG:  normalize_basetime() B1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S - 1_ESMFloc_KIND_I8
      basetime%Sn = basetime%Sn + basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() B2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
    IF ( ( basetime%S < 0 ) .AND. ( basetime%Sn > 0 ) ) THEN
      !PRINT *,'DEBUG:  normalize_basetime() C1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S + 1_ESMFloc_KIND_I8
      basetime%Sn = basetime%Sn - basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() C2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
  ENDIF
  !PRINT *,'DEBUG:  END normalize_basetime()'
END SUBROUTINE normalize_basetime

!==============================================================================

! Add two basetimes
      FUNCTION ESMFloc_BaseTimeSum( basetime1, basetime2 )
        TYPE(ESMFloc_BaseTime) :: ESMFloc_BaseTimeSum
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        ! locals
        INTEGER (ESMFloc_KIND_I8) :: Sn1, Sd1, Sn2, Sd2, lcd
!  PRINT *,'DEBUG:  BEGIN ESMFloc_BaseTimeSum()'
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
        ESMFloc_BaseTimeSum   = basetime1
        ESMFloc_BaseTimeSum%S = ESMFloc_BaseTimeSum%S + basetime2%S
        Sn1 = basetime1%Sn
        Sd1 = basetime1%Sd
        Sn2 = basetime2%Sn
        Sd2 = basetime2%Sd
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  Sd2 = ',Sd2
        IF      ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  no fractions'
          ESMFloc_BaseTimeSum%Sn = 0
          ESMFloc_BaseTimeSum%Sd = 0
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
          ESMFloc_BaseTimeSum%Sn = Sn1
          ESMFloc_BaseTimeSum%Sd = Sd1
        ELSE IF ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          ESMFloc_BaseTimeSum%Sn = Sn2
          ESMFloc_BaseTimeSum%Sd = Sd2
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CALL compute_lcd( Sd1 , Sd2 , lcd )
          ESMFloc_BaseTimeSum%Sd = lcd
          ESMFloc_BaseTimeSum%Sn = (Sn1 * lcd / Sd1) + (Sn2 * lcd / Sd2)
        ENDIF
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  ESMFloc_BaseTimeSum%S = ',ESMFloc_BaseTimeSum%S
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  ESMFloc_BaseTimeSum%Sn = ',ESMFloc_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  ESMFloc_BaseTimeSum():  ESMFloc_BaseTimeSum%Sd = ',ESMFloc_BaseTimeSum%Sd
        CALL normalize_basetime( ESMFloc_BaseTimeSum )
!  PRINT *,'DEBUG:  END ESMFloc_BaseTimeSum()'
      END FUNCTION ESMFloc_BaseTimeSum


! Subtract two basetimes
      FUNCTION ESMFloc_BaseTimeDifference( basetime1, basetime2 )
        TYPE(ESMFloc_BaseTime) :: ESMFloc_BaseTimeDifference
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        ! locals
        TYPE(ESMFloc_BaseTime) :: neg2

        neg2%S  = -basetime2%S
        neg2%Sn = -basetime2%Sn
        neg2%Sd =  basetime2%Sd

        ESMFloc_BaseTimeDifference = basetime1 + neg2

      END FUNCTION ESMFloc_BaseTimeDifference


! Divide basetime by 8-byte integer
      FUNCTION ESMFloc_BaseTimeQuotI8( basetime, divisor )
        TYPE(ESMFloc_BaseTime) :: ESMFloc_BaseTimeQuotI8
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime
        INTEGER(ESMFloc_KIND_I8), INTENT(IN) :: divisor
        ! locals
        INTEGER(ESMFloc_KIND_I8) :: d, n, dinit

!PRINT *,'DEBUG ESMFloc_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG ESMFloc_BaseTimeQuotI8() A:  divisor = ', divisor
        IF ( divisor == 0_ESMFloc_KIND_I8 ) THEN
          CALL wrf_error_fatal( 'ESMFloc_BaseTimeQuotI8:  divide by zero' )
        ENDIF

!$$$ move to default constructor
        ESMFloc_BaseTimeQuotI8%S  = 0
        ESMFloc_BaseTimeQuotI8%Sn = 0
        ESMFloc_BaseTimeQuotI8%Sd = 0

        ! convert to a fraction and divide by multipling the denonminator by
        ! the divisor
        IF ( basetime%Sd == 0 ) THEN
          dinit = 1_ESMFloc_KIND_I8
        ELSE
          dinit = basetime%Sd
        ENDIF
        n = basetime%S * dinit + basetime%Sn
        d = dinit * divisor
!PRINT *,'DEBUG ESMFloc_BaseTimeQuotI8() B:  n,d = ',n,d
        CALL simplify( n, d, ESMFloc_BaseTimeQuotI8%Sn, ESMFloc_BaseTimeQuotI8%Sd )
!PRINT *,'DEBUG ESMFloc_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  ESMFloc_BaseTimeQuotI8%S,ESMFloc_BaseTimeQuotI8%Sn,ESMFloc_BaseTimeQuotI8%Sd
        CALL normalize_basetime( ESMFloc_BaseTimeQuotI8 )
!PRINT *,'DEBUG ESMFloc_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  ESMFloc_BaseTimeQuotI8%S,ESMFloc_BaseTimeQuotI8%Sn,ESMFloc_BaseTimeQuotI8%Sd
      END FUNCTION ESMFloc_BaseTimeQuotI8

! Divide basetime by integer
      FUNCTION ESMFloc_BaseTimeQuotI( basetime, divisor )
        TYPE(ESMFloc_BaseTime) :: ESMFloc_BaseTimeQuotI
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime
        INTEGER, INTENT(IN) :: divisor
        IF ( divisor == 0 ) THEN
          CALL wrf_error_fatal( 'ESMFloc_BaseTimeQuotI:  divide by zero' )
        ENDIF
        ESMFloc_BaseTimeQuotI = basetime / INT( divisor, ESMFloc_KIND_I8 )
      END FUNCTION ESMFloc_BaseTimeQuotI


! .EQ. for two basetimes
      FUNCTION ESMFloc_BaseTimeEQ( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeEQ
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeEQ = ( retval .EQ. 0 )
      END FUNCTION ESMFloc_BaseTimeEQ


! .NE. for two basetimes
      FUNCTION ESMFloc_BaseTimeNE( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeNE
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeNE = ( retval .NE. 0 )
      END FUNCTION ESMFloc_BaseTimeNE


! .LT. for two basetimes
      FUNCTION ESMFloc_BaseTimeLT( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeLT
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeLT = ( retval .LT. 0 )
      END FUNCTION ESMFloc_BaseTimeLT


! .GT. for two basetimes
      FUNCTION ESMFloc_BaseTimeGT( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeGT
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeGT = ( retval .GT. 0 )
      END FUNCTION ESMFloc_BaseTimeGT


! .LE. for two basetimes
      FUNCTION ESMFloc_BaseTimeLE( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeLE
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeLE = ( retval .LE. 0 )
      END FUNCTION ESMFloc_BaseTimeLE


! .GE. for two basetimes
      FUNCTION ESMFloc_BaseTimeGE( basetime1, basetime2 )
        LOGICAL :: ESMFloc_BaseTimeGE
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMFloc_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMFloc_BaseTimeGE = ( retval .GE. 0 )
      END FUNCTION ESMFloc_BaseTimeGE

!==============================================================================

SUBROUTINE compute_lcd( e1, e2, lcd )
      IMPLICIT NONE
      INTEGER(ESMFloc_KIND_I8), INTENT(IN) :: e1, e2
      INTEGER(ESMFloc_KIND_I8), INTENT(OUT) :: lcd
      INTEGER, PARAMETER ::  nprimes = 9
      INTEGER(ESMFloc_KIND_I8), DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
      INTEGER i
      INTEGER(ESMFloc_KIND_I8) d1, d2, p

      d1 = e1 ; d2 = e2
      IF ( d1 .EQ. 0 .AND. d2 .EQ. 0 ) THEN ; lcd = 1 ; RETURN ; ENDIF
      IF ( d1 .EQ. 0 ) d1 = d2
      IF ( d2 .EQ. 0 ) d2 = d1
      IF ( d1 .EQ. d2 ) THEN ; lcd = d1 ; RETURN ; ENDIF
      lcd = d1 * d2
      DO i = 1, nprimes
        p = primes(i)
        DO WHILE (lcd/p .NE. 0 .AND. &
          mod(lcd/p,d1) .EQ. 0 .AND. mod(lcd/p,d2) .EQ. 0)
          lcd = lcd / p
        END DO
      ENDDO
END SUBROUTINE compute_lcd

!==============================================================================

SUBROUTINE simplify( ni, di, no, do )
    IMPLICIT NONE
    INTEGER(ESMFloc_KIND_I8), INTENT(IN)  :: ni, di
    INTEGER(ESMFloc_KIND_I8), INTENT(OUT) :: no, do
    INTEGER, PARAMETER ::  nprimes = 9
    INTEGER(ESMFloc_KIND_I8), DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
    INTEGER(ESMFloc_KIND_I8) :: pr, d, n
    INTEGER :: np
    LOGICAL keepgoing
    IF ( ni .EQ. 0 ) THEN
      do = 1
      no = 0
      RETURN
    ENDIF
    IF ( mod( di , ni ) .EQ. 0 ) THEN
      do = di / ni
      no = 1
      RETURN
    ENDIF
    d = di
    n = ni
    DO np = 1, nprimes
      pr = primes(np)
      keepgoing = .TRUE.
      DO WHILE ( keepgoing )
        keepgoing = .FALSE.
        IF ( d/pr .NE. 0 .AND. n/pr .NE. 0 .AND. MOD(d,pr) .EQ. 0 .AND. MOD(n,pr) .EQ. 0 ) THEN
          d = d / pr
          n = n / pr
          keepgoing = .TRUE.
        ENDIF
      ENDDO
    ENDDO
    do = d
    no = n
    RETURN
END SUBROUTINE simplify

!==============================================================================

! spaceship operator for seconds + Sn/Sd
SUBROUTINE seccmp(S1, Sn1, Sd1, S2, Sn2, Sd2, retval )
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: retval
!
! !ARGUMENTS:
  INTEGER(ESMFloc_KIND_I8), INTENT(IN) :: S1, Sn1, Sd1
  INTEGER(ESMFloc_KIND_I8), INTENT(IN) :: S2, Sn2, Sd2
! local
  INTEGER(ESMFloc_KIND_I8) :: lcd, n1, n2

  n1 = Sn1
  n2 = Sn2
  if ( ( n1 .ne. 0 ) .or. ( n2 .ne. 0 ) ) then
    CALL compute_lcd( Sd1, Sd2, lcd )
    if ( Sd1 .ne. 0 ) n1 = n1 * ( lcd / Sd1 )
    if ( Sd2 .ne. 0 ) n2 = n2 * ( lcd / Sd2 )
  endif

  if ( S1 .GT. S2 ) retval = 1
  if ( S1 .LT. S2 ) retval = -1
  IF ( S1 .EQ. S2 ) THEN
    IF (n1 .GT. n2) retval = 1
    IF (n1 .LT. n2) retval = -1
    IF (n1 .EQ. n2) retval = 0
  ENDIF
END SUBROUTINE seccmp

!==============================================================================

      end module ESMFloc_BaseTimeMod

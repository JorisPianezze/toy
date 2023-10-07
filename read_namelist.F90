! #########################################################
SUBROUTINE read_namelist(output_unit,IL_NB_TIME_STEPS, DELTA_T, &
                         DATA_FILENAME, &
                         CTYPE_FCT, VALUE, CNAME_FILE, &
                         NB_RECV_FIELDS, CRCVFIELDS, &
                         NB_SEND_FIELDS, CSNDFIELDS )
! #########################################################

! ---------------------------------------------------------
!
!        Read namelist TOYNAMELIST.nam
!
! --------------------------------------------------------

IMPLICIT NONE

INTEGER, INTENT(IN) :: output_unit
!
!-- NAM_OASIS
INTEGER, INTENT(OUT) :: IL_NB_TIME_STEPS ! number of time steps
INTEGER, INTENT(OUT) :: DELTA_T          ! time step
CHARACTER(LEN=30), INTENT(OUT) :: DATA_FILENAME
!
!-- NAM_FCT_SEND
CHARACTER(LEN=5), INTENT(OUT) :: CTYPE_FCT
REAL, INTENT(OUT) :: VALUE
CHARACTER(LEN=30), INTENT(OUT) :: CNAME_FILE
!
!-- NAM_RECV_FIELDS
INTEGER, INTENT(OUT) :: NB_RECV_FIELDS
INTEGER, PARAMETER :: NB_RECV_FIELDS_MAX=10
CHARACTER(LEN=8), DIMENSION(NB_RECV_FIELDS_MAX), INTENT(OUT) :: CRCVFIELDS
!
!-- NAM_SEND_FIELDS
INTEGER, INTENT(OUT) :: NB_SEND_FIELDS
INTEGER, PARAMETER :: NB_SEND_FIELDS_MAX=10
CHARACTER(LEN=8), DIMENSION(NB_SEND_FIELDS_MAX), INTENT(OUT) :: CSNDFIELDS
!
INTEGER :: ind_fields
!
NAMELIST /NAM_OASIS/ IL_NB_TIME_STEPS, DELTA_T, DATA_FILENAME
NAMELIST /NAM_FCT_SEND/ CTYPE_FCT, VALUE, CNAME_FILE
NAMELIST /NAM_RECV_FIELDS/ NB_RECV_FIELDS, CRCVFIELDS
NAMELIST /NAM_SEND_FIELDS/ NB_SEND_FIELDS, CSNDFIELDS
!
OPEN(UNIT=10,FILE='TOYNAMELIST.nam', ACTION='READ')
READ(UNIT=10,NML=NAM_OASIS)
READ(UNIT=10,NML=NAM_FCT_SEND)
READ(UNIT=10,NML=NAM_RECV_FIELDS)
READ(UNIT=10,NML=NAM_SEND_FIELDS)
CLOSE(UNIT=10)
!
! VERIFICATION
WRITE(output_unit,*) 'IL_NB_TIME_STEPS=', IL_NB_TIME_STEPS
WRITE(output_unit,*) 'DELTA_T=', DELTA_T
WRITE(output_unit,*) 'DATA_FILENAME=', DATA_FILENAME
!
WRITE(output_unit,*) 'CTYPE_FCT=', CTYPE_FCT
WRITE(output_unit,*) 'VALUE=', VALUE
WRITE(output_unit,*) 'CNAME_FILE=', CNAME_FILE
!
WRITE(output_unit,*) 'NB_RECV_FIELDS=', NB_RECV_FIELDS
DO ind_fields=1, NB_RECV_FIELDS
  WRITE(output_unit,*) 'CRCVFIELDS(',ind_fields,')=', CRCVFIELDS(ind_fields)
END DO
!
WRITE(output_unit,*) 'NB_SEND_FIELDS=', NB_SEND_FIELDS
DO ind_fields=1, NB_SEND_FIELDS
  WRITE(output_unit,*) 'CSNDFIELDS(',ind_fields,')=', CSNDFIELDS(ind_fields)
END DO

! #########################################################
END SUBROUTINE read_namelist
! #########################################################

! #########################################################
SUBROUTINE read_namelist(output_unit,ntime_steps, time_step, &
                         grid_file_name, type_send,          &
                         value_send, forcing_file_name,      &
                         nrecv_fields, name_recv_fields,     &
                         nsend_fields, name_send_fields      )
! #########################################################

! ---------------------------------------------------------
!
!        Read namelist TOYNAMELIST.nam
!
! --------------------------------------------------------

USE parameters

IMPLICIT NONE

INTEGER,                       INTENT(IN)  :: output_unit
INTEGER,                       INTENT(OUT) :: ntime_steps
INTEGER,                       INTENT(OUT) :: time_step
CHARACTER(LEN=len_file_names), INTENT(OUT) :: grid_file_name
CHARACTER(LEN=len_type_send),  INTENT(OUT) :: type_send
REAL,                          INTENT(OUT) :: value_send
CHARACTER(LEN=len_file_names), INTENT(OUT) :: forcing_file_name

INTEGER, INTENT(OUT) :: nrecv_fields
INTEGER, INTENT(OUT) :: nsend_fields

CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_recv_fields), INTENT(OUT) :: name_recv_fields
CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_send_fields), INTENT(OUT) :: name_send_fields

INTEGER :: ind_field

NAMELIST /nam_grid/        ntime_steps, time_step, grid_file_name
NAMELIST /nam_fct_send/    type_send, value_send, forcing_file_name
NAMELIST /nam_recv_fields/ nrecv_fields, name_recv_fields
NAMELIST /nam_send_fields/ nsend_fields, name_send_fields

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open and read TOYNAMELIST.nam
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
OPEN(UNIT=10,FILE='TOYNAMELIST.nam', ACTION='READ')
READ(UNIT=10,NML=nam_grid)
READ(UNIT=10,NML=nam_fct_send)
READ(UNIT=10,NML=nam_recv_fields)
READ(UNIT=10,NML=nam_send_fields)
CLOSE(UNIT=10)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Write TOYNAMELIST.nam values
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WRITE(output_unit,*) ' &nam_grid :'
WRITE(output_unit,*) '  ntime_steps    = ', ntime_steps
WRITE(output_unit,*) '  time_step      = ', time_step
WRITE(output_unit,*) '  grid_file_name = ', grid_file_name

WRITE(output_unit,*) ' &nam_fct_send :'
WRITE(output_unit,*) '  type_send=', type_send
WRITE(output_unit,*) '  value_send=', value_send
WRITE(output_unit,*) '  forcing_file_name=', forcing_file_name

WRITE(output_unit,*) ' &nam_recv_fields :'
WRITE(output_unit,*) '  nrecv_fields=', nrecv_fields
DO ind_field=1, nrecv_fields
  WRITE(output_unit,*) '  recv_fields(',ind_field,')=', name_recv_fields(ind_field)
END DO

WRITE(output_unit,*) ' &nam_send_fields :'
WRITE(output_unit,*) '  nsend_fields=', nsend_fields
DO ind_field=1, nsend_fields
  WRITE(output_unit,*) '  name_send_fields(',ind_field,')=', name_send_fields(ind_field)
END DO

! #########################################################
END SUBROUTINE read_namelist
! #########################################################

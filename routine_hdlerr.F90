! #########################################################
SUBROUTINE handle_netcdf_errors(istatus, current_line, current_file)
! #########################################################

! ---------------------------------------------------------
!
!        Check error messages for NetCDF call
!
! --------------------------------------------------------

USE netcdf

IMPLICIT NONE

INTEGER,          INTENT(IN) :: istatus, current_line
CHARACTER(LEN=*), INTENT(IN) :: current_file

IF (istatus .NE. nf90_noerr) THEN
  WRITE (*,*) 'NetCDF problem at line', current_line, 'in ', current_file, ' -> CALL ABORT'
  CALL ABORT
ENDIF

RETURN

! #########################################################
END SUBROUTINE handle_netcdf_errors
! #########################################################

! #########################################################
SUBROUTINE hdlerr(istatus, current_line, current_file)
! #########################################################

! ---------------------------------------------------------
!
!        Check error messages from NetCDF call
!
! --------------------------------------------------------

USE NETCDF
IMPLICIT NONE

INCLUDE 'mpif.h'

INTEGER,          INTENT(IN) :: istatus, current_line
CHARACTER(LEN=*), INTENT(IN) :: current_file
INTEGER                      :: ierror

IF (istatus .NE. NF90_NOERR) THEN
  WRITE (*,*) 'NetCDF problem at line', current_line, 'in ', current_file, ' -> MPI_Abort !'
  CALL MPI_Abort ( MPI_COMM_WORLD, 1, ierror )
ENDIF

RETURN

! #########################################################
END SUBROUTINE hdlerr
! #########################################################

! #########################################################
SUBROUTINE read_forcing (output_unit,nsend_fields,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end,ind_time, &
                         forcing_file_name,name_send_fields, &
                         nlon_local, nlat_local, send_fields)
! #########################################################

USE netcdf
USE parameters

IMPLICIT NONE

INTEGER,                                                          INTENT(IN)  :: output_unit, nsend_fields
INTEGER,                                                          INTENT(IN)  :: ind_lon_beg,ind_lon_end
INTEGER,                                                          INTENT(IN)  :: ind_lat_beg,ind_lat_end
INTEGER,                                                          INTENT(IN)  :: ind_time
INTEGER,                                                          INTENT(IN)  :: nlon_local, nlat_local
CHARACTER(len=len_file_names),                                    INTENT(IN)  :: forcing_file_name
CHARACTER(len=len_exchanged_fields), DIMENSION(nmax_send_fields), INTENT(IN)  :: name_send_fields
REAL, DIMENSION(nlon_local,nlat_local,nmax_send_fields),          INTENT(OUT) :: send_fields

INTEGER, DIMENSION(3) :: ind_start, ncount
INTEGER               :: ind_send_field
INTEGER               :: file_id
INTEGER               :: send_field_id

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Initialisation
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ind_start(1) = ind_lon_beg
ind_start(2) = ind_lat_beg
ind_start(3) = ind_time+1

ncount(1)    = nlon_local
ncount(2)    = nlat_local
ncount(3)    = 1

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_open(forcing_file_name, nf90_nowrite, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read fields to send
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DO ind_send_field=1, nsend_fields

  CALL handle_netcdf_errors( nf90_inq_varid(file_id,name_send_fields(ind_send_field),send_field_id), &
                             __LINE__, __FILE__ )
  CALL handle_netcdf_errors( nf90_get_var(file_id,send_field_id,send_fields(:,:,ind_send_field),&
                                          start=ind_start,count=ncount), __LINE__, __FILE__ )

ENDDO

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_close(file_id), __LINE__, __FILE__ )

! #########################################################
END SUBROUTINE read_forcing
! #########################################################

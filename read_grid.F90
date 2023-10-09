! #########################################################
SUBROUTINE read_grid (grid_file_name,nlon,nlat,ncorners,  &
                      lon,lat,clo,cla,srf,mask,output_unit)
! #########################################################

! ---------------------------------------------------------
!
!        Read grid (lon, lat, clon, clat, srf, mask)
!
! --------------------------------------------------------

USE netcdf
USE parameters

IMPLICIT NONE

CHARACTER(LEN=len_file_names),          INTENT(IN   ) :: grid_file_name
INTEGER,                                INTENT(IN   ) :: nlon, nlat, ncorners
INTEGER,                                INTENT(IN   ) :: output_unit

REAL,    DIMENSION(nlon,nlat),          INTENT(INOUT) :: lon, lat, srf
REAL,    DIMENSION(nlon,nlat,ncorners), INTENT(INOUT) :: clo, cla
INTEGER, DIMENSION(nlon,nlat),          INTENT(INOUT) :: mask

INTEGER :: file_id, lon_id, lat_id, clon_id, clat_id, srf_id, mask_id

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_open(grid_file_name, NF90_NOWRITE, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid id.
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_inq_varid(file_id,  'lon',  lon_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_varid(file_id,  'lat',  lat_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_varid(file_id,  'clo', clon_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_varid(file_id,  'cla', clat_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_varid(file_id,  'srf',  srf_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_varid(file_id, 'mask', mask_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid data
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_get_var(file_id,   lon_id, lon), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid longitudes reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id,   lat_id, lat), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid latitudes reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id,  clon_id, clo), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid longitude corners reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id,  clat_id, cla), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid latitude corners reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id,   srf_id, srf), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid surfaces reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, mask_id, mask), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid mask reading done'

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_close(file_id), __LINE__, __FILE__ )

! #########################################################
END SUBROUTINE read_grid
! #########################################################

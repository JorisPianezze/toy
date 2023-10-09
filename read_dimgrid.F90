! #########################################################
SUBROUTINE read_dimgrid (grid_file_name,nlon,nlat,output_unit)
! #########################################################

! ---------------------------------------------------------
!
!        Read grid dimensions (nlon, nlat)
!
! --------------------------------------------------------

USE netcdf
USE parameters

IMPLICIT NONE

CHARACTER(len=len_file_names), INTENT(IN   ) :: grid_file_name
INTEGER,                       INTENT(IN   ) :: output_unit
INTEGER,                       INTENT(  OUT) :: nlon, nlat

INTEGER :: file_id, lon_id, nlon_id, lat_id, nlat_id
               
WRITE(output_unit,*) 'Reading input file ', grid_file_name

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_open(grid_file_name, nf90_nowrite, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid dim id.
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_inq_dimid(file_id, 'nlon', nlon_id), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inq_dimid(file_id, 'nlat', nlat_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid dim
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_inquire_dimension(file_id, nlon_id, len=nlon), __LINE__, __FILE__ )
CALL handle_netcdf_errors( nf90_inquire_dimension(file_id, nlat_id, len=nlat), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_close(file_id), __LINE__, __FILE__ )

WRITE(output_unit,*) 'Global dimensions nlon=', nlon,' nlat=', nlat

! #########################################################
END SUBROUTINE read_dimgrid
! #########################################################

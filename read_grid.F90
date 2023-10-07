! #########################################################
  SUBROUTINE read_grid (grid_file_name,nlon,nlat,ncorners,   &
                        lon,lat,clo,cla,srf,mask,output_unit)
! #########################################################

USE netcdf

IMPLICIT NONE

CHARACTER(LEN=30),                      INTENT(IN   ) :: grid_file_name
INTEGER,                                INTENT(IN   ) :: nlon,nlat,ncorners
INTEGER,                                INTENT(IN   ) :: output_unit

REAL,    DIMENSION(nlon,nlat),          INTENT(INOUT) :: lon, lat, srf
REAL,    DIMENSION(nlon,nlat,ncorners), INTENT(INOUT) :: clo, cla
INTEGER, DIMENSION(nlon,nlat),          INTENT(INOUT) :: mask

INTEGER                  :: file_id, lon_id, lat_id,&
                            clon_id, clat_id, srf_id, mask_id

INTEGER,  DIMENSION(3)   :: ila_dim
INTEGER,  DIMENSION(3)   :: ila_corners,ila_what

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open NETCDF file
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
!   Define grid dimensions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ila_what(:)=1

ila_dim (1)=nlon
ila_dim (2)=nlat
ila_dim (3)=1

ila_corners(1)=nlon
ila_corners(2)=nlat
ila_corners(3)=ncorners

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read grid data
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_get_var(file_id, lon_id, lon, ila_what(1:2), ila_dim(1:2)), __LINE__, __FILE__ )
WRITE(output_unit,*) 'Global grid longitudes reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, lat_id, lat, ila_what(1:2), ila_dim(1:2)), __LINE__,__FILE__ )
WRITE(output_unit,*) 'Global grid latitudes reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, clon_id, clo, ila_what, ila_corners), __LINE__,__FILE__ )
WRITE(output_unit,*) 'Global grid longitude corners reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, clat_id, cla, ila_what, ila_corners), __LINE__,__FILE__ )
WRITE(output_unit,*) 'Global grid latitude corners reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, srf_id, srf, ila_what(1:2), ila_dim(1:2)), __LINE__,__FILE__ )
WRITE(output_unit,*) 'Global grid surfaces reading done'

CALL handle_netcdf_errors( nf90_get_var(file_id, mask_id, mask, ila_what, ila_dim), __LINE__,__FILE__ )
WRITE(output_unit,*) 'Global grid mask reading done'

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close NETCDF file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_close(file_id), __LINE__, __FILE__ )

! #########################################################
END SUBROUTINE read_grid
! #########################################################

! #########################################################
SUBROUTINE read_forcing (output_unit,nsend_fields,INDI_BEG,INDI_END,INDJ_BEG,INDJ_END,ind_time, &
                         forcing_file_name,name_send_fields, &
                         NLON, NLAT, send_fields)
! #########################################################

USE netcdf
USE parameters

IMPLICIT NONE

INTEGER :: ILOOP

INTEGER :: file_id, NDIMS, UNLIMDIMID
INTEGER :: IL_LON_ID, IL_LAT_ID, IL_TIME_ID 

INTEGER, DIMENSION(3) :: NDIM
INTEGER :: NX, NY, NT
CHARACTER(LEN=50), DIMENSION(3) :: CNAME_DIM
CHARACTER(LEN=50) :: VNAME
INTEGER, DIMENSION(2) :: DIMIDS
INTEGER :: XTYPE, VARID

INTEGER, INTENT(IN) :: output_unit, nsend_fields
INTEGER, INTENT(IN) :: INDI_BEG,INDI_END,INDJ_BEG,INDJ_END, ind_time
INTEGER, INTENT(IN) :: NLON, NLAT

INTEGER, DIMENSION(nsend_fields) :: IL_VAR_ID

CHARACTER(len=len_file_names), INTENT(IN) :: forcing_file_name
CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_send_fields), INTENT(IN) :: name_send_fields

INTEGER, DIMENSION(3) :: ILA_DIM

REAL, ALLOCATABLE, DIMENSION(:,:,:) :: forcing_data
REAL, DIMENSION(NLON,NLAT,10), INTENT(OUT) :: send_fields

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Open netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_open(forcing_file_name, nf90_nowrite, file_id), __LINE__, __FILE__ )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read dimensions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors(NF90_INQUIRE(file_id, NDIMENSIONS=NDIMS, UNLIMITEDDIMID=UNLIMDIMID),__LINE__,__FILE__)
!
CALL handle_netcdf_errors(NF90_INQUIRE_DIMENSION(file_id,1,CNAME_DIM(1),NDIM(1)),__LINE__,__FILE__)
CALL handle_netcdf_errors(NF90_INQUIRE_DIMENSION(file_id,2,CNAME_DIM(2),NDIM(2)),__LINE__,__FILE__)
CALL handle_netcdf_errors(NF90_INQUIRE_DIMENSION(file_id,3,CNAME_DIM(3),NDIM(3)),__LINE__,__FILE__)
!
DO ILOOP=1, 3
  IF ( (CNAME_DIM(ILOOP) .EQ. 'ni') .OR. (CNAME_DIM(ILOOP) .EQ. 'longitude') .OR. (CNAME_DIM(ILOOP) .EQ. 'nlon') &
                                    .OR. (CNAME_DIM(ILOOP) .EQ. 'xi_rho') ) THEN
    NX=NDIM(ILOOP)
  ELSE IF ( (CNAME_DIM(ILOOP) .EQ. 'nj') .OR. (CNAME_DIM(ILOOP) .EQ. 'latitude') .OR. (CNAME_DIM(ILOOP) .EQ. 'nlat') &
                                         .OR. (CNAME_DIM(ILOOP) .EQ. 'eta_rho') ) THEN
    NY=NDIM(ILOOP)
  ELSE IF (CNAME_DIM(ILOOP) .EQ. 'time' .OR. (CNAME_DIM(ILOOP) .EQ. 'ntime') ) THEN
    NT=NDIM(ILOOP)
  END IF
END DO

ALLOCATE(forcing_data(NX,NY,NT))

!CALL HDLERR( NF90_INQ_VARID(file_id, 'longitude', IL_LON_ID),__LINE__,__FILE__)
!CALL HDLERR( NF90_INQ_VARID(file_id, 'latitude', IL_LAT_ID),__LINE__,__FILE__)
!CALL HDLERR( NF90_INQ_VARID(file_id, 'time', IL_TIME_ID),__LINE__,__FILE__ )

DO ILOOP=1, nsend_fields
  CALL handle_netcdf_errors( NF90_INQ_VARID(file_id, TRIM(name_send_fields(ILOOP)), IL_VAR_ID(ILOOP)), __LINE__,__FILE__ )
  CALL handle_netcdf_errors(NF90_INQUIRE_VARIABLE(file_id,IL_VAR_ID(ILOOP),VNAME,XTYPE,NDIMS,ILA_DIM), __LINE__,__FILE__)
  CALL handle_netcdf_errors(NF90_INQ_VARID(file_id,VNAME,VARID), __LINE__,__FILE__)
  CALL handle_netcdf_errors(NF90_GET_VAR(file_id,VARID,forcing_data), __LINE__,__FILE__)
  send_fields(:,:,ILOOP)=RESHAPE(forcing_data(INDI_BEG:INDI_END,INDJ_BEG:INDJ_END,ind_time+1),&
                         (/ NLON, NLAT /))
END DO

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Close netcdf file
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CALL handle_netcdf_errors( nf90_close(file_id), __LINE__, __FILE__ )

! #########################################################
END SUBROUTINE read_forcing
! #########################################################

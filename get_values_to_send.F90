! #########################################################
SUBROUTINE get_values_to_send(output_unit,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end, &
                              nlon_local,nlat_local, nsend_fields, &
                              lon_local,lat_local, &
                              send_fields, ind_time, type_send, value_send, forcing_file_name, name_send_fields)
! #########################################################

! ---------------------------------------------------------
!
!        Read value_sends to send :
!          type_send = 'cnste'
!          type_send = 'sinus'
!          type_send = 'files'
!
! --------------------------------------------------------

USE parameters

IMPLICIT NONE

INTEGER,                                                          INTENT(IN   ) :: nlon_local,nlat_local,ind_time,nsend_fields
INTEGER,                                                          INTENT(IN   ) :: output_unit
INTEGER,                                                          INTENT(IN   ) :: ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end
REAL,                                                             INTENT(IN   ) :: value_send
REAL, DIMENSION(nlon_local,nlat_local),                           INTENT(IN   ) :: lon_local
REAL, DIMENSION(nlon_local,nlat_local),                           INTENT(IN   ) :: lat_local
CHARACTER(LEN=len_type_send),                                     INTENT(IN   ) :: type_send
CHARACTER(LEN=len_file_names),                                    INTENT(IN   ) :: forcing_file_name
CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_send_fields), INTENT(IN   ) :: name_send_fields
REAL, DIMENSION(nlon_local,nlat_local,nsend_fields),              INTENT(  OUT) :: send_fields

INTEGER            :: ind_lon, ind_lat

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   type_send .EQ. 'const'
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IF (type_send .EQ. 'const') THEN
  send_fields(:,:,:)=value_send
  IF (nsend_fields .GE. 3) THEN
    send_fields(:,:,2)=0.0
    send_fields(:,:,3)=0.0
  END IF

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   type_send .EQ. 'sinus'
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ELSE IF (type_send .EQ. 'sinus') THEN
  DO ind_lat=1,nlat_local
    DO ind_lon=1,nlon_local
      send_fields(ind_lon,ind_lat,:) = value_send*SIN(lat_local(ind_lon,ind_lat)*convert_degree_to_radian*length &
                                                      +pi/100.0*ind_time)
    ENDDO
  ENDDO

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   type_send .EQ. 'files'
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ELSE IF (type_send .EQ. 'files') THEN
  CALL read_forcing(output_unit,nsend_fields,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end,ind_time,&
                    forcing_file_name,name_send_fields, &
                    nlon_local,nlat_local,send_fields)
                      
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   type_send not supported
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ELSE
  WRITE(output_unit,*) 'type_send not supported : ', type_send
  CALL abort
END IF

! #########################################################
END SUBROUTINE get_values_to_send
! #########################################################

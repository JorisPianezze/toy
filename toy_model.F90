! #########################################################
PROGRAM toy_model
! #########################################################

! ---------------------------------------------------------
!
!             Toy model for OASIS's coupling
!
! --------------------------------------------------------

! ========================================================
!                List of modifications
!
!        2014-10 : CERFACS             - OASIS's tuto
!        2015-02 : J. Pianezze (  LPO) - Add forcing file
!        2015-04 : J. Pianezze (  LPO) - Add namelist file
!        2023-10 : J. Pianezze (LAERO) - Refactoring/Cleaning
!
! ========================================================

USE netcdf
USE mod_oasis
USE parameters

IMPLICIT NONE

INCLUDE 'mpif.h'

CHARACTER(LEN=6) :: model_name = 'toyexe'
INTEGER :: comp_id
INTEGER :: klocalcomm, ksize, krank
INTEGER :: ierr
INTEGER :: output_unit=51

! Global grid parameters for grid definition
! ----------------------------------------------------- 
INTEGER :: nlon, nlat     ! dimensions in the 2 directions of space
INTEGER :: ncorners=4     ! number of corners
INTEGER :: ind_lon_beg, ind_lon_end, ind_lat_beg, ind_lat_end
!
REAL,    DIMENSION(:,:),   POINTER :: lon,lat
REAL,    DIMENSION(:,:,:), POINTER :: clon,clat
REAL,    DIMENSION(:,:),   POINTER :: srf
INTEGER, DIMENSION(:,:),   POINTER :: mask ! mask, 0 == valid point, 1 == masked point 

CHARACTER(LEN=len_file_names) :: grid_file_name
CHARACTER(LEN=len_type_send)  :: type_send
REAL                          :: value_send
CHARACTER(LEN=len_file_names) :: forcing_file_name=''

! Global definition parition parameters
! -------------------------------------
INTEGER                            :: partition_id
INTEGER, PARAMETER                 :: partition_size=5
INTEGER, DIMENSION(partition_size) :: partition
!
! Global parameters for oasis_def_var
! -----------------------------------
!
INTEGER :: nrecv_fields
INTEGER :: nsend_fields

CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_recv_fields) :: name_recv_fields='        '
CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_send_fields) :: name_send_fields='        '
!
! Used in oasis_def_var
INTEGER :: VAR_ID(10) 
INTEGER :: VAR_NODIMS(2) 
INTEGER :: VAR_TYPE
INTEGER :: VAR_ACTUAL_SHAPE(4) ! local dimensions of the arrays to the pe
                               ! 2 x field rank (= 4 because fields are of rank = 2)
!
REAL, PARAMETER :: FIELD_INI = -1. ! initialisation of received fields
!
INTEGER :: ind_time, ind_field
INTEGER :: ntime_steps ! number of time steps
INTEGER :: time_step          ! time step
INTEGER :: ITAP_SEC         ! Time
!
! Exchanged local fields arrays
! used in routines oasis_put and oasis_get
REAL, POINTER :: FIELD_RECV(:,:)
REAL, POINTER :: send_fields(:,:,:)

OPEN(UNIT=output_unit,FILE='OUTPUT_TOY.txt')

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Initialisation                                                       '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

WRITE(output_unit,*) ' - oasis_init_comp'
CALL oasis_init_comp(comp_id,model_name,ierr)
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,'error in oasis_init_comp')

WRITE(output_unit,*) ' - oasis_get_localcomm'
CALL oasis_get_localcomm(klocalcomm,ierr)
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,'ERROR')

CALL mpi_comm_size (klocalcomm, ksize, ierr )
WRITE(output_unit,*) ' - ksize=', ksize
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during mpi_comm_size')

CALL mpi_comm_rank (klocalcomm, krank, ierr )
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during mpi_comm_rank')

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Read TOYNAMELIST.nam'
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

CALL read_namelist(output_unit,ntime_steps,time_step, grid_file_name, &
                   type_send, value_send, forcing_file_name,          &
                   nrecv_fields, name_recv_fields,                    &
                   nsend_fields, name_send_fields                     )

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Read grid                                                            '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

! Read nlon, nlat
CALL read_dimgrid(grid_file_name, nlon, nlat, output_unit)

ALLOCATE( lon (nlon,nlat         ) )
ALLOCATE( lat (nlon,nlat         ) )
ALLOCATE( clon(nlon,nlat,ncorners) )
ALLOCATE( clat(nlon,nlat,ncorners) )
ALLOCATE( srf (nlon,nlat         ) )
ALLOCATE( mask(nlon,nlat         ) )

! Read lon, lat, clon, clat, srf and mask
CALL read_grid(grid_file_name,nlon,nlat,ncorners,lon,lat, &
               clon,clat,srf,mask,output_unit             )

! (Global) grid definition for OASIS3
! Writing of the file grids.nc and masks.nc by the processor 0 from the grid read in 
IF (krank == 0) THEN

  ! Mask inversion to follow (historical) OASIS3 convention (0=not masked;1=masked)
  WHERE (mask == 1) 
    mask=0
  ELSEWHERE
    mask=1
  END WHERE
  
  CALL oasis_start_grids_writing(ierr)
  CALL oasis_write_grid  ('toyt', nlon, nlat, lon, lat)
  CALL oasis_write_corner('toyt', nlon, nlat, ncorners, clon, clat)
  CALL oasis_write_area  ('toyt', nlon, nlat, srf)
  CALL oasis_write_mask  ('toyt', nlon, nlat, mask)
  CALL oasis_terminate_grids_writing()

ENDIF

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Define partition                                                     '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

CALL decomp_def(partition,partition_size,nlon,nlat,krank,ksize,output_unit)
WRITE(output_unit,*) ' - After decomp_def, partition = ', partition

CALL oasis_def_partition(partition_id, partition, ierr)

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Define exchanged variables (send/recv)                               '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

! Define transient variables
VAR_NODIMS(1) = 2    ! Rank of the field array is 2
VAR_NODIMS(2) = 1    ! Bundles always 1 for OASIS3
VAR_TYPE = OASIS_Real

VAR_ACTUAL_SHAPE(1) = 1
VAR_ACTUAL_SHAPE(2) = partition(3)
VAR_ACTUAL_SHAPE(3) = 1 

VAR_ACTUAL_SHAPE(4) = partition(4)

! Declaration of the field associated with the partition of the grid
DO ind_field=1, nrecv_fields 
  CALL oasis_def_var(VAR_ID(ind_field),name_recv_fields(ind_field), partition_id, &
                   VAR_NODIMS, OASIS_IN, VAR_ACTUAL_SHAPE, VAR_TYPE, ierr)
  IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during definition of recv fields')
ENDDO

DO ind_field=1, nsend_fields
  CALL oasis_def_var(VAR_ID(ind_field+nrecv_fields),name_send_fields(ind_field), partition_id, &
                   VAR_NODIMS, OASIS_OUT, VAR_ACTUAL_SHAPE, VAR_TYPE, ierr)
  IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during definition of send fields')
ENDDO 

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      End of initialisation                                                '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

CALL oasis_enddef(ierr)
IF(ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during oasis_enddef')

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Time-stepping                                                        '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

ALLOCATE(FIELD_RECV(VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4)), STAT=ierr)
IF (ierr /= 0 ) WRITE(output_unit,*) 'ERROR ALLOCATING FIELD_RECV'

ALLOCATE(send_fields(VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4),nsend_fields),STAT=ierr)
IF (ierr /= 0 ) WRITE(output_unit,*) 'ERROR ALLOCATING send_fields'

ind_lon_beg=1
ind_lon_end=nlon
ind_lat_beg=((nlat/ksize)*krank)+1 

IF (krank .LT. ksize - 1) THEN
  ind_lat_end = (nlat/ksize)*(krank+1)
ELSE
  ind_lat_end = nlat 
ENDIF
!
DO ind_time=1, ntime_steps
  !
  ITAP_SEC = time_step * (ind_time-1) ! Time
  !
  WRITE(output_unit,*) ' - Current time : ', ITAP_SEC
  !
  ! Get the field from coupled model (atmosphere/wave/ocean)
  ! -------------------------------------------------------
  DO ind_field=1, nrecv_fields
    FIELD_RECV=FIELD_INI
    CALL OASIS_GET(VAR_ID(ind_field),ITAP_SEC, FIELD_RECV, ierr)
    WRITE(output_unit,*) 'RECEIVE FIELD : ', name_recv_fields(ind_field) , ' => ', ITAP_SEC, MINVAL(FIELD_RECV), MAXVAL(FIELD_RECV)
    IF ( ierr .NE. OASIS_Ok .AND. ierr .LT. OASIS_Recvd) THEN
      WRITE (output_unit,*) 'OASIS_GET ABORT BY TOY MODEL COMPID ',comp_id
      CALL oasis_abort(comp_id,model_name,'PROBLEM DURING OASIS_GET')
    ENDIF
  ENDDO
  !
  ! Send the field to coupled model (atmosphere/wave/ocean)
  ! -------------------------------------------------------
  !CALL get_values_to_send(output_unit,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end, &
  !                   VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4), nsend_fields, &
  !                   RESHAPE(lon(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end),&
  !                   (/ VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4) /)), &
  !                   RESHAPE(lat(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end),&
  !                   (/ VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4) /)), &
  !                   send_fields,ind_time, &
  !                   type_send, value_send, forcing_file_name, name_send_fields)
  
  CALL get_values_to_send(output_unit,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end, &
                     VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4), nsend_fields, &
                     lon(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end), &
                     lat(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end), &
                     send_fields,ind_time, &
                     type_send, value_send, forcing_file_name, name_send_fields)
  		     
  DO ind_field=1, nsend_fields 
    WRITE(output_unit,*) 'SEND FIELD : ', name_send_fields(ind_field), ' => ', ITAP_SEC, MINVAL(send_fields), MAXVAL(send_fields)
    CALL OASIS_PUT(VAR_ID(ind_field+nrecv_fields),ITAP_SEC, send_fields(:,:,ind_field), ierr)
    IF ( ierr .NE. OASIS_Ok .AND. ierr .LT. OASIS_Sent) THEN
      WRITE (output_unit,*) 'OASIS_PUT ABORT BY TOY MODEL COMPID ',comp_id
      CALL oasis_abort(comp_id,model_name,'PROBLEM DURING OASIS_PUT')
    ENDIF
  ENDDO
  !
ENDDO

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Termination                                                          '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

WRITE(output_unit,*) ' - oasis_terminate'
CALL oasis_terminate(ierr)
IF(ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during oasis_terminate')

CLOSE(UNIT=output_unit)

! ##############################################################################
END PROGRAM toy_model
! ##############################################################################

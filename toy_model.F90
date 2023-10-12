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

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Main's definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
CHARACTER(LEN=6) :: model_name = 'toyexe'
INTEGER          :: comp_id
INTEGER          :: klocalcomm, ksize, krank
INTEGER          :: ierr
INTEGER          :: output_unit=51

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Time's definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
INTEGER :: ind_time
INTEGER :: ntime_steps
INTEGER :: time_step
INTEGER :: current_time

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Grid's definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
CHARACTER(LEN=len_file_names) :: grid_file_name
INTEGER                       :: nlon, nlat
INTEGER                       :: ncorners=4
INTEGER                       :: ind_lon_beg, ind_lon_end
INTEGER                       :: ind_lat_beg, ind_lat_end

REAL,    DIMENSION(:,:  ), POINTER :: lon,lat
REAL,    DIMENSION(:,:,:), POINTER :: clon,clat
REAL,    DIMENSION(:,:  ), POINTER :: srf
INTEGER, DIMENSION(:,:  ), POINTER :: mask      ! mask, 0 == valid point, 1 == masked point 

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Fields to send
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
CHARACTER(LEN=len_type_send)  :: type_send
REAL                          :: value_send
CHARACTER(LEN=len_file_names) :: forcing_file_name=''

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Partition's definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
INTEGER                            :: partition_id
INTEGER, PARAMETER                 :: partition_size=5
INTEGER, DIMENSION(partition_size) :: partition

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Exchanged variable's definition
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
INTEGER :: ind_field
INTEGER :: nrecv_fields
INTEGER :: nsend_fields

CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_recv_fields) :: name_recv_fields='        '
CHARACTER(LEN=len_exchanged_fields), DIMENSION(nmax_send_fields) :: name_send_fields='        '

INTEGER,  DIMENSION(nmax_recv_fields) :: recv_fields_id 
INTEGER,  DIMENSION(nmax_send_fields) :: send_fields_id
INTEGER :: var_nodims(2) 
INTEGER :: var_type
INTEGER :: var_actual_shape(4) 

REAL, POINTER :: recv_fields(:,:,:)
REAL, POINTER :: send_fields(:,:,:)

OPEN(UNIT=output_unit,FILE='OUTPUT_TOY.txt')

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Initialisation                                                       '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

WRITE(output_unit,*) ' oasis_init_comp'
CALL oasis_init_comp(comp_id,model_name,ierr)
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,'error in oasis_init_comp')

WRITE(output_unit,*) ' oasis_get_localcomm'
CALL oasis_get_localcomm(klocalcomm,ierr)
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,'ERROR')

CALL mpi_comm_size (klocalcomm, ksize, ierr )
WRITE(output_unit,*) ' ksize=', ksize
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' error during mpi_comm_size')

CALL mpi_comm_rank (klocalcomm, krank, ierr )
IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' error during mpi_comm_rank')

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

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Read nlon, nlat
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

CALL read_dimgrid(grid_file_name, nlon, nlat, output_unit)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Allocate and read lon, lat, clon, clat, srf and mask
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

ALLOCATE( lon (nlon,nlat         ) )
ALLOCATE( lat (nlon,nlat         ) )
ALLOCATE( clon(nlon,nlat,ncorners) )
ALLOCATE( clat(nlon,nlat,ncorners) )
ALLOCATE( srf (nlon,nlat         ) )
ALLOCATE( mask(nlon,nlat         ) )

CALL read_grid(grid_file_name,nlon,nlat,ncorners,lon,lat, &
               clon,clat,srf,mask,output_unit             )

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Create netcdf grid files for OASIS
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

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
WRITE(output_unit,*) ' After decomp_def, partition = ', partition

CALL oasis_def_partition(partition_id, partition, ierr)

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Define exchanged variables (send/recv)                               '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

var_nodims(1) = 2          ! Rank of the field array is 2
var_nodims(2) = 1          ! Bundles always 1 for OASIS3
var_type      = oasis_real

var_actual_shape(1) = 1
var_actual_shape(2) = partition(3)
var_actual_shape(3) = 1
var_actual_shape(4) = partition(4)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Declare list of recv fields
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
DO ind_field=1, nrecv_fields 
  CALL oasis_def_var(recv_fields_id(ind_field),name_recv_fields(ind_field), partition_id, &
                   var_nodims, oasis_in, var_actual_shape, var_type, ierr)
  IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during definition of recv fields')
ENDDO

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Declare list of send fields
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
DO ind_field=1, nsend_fields
  CALL oasis_def_var(send_fields_id(ind_field),name_send_fields(ind_field), partition_id, &
                   var_nodims, oasis_out, var_actual_shape, var_type, ierr)
  IF (ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during definition of send fields')
ENDDO 

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      End of initialisation                                                '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

CALL oasis_enddef(ierr)
IF(ierr /= 0) CALL oasis_abort(comp_id,model_name,' - error during oasis_enddef')

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Define local variables                                               '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

ALLOCATE(recv_fields(var_actual_shape(2), var_actual_shape(4),nrecv_fields))
ALLOCATE(send_fields(var_actual_shape(2), var_actual_shape(4),nsend_fields))

ind_lon_beg = 1
ind_lon_end = nlon
ind_lat_beg = ((nlat/ksize)*krank)+1 

IF (krank .LT. ksize - 1) THEN
  ind_lat_end = (nlat/ksize)*(krank+1)
ELSE
  ind_lat_end = nlat 
ENDIF

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Time-stepping                                                        '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

DO ind_time=1, ntime_steps
  
  current_time = time_step * (ind_time-1)
  WRITE(output_unit,*) ' Current time : ', current_time

  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !   Get fields from coupled model
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  DO ind_field=1, nrecv_fields
    CALL oasis_get(recv_fields_id(ind_field),current_time,recv_fields(:,:,ind_field),ierr) 
    WRITE(output_unit,*) '  Receiving the field : ', name_recv_fields(ind_field)
    WRITE(output_unit,*) '  minval/maxval : ', MINVAL(recv_fields(:,:,ind_field)), MAXVAL(recv_fields(:,:,ind_field))
    IF (ierr .NE. oasis_ok .AND. ierr .LT. oasis_recvd) CALL oasis_abort(comp_id,model_name,' error during oasis_get')
  ENDDO
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !   Get and send fields to coupled model
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  CALL get_values_to_send(output_unit,ind_lon_beg,ind_lon_end,ind_lat_beg,ind_lat_end, &
                     var_actual_shape(2), var_actual_shape(4), nsend_fields, &
                     lon(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end), &
                     lat(ind_lon_beg:ind_lon_end,ind_lat_beg:ind_lat_end), &
                     send_fields,ind_time, &
                     type_send, value_send, forcing_file_name, name_send_fields)

  DO ind_field=1, nsend_fields 
    WRITE(output_unit,*) '  Sending the field : ', name_send_fields(ind_field)
    WRITE(output_unit,*) '  minval/maxval : ', MINVAL(send_fields(:,:,ind_field)), MAXVAL(send_fields(:,:,ind_field))    
    CALL oasis_put(send_fields_id(ind_field),current_time,send_fields(:,:,ind_field),ierr)
    IF (ierr .NE. oasis_ok .AND. ierr .LT. oasis_sent) CALL oasis_abort(comp_id,model_name,' error during oasis_put')
  ENDDO
  
ENDDO

WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
WRITE(output_unit,*) '      Termination                                                          '
WRITE(output_unit,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

WRITE(output_unit,*) ' oasis_terminate'
CALL oasis_terminate(ierr)
IF(ierr /= 0) CALL oasis_abort(comp_id,model_name,' error during oasis_terminate')

CLOSE(UNIT=output_unit)

! ##############################################################################
END PROGRAM toy_model
! ##############################################################################

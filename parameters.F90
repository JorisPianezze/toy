! #########################################################
MODULE parameters
! #########################################################

! ---------------------------------------------------------
!
!        Define parameters
!
! --------------------------------------------------------

INTEGER, PARAMETER :: nmax_recv_fields=10
INTEGER, PARAMETER :: nmax_send_fields=10
INTEGER, PARAMETER :: len_file_names=30
INTEGER, PARAMETER :: len_type_send=5
INTEGER, PARAMETER :: len_exchanged_fields=8

REAL, PARAMETER    :: pi=4.0*ATAN(1.0)
REAL, PARAMETER    :: length= 1000.0
REAL, PARAMETER    :: convert_degree_to_radian = pi/180.

! #########################################################
END MODULE parameters
! #########################################################

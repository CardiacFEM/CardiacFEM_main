MODULE INPUT

  !/****h* /input
  !*  NAME
  !*    MODULE: input
  !*  SYNOPSIS
  !*    Usage:      USE input
  !*  FUNCTION
  !*    Contains subroutines that handle input data. These subroutines are 
  !*    parallel and require MPI.
  !*    
  !*    Subroutine             Purpose
  !*
  !*    READ_MESH_DATA         Reads the information about the mesh
  !*    READ_NUMERICAL_DATA    Reads the numerical data of the system
  !*    MESH_ENSI              Creates ASCII ensight gold files
  !*    MESH_ENSI_BIN          Creates BINARY ensight gold files *not tested*
  !*    MESH_ENSI_GEO_BIN      Creates BINARY ensight geo files *not tested*
  !*    MESH_ENSI_MATID_BIN    Creates BINARY ensight MATID files *not tested*
  !*    MESH_ENSI_NDBND_BIN    Creates BINARY ensight NDBND files *not tested*
  !*    MESH_ENSI_NDLDS_BIN    Creates BINARY ensight LDS files *not tested*
  !*  AUTHOR
  !*    L. Margetts and F. Levrero-Florencio
  !*  COPYRIGHT
  !*    2004-2014 University of Manchester
  !******
  !*  Place remarks that should not be included in the documentation here.
  !*  If you contribute to this module, add your author name.
  !*
  !*/

  USE precision
  USE mp_interface

  CONTAINS

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE READ_MESH_DATA(fname,numpe,nels,nn,nr,loaded_nodes,fixed_nodes,&
   nod,nip)

  !/****f* input_output/read_mesh_data
  !*  NAME
  !*    SUBROUTINE: read_mesh_data
  !*  SYNOPSIS
  !*    Usage:      CALL read_mesh_data(fname,numpe,nels,nn,nr,loaded_nodes,&
  !*                                fixed_nodes,nod,nip)
  !*  FUNCTION
  !*    Master process reads the general data of the mesh of the problem
  !*    Master process broadcasts to slave processes.
  !*  INPUTS
  !*    The following arguments have the INTENT(IN) attribute:
  !*
  !*    fname                  : Character
  !*                           : File name to read
  !*
  !*    numpe                  : Integer
  !*                           : Process number
  !*
  !*    The following arguments have the INTENT(OUT) attribute:
  !*
  !*    nels                   : Integer
  !*                           : Total number of elements
  !*
  !*    nn                     : Integer
  !*                           : Total number of nodes 
  !*
  !*    nr                     : Integer
  !*                           : Number of nodes with restrained degrees of
  !*                             freedom 
  !*
  !*    loaded_nodes           : Integer
  !*                           : Number of nodes with applied forces
  !*
  !*    fixed_nodes            : Integer
  !*                           : Number of restrained degrees of freedom 
  !*                             with a non-zero applied value
  !*
  !*    nod                    : Integer
  !*                           : Number of nodes per element
  !*
  !*    nip                    : Integer
  !*                           : Number of Gauss integration points
  !*
  !*  AUTHOR
  !*    Francesc Levrero-Florencio
  !*    L. Margetts
  !*  CREATION DATE
  !*    01.06.2007
  !*  COPYRIGHT
  !*    (c) University of Manchester 2007-2011
  !******
  !*  Place remarks that should not be included in the documentation here.
  !*
  !*/
  
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)  :: fname
    INTEGER,      INTENT(IN)  :: numpe
    INTEGER,      INTENT(OUT) :: nels, nn, nr, nod, nip,
    INTEGER                   :: bufsize, ier, vec_integer(10)

  !----------------------------------------------------------------------
  ! 1. Master process reads the data and builds the integer and real
  !    vectors with the data
  !----------------------------------------------------------------------

    IF (numpe == 1) THEN
      OPEN(10,FILE=fname,STATUS='OLD',ACTION='READ')
      READ(10,*) nels, nn, nr, loaded_nodes, fixed_nodes, nod, nip
      CLOSE(10)
      
      vec_integer(1) = nels
      vec_integer(2) = nn
      vec_integer(3) = nr
      vec_integer(4) = loaded_nodes
      vec_integer(5) = fixed_nodes
      vec_integer(6) = nod
      vec_integer(7) = nip
    END IF

  !----------------------------------------------------------------------
  ! 2. Master process broadcasts the data to slave processes
  !----------------------------------------------------------------------

    bufsize = 7
    CALL MPI_BCAST(vec_integer,bufsize,MPI_INTEGER,0,MPI_COMM_WORLD,ier)

  !----------------------------------------------------------------------
  ! 3. Slave processes extract the variables back from the vectors
  !----------------------------------------------------------------------

    IF (numpe /= 1) THEN
      nels         = vec_integer(1)
      nn           = vec_integer(2)
      nr           = vec_integer(3)
      loaded_nodes = vec_integer(4)
      fixed_nodes  = vec_integer(5)
      nod          = vec_integer(6)
      nip          = vec_integer(7)
    END IF

    RETURN

  END SUBROUTINE READ_MESH_DATA

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE READ_NUMERICAL_DATA(fname,numpe,nels,nn,nr,loaded_nodes,fixed_nodes,&
   nod,nip)

  !/****f* input_output/read_numerical_data
  !*  NAME
  !*    SUBROUTINE: read_numerical_data
  !*  SYNOPSIS
  !*    Usage:      CALL read_numerical_data(fname,numpe,nels,nn,nr,loaded_nodes,&
  !*                                fixed_nodes,nod,nip)
  !*  FUNCTION
  !*    Master process reads the numerical data of the problem
  !*    Master process broadcasts to slave processes.
  !*  INPUTS
  !*    The following arguments have the INTENT(IN) attribute:
  !*
  !*    fname                  : Character
  !*                           : File name to read
  !*
  !*    numpe                  : Integer
  !*                           : Process number
  !*
  !*    limit                  : Integer
  !*                           : Maximum number of PCG iterations allowed
  !*
  !*    tol                    : Real
  !*
  !*    time                   : Real
  !*                           : Time interval of the simulation
  !*
  !*    max_steps              : Integer
  !*                             Maximum number of load steps
  !*
  !*    jump                   : Integer
  !*                           : Number of load steps to skip before writing
  !*                             results (periodically)
  !*
  !*    nr_tol                 : Real
  !*                           : Tolerance for Newton-Raphson loop
  !*
  !*  AUTHOR
  !*    Francesc Levrero-Florencio
  !*    L. Margetts
  !*  CREATION DATE
  !*    01.06.2007
  !*  COPYRIGHT
  !*    (c) University of Manchester 2007-2011
  !******
  !*  Place remarks that should not be included in the documentation here.
  !*
  !*/
  
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)  :: fname
    INTEGER,      INTENT(IN)  :: numpe
    INTEGER,      INTENT(OUT) :: limit, max_steps, jump
    REAL(iwp),    INTENT(OUT) :: tol, nr_tol
    INTEGER                   :: bufsize, ier, vec_integer(3)
    REAL(iwp)                 :: vec_real(2)

  !----------------------------------------------------------------------
  ! 1. Master process reads the data and builds the integer and real
  !    vectors with the data
  !----------------------------------------------------------------------

    IF (numpe == 1) THEN
      OPEN(10,FILE=fname,STATUS='OLD',ACTION='READ')
      READ(10,*) limit, tol, max_steps, jump, nr_tol
      CLOSE(10)
      
      vec_integer(1) = limit
      vec_real(1)    = tol
      vec_integer(2) = max_steps
      vec_integer(3) = jump
      vec_real(2)    = nr_tol
    END IF

  !----------------------------------------------------------------------
  ! 2. Master process broadcasts the data to slave processes
  !----------------------------------------------------------------------

    bufsize = 2
    CALL MPI_BCAST(vec_integer,bufsize,MPI_INTEGER,0,MPI_COMM_WORLD,ier)

    bufsize = 2
    CALL MPI_BCAST(vec_real,bufsize,MPI_REAL8,0,MPI_COMM_WORLD,ier)

  !----------------------------------------------------------------------
  ! 3. Slave processes extract the variables back from the vectors
  !----------------------------------------------------------------------

    IF (numpe /= 1) THEN
      limit     = vec_integer(1)
      tol       = vec_real(1)
      max_steps = vec_integer(2)
      jump      = vec_integer(3)
      nr_tol    = vec_real(2)
    END IF

    RETURN

  END SUBROUTINE READ_NUMERICAL_DATA

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    
END MODULE INPUT

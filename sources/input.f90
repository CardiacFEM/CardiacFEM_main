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
  !*    READ_XX7               Reads the control data for program xx7
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

      IF (numpe==1) THEN
        OPEN(10,FILE=fname,STATUS='OLD',ACTION='READ')
        READ(10,*)nels,nn,nr,loaded_nodes,fixed_nodes,nip
        READ(10,*)limit,tol,e,v
        READ(10,*)nod
        READ(10,*)num_load_steps,jump
        READ(10,*)tol2
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

    SUBROUTINE READ_DATA_XX7(fname,numpe,nels,nn,nr,loaded_nodes,fixed_nodes, &
                             nip,limit,tol,e,v,nod,num_load_steps,jump,tol2)

    !/****f* input_output/read_data_xx7
    !*  NAME
    !*    SUBROUTINE: read_data_xx7
    !*  SYNOPSIS
    !*    Usage:      CALL read_data_xx7(fname,numpe,nels,nn,nr,              &
    !*                                   loaded_nodes,fixed_nodes,nip,        &
    !*                                   limit,tol,e,v,nod,num_load_steps,    &
    !*                                   jump,tol2)
    !*  FUNCTION
    !*    Master process reads the general data of the problem
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
    !*    nip                    : Integer
    !*                           : Number of Gauss integration points
    !*
    !*    limit                  : Integer
    !*                           : Maximum number of PCG iterations allowed
    !*
    !*    tol                    : Real
    !*                           : Tolerance for PCG
    !*
    !*    e                      : Real
    !*                           : Young's modulus
    !*
    !*    v                      : Real
    !*                           : Poisson coefficient
    !*
    !*    nod                    : Integer
    !*                           : Number of nodes per element
    !*
    !*    num_load_steps         : Integer
    !*                           : Number of load steps
    !*
    !*    jump                   : Integer
    !*                           : Number of load steps to skip before writing
    !*                             results (periodically)
    !*
    !*    tol2                   : Real
    !*                           : Tolerance for Newton-Raphson loop
    !*
    !*  AUTHOR
    !*    Francisco Calvo
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
    INTEGER,      INTENT(OUT) :: nels, nn, nr, loaded_nodes, fixed_nodes, nip,&
                                 limit, nod, num_load_steps, jump
    REAL(iwp),    INTENT(OUT) :: tol, e, v, tol2
    INTEGER                   :: bufsize, ier, vec_integer(10)
    REAL(iwp)                 :: vec_real(4)

    !----------------------------------------------------------------------
    ! 1. Master process reads the data and builds the integer and real
    !    vectors with the data
    !----------------------------------------------------------------------

    IF (numpe==1) THEN
      OPEN(10,FILE=fname,STATUS='OLD',ACTION='READ')
      READ(10,*)nels,nn,nr,loaded_nodes,fixed_nodes,nip
      READ(10,*)limit,tol,e,v
      READ(10,*)nod
      READ(10,*)num_load_steps,jump
      READ(10,*)tol2
      CLOSE(10)
      
      vec_integer(1)  = nels
      vec_integer(2)  = nn
      vec_integer(3)  = nr
      vec_integer(4)  = loaded_nodes
      vec_integer(5)  = fixed_nodes
      vec_integer(6)  = nip
      vec_integer(7)  = limit
      vec_real(1)     = tol
      vec_real(2)     = e
      vec_real(3)     = v
      vec_integer(8)  = nod
      vec_integer(9)  = num_load_steps
      vec_integer(10) = jump
      vec_real(4)     = tol2
      
    END IF

    !----------------------------------------------------------------------
    ! 2. Master process broadcasts the data to slave processes
    !----------------------------------------------------------------------

    bufsize = 10
    CALL MPI_BCAST(vec_integer,bufsize,MPI_INTEGER,0,MPI_COMM_WORLD,ier)

    bufsize = 4
    CALL MPI_BCAST(vec_real,bufsize,MPI_REAL8,0,MPI_COMM_WORLD,ier)

    !----------------------------------------------------------------------
    ! 3. Slave processes extract the variables back from the vectors
    !----------------------------------------------------------------------

    IF (numpe/=1) THEN
      nels           = vec_integer(1)
      nn             = vec_integer(2)
      nr             = vec_integer(3)
      loaded_nodes   = vec_integer(4)
      fixed_nodes    = vec_integer(5)
      nip            = vec_integer(6)
      limit          = vec_integer(7)
      tol            = vec_real(1)
      e              = vec_real(2)
      v              = vec_real(3)
      nod            = vec_integer(8)
      num_load_steps = vec_integer(9)
      jump           = vec_integer(10)
      tol2           = vec_real(4)
    END IF

    RETURN

  END SUBROUTINE READ_DATA_XX7

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE READ_RFEMSOLVE(job_name,numpe,element,fixed_freedoms,limit,      &
                      loaded_nodes,mesh,mises,nels,nip,nn,nod,np_types,nr,    &
                      partition,tol)

  !/****f* input/read_rfemsolve
  !*  NAME
  !*    SUBROUTINE: read_rfemsolve
  !*  SYNOPSIS
  !*    Usage:      CALL read_rfemsolve(job_name,numpe,element,fixed_freedoms,
  !*                              limit,loaded_nodes,mesh,mises,nels,nip,nn,nod,
  !*                              np_types,nr,partition,tol)
  !*  FUNCTION
  !*    Master processor reads the general data for the problem and broadcasts 
  !*    it to the slave processors.
  !*  INPUTS
  !*    The following arguments have the INTENT(IN) attribute:
  !*
  !*    job_name               : Character
  !*                           : File name that contains the data to be read
  !*
  !*    numpe                  : Integer
  !*                           : Processor number
  !*
  !*    partition              : Integer
  !*                           : Type of partitioning 
  !*                           : 1 = internal partitioning
  !*                           : 2 = external partitioning with .psize file
  !*
  !*    The following arguments have the INTENT(INOUT) attribute:
  !*
  !*    element                : Character
  !*                           : Element type
  !*                           : Values: 'hexahedron' or 'tetrahedron'
  !*
  !*    fixed_freedoms         : Integer
  !*                           : Number of fixed displacements
  !*
  !*    limit                  : Integer
  !*                           : Maximum number of PCG iterations allowed
  !*
  !*    loaded_nodes           : Integer
  !*                           : Number of nodes with applied forces
  !*
  !*    mesh                   : Integer
  !*                           : 1 = Smith and Griffiths numbering scheme
  !*                           : 2 = Abaqus numbering scheme
  !*
  !*    nels                   : Integer
  !*                           : Total number of elements
  !*
  !*    nip                    : Integer
  !*                           : Number of Gauss integration points
  !*
  !*    nn                     : Integer
  !*                           : Total number of nodes in the mesh
  !*
  !*    nod                    : Integer
  !*                           : Number of nodes per element
  !*
  !*    nr                     : Integer
  !*                           : Number of nodes with restrained degrees of
  !*                             freedom 
  !*
  !*    tol                    : Real
  !*                           : Tolerance for PCG
  !*
  !*    mises                  : Real
  !*                           : Threshold value for von Mises stress
  !*
  !*  AUTHOR
  !*    Lee Margetts
  !*  CREATION DATE
  !*    12.06.2012
  !*  COPYRIGHT
  !*    (c) University of Manchester 2012
  !******
  !*  Place remarks that should not be included in the documentation here.
  !*  Need to add some error traps
  !*/

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)    :: job_name
  CHARACTER(LEN=15), INTENT(INOUT) :: element
  INTEGER, INTENT(IN)              :: numpe
  INTEGER, INTENT(INOUT)           :: nels,nn,nr,nod,nip,loaded_nodes
  INTEGER, INTENT(INOUT)           :: limit,mesh,fixed_freedoms,partition 
  INTEGER, INTENT(INOUT)           :: np_types 
  REAL(iwp), INTENT(INOUT)         :: mises,tol

!------------------------------------------------------------------------------
! 1. Local variables
!------------------------------------------------------------------------------

  INTEGER                          :: bufsize,ier,integer_store(11)
  REAL(iwp)                        :: real_store(2)
  CHARACTER(LEN=50)                :: fname
  
!------------------------------------------------------------------------------
! 2. Master processor reads the data and copies it into temporary arrays
!------------------------------------------------------------------------------

  IF (numpe==1) THEN
    fname = job_name(1:INDEX(job_name, " ") -1) // ".dat"
    OPEN(10,FILE=fname,STATUS='OLD',ACTION='READ')
    READ(10,*) element,mesh,partition,np_types,nels,nn,nr,nip,nod,            &
               loaded_nodes,fixed_freedoms,tol,limit,mises
    CLOSE(10)
   
    integer_store      = 0

    integer_store(1)   = mesh
    integer_store(2)   = nels
    integer_store(3)   = nn
    integer_store(4)   = nr 
    integer_store(5)   = nip
    integer_store(6)   = nod
    integer_store(7)   = loaded_nodes
    integer_store(8)   = fixed_freedoms
    integer_store(9)   = limit
    integer_store(10)  = partition
    integer_store(11)  = np_types

    real_store         = 0.0_iwp

    real_store(1)      = mises
    real_store(2)      = tol  

  END IF

!------------------------------------------------------------------------------
! 3. Master processor broadcasts the temporary arrays to the slave processors
!------------------------------------------------------------------------------

  bufsize = 11
  CALL MPI_BCAST(integer_store,bufsize,MPI_INTEGER,0,MPI_COMM_WORLD,ier)

  bufsize = 2
  CALL MPI_BCAST(real_store,bufsize,MPI_REAL8,0,MPI_COMM_WORLD,ier)

  bufsize = 15
  CALL MPI_BCAST(element,bufsize,MPI_CHARACTER,0,MPI_COMM_WORLD,ier)

!------------------------------------------------------------------------------
! 4. Slave processors extract the variables from the temporary arrays
!------------------------------------------------------------------------------

  IF (numpe/=1) THEN

    mesh            = integer_store(1)
    nels            = integer_store(2)
    nn              = integer_store(3)
    nr              = integer_store(4)
    nip             = integer_store(5)
    nod             = integer_store(6)
    loaded_nodes    = integer_store(7)
    fixed_freedoms  = integer_store(8)
    limit           = integer_store(9)
    partition       = integer_store(10)
    np_types        = integer_store(11)

    mises           = real_store(1)
    tol             = real_store(2)

  END IF

  IF(fixed_freedoms > 0 .AND. loaded_nodes > 0) THEN
    PRINT *
    PRINT *, "Error - model has", fixed_freedoms, " fixed freedoms and"
    PRINT *, loaded_nodes, " loaded nodes"
    PRINT *, "Mixed displacement and load control not supported"
    PRINT *
    CALL shutdown()
  END IF

  RETURN
  END SUBROUTINE READ_RFEMSOLVE
  
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI(argv,nlen,g_coord,g_num,element,etype,nf,loads,        &
                       nstep,npri,dtim,solid)

   !/****f* input/mesh_ensi
   !*  NAME
   !*    SUBROUTINE: mesh_ensi
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi(argv,nlen,g_coord,g_num,element,etype,nf, &
   !*                               loads,nstep,npri,dtim,solid)
   !*  FUNCTION
   !*    This subroutine outputs a set of files in the Ensight gold format.
   !*    Models in this format can be viewed in ParaView.
   !* 
   !*    Element types supported:                Tested with:
   !*
   !*    2-node bar
   !*    3-node triangle                         p51  (4th edition p51_1.dat)
   !*    6-node triangle
   !*    4-node quadrilateral                    p115 (4th edition)
   !*    8-node quadrilateral                    p116 (4th edition)
   !*    4-node tetrahedron                      p54  (4th edition p54_2.dat)
   !*    8-node hexahedron                       p86  (4th edition)       
   !*    20-node hexahedron                      p55  (4th edition)
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    npri             : print interval
   !*	 nstep            : number of time steps in analysis
   !*
   !*    Scalar reals
   !*    dtim             : time step
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*	 element          : element type
   !*
   !*    Scalar logicals
   !*    solid            : type of analysis solid if .true. fluid if .false.
   !*
   !*    Dynamic scalar arrays
   !*    g_num            : global element node numbers vector
   !*    etype            : element property type vector
   !*    nf               : nodal freedom matrix
   !* 
   !*    Dynamic real arrays
   !* 	 g_coord          : global nodal coordinates
   !*	 oldlds           : initial loads vector
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    I.M. Smith
   !*    D.V. Griffiths
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*  Subroutine required by the 5th Edition of "Programming the Finite
   !*  Element Method". Take care when modifying
   !*/

    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen,nstep,npri
    INTEGER,   INTENT(IN)         :: g_num(:,:),etype(:),nf(:,:)
    INTEGER                       :: i,j,k,l,m,n,nfe,nod,nels,ndim,nn
    INTEGER                       :: prnwidth,remainder
    REAL(iwp), INTENT(IN)         :: g_coord(:,:),loads(:),dtim
    CHARACTER(LEN=15), INTENT(IN) :: argv,element  
    LOGICAL, INTENT(IN)           :: solid
    
  !------------------------------------------------------------------------------
  ! 1. Initialisation
  !------------------------------------------------------------------------------
  
    nn   = UBOUND(g_coord,2) ; ndim = UBOUND(g_coord,1)
    nels = UBOUND(g_num,2)   ; nod  = UBOUND(g_num,1)
  
  !------------------------------------------------------------------------------
  ! 2. Write case file
  !------------------------------------------------------------------------------
  
    OPEN(12,FILE=argv(1:nlen)//'.ensi.case')
  
    WRITE(12,'(A/A)')    "#", "# Post-processing file generated by subroutine &
                               &WRITE_ENSI in "
    WRITE(12,'(A,A,/A)') "#", " Smith, Griffiths and Margetts, 'Programming the &
                               &Finite Element Method',","# Wiley, 2013."        
    WRITE(12,'(A/A/A)')  "#","# Ensight Gold Format","#"
    WRITE(12,'(2A/A)')   "# Problem name: ",argv(1:nlen),"#"
    WRITE(12,'(A/A/A)')  "FORMAT","type:  ensight gold","GEOMETRY"
    WRITE(12,'(2A/A)')   "model: 1  ",argv(1:nlen)//'.ensi.geo',"VARIABLE"
    WRITE(12,'(2A)')     "scalar per element:  material      ",                &
                          argv(1:nlen)//'.ensi.MATID'
    IF(solid) THEN
      WRITE(12,'(2A)')   "scalar per node:     restraint     ",                &
                          argv(1:nlen)//'.ensi.NDBND'
      WRITE(12,'(2A)')   "vector per node:     displacement  ",                &
                          argv(1:nlen)//'.ensi.DISPL-******'
    ELSE
      WRITE(12,'(2A)')   "scalar per node:     pressure      ",                &
                          argv(1:nlen)//'.ensi.PRESSURE-******'
    END IF
    WRITE(12,'(2A)')     "vector per node:     load          ",                &
                          argv(1:nlen)//'.ensi.NDLDS'
    WRITE(12,'(A/A)')     "TIME","time set:     1"
    WRITE(12,'(A,I5)')    "number of steps:",nstep/npri
    WRITE(12,'(A,I5)')    "filename start number:",npri
    WRITE(12,'(A,I5)')    "filename increment:",npri
    WRITE(12,'(A)')       "time values:"
    prnwidth  = 5
    remainder = mod(nstep/npri,prnwidth)
    n         = ((nstep/npri) - remainder)/prnwidth
    IF(nstep/npri<=prnwidth) THEN
      DO i=1,nstep,npri
        IF(i==nstep) THEN
          WRITE(12,'(E12.5)') i*dtim
        ELSE
          WRITE(12,'(E12.5)',ADVANCE='no') i*dtim
        END IF
      END DO
    ELSE
      IF(remainder==0) THEN
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
      ELSE
  !     DO j=1,n-1
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
        m = (n*prnwidth)+1
        l = (n*prnwidth)+remainder
        DO i=m,l
          IF(i==l) THEN
            WRITE(12,'(E12.5)') dtim*i
          ELSE
            WRITE(12,'(E12.5)',ADVANCE='no') dtim*i
          END IF
        END DO
      END IF
    END IF
   
    CLOSE(12)
  
  !------------------------------------------------------------------------------
  ! 3. Write geometry file
  !------------------------------------------------------------------------------
  
    OPEN(13,FILE=argv(1:nlen)//'.ensi.geo')
    WRITE(13,'(/2A)')   "Problem name: ", argv(1:nlen)
    WRITE(13,'(A/A/A)') "Geometry files","node id given","element id given"
    WRITE(13,'(A/A)')   "part","      1"
    IF(ndim==2) WRITE(13,'(A)') "2d-mesh"
    IF(ndim==3) WRITE(13,'(A)') "Volume Mesh"
    WRITE(13,'(A)')     "coordinates"
    
    WRITE(13,'(I10)') nn
    DO j=1,ndim
      DO i=1,nn  
        WRITE(13,'(E12.5)') g_coord(j,i)
      END DO
    END DO
  
    IF(ndim==2) THEN ! ensight requires zeros for the z-ordinate
      DO i=1,nn
        WRITE(13,'(A)') " 0.00000E+00"
      END DO
    END IF
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod)
          CASE(3)
            WRITE(13,'(A/I10)') "tria3", nels
            DO i = 1,nels
              WRITE(13,'(3I10)')g_num(3,i),g_num(2,i),g_num(1,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod)
          CASE(4)
            WRITE(13,'(A/I10)') "quad4", nels
            DO i = 1,nels
              WRITE(13,'(4I10)')g_num(1,i),g_num(4,i),g_num(3,i),g_num(2,i)
            END DO
          CASE(8)
            WRITE(13,'(A/I10)') "quad8", nels
            DO i = 1,nels
              WRITE(13,'(8I10)')g_num(1,i),g_num(7,i),g_num(5,i),g_num(3,i),    &
                                g_num(8,i),g_num(6,i),g_num(4,i),g_num(2,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod)
          CASE(8)
            WRITE(13,'(A/I10)') "hexa8", nels
            DO i = 1,nels
              WRITE(13,'(8I10)') g_num(1,i),g_num(4,i),g_num(8,i),g_num(5,i),   &
                                 g_num(2,i),g_num(3,i),g_num(7,i),g_num(6,i)
            END DO
          CASE(20)
            WRITE(13,'(A/I10)') "hexa20", nels
            DO i = 1,nels
              WRITE(13,'(20I10)')                                               &
                g_num(1,i), g_num(7,i), g_num(19,i),g_num(13,i),g_num(3,i),     &
                g_num(5,i), g_num(17,i),g_num(15,i),g_num(8,i), g_num(12,i),    &
                g_num(20,i),g_num(9,i), g_num(4,i), g_num(11,i),g_num(16,i),    &
                g_num(10,i),g_num(2,i), g_num(6,i), g_num(18,i),g_num(14,i) 
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            WRITE(13,'(A/I10)') "tetra4", nels
            DO i = 1,nels
              WRITE(13,'(4I10)') g_num(1,i),g_num(3,i),g_num(2,i),g_num(4,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE DEFAULT
        WRITE(13,'(A)')       "# Element type not recognised"
    END SELECT
  
    CLOSE(13)
  
  !------------------------------------------------------------------------------
  ! 4. Write file containing material IDs
  !------------------------------------------------------------------------------
  
    OPEN(14,FILE=argv(1:nlen)//'.ensi.MATID')
    WRITE(14,'(A)') "Alya Ensight Gold --- Scalar per-element variable file"
    WRITE(14,'(A/A)') "part", "      1"
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod) 
          CASE(3)
            WRITE(14,'(A)') "tria3"
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod) 
          CASE(4)
            WRITE(14,'(A)') "quad4"
          CASE(8)
            WRITE(14,'(A)') "quad8"
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod) 
          CASE(8)
            WRITE(14,'(A)') "hexa8"
          CASE(20)
            WRITE(14,'(A)') "hexa20"
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            WRITE(14,'(A)') "tetra4"
          CASE DEFAULT
          WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE DEFAULT
        WRITE(14,'(A)')   "# Element type not recognised"
    END SELECT
   
    DO i=1,nels; WRITE(14,'(I10)') etype(i); END DO
  
    WRITE(14,'(A)')
  
    CLOSE(14)
  
  !------------------------------------------------------------------------------
  ! 5. Write boundary conditions. Encoded using formula: 4z + 2y + 1x
  !
  !    110 = 1   010 = 2   100 = 3   011 = 4   101 = 5   001 = 6   000 = 7
  !------------------------------------------------------------------------------
  
    IF(solid) THEN
      OPEN(15,FILE=argv(1:nlen)//'.ensi.NDBND')
      WRITE(15,'(A)')     "Alya Ensight Gold --- Scalar per-node variable file"
      WRITE(15,'(A/A/A)') "part", "      1","coordinates"
      IF(ndim==3) THEN
        DO i=1,UBOUND(g_coord,2) 
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          IF(nf(3,i)==0) nfe=nfe+4
          WRITE(15,'(I2)') nfe
        END DO
      ELSE IF(ndim==2) THEN
        DO i=1,nn
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          WRITE(15,'(I2)') nfe
        END DO
      ELSE
        PRINT *, "Wrong number of dimensions in mesh_ensi"
      END IF   
    END IF
  
    CLOSE(15)
  
  !------------------------------------------------------------------------------
  ! 6. Write loaded nodes
  !------------------------------------------------------------------------------
  
    OPEN(16,FILE=argv(1:nlen)//'.ensi.NDLDS')
    WRITE(16,'(A)')     "Alya Ensight Gold --- Vector per-node variable file"
    WRITE(16,'(A/A/A)') "part", "      1","coordinates"
    DO j=1,UBOUND(nf,1)
      DO i=1, UBOUND(nf,2)
        WRITE(16,'(E12.5)') loads(nf(j,i))
      END DO
    END DO
    CLOSE(16)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI
  
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_BIN(argv,nlen,g_coord,g_num,element,etype,nf,loads,    &
                           nstep,npri,dtim,solid)

   !/****f* input/mesh_ensi_bin
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_bin
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_bin(argv,nlen,g_coord,g_num,element,      &
   !*                                   etype,nf,loads,nstep,npri,dtim,solid)
   !*  FUNCTION
   !*    This subroutine outputs a set of files in the C binary version of the 
   !*    Ensight gold format. Models in this format can be viewed in ParaView.
   !* 
   !*    Element types supported:                Tested with:
   !*
   !*    2-node bar
   !*    3-node triangle                         p51  (4th edition p51_1.dat)
   !*    6-node triangle
   !*    4-node quadrilateral                    p115 (4th edition)
   !*    8-node quadrilateral                    p116 (4th edition)
   !*    4-node tetrahedron                      p54  (4th edition p54_2.dat)
   !*    8-node hexahedron                       p86  (4th edition)       
   !*    20-node hexahedron                      p55  (4th edition)
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    npri             : print interval
   !*	 nstep            : number of time steps in analysis
   !*
   !*    Scalar reals
   !*    dtim             : time step
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*	 element          : element type
   !*
   !*    Scalar logicals
   !*    solid            : type of analysis solid if .true. fluid if .false.
   !*
   !*    Dynamic scalar arrays
   !*    g_num            : global element node numbers vector
   !*    nf               : nodal freedom matrix
   !* 
   !*    Dynamic real arrays
   !* 	 g_coord          : global nodal coordinates
   !*	 oldlds           : initial loads vector
   !*    etype            : element property type vector
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen,nstep,npri
    INTEGER,   INTENT(IN)         :: g_num(:,:),nf(:,:)
    INTEGER                       :: i,j,k,l,m,n,nfe,nod,nels,ndim,nn
    INTEGER                       :: prnwidth,remainder
    REAL(iwp), INTENT(IN)         :: g_coord(:,:),loads(:),dtim
    REAL(iwp), INTENT(IN)         :: etype(:)
    CHARACTER(LEN=15), INTENT(IN) :: argv,element  
    CHARACTER(LEN=80)             :: cbuffer
    LOGICAL, INTENT(IN)           :: solid
    
!------------------------------------------------------------------------------
! 1. Initialisation
!------------------------------------------------------------------------------
  
    nn   = UBOUND(g_coord,2) ; ndim = UBOUND(g_coord,1)
    nels = UBOUND(g_num,2)   ; nod  = UBOUND(g_num,1)
  
!------------------------------------------------------------------------------
! 2. Write case file
!------------------------------------------------------------------------------
  
    OPEN(12,FILE=argv(1:nlen)//'.bin.ensi.case')
  
    WRITE(12,'(A/A)')    "#", "# Post-processing file generated by subroutine &
                               &WRITE_ENSI in "
    WRITE(12,'(A,A,/A)') "#"," Smith, Griffiths and Margetts, 'Programming the &
                               &Finite Element Method',","# Wiley, 2013."        
    WRITE(12,'(A/A/A)')  "#","# Ensight Gold Format","#"
    WRITE(12,'(2A/A)')   "# Problem name: ",argv(1:nlen),"#"
    WRITE(12,'(A/A/A)')  "FORMAT","type:  ensight gold","GEOMETRY"
    WRITE(12,'(2A/A)')   "model: 1  ",argv(1:nlen)//'.bin.ensi.geo',"VARIABLE"
    WRITE(12,'(2A)')     "scalar per element:  material      ",                &
                          argv(1:nlen)//'.bin.ensi.MATID'
    IF(solid) THEN
      WRITE(12,'(2A)')   "scalar per node:     restraint     ",                &
                          argv(1:nlen)//'.bin.ensi.NDBND'
      WRITE(12,'(2A)')   "vector per node:     displacement  ",                &
                          argv(1:nlen)//'.bin.ensi.DISPL-******'
    ELSE
      WRITE(12,'(2A)')   "scalar per node:     pressure      ",                &
                          argv(1:nlen)//'.bin.ensi.PRESSURE-******'
    END IF
    WRITE(12,'(2A)')     "vector per node:     load          ",                &
                          argv(1:nlen)//'.bin.ensi.NDLDS'
    WRITE(12,'(A/A)')     "TIME","time set:     1"
    WRITE(12,'(A,I5)')    "number of steps:",nstep/npri
    WRITE(12,'(A,I5)')    "filename start number:",npri
    WRITE(12,'(A,I5)')    "filename increment:",npri
    WRITE(12,'(A)')       "time values:"
    prnwidth  = 5
    remainder = mod(nstep/npri,prnwidth)
    n         = ((nstep/npri) - remainder)/prnwidth
    IF(nstep/npri<=prnwidth) THEN
      DO i=1,nstep,npri
        IF(i==nstep) THEN
          WRITE(12,'(E12.5)') i*dtim
        ELSE
          WRITE(12,'(E12.5)',ADVANCE='no') i*dtim
        END IF
      END DO
    ELSE
      IF(remainder==0) THEN
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
      ELSE
  !     DO j=1,n-1
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
        m = (n*prnwidth)+1
        l = (n*prnwidth)+remainder
        DO i=m,l
          IF(i==l) THEN
            WRITE(12,'(E12.5)') dtim*i
          ELSE
            WRITE(12,'(E12.5)',ADVANCE='no') dtim*i
          END IF
        END DO
      END IF
    END IF
   
    CLOSE(12)
  
  !----------------------------------------------------------------------------
  ! 3. Write geometry file
  !
  !    Only 8 node bricks tested
  !----------------------------------------------------------------------------
  
    OPEN(13,FILE=argv(1:nlen)//'.bin.ensi.geo',STATUS="REPLACE",              &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "C Binary"                     ; WRITE(13) cbuffer
    cbuffer = "Problem name: "//argv(1:nlen) ; WRITE(13) cbuffer
    cbuffer = "Geometry files"               ; WRITE(13) cbuffer
    cbuffer = "node id off"                  ; WRITE(13) cbuffer
    cbuffer = "element id off"               ; WRITE(13) cbuffer
    cbuffer = "part"                         ; WRITE(13) cbuffer
    WRITE(13) int(1,kind=c_int)
    IF(ndim==2) THEN 
       cbuffer = "2d-mesh"                   ; WRITE(13) cbuffer
    END IF
    IF(ndim==3) THEN
       cbuffer = "Volume"                    ; WRITE(13) cbuffer
    END IF
    cbuffer = "coordinates"                  ; WRITE(13) cbuffer
    
    WRITE(13) int(nn,kind=c_int)
    DO j=1,ndim
      DO i=1,nn  
        WRITE(13) real(g_coord(j,i),kind=c_float)
      END DO
    END DO
  
    IF(ndim==2) THEN ! ensight requires zeros for the z-ordinate
      DO i=1,nn
        WRITE(13,'(A)') " 0.00000E+00" ! needs fixing for binary
      END DO
    END IF
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod)
!         CASE(3)
!           WRITE(13,'(A/I10)') "tria3", nels
!           DO i = 1,nels
!             WRITE(13,'(3I10)')g_num(3,i),g_num(2,i),g_num(1,i)
!           END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod)
          CASE(4)
            WRITE(13,'(A/I10)') "quad4", nels
            DO i = 1,nels
              WRITE(13,'(4I10)')g_num(1,i),g_num(4,i),g_num(3,i),g_num(2,i)
            END DO
          CASE(8)
            WRITE(13,'(A/I10)') "quad8", nels
            DO i = 1,nels
              WRITE(13,'(8I10)')g_num(1,i),g_num(7,i),g_num(5,i),g_num(3,i),   &
                                g_num(8,i),g_num(6,i),g_num(4,i),g_num(2,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod)
          CASE(8)
            cbuffer = "hexa8"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(4,i),kind=c_int),&
                        int(g_num(8,i),kind=c_int),int(g_num(5,i),kind=c_int),&
                        int(g_num(2,i),kind=c_int),int(g_num(3,i),kind=c_int),&
                        int(g_num(7,i),kind=c_int),int(g_num(6,i),kind=c_int)
            END DO
          CASE(20)
            cbuffer = "hexa20"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13)                                                       &
                int(g_num(1,i),kind=c_int), int(g_num(7,i),kind=c_int),       &
                int(g_num(19,i),kind=c_int),int(g_num(13,i),kind=c_int),      &
                int(g_num(3,i),kind=c_int),int(g_num(5,i),kind=c_int),        &
                int(g_num(17,i),kind=c_int),int(g_num(15,i),kind=c_int),      &
                int(g_num(8,i),kind=c_int),int(g_num(12,i),kind=c_int),       &
                int(g_num(20,i),kind=c_int),int(g_num(9,i),kind=c_int),       &
                int(g_num(4,i),kind=c_int),int(g_num(11,i),kind=c_int),       &
                int(g_num(16,i),kind=c_int),int(g_num(10,i),kind=c_int),      &
                int(g_num(2,i),kind=c_int),int(g_num(6,i),kind=c_int),        &
                int(g_num(18,i),kind=c_int),int(g_num(14,i),kind=c_int) 
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13) cbuffer
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            cbuffer = "tetra4" ; WRITE(13)
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(3,i),kind=c_int), &
                        int(g_num(2,i),kind=c_int),int(g_num(4,i),kind=c_int)
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13)
        END SELECT
      CASE DEFAULT
        cbuffer = "# Element type not recognised" ; WRITE(13)
    END SELECT
  
    CLOSE(13)
  
  !-----------------------------------------------------------------------------
  ! 4. Write file containing material IDs
  !-----------------------------------------------------------------------------
  
    OPEN(14,FILE=argv(1:nlen)//'.bin.ensi.MATID',STATUS="REPLACE",             &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "Alya Ensight Gold --- Scalar per-element variable file"
    WRITE(14) cbuffer
    cbuffer = "part"
    WRITE(14) cbuffer
    WRITE(14) int(1,kind=c_int)
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod) 
          CASE(3)
            cbuffer = "tria3" ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod) 
          CASE(4)
            cbuffer = "quad4" ; WRITE(14) cbuffer
          CASE(8)
            cbuffer = "quad8" ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod) 
          CASE(8)
            cbuffer = "hexa8"   ; WRITE(14) cbuffer
          CASE(20)
            cbuffer = "hexa20"  ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            WRITE(14,'(A)') "tetra4"
          CASE DEFAULT
          WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE DEFAULT
        WRITE(14,'(A)')   "# Element type not recognised"
    END SELECT
   
    WRITE(14) real(etype(:),kind=c_float) 
  
    CLOSE(14)
  
  !-----------------------------------------------------------------------------
  ! 5. Write boundary conditions. Encoded using formula: 4z + 2y + 1x
  !
  !    110 = 1   010 = 2   100 = 3   011 = 4   101 = 5   001 = 6   000 = 7
  !-----------------------------------------------------------------------------
  
    IF(solid) THEN

      OPEN(15,FILE=argv(1:nlen)//'.bin.ensi.NDBND',STATUS="REPLACE",           &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

      cbuffer = "Alya Ensight Gold --- Scalar per-node variable file"
      WRITE(15)
      cbuffer = "part"         ; WRITE(15)
      WRITE(15) int(1,kind=c_int)
      cbuffer = "coordinates"  ; WRITE(15)

      IF(ndim==3) THEN
        DO i=1,UBOUND(g_coord,2) 
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          IF(nf(3,i)==0) nfe=nfe+4
          WRITE(15) int(nfe,kind=c_int)
        END DO
      ELSE IF(ndim==2) THEN
        DO i=1,nn
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          WRITE(15) int(nfe,kind=c_int)
        END DO
      ELSE
        PRINT *, "Wrong number of dimensions in mesh_ensi"
      END IF   
    END IF
  
    CLOSE(15)
  
  !-----------------------------------------------------------------------------
  ! 6. Write loaded nodes
  !-----------------------------------------------------------------------------
  
    OPEN(16,FILE=argv(1:nlen)//'.bin.ensi.NDLDS',STATUS="REPLACE",             &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "Alya Ensight Gold --- Vector per-node variable file"
    WRITE(16)
    cbuffer = "part"        ; WRITE(16)
    WRITE(16) int(1,kind=c_int)
    cbuffer = "coordinates" ; WRITE(16)

    DO j=1,UBOUND(nf,1)
      DO i=1, UBOUND(nf,2)
        WRITE(16) real(loads(nf(j,i)),kind=c_float)
      END DO
    END DO
    CLOSE(16)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI_BIN

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_GEO_BIN(argv,nlen,g_coord,g_num,element)

   !/****f* input/mesh_ensi_geo_bin
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_geo_bin
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_geo_bin(argv,nlen,g_coord,g_num,element)
   !*  FUNCTION
   !*    This subroutine outputs the "geo" file in the C binary version of the 
   !*    Ensight gold format. Models in this format can be viewed in ParaView.
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*	 element          : element type
   !*
   !*    Scalar logicals
   !*
   !*    Dynamic scalar arrays
   !*    g_num            : global element node numbers vector
   !* 
   !*    Dynamic real arrays
   !* 	 g_coord          : global nodal coordinates
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !* 
   !*  Used in program p12meshgenbin
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen
    INTEGER,   INTENT(IN)         :: g_num(:,:)
    INTEGER                       :: i,j
    INTEGER                       :: nod,nels,ndim,nn
    REAL(iwp), INTENT(IN)         :: g_coord(:,:)
    CHARACTER(LEN=15), INTENT(IN) :: argv,element  
    CHARACTER(LEN=80)             :: cbuffer
    
  !----------------------------------------------------------------------------
  ! 1. Initialisation
  !----------------------------------------------------------------------------
  
    nn   = UBOUND(g_coord,2) ; ndim = UBOUND(g_coord,1)
    nels = UBOUND(g_num,2)   ; nod  = UBOUND(g_num,1)
  
  !----------------------------------------------------------------------------
  ! 2. Write geometry file
  !
  !    Only 8 node bricks tested
  !----------------------------------------------------------------------------
  
    OPEN(13,FILE=argv(1:nlen)//'.bin.ensi.geo',STATUS="REPLACE",              &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "C Binary"                     ; WRITE(13) cbuffer
    cbuffer = "Problem name: "//argv(1:nlen) ; WRITE(13) cbuffer
    cbuffer = "Geometry files"               ; WRITE(13) cbuffer
    cbuffer = "node id off"                  ; WRITE(13) cbuffer
    cbuffer = "element id off"               ; WRITE(13) cbuffer
    cbuffer = "part"                         ; WRITE(13) cbuffer
    WRITE(13) int(1,kind=c_int)
    IF(ndim==2) THEN 
       cbuffer = "2d-mesh"                   ; WRITE(13) cbuffer
    END IF
    IF(ndim==3) THEN
       cbuffer = "Volume"                    ; WRITE(13) cbuffer
    END IF
    cbuffer = "coordinates"                  ; WRITE(13) cbuffer
    
    WRITE(13) int(nn,kind=c_int)
    DO j=1,ndim
      DO i=1,nn  
        WRITE(13) real(g_coord(j,i),kind=c_float)
      END DO
    END DO
  
    IF(ndim==2) THEN ! ensight requires zeros for the z-ordinate
      DO i=1,nn
        WRITE(13,'(A)') " 0.00000E+00" ! needs fixing for binary
      END DO
    END IF
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod)
!         CASE(3)
!           WRITE(13,'(A/I10)') "tria3", nels
!           DO i = 1,nels
!             WRITE(13,'(3I10)')g_num(3,i),g_num(2,i),g_num(1,i)
!           END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod)
          CASE(4)
            WRITE(13,'(A/I10)') "quad4", nels
            DO i = 1,nels
              WRITE(13,'(4I10)')g_num(1,i),g_num(4,i),g_num(3,i),g_num(2,i)
            END DO
          CASE(8)
            WRITE(13,'(A/I10)') "quad8", nels
            DO i = 1,nels
              WRITE(13,'(8I10)')g_num(1,i),g_num(7,i),g_num(5,i),g_num(3,i),  &
                                g_num(8,i),g_num(6,i),g_num(4,i),g_num(2,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod)
          CASE(8)
            cbuffer = "hexa8"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(4,i),kind=c_int),&
                        int(g_num(8,i),kind=c_int),int(g_num(5,i),kind=c_int),&
                        int(g_num(2,i),kind=c_int),int(g_num(3,i),kind=c_int),&
                        int(g_num(7,i),kind=c_int),int(g_num(6,i),kind=c_int)
            END DO
          CASE(20)
            cbuffer = "hexa20"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13)                                                       &
                int(g_num(1,i),kind=c_int), int(g_num(7,i),kind=c_int),       &
                int(g_num(19,i),kind=c_int),int(g_num(13,i),kind=c_int),      &
                int(g_num(3,i),kind=c_int),int(g_num(5,i),kind=c_int),        &
                int(g_num(17,i),kind=c_int),int(g_num(15,i),kind=c_int),      &
                int(g_num(8,i),kind=c_int),int(g_num(12,i),kind=c_int),       &
                int(g_num(20,i),kind=c_int),int(g_num(9,i),kind=c_int),       &
                int(g_num(4,i),kind=c_int),int(g_num(11,i),kind=c_int),       &
                int(g_num(16,i),kind=c_int),int(g_num(10,i),kind=c_int),      &
                int(g_num(2,i),kind=c_int),int(g_num(6,i),kind=c_int),        &
                int(g_num(18,i),kind=c_int),int(g_num(14,i),kind=c_int) 
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13) cbuffer
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            cbuffer = "tetra4" ; WRITE(13)
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(3,i),kind=c_int),&
                        int(g_num(2,i),kind=c_int),int(g_num(4,i),kind=c_int)
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13)
        END SELECT
      CASE DEFAULT
        cbuffer = "# Element type not recognised" ; WRITE(13)
    END SELECT
  
    CLOSE(13)

    RETURN
  
  END SUBROUTINE MESH_ENSI_GEO_BIN

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_MATID_BIN(argv,nlen,nod,element,etype)

   !/****f* input/mesh_ensi_matid_bin
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_matid_bin
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_matid_bin(argv,nlen,nod,element,etype)
   !*  FUNCTION
   !*    This subroutine outputs material type for each element in the mesh
   !*    in the C binary version of the Ensight gold format. Models in this 
   !*    format can be viewed in ParaView.
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    nod              : number of nodes in the element
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*	 element          : element type
   !*
   !*    Dynamic scalar arrays
   !*    etype            : element property type vector
   !* 
   !*  OUTPUTS
   !*  AUTHOR
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*  ParaView has a bug which prevents the 'Material' section of the 
   !*  ENSIGHT Gold to be read and therefore integer MATID values
   !*  http://www.paraview.org/Bug/view.php?id=15151
   !*  http://www3.ensight.com/EnSight10_Docs/UserManual.pdf pp.713
   !*  Workaround - use MATIDs as reals and convert into int for ParaFEM
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen,nod
    REAL(iwp), INTENT(IN)         :: etype(:)
    CHARACTER(LEN=15), INTENT(IN) :: argv,element  
    CHARACTER(LEN=80)             :: cbuffer
  
!------------------------------------------------------------------------------
! 1. Write file containing material IDs
!------------------------------------------------------------------------------
  
    OPEN(14,FILE=argv(1:nlen)//'.bin.ensi.MATID',STATUS="REPLACE",            &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "Alya Ensight Gold --- Scalar per-element variable file"
    WRITE(14) cbuffer
    cbuffer = "part"
    WRITE(14) cbuffer
    WRITE(14) int(1,kind=c_int)
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod) 
          CASE(3)
            cbuffer = "tria3" ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod) 
          CASE(4)
            cbuffer = "quad4" ; WRITE(14) cbuffer
          CASE(8)
            cbuffer = "quad8" ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod) 
          CASE(8)
            cbuffer = "hexa8"   ; WRITE(14) cbuffer
          CASE(20)
            cbuffer = "hexa20"  ; WRITE(14) cbuffer
          CASE DEFAULT
            WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            WRITE(14,'(A)') "tetra4"
          CASE DEFAULT
          WRITE(14,'(A)') "# Element type not recognised"
        END SELECT
      CASE DEFAULT
        WRITE(14,'(A)')   "# Element type not recognised"
    END SELECT
   
    WRITE(14) real(etype(:),kind=c_float) 
  
    CLOSE(14)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI_MATID_BIN
  
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_NDBND_BIN(argv,nf,nlen,nod,solid)

   !/****f* input/mesh_ensi_ndbnd_bin
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_ndbnd_bin
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_ndbnd_bin(argv,nf,nlen,nod,solid)
   !*  FUNCTION
   !*    This subroutine outputs a file of restrained nodes in the C binary 
   !*    version of the Ensight gold format. Models in this format can be 
   !*    viewed in ParaView.
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    nod              : number of nodes per element
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*
   !*    Scalar logicals
   !*    solid            : type of analysis solid if .true. fluid if .false.
   !*
   !*    Dynamic scalar arrays
   !*    nf               : nodal freedom matrix
   !*  OUTPUTS
   !*    File: <job_name>.bin.ensi.NDBND 
   !*  AUTHOR
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen,nod
    INTEGER,   INTENT(IN)         :: nf(:,:)
    INTEGER                       :: i,nfe,nn,ndim
    CHARACTER(LEN=15), INTENT(IN) :: argv  
    CHARACTER(LEN=80)             :: cbuffer
    LOGICAL, INTENT(IN)           :: solid
    
  !------------------------------------------------------------------------------
  ! 1. Initialisation
  !------------------------------------------------------------------------------
  
    ndim = UBOUND(nf,1)-1  
    nn   = UBOUND(nf,2)
  
  !------------------------------------------------------------------------------
  ! 2. Write boundary conditions. Encoded using formula: 4z + 2y + 1x
  !
  !    110 = 1   010 = 2   100 = 3   011 = 4   101 = 5   001 = 6   000 = 7
  !-----------------------------------------------------------------------------
  
    IF(solid) THEN

      OPEN(15,FILE=argv(1:nlen)//'.bin.ensi.NDBND',STATUS="REPLACE",           &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

      cbuffer = "Alya Ensight Gold --- Scalar per-node variable file"
      WRITE(15)
      cbuffer = "part"         ; WRITE(15)
      WRITE(15) int(1,kind=c_int)
      cbuffer = "coordinates"  ; WRITE(15)

      IF(ndim==3) THEN
        DO i=1,nod 
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          IF(nf(3,i)==0) nfe=nfe+4
          WRITE(15) int(nfe,kind=c_int)
        END DO
      ELSE IF(ndim==2) THEN
        DO i=1,nn
          nfe=0
          IF(nf(1,i)==0) nfe=nfe+1
          IF(nf(2,i)==0) nfe=nfe+2
          WRITE(15) int(nfe,kind=c_int)
        END DO
      ELSE
        PRINT *, "Wrong number of dimensions in mesh_ensi"
      END IF   
    END IF
  
    CLOSE(15)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI_NDBND_BIN

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_NDLDS_BIN(argv,nlen,nf,loads)

   !/****f* input/mesh_ensi_ndlds_bin
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_ndlds_bin
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_ndlds_bin(argv,nlen,nf,loads)
   !*  FUNCTION
   !*    This subroutine outputs a file of loads in the C binary version of the 
   !*    Ensight gold format. Models in this format can be viewed in ParaView.
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*
   !*    Dynamic scalar arrays
   !*    nf               : nodal freedom matrix
   !* 
   !*    Dynamic real arrays
   !*	 oldlds           : initial loads vector
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen
    INTEGER,   INTENT(IN)         :: nf(:,:)
    INTEGER                       :: i,j
    REAL(iwp), INTENT(IN)         :: loads(:)
    CHARACTER(LEN=15), INTENT(IN) :: argv
    CHARACTER(LEN=80)             :: cbuffer
    
  !-----------------------------------------------------------------------------
  ! 1. Write loaded nodes
  !-----------------------------------------------------------------------------
  
    OPEN(16,FILE=argv(1:nlen)//'.bin.ensi.NDLDS',STATUS="REPLACE",             &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "Alya Ensight Gold --- Vector per-node variable file"
    WRITE(16)
    cbuffer = "part"        ; WRITE(16)
    WRITE(16) int(1,kind=c_int)
    cbuffer = "coordinates" ; WRITE(16)

    DO j=1,UBOUND(nf,1)
      DO i=1, UBOUND(nf,2)
        WRITE(16) real(loads(nf(j,i)),kind=c_float)
      END DO
    END DO
    CLOSE(16)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI_NDLDS_BIN

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_CASE(argv,nlen,nstep,npri,dtim,solid)

   !/****f* input/mesh_ensi_case
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_case
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_case(argv,nlen,nstep,npri,dtim,solid)
   !*  FUNCTION
   !*    This subroutine outputs the "case" file required for the C binary 
   !*    version of the Ensight gold format. Models in this format can be 
   !*    viewed in ParaView.
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    npri             : print interval
   !*	 nstep            : number of time steps in analysis
   !*
   !*    Scalar reals
   !*    dtim             : time step
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*
   !*    Scalar logicals
   !*    solid            : type of analysis solid if .true. fluid if .false.
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    I.M. Smith
   !*    D.V. Griffiths
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen,nstep,npri
    INTEGER                       :: i,j,k,l,m,n,nfe,nod,nels,ndim,nn
    INTEGER                       :: prnwidth,remainder
    REAL(iwp), INTENT(IN)         :: dtim
    CHARACTER(LEN=15), INTENT(IN) :: argv
    CHARACTER(LEN=80)             :: cbuffer
    LOGICAL, INTENT(IN)           :: solid
    
  !-----------------------------------------------------------------------------
  ! 1. Write case file
  !-----------------------------------------------------------------------------
  
    OPEN(12,FILE=argv(1:nlen)//'.bin.ensi.case')
  
    WRITE(12,'(A/A)')    "#", "# Post-processing file generated by subroutine  &
                               &WRITE_ENSI in "
    WRITE(12,'(A,A,/A)') "#"," Smith, Griffiths and Margetts, 'Programming the &
                               &Finite Element Method',","# Wiley, 2013."        
    WRITE(12,'(A/A/A)')  "#","# Ensight Gold Format","#"
    WRITE(12,'(2A/A)')   "# Problem name: ",argv(1:nlen),"#"
    WRITE(12,'(A/A/A)')  "FORMAT","type:  ensight gold","GEOMETRY"
    WRITE(12,'(2A/A)')   "model: 1  ",argv(1:nlen)//'.bin.ensi.geo',"VARIABLE"
    WRITE(12,'(2A)')     "scalar per element:  material      ",                &
                          argv(1:nlen)//'.bin.ensi.MATID'
    IF(solid) THEN
      WRITE(12,'(2A)')   "scalar per node:     restraint     ",                &
                          argv(1:nlen)//'.bin.ensi.NDBND'
      WRITE(12,'(2A)')   "vector per node:     displacement  ",                &
                          argv(1:nlen)//'.bin.ensi.DISPL-******'
    ELSE
      WRITE(12,'(2A)')   "scalar per node:     pressure      ",                &
                          argv(1:nlen)//'.bin.ensi.PRESSURE-******'
    END IF
    WRITE(12,'(2A)')     "vector per node:     load          ",                &
                          argv(1:nlen)//'.bin.ensi.NDLDS'
    WRITE(12,'(A/A)')     "TIME","time set:     1"
    WRITE(12,'(A,I5)')    "number of steps:",nstep/npri
    WRITE(12,'(A,I5)')    "filename start number:",npri
    WRITE(12,'(A,I5)')    "filename increment:",npri
    WRITE(12,'(A)')       "time values:"
    prnwidth  = 5
    remainder = mod(nstep/npri,prnwidth)
    n         = ((nstep/npri) - remainder)/prnwidth
    IF(nstep/npri<=prnwidth) THEN
      DO i=1,nstep,npri
        IF(i==nstep) THEN
          WRITE(12,'(E12.5)') i*dtim
        ELSE
          WRITE(12,'(E12.5)',ADVANCE='no') i*dtim
        END IF
      END DO
    ELSE
      IF(remainder==0) THEN
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
      ELSE
  !     DO j=1,n-1
        DO j=1,n
          m = ((j-1)*prnwidth)+1
          l = ((j-1)*prnwidth)+prnwidth
          WRITE(12,'(5E12.5)') (k*dtim,k=m,l)
        END DO
        m = (n*prnwidth)+1
        l = (n*prnwidth)+remainder
        DO i=m,l
          IF(i==l) THEN
            WRITE(12,'(E12.5)') dtim*i
          ELSE
            WRITE(12,'(E12.5)',ADVANCE='no') dtim*i
          END IF
        END DO
      END IF
    END IF
   
    CLOSE(12)
    
    RETURN
  
  END SUBROUTINE MESH_ENSI_CASE
  
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE MESH_ENSI_GEO(argv,nlen,g_coord,g_num,element)

   !/****f* input/mesh_ensi_geo
   !*  NAME
   !*    SUBROUTINE: mesh_ensi_geo
   !*  SYNOPSIS
   !*    Usage:      CALL mesh_ensi_geo(argv,nlen,g_coord,g_num,element)
   !*  FUNCTION
   !*    This subroutine outputs a set of files in the C binary version of the 
   !*    Ensight gold format. Models in this format can be viewed in ParaView.
   !* 
   !*    Element types supported:                Tested with:
   !*
   !*    2-node bar
   !*    3-node triangle                         p51  (4th edition p51_1.dat)
   !*    6-node triangle
   !*    4-node quadrilateral                    p115 (4th edition)
   !*    8-node quadrilateral                    p116 (4th edition)
   !*    4-node tetrahedron                      p54  (4th edition p54_2.dat)
   !*    8-node hexahedron                       p86  (4th edition)       
   !*    20-node hexahedron                      p55  (4th edition)
   !*  INPUTS
   !*    Scalar integers
   !*    nlen             : number of characters in data file base name
   !*    npri             : print interval
   !*	 nstep            : number of time steps in analysis
   !*
   !*    Scalar reals
   !*    dtim             : time step
   !*
   !*    Scalar characters
   !*    argv             : holds data file base name
   !*	 element          : element type
   !*
   !*    Scalar logicals
   !*    solid            : type of analysis solid if .true. fluid if .false.
   !*
   !*    Dynamic scalar arrays
   !*    g_num            : global element node numbers vector
   !* 
   !*    Dynamic real arrays
   !* 	 g_coord          : global nodal coordinates
   !*
   !*  OUTPUTS
   !*  AUTHOR
   !*    I.M. Smith
   !*    D.V. Griffiths
   !*    L. Margetts
   !*  COPYRIGHT
   !*    (c) University of Manchester 2004-2014
   !******
   !*  Place remarks that should not be included in the documentation here.
   !*
   !*/

    USE, INTRINSIC :: ISO_C_BINDING
    
    IMPLICIT none
  
    INTEGER,PARAMETER             :: iwp=SELECTED_REAL_KIND(15)
    INTEGER,   INTENT(IN)         :: nlen
    INTEGER,   INTENT(IN)         :: g_num(:,:)
    INTEGER                       :: i,j,k,l,m,n,nod,nels,ndim,nn
    REAL(iwp), INTENT(IN)         :: g_coord(:,:)
    CHARACTER(LEN=15), INTENT(IN) :: argv,element  
    CHARACTER(LEN=80)             :: cbuffer
    
  !----------------------------------------------------------------------------
  ! 1. Initialisation
  !----------------------------------------------------------------------------
  
    nn   = UBOUND(g_coord,2) ; ndim = UBOUND(g_coord,1)
    nels = UBOUND(g_num,2)   ; nod  = UBOUND(g_num,1)
  
  !----------------------------------------------------------------------------
  ! 2. Write geometry file
  !
  !    Only 8 node bricks tested
  !----------------------------------------------------------------------------
  
    OPEN(13,FILE=argv(1:nlen)//'.bin.ensi.geo',STATUS="REPLACE",              &
                 FORM="UNFORMATTED", ACTION="WRITE", ACCESS="STREAM")

    cbuffer = "C Binary"                     ; WRITE(13) cbuffer
    cbuffer = "Problem name: "//argv(1:nlen) ; WRITE(13) cbuffer
    cbuffer = "Geometry files"               ; WRITE(13) cbuffer
    cbuffer = "node id off"                  ; WRITE(13) cbuffer
    cbuffer = "element id off"               ; WRITE(13) cbuffer
    cbuffer = "part"                         ; WRITE(13) cbuffer
    WRITE(13) int(1,kind=c_int)
    IF(ndim==2) THEN 
       cbuffer = "2d-mesh"                   ; WRITE(13) cbuffer
    END IF
    IF(ndim==3) THEN
       cbuffer = "Volume"                    ; WRITE(13) cbuffer
    END IF
    cbuffer = "coordinates"                  ; WRITE(13) cbuffer
    
    WRITE(13) int(nn,kind=c_int)
    DO j=1,ndim
      DO i=1,nn  
        WRITE(13) real(g_coord(j,i),kind=c_float)
      END DO
    END DO
  
    IF(ndim==2) THEN ! ensight requires zeros for the z-ordinate
      DO i=1,nn
        WRITE(13,'(A)') " 0.00000E+00" ! needs fixing for binary
      END DO
    END IF
  
    SELECT CASE(element)
      CASE('triangle')
        SELECT CASE(nod)
!         CASE(3)
!           WRITE(13,'(A/I10)') "tria3", nels
!           DO i = 1,nels
!             WRITE(13,'(3I10)')g_num(3,i),g_num(2,i),g_num(1,i)
!           END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('quadrilateral')
        SELECT CASE(nod)
          CASE(4)
            WRITE(13,'(A/I10)') "quad4", nels
            DO i = 1,nels
              WRITE(13,'(4I10)')g_num(1,i),g_num(4,i),g_num(3,i),g_num(2,i)
            END DO
          CASE(8)
            WRITE(13,'(A/I10)') "quad8", nels
            DO i = 1,nels
              WRITE(13,'(8I10)')g_num(1,i),g_num(7,i),g_num(5,i),g_num(3,i),   &
                                g_num(8,i),g_num(6,i),g_num(4,i),g_num(2,i)
            END DO
          CASE DEFAULT
            WRITE(13,'(A)')   "# Element type not recognised"
        END SELECT
      CASE('hexahedron')
        SELECT CASE(nod)
          CASE(8)
            cbuffer = "hexa8"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(4,i),kind=c_int),&
                        int(g_num(8,i),kind=c_int),int(g_num(5,i),kind=c_int),&
                        int(g_num(2,i),kind=c_int),int(g_num(3,i),kind=c_int),&
                        int(g_num(7,i),kind=c_int),int(g_num(6,i),kind=c_int)
            END DO
          CASE(20)
            cbuffer = "hexa20"       ; WRITE(13) cbuffer
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13)                                                       &
                int(g_num(1,i),kind=c_int), int(g_num(7,i),kind=c_int),       &
                int(g_num(19,i),kind=c_int),int(g_num(13,i),kind=c_int),      &
                int(g_num(3,i),kind=c_int),int(g_num(5,i),kind=c_int),        &
                int(g_num(17,i),kind=c_int),int(g_num(15,i),kind=c_int),      &
                int(g_num(8,i),kind=c_int),int(g_num(12,i),kind=c_int),       &
                int(g_num(20,i),kind=c_int),int(g_num(9,i),kind=c_int),       &
                int(g_num(4,i),kind=c_int),int(g_num(11,i),kind=c_int),       &
                int(g_num(16,i),kind=c_int),int(g_num(10,i),kind=c_int),      &
                int(g_num(2,i),kind=c_int),int(g_num(6,i),kind=c_int),        &
                int(g_num(18,i),kind=c_int),int(g_num(14,i),kind=c_int) 
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13) cbuffer
        END SELECT
      CASE('tetrahedron')
        SELECT CASE(nod)
          CASE(4)
            cbuffer = "tetra4" ; WRITE(13)
            WRITE(13) int(nels,kind=c_int)
            DO i = 1,nels
              WRITE(13) int(g_num(1,i),kind=c_int),int(g_num(3,i),kind=c_int), &
                        int(g_num(2,i),kind=c_int),int(g_num(4,i),kind=c_int)
            END DO
          CASE DEFAULT
            cbuffer = "# Element type not recognised" ; WRITE(13)
        END SELECT
      CASE DEFAULT
        cbuffer = "# Element type not recognised" ; WRITE(13)
    END SELECT
  
    CLOSE(13)
  
    RETURN
  
  END SUBROUTINE MESH_ENSI_GEO
    
END MODULE INPUT

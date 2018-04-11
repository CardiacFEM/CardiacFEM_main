MODULE LOADING

  !/****h* /loading
  !*  NAME
  !*    MODULE: loading
  !*  SYNOPSIS
  !*    Usage:      USE loading
  !*  FUNCTION
  !*    Contains subroutines used for loading. These subroutines are parallel 
  !*    and require MPI.
  !*    
  !*    Subroutine             Purpose
  !*
  !*    READ_LOADS             Reads the information about the loads
  !*    LOADS                  Creates the distributed applied loads vector
  !*
  !*  AUTHOR
  !*    L. Margetts
  !*    I.M. Smith
  !*    D.V. Griffiths
  !*  COPYRIGHT
  !*    2004-2010 University of Manchester
  !******
  !*  Place remarks that should not be included in the documentation here.
  !*
  !*/
  
  USE precision
  USE gather_scatter

  CONTAINS

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  SUBROUTINE LOADS(nn_start,g_num_pp,load_node,load_value,nf_pp,fext_pp)

    !/****f* loading/loads
    !*  NAME
    !*    SUBROUTINE: loads
    !*  SYNOPSIS
    !*    Usage:      CALL loads(g_num_pp,load_node,load_value,nf_pp,         &
    !*                            nn_start,fext_pp)
    !*  FUNCTION
    !*    Build the distributed array (vector) containing the natural boundary
    !*    conditions (forces in mechanics, fluxes in thermal problems)
    !*  
    !*  INPUTS
    !*    The following arguments have the INTENT(IN) attribute:
    !*
    !*    nn_start                : Integer
    !*                            : Lowest node number in the process
    !*
    !*    g_num_pp(nod,nels_pp)   : Integer
    !*                            : Elements connectivity
    !*
    !*    load_node(loaded_nodes) : Integer
    !*                            : Node numbers that have a load imposed
    !*                          
    !*    load_value(ndim,loaded_nodes) : Real
    !*                                  : Value of the force at each loaded node
    !*                          
    !*    nf_pp(nodof,nn_pp)      : Integer
    !*                            : Nodes where the equation number has been
    !*                              assigned to each the degree of freedom of
    !*                              the nodes
    !*
    !*    The following arguments have the INTENT(OUT) attribute:
    !*                          
    !*    fext_pp(neq_pp)         : Real
    !*                            : Natural boundary conditions vector
    !*  AUTHOR
    !*    *** ****
    !*  CREATION DATE
    !*    **.**.****
    !*  COPYRIGHT
    !*    (c) University of Manchester 2007-2011
    !******

    IMPLICIT NONE

    INTEGER, INTENT(IN)    :: nn_start, g_num_pp(:,:), load_node(:), nf_pp(:,:)
    REAL(iwp), INTENT(IN)  :: load_value(:,:)
    REAL(iwp), INTENT(OUT) :: fext_pp(:)
    INTEGER :: i, j, k, l, nels_pp, idx, loaded_nodes, ntot, nodof, nod, nn_pp
    REAL(iwp), ALLOCATABLE :: r_temp(:,:)

    loaded_nodes = UBOUND(load_node,1)
    nod          = UBOUND(g_num_pp,1)
    nels_pp      = UBOUND(g_num_pp,2)
    nodof        = UBOUND(nf_pp,1)
    nn_pp        = UBOUND(nf_pp,2)
    ntot         = nod*nodof

    ALLOCATE(r_temp(ntot,nels_pp))

    r_temp = 0

    DO i = 1, nels_pp
      DO j = 1, nod
        DO k = 1, loaded_nodes
          IF (g_num_pp(j,i) == load_node(k)) THEN  !if it's a loaded node
            DO l = 1, nodof
              IF (nf_pp(l, load_node(k)-nn_start+1) /= 0) THEN
                r_temp((j-1)*nodof+l,i) = load_value(l,k)
              END IF
            END DO
          END IF
        END DO
      END DO
    END DO

    CALL SCATTER_NOADD(r_temp,fext_pp)

    DEALLOCATE(r_temp)

  END SUBROUTINE LOADS

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
END MODULE LOADING

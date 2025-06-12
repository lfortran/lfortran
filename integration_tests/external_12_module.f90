module external_12_module1
    LOGICAL(4), ALLOCATABLE, DIMENSION(:) :: inrdone
end module external_12_module1

MODULE external_12_module2

  use external_12_module1, only: inrdone
  REAL(4), PARAMETER :: one  = 1.0

  PUBLIC :: inner

  CONTAINS

  SUBROUTINE inner ( )

    REAL(4), ALLOCATABLE, DIMENSION(:) :: dfmxi
    REAL(4) :: epsi=1.0E-4

    ALLOCATE( dfmxi(5) )
    ALLOCATE( inrdone(5) )

    dfmxi = -one
    WHERE( dfmxi <= epsi ) inrdone = .TRUE.

    print *, "inrdone: ", inrdone
    if (any(inrdone .neqv. .TRUE.)) error stop

  END SUBROUTINE inner

END MODULE external_12_module2

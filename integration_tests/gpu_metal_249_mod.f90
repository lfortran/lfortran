! A `module procedure` whose implementation lives in a submodule reaches
! its caller through the parent module's interface declaration.  See
! gpu_metal_249.f90 for what this exercises.
module gpu_metal_249_mod
implicit none

type :: op_t
    real, allocatable :: w(:)
end type

interface
    pure module function apply(o, v, k) result(r)
    type(op_t), intent(in) :: o
    real, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    end function

    pure module function twice(o, v, k) result(r)
    type(op_t), intent(in) :: o
    real, intent(in) :: v(:)
    integer, intent(in) :: k
    real, allocatable :: r(:)
    end function
end interface

end module

submodule (gpu_metal_249_mod) gpu_metal_249_smod
implicit none
contains

    ! `o%w * v` and the array constructor around it are both temporaries
    ! sized from the extent of an allocatable component of a dummy, so
    ! this callee can only reach the shader spliced into the kernel.
    module procedure apply
    r = [real(100*k), o%w * v, real(100*k)]
    end procedure

    ! A second `module procedure` in the same submodule, so that the
    ! caching of the resolved implementation is exercised for more than
    ! one name.
    module procedure twice
    r = [real(10*k), 2.0 * (o%w * v), real(10*k)]
    end procedure

end submodule

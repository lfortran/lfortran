! Used by save_48.f90.
module save_48_m
implicit none
interface
    module subroutine save_48_sub_counter(x, n)
    real, intent(in) :: x(:)
    integer, intent(out) :: n
    end subroutine
end interface
contains
subroutine save_48_counter(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k + 0*size(x)
end subroutine
end module

submodule (save_48_m) save_48_s
implicit none
contains
module subroutine save_48_sub_counter(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k + 0*size(x)
end subroutine
end submodule

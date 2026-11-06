! Zero extent of an explicit-shape result through an interface body: an
! external procedure, a separate module procedure and a dummy procedure.
module automatic_array_04_m
implicit none
interface
    module function mseq(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
    end function
end interface
abstract interface
    function seq_iface(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
    end function
end interface
contains
    real function total(f, n)
        procedure(seq_iface) :: f
        integer, intent(in) :: n
        total = sum(f(n)) + 1000*size(f(n))
    end function
end module

submodule (automatic_array_04_m) automatic_array_04_s
implicit none
contains
    module function mseq(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
        r = 7
    end function
end submodule

function ext_seq(n) result(r)
    implicit none
    integer, intent(in) :: n
    real :: r(n)
    r = 7
end function

program automatic_array_04
use automatic_array_04_m, only: mseq, total
implicit none
interface
    function ext_seq(n) result(r)
        integer, intent(in) :: n
        real :: r(n)
    end function
end interface
procedure(ext_seq), pointer :: p
integer :: k
k = -3
p => ext_seq
print *, sum(ext_seq(-3)), sum(mseq(-3)), sum(p(-3)), total(ext_seq, -3)
if (sum(ext_seq(-3)) /= 0 .or. size(ext_seq(-3)) /= 0) error stop 1
if (sum(ext_seq(k)) /= 0 .or. size(ext_seq(k)) /= 0) error stop 2
if (sum(ext_seq(2)) /= 14) error stop 3
if (sum(mseq(-3)) /= 0 .or. size(mseq(-3)) /= 0) error stop 4
if (sum(mseq(k)) /= 0 .or. size(mseq(k)) /= 0) error stop 5
if (sum(mseq(2)) /= 14) error stop 6
if (sum(p(-3)) /= 0 .or. size(p(-3)) /= 0) error stop 7
if (sum(p(k)) /= 0 .or. sum(p(2)) /= 14) error stop 8
if (total(ext_seq, -3) /= 0 .or. total(ext_seq, k) /= 0) error stop 9
if (total(mseq, -3) /= 0 .or. total(mseq, 2) /= 2014) error stop 10
end program

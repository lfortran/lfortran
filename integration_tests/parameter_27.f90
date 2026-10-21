! Elements of named constant arrays passed as actual arguments to dummies
! without the VALUE attribute, and to bind(c) VALUE dummies.
module parameter_27_m
use iso_c_binding, only: c_int, c_double
implicit none
type :: t
    integer :: a(3)
end type
type :: obj
    integer :: k = 1
contains
    procedure :: get
end type
integer, parameter :: ip(4) = [10, 20, 30, 40]
integer, parameter :: ip2(2,2) = reshape([1, 2, 3, 4], [2, 2])
integer(c_int), parameter :: icp(3) = [1, 2, 3]
real(c_double), parameter :: dcp(2) = [0.5_c_double, 1.25_c_double]
real, parameter :: rp(2) = [1.5, 2.5]
complex, parameter :: cp(2) = [(1.0, 2.0), (3.0, 4.0)]
logical, parameter :: lp(2) = [.true., .false.]
character(len=3), parameter :: chp(2) = ["abc", "def"]
type(t), parameter :: tp(2) = [t([1, 2, 3]), t([4, 5, 6])]
contains
integer function fi(n)
    integer, intent(in) :: n
    fi = n
end function
integer function fu(n)
    integer :: n
    fu = n
end function
elemental integer function fe(n)
    integer, intent(in) :: n
    fe = n + 9
end function
integer function fo(n)
    integer, intent(in), optional :: n
    fo = -1
    if (present(n)) fo = n
end function
subroutine si(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    r = n
end subroutine
real function fr(x)
    real, intent(in) :: x
    fr = x
end function
complex function fc(x)
    complex, intent(in) :: x
    fc = x
end function
logical function fl(x)
    logical, intent(in) :: x
    fl = x
end function
character(len=3) function fch(x)
    character(len=3), intent(in) :: x
    fch = x
end function
integer function ft(x)
    type(t), intent(in) :: x
    ft = sum(x%a)
end function
integer function fany(x)
    class(*), intent(in) :: x
    fany = -1
    select type (x)
    type is (integer)
        fany = x
    end select
end function
integer(c_int) function fbc(n) bind(c)
    integer(c_int), intent(in) :: n
    fbc = n
end function
integer(c_int) function fvi(n) bind(c)
    integer(c_int), value, intent(in) :: n
    fvi = n
end function
real(c_double) function fvd(x) bind(c)
    real(c_double), value, intent(in) :: x
    fvd = x
end function
integer function get(self, n)
    class(obj), intent(in) :: self
    integer, intent(in) :: n
    get = self%k + n
end function
end module

program parameter_27
use parameter_27_m
implicit none
interface
    integer function fp_if(n)
        integer, intent(in) :: n
    end function
end interface
procedure(fp_if), pointer :: fp
type(obj) :: o
integer :: r
fp => fi
if (fi(ip(4)) /= 40) error stop 1
if (fu(ip(3)) /= 30) error stop 2
if (fe(ip(4)) /= 49) error stop 3
if (fo(ip(2)) /= 20) error stop 4
call si(ip(2), r)
if (r /= 20) error stop 5
if (fi(ip2(2,2)) /= 4) error stop 6
if (fr(rp(2)) /= 2.5) error stop 7
if (fc(cp(2)) /= (3.0, 4.0)) error stop 8
if (.not. fl(lp(1))) error stop 9
if (fch(chp(2)) /= "def") error stop 10
if (ft(tp(2)) /= 15) error stop 11
if (fi(tp(2)%a(3)) /= 6) error stop 12
if (fany(ip(1)) /= 10) error stop 13
if (fbc(icp(3)) /= 3) error stop 14
if (fvi(icp(2)) /= 2) error stop 18
if (fvd(dcp(2)) /= 1.25_c_double) error stop 19
if (o%get(ip(1)) /= 11) error stop 15
if (fp(ip(3)) /= 30) error stop 16
call implicit_ext(ip(4))
print *, "ok"
end program

subroutine implicit_ext(n)
integer n
if (n /= 40) error stop 17
end subroutine

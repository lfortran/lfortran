! A `procedure(<type-spec>)` dummy keeps the kind of its type-spec: its
! references return `real(8)`, `integer(8)`, `complex(8)`, ... and not the
! default kind.
double precision function apply_dp(f, x)
double precision, intent(in) :: x
procedure(double precision) :: f
apply_dp = 2d0 * f(x) + f(x + 1d0)
end function

doubleprecision function apply_dp_token(f, x)
doubleprecision, intent(in) :: x
procedure(doubleprecision) :: f
apply_dp_token = f(x)
end function

real(8) function apply_r8(f, x)
implicit none
real(8), intent(in) :: x
procedure(real(8)) :: f
apply_r8 = f(x)
end function

real(8) function apply_r8_kind(f, x)
implicit none
real(8), intent(in) :: x
procedure(real(kind=8)) :: f
apply_r8_kind = f(x)
end function

integer(8) function apply_i8(f, i)
implicit none
integer(8), intent(in) :: i
procedure(integer(8)) :: f
apply_i8 = f(i) + f(i + 1_8)
end function

subroutine apply_c8(f, z)
implicit none
complex(8) :: z
procedure(complex(8)) :: f
z = f(z) + 1
end subroutine

real function apply_r4(f, x)
implicit none
real, intent(in) :: x
procedure(real(4)) :: f
apply_r4 = f(x)
end function

program implicit_interface_100
implicit none
double precision, external :: twice_dp, apply_dp, apply_dp_token
real(8), external :: apply_r8, apply_r8_kind
integer(8), external :: next_i8, apply_i8
complex(8), external :: twice_c8
real, external :: triple_r4, apply_r4
complex(8) :: z
if (abs(apply_dp(twice_dp, 1d0) - 8d0) > 1d-12) error stop
if (abs(apply_dp_token(twice_dp, 1.5d0) - 3d0) > 1d-12) error stop
if (abs(apply_r8(twice_dp, 2d0) - 4d0) > 1d-12) error stop
if (abs(apply_r8_kind(twice_dp, 3d0) - 6d0) > 1d-12) error stop
if (apply_i8(next_i8, 3000000000_8) /= 6000000003_8) error stop
z = (1d0, 2d0)
call apply_c8(twice_c8, z)
if (abs(z - (3d0, 4d0)) > 1d-12) error stop
if (abs(apply_r4(triple_r4, 1.0) - 3.0) > 1e-6) error stop
print *, apply_dp(twice_dp, 1d0), apply_i8(next_i8, 3000000000_8), z
end program

double precision function twice_dp(y)
double precision :: y
twice_dp = 2 * y
end function

integer(8) function next_i8(k)
integer(8) :: k
next_i8 = k + 1
end function

complex(8) function twice_c8(y)
complex(8) :: y
twice_c8 = 2 * y
end function

real function triple_r4(y)
real :: y
triple_r4 = 3 * y
end function

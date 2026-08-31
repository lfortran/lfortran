module gpu_metal_228_mod
! dot_product inside a DO CONCURRENT offloaded to the GPU. Unlike matmul,
! dot_product survives array lowering as a call to the generated helper
! _lcompilers_dot_product_*, whose definition never reaches the shader.
! It has to be expanded into an explicit accumulation loop instead.
implicit none
type :: base_t
    real, allocatable :: inner_(:)
end type base_t
type, extends(base_t) :: stencil_t
    integer :: rows_
end type stencil_t
end module gpu_metal_228_mod

program gpu_metal_228
use gpu_metal_228_mod
implicit none
integer, parameter :: n = 4
real :: a(3), v(6)
real :: r_plain(n), r_section(n), r_member(n)
integer :: ia(3), iv(6), r_int(n)
integer :: j
type(stencil_t) :: s

a = [1.0, 2.0, 3.0]
v = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
ia = [1, 2, 3]
iv = [1, 2, 3, 4, 5, 6]
allocate(s%inner_(3))
s%inner_ = [1.0, 1.0, 1.0]
s%rows_ = n
r_plain = 0.0
r_section = 0.0
r_member = 0.0
r_int = 0

! plain local arrays
do concurrent (j = 1:n)
    r_plain(j) = dot_product(a, a)
end do

! an array section as the second argument
do concurrent (j = 1:n)
    r_section(j) = dot_product(a, v(j:j+2))
end do

! an inherited allocatable struct member as the first argument
do concurrent (j = 1:n)
    r_member(j) = dot_product(s%inner_, v(j:j+2))
end do

! integer operands
do concurrent (j = 1:n)
    r_int(j) = dot_product(ia, iv(j:j+2))
end do

do j = 1, n
    if (abs(r_plain(j) - 14.0) > 1.0e-5) error stop "plain dot_product wrong"
end do
if (abs(r_section(1) - 14.0) > 1.0e-5) error stop "section dot_product wrong"
if (abs(r_section(2) - 20.0) > 1.0e-5) error stop "section dot_product wrong"
if (abs(r_section(3) - 26.0) > 1.0e-5) error stop "section dot_product wrong"
if (abs(r_section(4) - 32.0) > 1.0e-5) error stop "section dot_product wrong"
if (abs(r_member(1) - 6.0) > 1.0e-5) error stop "member dot_product wrong"
if (abs(r_member(2) - 9.0) > 1.0e-5) error stop "member dot_product wrong"
if (abs(r_member(3) - 12.0) > 1.0e-5) error stop "member dot_product wrong"
if (abs(r_member(4) - 15.0) > 1.0e-5) error stop "member dot_product wrong"
if (r_int(1) /= 14) error stop "integer dot_product wrong"
if (r_int(4) /= 32) error stop "integer dot_product wrong"
print *, r_plain
print *, r_section
print *, r_member
print *, r_int
print *, "ok"
end program gpu_metal_228

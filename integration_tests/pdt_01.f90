program pdt_01
  implicit none

  type :: container(rk, ik)
    integer, kind :: rk
    integer, kind :: ik = selected_int_kind(9)
    integer(kind=ik)  :: i_val 
    real(kind=rk)     :: r_val
    complex(kind=rk)  :: c_val
  end type container

  type(container(8)) :: obj1

  type(container(4, 8)) :: obj2

  obj1%i_val = 42_8
  obj1%r_val = 3.14_8
  obj1%c_val = (1.0_8, -2.0_8)

  if (kind(obj1%i_val) /= selected_int_kind(9)) error stop "obj1 integer kind check failed"
  if (kind(obj1%r_val) /= 8) error stop "obj1 real kind check failed"
  if (obj1%i_val /= 42) error stop "obj1 integer check failed"
  if (abs(obj1%r_val - 3.14_8) > 1.0e-12_8) error stop "obj1 real check failed"
  if (abs(real(obj1%c_val) - 1.0_8) > 1.0e-12_8) error stop "obj1 complex real part failed"

  obj2%i_val = 100_8
  obj2%r_val = 2.5_4
  obj2%c_val = (5.0_4, 6.0_4)

  if (kind(obj2%i_val) /= 8) error stop "obj2 integer kind check failed"
  if (kind(obj2%r_val) /= 4) error stop "obj2 real kind check failed"
  if (obj2%i_val /= 100_8) error stop "obj2 integer check failed"
  if (abs(obj2%r_val - 2.5_4) > 1.0e-6_4) error stop "obj2 real check failed"
  if (abs(aimag(obj2%c_val) - 6.0_4) > 1.0e-6_4) error stop "obj2 complex imag part failed"

  print *, "PDT kind test passed successfully."

end program pdt_01
program class_allocate_07
  implicit none

  type :: base_t
    real :: val = 0.0
  end type base_t

  type, extends(base_t) :: child_a_t
  end type child_a_t

  type, extends(base_t) :: child_b_t
  end type child_b_t

  class(base_t), allocatable :: poly

  poly = make_a()
  call check_a(poly)

  poly = make_b()
  call check_b(poly)

  poly = make_a()
  call check_a(poly)

contains

  function make_a() result(out)
    type(child_a_t) :: out
    out%val = 1.0
  end function

  function make_b() result(out)
    type(child_b_t) :: out
    out%val = 2.0
  end function

  subroutine check_a(p)
    class(base_t), intent(in) :: p
    select type (p)
    type is (child_a_t)
      if (abs(p%val - 1.0) > 1.0e-6) error stop
    class default
      error stop
    end select
  end subroutine

  subroutine check_b(p)
    class(base_t), intent(in) :: p
    select type (p)
    type is (child_b_t)
      if (abs(p%val - 2.0) > 1.0e-6) error stop
    class default
      error stop
    end select
  end subroutine
end program

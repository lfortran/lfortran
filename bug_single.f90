module m
  implicit none
  type :: string_t
    character(len=:), allocatable :: s
  end type
  interface operator(.cat.)
    pure function concat(strings) result(r)
      type(string_t), intent(in) :: strings(:)
      type(string_t) :: r
    end function
  end interface

  type :: diag_t
    logical :: passed = .false.
  end type
  interface diag_t
    pure function make_diag(test_passed, diagnostics_string) result(d)
      import diag_t, string_t
      logical, intent(in) :: test_passed
      type(string_t), intent(in) :: diagnostics_string
      type(diag_t) :: d
    end function
  end interface
contains
  pure function aggregate(diagnoses) result(d)
    type(diag_t), intent(in) :: diagnoses(:)
    type(diag_t) :: d
    type(string_t) :: array(3)
    d = diag_t( &
      test_passed = all(diagnoses%passed), &
      diagnostics_string = .cat. pack(array, mask = .not. diagnoses%passed) &
    )
  end function
end module

pure function concat(strings) result(r)
  use m, only: string_t
  type(string_t), intent(in) :: strings(:)
  type(string_t) :: r
  r%s = "ok"
end function

pure function make_diag(test_passed, diagnostics_string) result(d)
  use m, only: diag_t, string_t
  logical, intent(in) :: test_passed
  type(string_t), intent(in) :: diagnostics_string
  type(diag_t) :: d
  d%passed = test_passed
end function

program p
  use m, only: diag_t
  implicit none
  print *, "hello"
end program

module operator_overloading_23_mod
  implicit none
  type :: string_t
    character(len=:), allocatable :: s
  end type
  type :: inner_t
    logical :: passed(2) = .true.
  end type

  type :: diag_t
    type(inner_t) :: stats(2)
  end type

  interface operator(.cat.)
    pure function concat(strings) result(r)
      import string_t
      type(string_t), intent(in) :: strings(:)
      type(string_t) :: r
    end function
  end interface

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
    type(string_t) :: array(2)
    logical :: overall_passed
    overall_passed = all(diagnoses%stats(1)%passed(1))

    d = diag_t( &
      test_passed = overall_passed, &
      diagnostics_string = .cat. pack(array, mask = .not. diagnoses%stats(2)%passed) &
    )
  end function
end module

pure function concat(strings) result(r)
  use operator_overloading_23_mod, only: string_t
  type(string_t), intent(in) :: strings(:)
  type(string_t) :: r
  r%s = "ok"
end function

pure function make_diag(test_passed, diagnostics_string) result(d)
  use operator_overloading_23_mod, only: diag_t, string_t
  logical, intent(in) :: test_passed
  type(string_t), intent(in) :: diagnostics_string
  type(diag_t) :: d
  d%stats(1)%passed(1) = test_passed
end function

program operator_overloading_23
  use operator_overloading_23_mod, only: diag_t, aggregate
  implicit none
  type(diag_t) :: diagnoses(2), result

  diagnoses(1)%stats(1)%passed(1) = .true.
  diagnoses(1)%stats(2)%passed(:) = .true.
  diagnoses(2)%stats(1)%passed(1) = .true.
  diagnoses(2)%stats(2)%passed(:) = .true.
  
  result = aggregate(diagnoses)
  if (.not. result%stats(1)%passed(1)) error stop
  diagnoses(2)%stats(1)%passed(1) = .false.
  result = aggregate(diagnoses)
  if (result%stats(1)%passed(1)) error stop

  print *, "ok"
end program
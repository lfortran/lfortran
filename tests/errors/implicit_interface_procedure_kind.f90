! A procedure is either a function or a subroutine (F2018 15.5.1, 19.5.1.4).
! A procedure with an implicit interface referenced both ways is reported at
! the reference that contradicts the first one, or its definition in this
! file.
subroutine subroutine_called_as_function(x)
    implicit none
    real :: x
    external :: sub_defined
    call sub_defined(x)
    x = sub_defined(1.0)
end subroutine

subroutine subroutine_definition_called_as_function(x)
    implicit none
    real :: x
    external :: sub_defined
    x = sub_defined(1.0)
end subroutine

subroutine dummy_subroutine_called_as_function(s, x)
    implicit none
    external :: s
    real :: x
    call s(x)
    x = s(1.0)
end subroutine

subroutine function_called_as_subroutine(x)
    implicit none
    real :: x
    real, external :: fun
    x = fun(1.0)
    call fun(x)
end subroutine

subroutine dummy_function_called_as_subroutine(g, x)
    implicit none
    real, external :: g
    real :: x
    x = g(1.0)
    call g(x)
end subroutine

subroutine other_unit_calls_subroutine(x)
    implicit none
    real :: x
    external :: sub_other
    call sub_other(x)
end subroutine

subroutine other_unit_references_function(x)
    implicit none
    real :: x
    real, external :: sub_other
    x = sub_other(1.0)
end subroutine

subroutine sub_defined(x)
    real :: x
    x = 1
end subroutine

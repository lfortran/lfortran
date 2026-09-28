! The component, declared as `type(l%u)` in a module compiled separately, is
! passed for a dummy argument of another type: the type is shown by its name,
! `u`.
program namespace_separate_component
    use nssep_types, only: take
    use nssep_holder, only: holder
    implicit none
    type(holder) :: h
    call take(h%c)
end program

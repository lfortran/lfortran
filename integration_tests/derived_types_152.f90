module m_152
  implicit none
  contains

  function method_create(f) result(that)
    procedure() :: f
    integer :: that
  end function

  subroutine compute(n)
    integer :: n
  end subroutine
end module

program derived_types_152
    use m_152
    implicit none
    integer :: ll
    ll = method_create(compute)
end program

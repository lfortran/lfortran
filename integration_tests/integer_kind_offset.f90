module module_integer_kind_offset
  use iso_c_binding
  implicit none
contains
  subroutine sub(x)
    integer(c_size_t), optional :: x
    if (present(x)) then
      if (x /= 0_c_size_t) error stop
    end if
  end subroutine
end module module_integer_kind_offset

program integer_kind_offset
  use module_integer_kind_offset
#if defined(__GFORTRAN__)
  call sub(0_c_size_t)
#else
  call sub(0)
#endif
end program integer_kind_offset

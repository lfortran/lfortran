module separate_compilation_50_mod_b
contains
  subroutine sc50b(x)
    real, intent(inout) :: x
    x = sin(x)
  end subroutine
end module

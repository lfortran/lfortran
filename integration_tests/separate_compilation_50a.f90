module separate_compilation_50_mod_a
contains
  subroutine sc50a(x)
    real, intent(inout) :: x
    x = sin(x)
  end subroutine
end module

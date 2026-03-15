module separate_compilation_38_module
  implicit none

  type recursive_t(k)
    integer, kind :: k = kind(1.0)
    type(recursive_t(k)), allocatable :: next
  end type recursive_t

  interface recursive_t
    pure module function make() result(n)
      implicit none
      type(recursive_t) :: n
    end function make
  end interface recursive_t
end module separate_compilation_38_module

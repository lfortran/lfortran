type, abstract :: t
contains
  procedure(tf), deferred :: nxt
  procedure(f), deferred :: mul
  procedure(asn), deferred :: asn
  generic :: operator(*) => mul
  generic :: assignment(=) => asn
end type
abstract interface
  function tf(a)
    import :: t
    class(t), intent(in) :: a
    class(t), allocatable :: tf
  end function
  function f(a, b)
    import :: t
    class(t), intent(in) :: a
    real, intent(in) :: b
    class(t), allocatable :: f
  end function
  subroutine asn(l, r)
    import :: t
    class(t), intent(inout) :: l
    class(t), intent(in) :: r
  end subroutine
end interface
contains
  subroutine s(x, d)
    class(t) :: x
    real :: d
    x = x%nxt() * d
  end subroutine
end

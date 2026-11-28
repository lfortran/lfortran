module m
  implicit none
  type :: t
  contains
    procedure(f), deferred :: g
  end type
  abstract interface
    subroutine f(this)
      import :: t
      class(t), intent(in) :: this
    end subroutine
  end interface
end module

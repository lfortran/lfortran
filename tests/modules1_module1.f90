module abc
    implicit none
    type :: t1
    contains
        procedure :: f
    end type t1

contains
    pure function f(self) result(match)
        class(t1), intent(in), optional :: self
        logical :: match
    end function f
end module abc

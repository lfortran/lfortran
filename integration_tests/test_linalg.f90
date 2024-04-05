program alloc_of_alloc_bug
implicit none
type :: et
    character(len=:), allocatable :: message
end type

type(et), allocatable :: e
e = f()

contains

    type(et) function f()
    f%message = "x"
    end function

end program

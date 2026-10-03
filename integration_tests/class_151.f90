module m_unicode
    implicit none
    private

    type, public :: mytype
    contains
        procedure, private :: oop_sub1
        generic, public :: sub => oop_sub1
    end type mytype

contains

    function oop_sub1(self, new) result(newline)
        class(mytype), intent(in) :: self
        character(len=*), intent(in) :: new
        character(len=:), allocatable :: newline

        newline = new
    end function oop_sub1

end module m_unicode


module M_Unused
end module M_Unused


program demo
    use m_unicode
    implicit none

    type(mytype) :: obj
    character(:), allocatable :: result

    result = obj%sub("hello")

    print *, result
end program demo
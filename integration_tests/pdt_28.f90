module pdt_28_mod
    ! An intent(out) dummy argument keeps the storage of its components
    ! that depend on LEN type parameters.
    implicit none

    type :: record_t(n)
        integer, len :: n
        integer :: id
        character(len=n) :: name
        real :: values(n)
    end type

contains

    subroutine reset(r)
        type(record_t(*)), intent(out) :: r
        r%id = 0
        r%name = "none"
        r%values = -1.0
    end subroutine

end module

program pdt_28
    use pdt_28_mod
    implicit none
    type(record_t(3)) :: r

    r%id = 7
    r%name = "abcdef"
    r%values = [1.0, 2.0, 3.0]
    call reset(r)
    if (r%n /= 3 .or. len(r%name) /= 3 .or. size(r%values) /= 3) error stop
    if (r%id /= 0 .or. r%name /= "non") error stop
    if (any(r%values /= -1.0)) error stop
    print *, r%id, r%name, r%values
end program

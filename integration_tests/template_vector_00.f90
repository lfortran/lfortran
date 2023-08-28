module template_vector_00_m

    type, public :: Vector
        integer, allocatable :: elements(:)
        integer :: sz = 0
    contains
        procedure :: resize
        procedure :: push_back
    end type

contains

    subroutine resize(this, n)
        class(Vector), intent(inout) :: this
        integer, intent(in) :: n
        integer, allocatable :: tmp(:)
        integer :: i

        if (.not. allocated(this%elements)) then
            allocate(this%elements(n))
            return
        end if

        if (this%sz >= n) return

        allocate(tmp(this%sz))

        do i = 1, this%sz
            tmp(i) = this%elements(i)
        end do

        allocate(this%elements(n))

        this%elements(1:this%sz) = tmp   
    end subroutine
    

    subroutine push_back(this, item)
        class(Vector), intent(in) :: this
        integer, intent(in) :: item

        integer :: new_size
        new_size = this%sz + 1

        call this%resize(new_size)
        this%elements(new_size) = item
        this%sz = new_size
    end subroutine

    subroutine main()
        type(Vector) :: v
        integer :: i
        v = Vector()
        call push_back(v, 1)
    end subroutine

end module

program template_vector_00
    use template_vector_00_m
    implicit none
    call main()
end program
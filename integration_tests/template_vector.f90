module template_vector_m
    implicit none
    private
    public :: vector_t

    template vector_t(T)
        type, deferred :: T
        public :: Vector

        type :: Vector
            type(T), allocatable :: elements(:)
            integer :: sz = 0
        contains
            procedure :: resize
            procedure :: push_back
        end type
    
    contains

        subroutine push_back(this, item)
            class(Vector), intent(in) :: this
            type(T), intent(in) :: item

            integer :: new_size
            new_size = this%sz + 1

            call this%resize(new_size)
            this%elements(new_size) = item
            this%sz = new_size
        end subroutine

        subroutine resize(this, n)
            class(Vector), intent(inout) :: this
            integer, intent(in) :: n
            type(T), allocatable :: tmp(:)
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

    end template

contains

    subroutine main()
        instantiate vector_t(integer), only: IntVector => Vector
        type(IntVector) :: v
        call v%push_back(10)
        if (v%elements(1) /= 10) error stop
    end subroutine

end module

program template_vector
    use template_vector_m
    implicit none
    call main()
end program

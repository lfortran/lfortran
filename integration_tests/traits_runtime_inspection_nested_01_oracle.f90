module traits_runtime_inspection_nested_01_oracle_m
    implicit none
    type :: Parent
        integer :: n
    end type
    type, extends(Parent) :: Child
        integer :: extra
    end type
    type, extends(Child) :: Grandchild
        integer :: last
    end type
    type :: Other
        integer :: n
    end type
contains
    integer function inspect(view) result(r)
        class(*), intent(in) :: view
        r = 0
        select type (outer => view)
        type is (Other)
            error stop 1
        class is (Parent)
            select type (middle => outer)
            type is (Parent)
                error stop 2
            class is (Child)
                r = middle%n + middle%extra
                select type (inner => middle)
                type is (Grandchild)
                    r = r + inner%last
                class default
                    error stop 3
                end select
            class is (Parent)
                error stop 4
            end select
        class default
            error stop 5
        end select
    end function
    subroutine mutate(view)
        class(*), pointer, intent(in) :: view
        select type (outer => view)
        class is (Parent)
            select type (inner => outer)
            class is (Grandchild)
                inner%n = inner%n + 1
            class default
                error stop 6
            end select
        end select
    end subroutine
end module

program traits_runtime_inspection_nested_01_oracle
    use traits_runtime_inspection_nested_01_oracle_m
    implicit none
    type(Grandchild), target :: object
    class(*), pointer :: view
    object%n = 47
    object%extra = 11
    object%last = 13
    if (inspect(object) /= 71 .or. object%n /= 47) error stop 7
    view => object
    call mutate(view)
    if (inspect(view) /= 72 .or. object%n /= 48) error stop 8
    nullify(view)
end program

program polymorphic_argument_01
    implicit none

    type :: MyStruct
        integer :: intMember
        real :: realMember
    end type MyStruct

    type(MyStruct) :: structInstance

    call unused_dummy_argument(structInstance)

    contains

    subroutine unused_dummy_argument(dummy)
        class(*), intent(in) :: dummy

        select type(dummy)
            type is (MyStruct)
                print *, "Hello world"
            class default
                print *, "Surprise!!"
        end select
    end subroutine

end program polymorphic_argument_01

! Testing nested variable (structType) `ll`
module select_type_15
    implicit none

    type, abstract :: abstract_t
        integer :: i
    end type
    
    type, extends(abstract_t) :: tt
        integer, allocatable :: arr(:)
    end type

    integer, parameter :: CONSTANT_NUMBER = 101

 contains
    subroutine xx(ll_abstract)
       class(abstract_t), intent(inout) :: ll_abstract
       call nested1()

       ! Verify changed `i` value
       print *, ll_abstract%i
       if(ll_abstract%i /= -1) ERROR STOP
       
       ! Verify changed `arr` value
       select type(ll_abstract)
           type is(tt)
               print *, ll_abstract%arr(10)
               if(ll_abstract%arr(10) /= -1) error stop
           class default
               print *, "Unknown ll type"
               error stop
       end select
       
 
       contains

        subroutine nested1()
            call ss(ll_abstract)
        end subroutine nested1

    end subroutine xx

    subroutine ss(ll_abstract)
       class(abstract_t), intent(inout) :: ll_abstract
       ! select on type + Check member value + Change it
        select type(ll_abstract)
            type is(tt)
                print *, "This is a tt"

                PRINT *, "ll_abstract%i   =", ll_abstract%i
                IF(ll_abstract%i /=CONSTANT_NUMBER) ERROR STOP

                print *, ll_abstract%arr(10)
                IF(ll_abstract%arr(10) /= 1) error stop

                ll_abstract%i = -1
                ll_abstract%arr(10) = -1
            class default
                print *, "Unknown type"
                error stop
        end select
    end subroutine

 end module select_type_15
 
 program select_type_15_prog
    use select_type_15
    implicit none
    type(tt) :: ll
    
    ll%i = CONSTANT_NUMBER
    allocate(ll%arr(10))
    ll%arr = 1
    call xx(ll)
 end program
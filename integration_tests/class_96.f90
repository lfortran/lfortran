program class_95
    implicit none

    character(len=5) :: arr(2) = ["hello", "world"]

    call ss(arr)

contains

    subroutine ss(generic)
        class(*) :: generic(:)

        select type (generic)
        type is (character(len=*))
            call ff(generic)
        end select
    end subroutine 


    subroutine ff(x)
        character(len=*) :: x(:)
        if(x(1) /= "hello") error stop
        if(x(2) /= "world") error stop
    end subroutine

end program
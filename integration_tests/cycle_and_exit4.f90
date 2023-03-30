program test_blocks
    integer :: i
    logical :: pass = .true.
    b1 : block
        print *, "b1"
        i = i + 1
        b2 : block
            print *, "b2"
            i = i + 2
            exit b1
            i = i + 3
            exit b2
            i = i + 4
        end block b2
        i = i + 5
        print *, "error b1"
        pass = .false.
    end block b1
    b3 : block
        print *, "b3"
        b4 : block
            print *, "b4"
            b5 : block
                print *, "b5"
                exit b3
            end block b5
            print *, "error b4"
            pass = .false.
        end block b4
        print *, "error b3"
        pass = .false.
    end block b3
    b6 : block
        integer :: i
        print *, "b6"
        block
            print *, "b7"
            ! TODO: Implement this in asr_to_llvm.cpp
            ! do i = 1, 10
            !     if (i > 5) then
            !         exit
            !     end if
            ! end do
            ! if (i /= 6) then
            !     pass = .false.
            ! end if
        end block
    end block b6
    if (pass) then
        print *, "pass"
    end if
end program
program array_constructor_18
    ! Without a type-spec, the elements of an array constructor have the
    ! length of its first element, here the length of trim(word)
    implicit none
    character(len=5) :: word
    character(len=5), parameter :: pword = 'hi'
    character(len=3) :: parts(2)
    character(len=:), allocatable :: words(:)
    character(len=16) :: line

    word = 'hi'

    parts = [trim(word), 'yo'] // '!'
    if (parts(1) /= 'hi!') error stop 1
    if (parts(2) /= 'yo!') error stop 2

    if (len([trim(word), 'yo']) /= 2) error stop 3

    words = [trim(word), 'yo']
    if (len(words) /= 2) error stop 4
    if (size(words) /= 2) error stop 5
    if (words(1) /= 'hi' .or. words(2) /= 'yo') error stop 6

    write (line, '(a,"|",a)') [trim(word), 'yo']
    if (line /= 'hi|yo') error stop 7

    call check([trim(word), 'yo'])

    if (len([trim(pword), 'yo']) /= 2) error stop 8
    parts = [trim(pword), 'yo'] // '!'
    if (parts(1) /= 'hi!' .or. parts(2) /= 'yo!') error stop 9

    print *, "OK"

contains

    subroutine check(a)
        character(len=*), intent(in) :: a(:)
        if (len(a) /= 2) error stop 10
        if (a(1) /= 'hi' .or. a(2) /= 'yo') error stop 11
    end subroutine check

end program array_constructor_18

! A trait message whose result is `class(*), allocatable`, selected by an
! assumed-length character argument, is dispatched at run time through
! borrowed views, owners, an allocatable trait component, trait pointers and
! CLASS IS guards. The results hold integers, reals, characters of varying
! length, derived types and an extended type, and are assigned, allocated
! with SOURCE=, passed as actual arguments, inspected with SELECT TYPE and
! returned as the result of another function.
module traits_runtime_polymorphic_result_01_m
    implicit none
    abstract interface :: ISomeTrait
        function method(self, string) result(res)
            type(itself), pass, intent(in) :: self
            character(*), intent(in) :: string
            class(*), allocatable :: res
        end function method
    end interface

    type :: Point
        integer :: x = 0, y = 0
    end type Point

    type, extends(Point) :: Point3
        integer :: z = 0
    end type Point3

    ! A table of settings, looked up by name.
    type :: Settings
        integer :: level = 0
    end type Settings
    implements ISomeTrait :: Settings
        procedure, pass :: method => settings_method
    end implements

    ! Echoes the key it is asked for, in several forms.
    type :: Echo
        integer :: times = 1
    end type Echo
    implements ISomeTrait :: Echo
        procedure, pass :: method => echo_method
    end implements

    type :: Holder
        class(ISomeTrait), allocatable :: source
    end type Holder
contains
    function settings_method(self, string) result(res)
        class(Settings), intent(in) :: self
        character(*), intent(in) :: string
        class(*), allocatable :: res
        select case (string)
        case ("level")
            res = self%level
        case ("scale")
            res = 0.5d0 * self%level
        case ("name")
            res = "settings"
        case ("origin")
            res = Point(self%level, -self%level)
        case default
            res = Point3(1, 2, self%level)
        end select
    end function settings_method

    function echo_method(self, string) result(res)
        class(Echo), intent(in) :: self
        character(*), intent(in) :: string
        class(*), allocatable :: res
        if (len(string) == 0) then
            res = self%times
        else
            res = repeat(string, self%times)
        end if
    end function echo_method

    function myfunc(c) result(res)
        class(ISomeTrait), intent(in) :: c
        class(*), allocatable :: res
        res = c%method("name")
    end function myfunc

    integer function as_integer(x)
        class(*), intent(in) :: x
        select type (x)
        type is (integer)
            as_integer = x
        class default
            as_integer = -huge(0)
        end select
    end function as_integer

    function as_string(x) result(s)
        class(*), intent(in) :: x
        character(:), allocatable :: s
        select type (x)
        type is (character(*))
            s = x
        class default
            s = "?"
        end select
    end function as_string

    ! The receiver is a borrowed view of an object with level 4.
    subroutine check_settings(c)
        class(ISomeTrait), intent(in) :: c
        class(*), allocatable :: r
        r = c%method("level")
        if (as_integer(r) /= 4) error stop 1
        ! The result replaces one of another dynamic type.
        r = c%method("scale")
        select type (r)
        type is (real(8))
            if (r /= 2d0) error stop 2
        class default
            error stop 3
        end select
        r = c%method("name")
        if (as_string(r) /= "settings") error stop 4
        r = c%method("origin")
        select type (r)
        type is (Point)
            if (r%x /= 4 .or. r%y /= -4) error stop 5
        class default
            error stop 6
        end select
        r = c%method("other")
        select type (r)
        type is (Point3)
            if (r%x /= 1 .or. r%y /= 2 .or. r%z /= 4) error stop 7
        class default
            error stop 8
        end select
        select type (r)
        class is (Point)
            if (r%y /= 2) error stop 9
        class default
            error stop 10
        end select
    end subroutine check_settings

    ! References as actual arguments, selectors and SOURCE=, in a loop and in
    ! another function, through a view of an object with level 4.
    subroutine check_settings_references(c)
        class(ISomeTrait), intent(in) :: c
        class(*), allocatable :: r
        integer :: i
        if (as_integer(c%method("level")) /= 4) error stop 11
        if (as_string(c%method("name")) /= "settings") error stop 12
        if (as_integer(c%method("name")) /= -huge(0)) error stop 13
        select type (q => c%method("origin"))
        type is (Point)
            if (q%x /= 4) error stop 14
        class default
            error stop 15
        end select
        associate (q => c%method("other"))
            select type (q)
            class is (Point)
                if (q%x /= 1) error stop 20
            class default
                error stop 21
            end select
        end associate
        allocate(r, source=c%method("scale"))
        select type (r)
        type is (real(8))
            if (r /= 2d0) error stop 16
        class default
            error stop 17
        end select
        do i = 1, 3
            r = c%method("level")
        end do
        if (as_integer(r) /= 4) error stop 18
        r = myfunc(c)
        if (as_string(r) /= "settings") error stop 19
    end subroutine check_settings_references

    ! The receiver echoes `n` times.
    subroutine check_echo(c, n)
        class(ISomeTrait), intent(in) :: c
        integer, intent(in) :: n
        class(*), allocatable :: r
        character(:), allocatable :: key
        r = c%method("")
        if (as_integer(r) /= n) error stop 30
        key = "ab"
        r = c%method(key)
        if (len(as_string(r)) /= 2 * n) error stop 31
        if (as_string(r) /= repeat("ab", n)) error stop 32
        r = c%method(key // "c")
        if (as_string(r) /= repeat("abc", n)) error stop 33
        r = c%method(key(2:2))
        if (as_string(r) /= repeat("b", n)) error stop 34
        if (as_string(myfunc(c)) /= repeat("name", n)) error stop 35
    end subroutine check_echo
end module traits_runtime_polymorphic_result_01_m

program traits_runtime_polymorphic_result_01
    use traits_runtime_polymorphic_result_01_m
    implicit none
    class(ISomeTrait), allocatable :: owner
    class(ISomeTrait), pointer :: p
    type(Settings), target :: s
    type(Holder) :: h
    class(*), allocatable :: thing, r

    call check_settings(Settings(4))
    call check_settings_references(Settings(4))
    call check_echo(Echo(3), 3)
    call check_echo(Echo(0), 0)

    owner = Settings(4)
    r = owner%method("origin")
    select type (r)
    type is (Point)
        if (r%x /= 4) error stop 40
    class default
        error stop 41
    end select
    call check_settings(owner)
    call check_settings_references(owner)
    deallocate(owner)
    allocate(owner, source=Echo(2))
    if (as_string(owner%method("xy")) /= "xyxy") error stop 42
    call check_echo(owner, 2)

    s%level = 4
    p => s
    r = p%method("level")
    if (as_integer(r) /= 4) error stop 43
    call check_settings(p)
    call check_settings_references(p)

    allocate(h%source, source=Settings(4))
    r = h%source%method("name")
    if (as_string(r) /= "settings") error stop 44
    call check_settings(h%source)
    call check_settings_references(h%source)
    r = myfunc(h%source)
    if (as_string(r) /= "settings") error stop 45

    allocate(thing, source=Echo(1))
    select type (x => thing)
    class is (ISomeTrait)
        r = x%method("z")
        if (as_string(r) /= "z") error stop 46
        call check_echo(x, 1)
    class default
        error stop 47
    end select
    print '(a)', 'traits_runtime_polymorphic_result_01 ok'
end program traits_runtime_polymorphic_result_01

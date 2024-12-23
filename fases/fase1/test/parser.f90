
module parser
implicit none

contains

subroutine parse(input)
    character(len=:), intent(inout), allocatable :: input
    character(len=:), allocatable :: lexeme
    integer :: cursor
    cursor = 1
    do while (lexeme /= "EOF" )
        if(lexeme == "ERROR") THEN 
            cursor = cursor + 1
            lexeme = nextSym(input, cursor)
        else 
            lexeme = nextSym(input, cursor)
            
        end if
        print *, lexeme
    end do
end subroutine parse

function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
end function tolower

function is_digit(str) result(res)
    implicit none
    character(len=*), intent(in) :: str
    integer :: i
    logical :: res

    res = .true.
    do i = 1, len(str)
        if (.not. (str(i:i) >= '0' .and. str(i:i) <= '9')) then
            res = .false.
            return
        end if
    end do
end function is_digit

function is_str(str) result(res)
    implicit none
    character(len=*), intent(in) :: str
    integer :: i
    logical :: res

    res = .true.
    do i = 1, len(str)
        if (.not. ((str(i:i) >= 'A' .and. str(i:i) <= 'Z') .or. &
                   (str(i:i) >= 'a' .and. str(i:i) <= 'z'))) then
            res = .false.
            return
        end if
    end do
end function is_str

function with_whitespace(str) result(res)
    implicit none
    character(len=*), intent(in) :: str
    integer :: i
    logical :: res

    res = .false.
    do i = 1, len(str)
        if (str(i:i) == ' ' .or. str(i:i) == char(9) .or. str(i:i) == char(10) .or. str(i:i) == char(13)) then
            res = .true.
            return
        end if
    end do
end function with_whitespace

function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: initialCursor

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    






    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((iachar(input(cursor:cursor)) >= iachar("0") .and. &
        iachar(input(cursor:cursor)) <= iachar("9"))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1) 

        lexeme = lexeme // " -" // "integer"
        return
    end if

    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((iachar(input(cursor:cursor)) >= iachar("a") .and. &
        iachar(input(cursor:cursor)) <= iachar("z"))& 
    .or. (iachar(input(cursor:cursor)) >= iachar("A") .and. &
        iachar(input(cursor:cursor)) <= iachar("Z"))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1) 

        lexeme = lexeme // " -" // "string"
        return
    end if

    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. ((input(cursor:cursor) == char(32))& 
    .or. (input(cursor:cursor) == char(9))& 
    .or. (input(cursor:cursor) == char(10))& 
    .or. (input(cursor:cursor) == char(13))))
        cursor = cursor + 1
    end do
    if (cursor > initialCursor) then
        allocate(character(len=cursor-initialCursor)::lexeme)
        lexeme = input(initialCursor:cursor-1) 

        lexeme = lexeme // " -" // "whitespace"
        return
    end if


    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module parser 
        
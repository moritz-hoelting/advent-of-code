program part2
    implicit none

    character(len=20) :: line
    character :: dir
    integer :: value
    integer :: ios
    integer :: count, dial, dial_prev, div

    open(unit=10, file="input.txt", status="old", action="read")

    count = 0
    dial = 50

    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit

        dir = line(1:1)
        read(line(2:), *) value

        dial_prev = dial

        if (dir == 'R') then
            dial = dial + value
            div = dial / 100
            if (dial == 0) div = div + 1
            dial = mod(dial, 100)
            count = count + div
            ! print '(A,1X,I0)', "Move right:", value
        else if (dir == 'L') then
            dial = dial - value
            div = (-dial) / 100
            if (dial <= 0 .AND. dial_prev > 0) div = div + 1
            dial = mod(dial + 100 * (div + 1), 100)
            count = count + div
            ! print '(A,1X,I0)', "Move left:", value
        end if
    end do

    print '(A,1X,I0)', "End dial value: ", dial
    print '(A,1X,I0)', "Total times dial was zero: ", count

    close(10)

end program part2
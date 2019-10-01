program main
    implicit none
    integer,parameter :: SIZE = 4, n = 500
    double precision,parameter :: dh = SIZE/n
    integer i, j, loop_var
    double precision x, y
    double complex z
    open(10, file="data.txt")

    do j = 1, n
        y = j * (SIZE/dble(n)) - 0.5d0*SIZE
        do i = 1, n
            x = i * (SIZE/dble(n)) - 0.5d0*SIZE
            z = dcmplx(0d0, 0d0)
            
            do loop_var = 1, 50
                z = z*z + dcmplx(x, y)
                if (abs(z) > 2d0) then
                    exit
                end if
            end do
            write (10, *) x, y, loop_var

        end do
        write (10, *)
    end do

    close(10)
end program
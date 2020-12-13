module my_dydx
implicit none
contains

! dy/dx
pure real function exercise1(x, y)
    real, intent(in) :: x, y
    exercise1 = log(2.34 * x * y)
end function

end module my_dydx

program main
    use ode
    use my_dydx
    real :: xn, yn, step
    integer :: i

    ! Initial conditions
    xn = 1.0
    yn = 1.0
    step = 1e-1

    do i = 1, 5
        yn = rk4(exercise1, xn, yn, step)
        xn = xn + step
        print *, yn
    end do

end program main
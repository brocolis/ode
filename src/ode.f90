module ode
implicit none

interface
    pure real function dydx_prototype(x, y)
        real, intent(in) :: x, y
    end function
end interface

contains

pure real function rk4(f, x, y, h)
    procedure(dydx_prototype) :: f
    real, intent(in) :: x, y, h
    real :: k1, k2, k3, k4

    k1 = f(x, y)
    k2 = f(x + 0.5*h, y + 0.5*h*k1)
    k3 = f(x + 0.5*h, y + 0.5*h*k2)
    k4 = f(x + h, y + h*k3)
    rk4 = y + h/6.0 * (k1 + 2*k2 + 2*k3 + k4)
end function

end module ode

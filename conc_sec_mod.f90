MODULE conc_sec_mod
        
        implicit none

        private
        public tri_area, tri_2nd_area, tri_1st_area, plane_koeff

        integer, parameter :: dp = selected_real_kind(15,307)
        
        contains

        FUNCTION plane_koeff(x,y,z) result(k)
                real(dp), intent (in) :: x(3),y(3),z(3)
                real(dp) :: k(3), denom
                !k(1) + k(2)x + k(3)y = z

                denom =  dot_product(x,(/y(3)-y(2),y(1)-y(3),y(2)-y(1)/))

                k(1) = dot_product(x,(/y(3)*z(2)-y(2)*z(3),y(1)*z(3)-y(3)*z(1), y(2)*z(1)-y(1)*z(2) /)) / denom
                k(2) = dot_product(y,(/z(3)-z(1),z(1)-z(3),z(2)-z(1)/)) / denom 
                k(3) = dot_product(x,(/z(3)-z(1),z(1)-z(3),z(2)-z(1)/)) / denom 
                
        END FUNCTION plane_koeff







        FUNCTION tri_area(x,y) result(area)
                real(dp), intent (in) :: x(3),y(3)
                real(dp) :: area
                
                area = x(2)*(y(3)-y(1))-x(3)*(y(2)-y(1)-x(1)*(y(3)-y(2)
                area = area/2
        END FUNCTION tri_area

        FUNCTION tri_2nd_area(x,y) result(area_2)
                real(dp), intent (in) :: x(3),y(3)
                real(dp) :: area_2 = 0.0_dp
                integer :: i1, imod
                !Računa se okoli 1. koordinate,
                !ki jo predstavlja 1. argument.
                !Za drugo smeri zamenjaj argumenta
                DO i1 = 1,3
                        imod = mod(i1+1,3)
                        area_2 = area_2 +
                        (x(i1)*y(imod)-y(i1)*x(imod))*(y(imod)**2+y(imod)*y(i1)+y(i1)**2)/12
                END DO
        END FUNCTION tri_2nd_area

        FUNCTION tri_1st_area(x,y) result(area_1)
                real(dp), intent(in) :: x(3),y(3)
                real(dp) :: area_1 = 0.0_dp
                integer :: i1,imod
                !Računa se okoli 1. koordinate,
                !ki jo predstavlja 1. argument.
                !Za drugo smeri zamenjaj argumenta
                DO i1 = 1,3
                        imod = mod(i1+1,3)
                area_1 = area_1+(x(i1)-x(imod))*(y(i1)**2+y(i1)*y(imod)+y(imod)**2)/6
                END DO
        END FUNCTION tri_1st_area
       





        FUNCTION conc_sigma1(ei,e1,eu1,fc,E) result(sigma)
                real(dp), intent (in) :: ei,e1,eu1,fc,E
                real(dp) :: sigma,k,eta
                ! Tlačne deformacije so negativne
                ! Tlačne napetosti so negativne
                ! Preverit bo treba predznake

                k = 1.05*E*e1/fc
                eta = ei/e1
                
                IF (ei < eu1) THEN
                        sigma = 0.0_dp
                ELSE IF (ei>0) THEN
                        sigma = 0.0_dp
                ELSE
                        sigma = fc*(k*eta-eta**2)/(1+(k-2)*eta)
                END IF
        END FUNCTION conc_sigma1

        FUNCTION conc_sigma2(ei,e2,eu2,fc,n) result(sigma)
                real(dp), intent (in) :: ei,e2,eu2,fc
                real(dp) :: sigma
                ! Tlačne deformacije so negativne
                ! Tlačne napetosti so negativne
                ! Preverit bo treba predznake
                IF (ei < eu2) THEN
                        sigma = 0.0_dp
                ELSE IF (ei>0) THEN
                        sigma = 0.0_dp
                ELSE IF (ei<e2) THEN
                        sigma = fc
                ELSE 
                        sigma = fc*(1-(1-ei/e2)**n)
                END IF
        END FUNCTION conc_sigma_2

        FUNCTION reb_sigma(ei,fy,E,K) result(sigma)
                real(dp), intent (in) :: ei,fy,E,K
                real(dp) :: sigma

                IF (abs(ei)>fy/E)
                        sigma = (fy+K*(abs(ei)-fy/E))*sign(ei)
                ELSE
                        sigma = ei*E
                END IF
        END FUNCTION reb_sigma

END MODULE


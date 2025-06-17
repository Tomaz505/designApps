! Površina prereza
pure real function A(n,x,y)
        integer, intent (in) :: n
        real, dimension(:), intent (in) :: x,y
        A = 0.
        do i = 1,n
                A = A + (x(i)*y(i+1)-x(i+1)*y(i))/2
        end do
end function

! Vztrajnost okoli osi x
pure real function Ix(n,x,y)
        integer, intent (in) :: n
        real, dimension(:), intent (in) :: x,y
        Ix = 0.
        do i=1,n
                Ix = Ix + (x(i)*y(i+1)-y(i)*x(i+1))*(y(i)**2+y(i+1)*y(i)+y(i+1)**2)/12
        end do
end function
pure real function Sx(n,x,y)
        integer, intent (in) ::n
        real, dimension(:), intent (in) :: x,y
        Sx = 0.
        do i=1,n
                Sx = Sx + (x(i)-x(i+1))*(y(i+1)**2+y(i+1)*y(i)+y(i)**2)/6
        end do
end function

!Vztrajnost okoli osi y
pure real function Iy(n,x,y)
        integer, intent (in) :: n
        real, dimension(:), intent (in) :: x,y
        Iy = 0.
        do i=1,n
                Iy = Iy+(x(i)*y(i+1)-y(i)*x(i+1))*(x(i)**2+x(i+1)*x(i)+x(i+1)**2)/12
        end do
end function
pure real function Sy(n,x,y)
        integer, intent (in) :: n
        real, dimension(:), intent (in) :: x,y
        Sy = 0.
        do i=1,n
                Sy = Sy + (y(i+1)-y(i))*(x(i+1)**2+x(i)*x(i+1)+x(i)**2)/6
        end do
end function

pure real function sigmac(d,d1,d2,fc)
        real, intent (in) :: d,d1,d2,fc
        sigmac = d*(d1+d2-d)*4/(d1+d2)**2*fc
end function
pure real function epsc(di,xi,yi,x,y)
        real, intent(in) :: x,y
        real :: k1,k2,k3
        real, dimension(3), intent (in) :: di,xi,yi
        k1 =-(-xi(3)*yi(2)*di(1)+xi(2)*yi(3)*di(1)+xi(3)*yi(1)*di(2)-xi(1)*yi(3)*di(2)-xi(2)*yi(1)*di(3)+xi(1)*yi(2)*di(3))/(xi(2)*yi(1)-xi(3)*yi(1)-xi(1)*yi(2)+xi(1)*yi(3)-xi(2)*yi(3))
        k2 =-(-yi(2)*di(1)-yi(3)*di(1)-yi(1)*di(2)+yi(3)*di(2)+yi(1)*di(3)-yi(2)*di(3))/(xi(2)*yi(1)-xi(3)*yi(1)-xi(1)*yi(2)+xi(3)*yi(2)+xi(1)*yi(3)-xi(2)*yi(3))
        k3 =-(-xi(2)*di(1)+xi(3)*di(1)+xi(1)*di(2)-xi(3)*di(2)-yi(1)*di(3)+xi(2)*di(3))/(xi(2)*yi(1)-xi(3)*yi(1)-xi(1)*yi(2)+xi(3)*yi(2)+xi(1)*yi(3)-xi(2)*yi(3))

        epsc = k1+k2*x+k3*y
end function


program secdesign
        use iso_fortran_env
        implicit none 
        interface 
                pure real function A(n,x,y)
                        integer, intent (in) :: n
                        real, dimension(:), intent (in) ::x,y
                end function
                pure real function Ix(n,x,y)
                        integer, intent (in) :: n
                        real, dimension(:), intent (in) :: x,y
                end function
                pure real function Iy(n,x,y)
                        integer, intent (in) ::n
                        real , dimension(:), intent (in) :: x,y
                end function
                pure real function Sx(n,x,y)
                        integer, intent (in) :: n
                        real, dimension(:), intent (in) :: x,y
                end function
                pure real function Sy(n,x,y)
                        integer, intent (in) ::n
                        real, dimension(:), intent (in) ::x,y
                end function
                pure real function sigmac(d,d1,d2,fc)
                        real, intent(in) :: d,d1,d2,fc
                end function
        end interface
        

        integer :: nv,ns,i1,i2,i3,i4, ttime
        character(10) :: titletime,Astring,Ixstring,Iystring
        real :: Ss1,Ss2,Is1,Is2,As,c1,c2
        !Ss1,Ss2,Is1,Is2,As,c1,c2 Karakteristike prereza 1->x in 2->y
        !epsi,xei,yei so interpolacijske točke za deformacijsko ravnino
        real, dimension(:), allocatable :: xi,yi,xsi,ysi,psi
        real, dimension(3) :: epsi,xei,yei



        !Vnos podatkov o prerezu
        print *, 'Vnesi stevilo ogljisc prereza :'
        read *, nv
        ttime = time()
        write(titletime, '(i10)') ttime
        allocate(xi(1:(nv+1)))
        allocate(yi(1:(nv+1)))
       
        do i1 = 1,nv
                print *, i1
                print *, 'Vnesi koordinato x:'
                read *, xi(i1)
                print *, 'Vnesi koordinato y:'
                read *, yi(i1)
        end do
        xi(nv+1) = xi(1)
        yi(nv+1) = yi(1)
        
        print *, 'Vnesi stevilo armaturnih palic:'
        read *, ns
        allocate(xsi(1:ns))
        allocate(ysi(1:ns))
        allocate(psi(1:ns))

        do i1 = 1,ns
                print *, i1
                print *, 'Vnesi x koordinato palice:'
                read *, xsi(i1)
                print *, 'Vnesi y koordinato palice:'
                read *, ysi(i1)
                print *, 'Vnesi premer palice:'
                read *, psi(i1)
        end do

        !Račun karakteristik
        Ss1 = Sx(nv,xi,yi)
        Ss2 = Sy(nv,xi,yi)
        As = A(nv,xi,yi) 
        !write(Astring,'(f5.3)',iostat = cvtErr) As

        c1 = Ss1/As
        c2 = Ss2/As

        do i1 = 1,(nv+1)
                xi(i1) = xi(i1)-c2
                yi(i1) = yi(i1)-c1
        end do
        do i1=1,ns
                xsi(i1) = xsi(i1)-c2
                ysi(i1) = ysi(i1)-c1
        end do

        Is1 = Ix(nv,xi,yi)
        Is2 = Iy(nv,xi,yi)
        !write(Ixstring, '(f5.3)',iostat = cvtErr) Is1
        !write(Iystring,'(f5.3)',iostat = cvtErr) Is2
        print *, "A = ",As
        print *, "Ix = ",Is1,"Iy = ",Is2

        

        











        !Poročilo
        print *, 'Zelis porocilo (rabis latex) ? (1->Ja)'
        read *, i1
        if ( i1 .EQ. 1) then
                
                open (unit = 1, file = "Sec"//titletime//".dat", status = 'new' )
                write(1,*) 'x   ', 'y'
                do i1 = 1,(nv+1)
                        write(1,*) xi(i1), yi(i1)
                end do
                close(1)

                open (unit = 3, file = "Reinf"//titletime//".dat",status = 'new')
                write(3,*) 'x   ','y'
                do i1=1,ns
                        write(3,*) xsi(i1),ysi(i1)
                end do
                close(3)


                open (unit=2, file = "report"//titletime//".tex", status = 'new')
                write(2,*) "\documentclass[a4paper]{article}"
                write(2,*) "\usepackage[slovene]{babel}"
                write(2,*) "\usepackage{pgfplots}"
                write(2,*) "\pgfplotsset{compat = 1.5}"
                write(2,*) "\begin{document}"
                write(2,*) "\begin{figure}[!h]"
                write(2,*) "\centering"
                write(2,*) "\begin{tikzpicture}"
                write(2,*) "\begin{axis}[axis lines = middle, xlabel = $x$, ylabel = $y$]"
                write(2,*) "\addplot [thin,color = blue] table {Sec"//titletime//".dat};"
                write(2,*) "\addplot[scatter,only marks, mark options = {fill = red}] table {Reinf"//titletime//".dat};"
                write(2,*) "\end{axis}"
                write(2,*) "\end{tikzpicture}"
                write(2,*) "\caption{Skica prereza}"
                write(2,*) "\end{figure}"
                !write(2,*) "$$A = "//Astring//"\qquad Ix = "//Ixstring//"\qquad Iy = "//Iystring//"$$"
                write(2,*) "\end{document}"
                close(2)

                call execute_command_line ("pdflatex report"//titletime//".tex")
                call execute_command_line ("rm report"//titletime//".tex")
                call execute_command_line ("rm Sec"//titletime//".dat")
                call execute_command_line ("rm Reinf"//titletime//".dat")
                call execute_command_line ("rm report"//titletime//".log")
                call execute_command_line ("rm report"//titletime//".aux")
        end if

        print *, "Ko zelis zapret koncas pritisni Enter"
        read *
end program secdesign

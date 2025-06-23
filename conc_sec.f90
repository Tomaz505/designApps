program secdesign
        use conc_sec_mod
        implicit none         
        integer, parameter :: dp = selected_real_kind(15,307)
        integer :: nv,ns,i1,i2,i3,i4, ttime
        character(10) :: titletime,Astring,Ixstring,Iystring
        real(dp) :: Ss1=0.0_dp,Ss2=0.0_dp,Is1=0.0_dp,Is2=0.0_dp,As=0.0_dp,c1,c2
        !Ss1,Ss2,Is1,Is2,As,c1,c2 Karakteristike prereza 1->x in 2->y
        !epsi,xei,yei so interpolacijske točke za deformacijsko ravnino
        real(dp), dimension(:), allocatable :: xi,yi,xsi,ysi,psi
        real(dp), dimension(3) :: epsi,xei,yei

        ttime = time()
        write(titletime, '(i10)') ttime

        !Vnos podatkov o prerezu
        print *, 'Vnesi stevilo ogljisc prereza :'
        read *, nv
        allocate(xi(1:(nv+1)))
        allocate(yi(1:(nv+1)))
       
        do i1 = 1,nv
                print *, i1,": Vnesi koordinati x,y:"
                read *, xi(i1),yi(i1)
        end do

        xi(nv+1) = xi(1)
        yi(nv+1) = yi(1)
        

        print *, 'Vnesi stevilo armaturnih palic:'
        read *, ns
        allocate(xsi(1:ns))
        allocate(ysi(1:ns))
        allocate(psi(1:ns))

        do i1 = 1,ns
                print *,i1, ": Vnesi koordinati in premer x,y,p:"
                read *, xsi(i1),ysi(i1),psi(i1)
        end do

        !Račun karakteristik
        do i1 = 1,nv
                Ss1 = Ss1 + tri_1st_area((/0.0_dp,xi(i1),xi(i1+1)/), (/0.0_dp,yi(i1),yi(i1+1)/))
                Ss2 = Ss2 - tri_1st_area((/0.0_dp,yi(i1),yi(i1+1)/), (/0.0_dp,xi(i1),xi(i1+1)/))
                As = As + tri_area((/0.0_dp,xi(i1),xi(i1+1)/), (/0.0_dp,yi(i1),yi(i1+1)/))
                Is1 = Is1 + tri_2nd_area((/0.0_dp,xi(i1),xi(i1+1)/), (/0.0_dp,yi(i1),yi(i1+1)/))
                Is2 = Is2 - tri_2nd_area((/0.0_dp,yi(i1),yi(i1+1)/), (/0.0_dp,xi(i1),xi(i1+1)/))
        end do

                !write(Astring,'(f5.3)',iostat = cvtErr) As
        !Račun težišča

        c1 = Ss1/As
        c2 = Ss2/As

        Is1 = Is1-c1**2*As
        Is2 = Is2-c2**2*As

        do i1 = 1,(nv+1)
                xi(i1) = xi(i1)-c2
                yi(i1) = yi(i1)-c1
        end do
        do i1=1,ns
                xsi(i1) = xsi(i1)-c2
                ysi(i1) = ysi(i1)-c1
        end do

        
        print *, "A = ",As
        print *, "cx = ",c1,"cy = ",c2
        print *, "Sx = ",Ss1,"Sy = ",Ss2
        print *, "Ix = ",Is1,"Iy = ",Is2

        

        











        !Poročilo
        !print *, 'Zelis porocilo (rabis latex) ? (1->Ja)'
        !read *, i1
        !if ( i1 .EQ. 1) then
        !        
        !        open (unit = 1, file = "Sec"//titletime//".dat", status = 'new' )
        !        write(1,*) 'x   ', 'y'
        !        do i1 = 1,(nv+1)
        !                write(1,*) xi(i1), yi(i1)
        !        end do
        !        close(1)
        
        !        open (unit = 3, file = "Reinf"//titletime//".dat",status = 'new')
        !        write(3,*) 'x   ','y'
        !        do i1=1,ns
        !                write(3,*) xsi(i1),ysi(i1)
        !        end do
        !        close(3)


         !       open (unit=2, file = "report"//titletime//".tex", status = 'new')
         !       write(2,*) "\documentclass[a4paper]{article}"
         !       write(2,*) "\usepackage[slovene]{babel}"
         !       write(2,*) "\usepackage{pgfplots}"
         !       write(2,*) "\pgfplotsset{compat = 1.5}"
         !       write(2,*) "\begin{document}"
         !       write(2,*) "\begin{figure}[!h]"
         !       write(2,*) "\centering"
         !       write(2,*) "\begin{tikzpicture}"
         !       write(2,*) "\begin{axis}[axis lines = middle, xlabel = $x$, ylabel = $y$]"
         !       write(2,*) "\addplot [thin,color = blue] table {Sec"//titletime//".dat};"
         !       write(2,*) "\addplot[scatter,only marks, mark options = {fill = red}] table {Reinf"//titletime//".dat};"
         !       write(2,*) "\end{axis}"
         !       write(2,*) "\end{tikzpicture}"
         !       write(2,*) "\caption{Skica prereza}"
         !       write(2,*) "\end{figure}"
         !       !write(2,*) "$$A = "//Astring//"\qquad Ix = "//Ixstring//"\qquad Iy = "//Iystring//"$$"
         !       write(2,*) "\end{document}"
         !       close(2)

         !       call execute_command_line ("pdflatex report"//titletime//".tex")
         !       call execute_command_line ("rm report"//titletime//".tex")
         !       call execute_command_line ("rm Sec"//titletime//".dat")
         !       call execute_command_line ("rm Reinf"//titletime//".dat")
         !       call execute_command_line ("rm report"//titletime//".log")
         !       call execute_command_line ("rm report"//titletime//".aux")
        !end if

        print *, "Ko zelis zapret koncas pritisni Enter"
        read *
end program secdesign

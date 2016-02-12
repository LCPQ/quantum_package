program pouet

 implicit none
 read_wf = .true.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer:: i,j

 write(6,*) 'Total orbitals: ', mo_tot_num
 write(6,*) 'Orbitals for entanglement calculation: ', mo_inp_num
 write(6,*) 'Index: ',(mo_inp_list(i),i=1,mo_inp_num)
 write(6,*)
 write(6,*) "s1: One-Orbital von Neumann entropy"
 write(6,'(1000f8.5)') entropy_one_orb
 write(6,*) 
 write(6,*) "s2: Two-Orbital von Neumann entropy"
 do i=1,mo_inp_num
  write(6,'(1000f8.5)') (entropy_two_orb(i,j),j=1,mo_inp_num)
 enddo
 write(6,*) 

! mutal information:
 write(6,*) "Mutual Information (Entanglement)"
 do i=1,mo_inp_num
  write(6,'(1000f8.5)') (mutinf(i,j),j=1,mo_inp_num)
 enddo

 
 open(17,file=(trim(ezfio_filename)//".entanglement"),status='unknown',form='formatted')
  
 write(17,'(1000f8.5)') entropy_one_orb
 do i=1,mo_inp_num
  write(17,'(1000f8.5)') (mutinf(i,j),j=1,mo_inp_num)
 enddo


 close(17)
 write(6,*)
 write(6,*) "The .entanglement file is the input for the entanglement.py script."
 write(6,*) "You can find the script in the directory Scripts of QP."
 write(6,*)
end

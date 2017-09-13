 BEGIN_PROVIDER [ double precision, one_body_dm_mo_detachment, (mo_tot_num,mo_tot_num,2:N_states) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_attachment, (mo_tot_num,mo_tot_num,2:N_states) ]
 implicit none
 BEGIN_DOC
 ! Detachment and attachment density matrices in MO basis
 END_DOC
 integer :: i,j, k, istate
 double precision :: km(mo_tot_num), kp(mo_tot_num)

 one_body_dm_mo_detachment = 0.d0
 one_body_dm_mo_attachment = 0.d0

 do istate=2,N_states

   km(:) = 0.d0
   kp(:) = 0.d0
   do i=1,mo_tot_num
      if (one_body_dm_mo_diff_eigvalues(i,istate) < 0) then
        km(i) = -one_body_dm_mo_diff_eigvalues(i,istate)
      else
        kp(i) =  one_body_dm_mo_diff_eigvalues(i,istate)
      endif
   enddo

   ! Attachment
  do k=1,mo_tot_num
     do j=1,mo_tot_num
       do i=1,mo_tot_num
         one_body_dm_mo_detachment(i,j,istate) = one_body_dm_mo_detachment(i,j,istate) + &
           one_body_dm_mo_diff_eigvectors(i,k,istate)*km(k)* &
           one_body_dm_mo_diff_eigvectors(j,k,istate)
         one_body_dm_mo_attachment(i,j,istate) = one_body_dm_mo_attachment(i,j,istate) + &
           one_body_dm_mo_diff_eigvectors(i,k,istate)*kp(k)* &
           one_body_dm_mo_diff_eigvectors(j,k,istate)
       enddo
    enddo
  enddo

 enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, one_body_dm_ao_detachment, (ao_num,ao_num,2:N_states) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_ao_attachment, (ao_num,ao_num,2:N_states) ]
 implicit none
 BEGIN_DOC
 ! Detachment and attachment density matrices in AO basis
 END_DOC
 integer :: istate
 do istate=2,N_states
   call mo_to_ao_no_overlap(                                         &
       one_body_dm_mo_attachment(1,1,istate),                        &
       size(one_body_dm_mo_attachment,1),                            &
       one_body_dm_ao_attachment(1,1,istate),                        &
       size(one_body_dm_ao_attachment,1) )
   call mo_to_ao_no_overlap(                                         &
       one_body_dm_mo_detachment(1,1,istate),                        &
       size(one_body_dm_mo_detachment,1),                            &
       one_body_dm_ao_detachment(1,1,istate),                        &
       size(one_body_dm_ao_detachment,1) )
 enddo

END_PROVIDER


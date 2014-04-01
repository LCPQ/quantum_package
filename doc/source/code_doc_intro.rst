=============================
The Documentation of the code
=============================



The heart of the problem : how do we compute the perturbation ?
===============================================================

In this section we will present the basic ideas of how do we compute any kind of perturbative quantity.

The main problem
^^^^^^^^^^^^^^^^

Consider a simple problem of perturbation theory in which you have a *general* multireference wave function :math:`| \psi \rangle`
(no trivial way to know the kind of relations between those determinants)  : 

.. math:: 
   | \psi \rangle = \sum_{I=1,N_{det}} c_I | D_I \rangle

and you would like to compute its second order :term:`perturbative energetic correction`, which we can write like this for the sake of simplicity: 

.. math:: 
   E_{PT2} = \sum_{P \, \rm{that} \, \rm{are}  \,  \rm{not}  \, \rm{in}  \, | \psi \rangle } \frac{\langle \psi | H | D_P \rangle^2}{\Delta E_P}

and the :math:`\Delta E_P` will determine what kind of PT you use. Note that you must not double count a determinant :math:`| D_P \rangle` and that you must not count those which are in :math:`| \psi \rangle`.

What you have to do is to apply the :math:`H` operator on this :math:`| \psi \rangle` that would generate a lot of determinants :math:`|D \rangle`, 
and you must find a way to see if :

 #) the determinant :math:`|D \rangle` is in :math:`| \psi \rangle`

 #) the determinant :math:`|D \rangle` have already been counted

How do we do in practice ? We apply :math:`H` succesively on each determinant of :math:`| \psi \rangle` and each :math:`H` application generates a lot of determinant :math:`|D \rangle`. For each determinants :math:`|D \rangle` we check with a very optimized subroutine if 

    #) :math:`|D\rangle` was a single or a double excitation respect to all the determinant on which we previously applyed :math:`H` 
    
       :math:`\Rightarrow` if it is the case then it have already been computed in the past and so we don't double count it.

    #) :math:`|D\rangle` is already in the rest of the :math:`| \psi \rangle`
    
       :math:`\Rightarrow` if it is the case you must not count it.

This subroutine (:samp:`connected_to_ref` ) is called a **HUGE** number of times and so it have been optimized in a proper way. 
Its basis is the :samp:`popcnt` hardware instruction that figures in the :samp:`SSE4.2` releases of processors. 
It allows to know how many bites are set to one in an integer within a few cycles of CPU. 
By manipulation of bits masks you can easily extract the excitation degree between two determinants.

One interesting feature of this approach is that it is easily and efficiently parallelizable (which of cours have been done),
and you can easily reach an parallel efficiency of about :math:`95\%`.


The link between the perturbation and the selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the selected CI algorithm you have general :math:`| \psi \rangle` multi determinantal wave function and you want to make it better 
by proposing some new candidates to enter in this wave function. 
Those candidates must of course not be already in :math:`| \psi \rangle` and since their are selected thanks to their perturbative properties (on the energy or on the coefficient), their are generated through some application of the :math:`H` operator. So we see that we have exactly the same kind of feature than in the perturbation. 

How do we select the determinant in practice ? Exactly like we do the perturbation !

do G = 1, :term:`N_{Generators}`

   #) We apply :math:`H` on one :term:`generators` 
     
        :math:`\Rightarrow` :math:`H|D_G \rangle = \sum_D \langle D | H |D_G \rangle |D \rangle`
    
   #) For each determinant :math:`|D \rangle` we check if it could have been generated from previous :term:`generators` :math:`| D_{G'} \rangle`
     
        :math:`\Rightarrow` If it is not the case we check if it belongs to :math:`| \psi \rangle`
   
   #) We compute its perturbative property 
 
   #) If it is important we put it in a buffer of the potential candidates to the new set of :term:`internal determinants`
 
   #) go to 1

enddo


So once you have applyed :math:`H` on all the :term:`generators`, you sort all the buffer of the candidates by their importance, 
and after you pick up the most important ones, which will enter in the wave function and be diagonalized.


Just to be more precise, what we drescribe here is the standard CIPSI algorithm (which :term:`target space` is always the FCI). In practice, if you replace the :math:`H` operator by the :term:`restricted H operator` defined by the :term:`target` space you have exactly what is emplemented.

The typical feature of an iteration
===================================

An iteration of the selected CI program is always built in the same way. This can be resumed in the following simple tasks.

Iteration : 
  
   #) :term:`restricted H operator` applyed on the :term:`generators`

    :math:`\Rightarrow` :term:`perturbativ action` (*e.g* Selection of some :term:`perturbers`, calculation of the :math:`E_{PT2}^m`, etc ...)

   #) Some update induced by the :term:`perturbative action` (*e.g* diagonalization of the new :math:`H` matrix, etc ...)
   #) Check the :term:`stopping criterion`
   #) Update the :term:`generators` subset
   #) Save restart data if needed
   #) Iterate

To go into details we list the various available options for each task.

The :samp:`restricted_H_apply` like subroutines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 Here we enter into details on the part of the subroutines that is responsible for the :term:`restricted H operator` part of the tasks.

The general ideas
^^^^^^^^^^^^^^^^^

 This subroutine takes in input a determinant (in term of an integer key) and some bits masks
 that are used to restrict the excitations (see the :term:`excitations bits masks` and :term:`excitations restrictions`).
 It generates the singles and doubles excitations from the input determinant and these :term:`excitations bits masks`.
 This subroutine will be applyed on the :term:`generators` determinants to generate the :term:`perturbers`.

 This subroutine in itself does not exist, it is just a skeleton that generates all possible singles and doubles.
 As seen in the previsous section, once you apply :math:`H` on a given determinant, you will use the generated determinants 
 to do a certain number of things that deal with in general a perturbative quantity, this is the :term:`perturbative action`.

 A way to resume what is done in the subroutine and to make a mental representation can be explained like this :


.. code-block:: fortran

     subroutine restricted_H_apply(key_in)
     
     do i = 1, available_holes(1)
      do j = 1, available_holes(2)
       do k = 1, available_particles(1)
        do l = 1, available_particles(2)
         ! you generate some excitations on key_in that will generate some key_out
         call excitation(i,j,k,l,key_in,key_out) 
         ! you exploit key_out to do some perturbative work
         call perturbative_action(key_out)
        enddo
       enddo
      enddo
     enddo
     
     end

 
So we see that here once we have made an excitation on :samp:`key_in` that generates :samp:`key_out`,
we can do some work related to the :term:`perturbative action` on this :samp:`key_out`.

In this simple representation of the subroutine, there are some :samp:`available_holes` and :samp:`available_particle`.
This is due to the :term:`excitation restrictions` that are implicitly defined by the :term:`target space`, 
and to the :term:`restricted orbitals` that are defined by the user.
In practice those  :term:`excitation restrictions` are just the excitations that are going to be allowed to a given :term:`generator determinant`.
We do this by using some :term:`excitations bits masks`.
The :term:`excitation restrictions` and the :term:`restricted orbitals` are built thanks to the use of :term:`excitations bits masks`.

Available :term:`excitation restrictions`  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :term:`excitation restrictions` prohibits some kind of excitations because it is in the definition 
of the :term:`target space` to avoid a certain class of excitation.
For instance, in the :term:`CAS+DDCI` method, you will apply all the single and double excitations on the top of the :term:`CAS wave function`.
After those :term:`excitation restrictions` defined by the :term:`target space`, there can be some kind of excitations that the user wishes to avoid.
For instance, within a :term:`CISD` or a :term:`CAS+DDCI` you can wish that all the excitations of the core electrons can be neglected, 
or that there are some virtuals that are not relevant for a certain kind of correlation effects.

This restrictions are done in the program by defining some classes of orbitals that depend both on the method you would like to use,
and by the specific restrictions you would like to do on the top of that. So we see that there are classes of orbitals that depend on the method, 
and other classes that can be defined for any class of method.

This classes are the the :term:`frozen occupied orbitals` and the :term:`deleted virtual orbitals` .

Available :term:`perturbative action`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

From what we saw previously, when an excitation is performed on a given :term:`generator`, 
depending on the method defined by the user, different actions can be performed at that point of the calculation.

Nevertheless, all this actions here deal with the perturbation, that is why we called this step the :term:`perturbative action`.

The :term:`perturbative action` is very flexible. It consists in doing (or not) a certain kind of things.

When a given determinant :samp:`key_out` is generated, you can :
 
   #) check if this determinants have to be taken into account (see :samp:`connected_to_ref` and :samp:`is_in_ref`)
   #) compute its :term:`perturbative energetic contribution` and its :term:`perturbative coefficient` (see :term:`perturbation theory`)
   #) use those perturbative quantities to do something that deals with it (see :term:`perturbative possibility` )

In principle, for each of those actions one would put a :samp:`if` statement and decline all the possible actions to do. 
However, because there can exist a *lot* of possible action and because this loop is really intern, putting a lot of :samp:`if` statement
is not a good idea and will slow the code. 

To avoid that we generate with a python script all possible subroutines corresponding to some actions, and the program will use the one
that will be defined by the method desired by the user. In this way there is no unnecessary tests in the intern loop, it done in the input.

The :term:`perturbative possibility`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once you have compute the :term:`perturbative energetic contribution` and the :term:`perturbative coefficient` of a given :term:`perturber`,
you must use those quantities. Here is listed what is available : 

       #) accumulate it :term:`perturbative energetic contribution` to compute the :term:`Energetic perturbative correction`
       #) accumulate it :term:`perturbative coefficient` to compute the :term:`first order perturbative norm`
       #) put or not the :samp:`key_out` determinant in a buffer to select some new :term:`intenal determinants` see :term:`selection`
       #) update the arrays of the :term:`correlation energy by holes and particles` (see :term:`CISD+SC2`)
       #) dress all the diagonal matrix elements of the :term:`internal determiants` (see :term:`Dressed MRCI`)
      



Connected to ref / is in ref
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  This subroutine takes in input a determinant (in term of an integer key), an array of determinants :samp:`keys` (containing :samp:`N_det` determinants) 
  and an integer :samp:`i_past` which is smaller or equal to :samp:`N_det`.

  It checks if the input determinant is connected by the :math:`H` matrix to all the determinants in :samp:`keys` that are before :samp:`i_past`.
  It also check if the input determinant is in the whole list of determinants :samp:`keys`.

  In output you have an integer :samp:`c_ref` that have the following values :
     
     #) 0 : the input determinant is not in :samp:`keys` and is not connected to any determinant
        in :samp:`keys` that is before :samp:`i_past`.

     #) +m : the input determinant is connected by the :math:`H` matrix to the *m* th determinant :samp:`keys`.

     #) -m : the input determinant is already in :samp:`keys` and it is the *m* th determinant in :samp:`keys`
  


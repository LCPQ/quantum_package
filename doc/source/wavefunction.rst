=====================================
Selection, perturbation ... keywords
=====================================

.. |CISD| replace:: :abbr:`CISD (Configuration Interaction with Single and Double excitations)`
.. |HF| replace:: :abbr:`HF (Hartree Fock)`
.. |CAS-CI| replace:: :abbr:`CAS-CI (Complete Active Space Configuration Interaction)`
.. |DDCI| replace:: :abbr:`DDCI (Difference Dedicated Configuration Interaction)`

.. glossary:: 
  :sorted:
  
  Energetic perturbative correction 
    Corresponds to the correction to the energy at the second order of a given perturbtation theory 
    to a given state m. 
    By convention it noted :math:`E_{PT2}^m`
 
  Variational energy
    Corresponds to the variational energy of the :term:`selected wave function` for a given state . 
    By convention it noted :math:`E_{Var}^m` for the mth eigenvector.

     .. math::
      E_{Var}^m =   \langle \psi_m |H|\psi_m \rangle 

  Estimated target energy
    Corresponds to the estimation of the target energy for a given :term:`selected wave function` and a given state.
    By convention it noted :math:`E_{Target}^m`.
    Its mathematical expression is :

     .. math::
      E_{Target}^m =   E_{Var}^m + E_{PT2}^m

  Selected wave function
    Corresponds to the wave function that have been previously selected for a given state m at a current iteration.
    This wave function is defined by the set of the :term:`internal determinants` and by their coefficients
    on the state m.
    By convention it is noted :math:`|\psi_m\rangle`

     .. math::
      | \psi_m \rangle = \sum_{I=1,N_{selected}} c_I^m | D_I \rangle




  EN EG 
    Stands for Eipstein Nesbet with EigenValues zeroth order energy perturbation theory.
    It is a state specific 2nd order perturbation theory. Here m is the index of the eigenstate.
    The :math:`H_0` of this PT is defined as the diagonal part of the Hamiltonian such as
    the :math:`E_m` is equal to the average value of the Hamiltonian on the :term:`selected wave function`
    and the :math:`E_P` is equal to the average value of the Hamiltonian on the :term:`perturbers`

    This perturbation have bad formal properties but some nice numerical features of convergence.


    From the definition, one get the first order coefficient and its related second order energetic contribution of a a perturber :

     .. math::
       c_{D_P}^m= \sum_{S=1,N_{\rm selectors}} \frac{c_S^m \langle D_S|H|D_P\rangle}{  \langle \psi_m |H|\psi_m \rangle - \langle D_P|H| D_P \rangle } \\
       e_{D_P}^m= \frac{(\sum_{S=1,N_{\rm selectors}}  c_S^m \langle D_S|H|D_P\rangle)^2}{\langle \psi_m |H|\psi_m \rangle - \langle D_P|H| D_P \rangle }


  Stopping criterion
    Condition decided by the user to stop the calculation. 
    This criterion might be on the :term:`Energetic perturbative correction`, on the number of :term:`internal determinants` N_selected_max
    or on the stability of the :term:`estimated target energy`
    The user can also send a Ctrl+C to stop the calculation, and it will kill itself properly, saving the datas that need to be saved.

 
  Target wave function
    Wave function of the :term:`target space`



  Target space
    Target of the CI calculation. Defining a method (CISD, CAS-CI and so on) is equivalent to define the :term:`target space`.

    The target space defines the rules to define the :term:`Generators` ,
    the rules of the :term:`excitation restrictions`,
    and the perturbation theory to be used. 

    There are two type of methods/:term:`target space` proposed in the code : 

      #) the CAS-CI type methods where you do not restrict any kind of excitation degree within a given list of orbitals.
      #) the singles and doubles excitations on the top of a given reference wave function (:term:`CISD`, :term:`CISD+SC2`, :term:`CAS+SD`, :term:`CAS+DDCI`, :term:`CAS+MRPT2`)

    Their is a great difference between those two types of method in the way it is implemented.

    In the CAS-CI method, when you have chosen an :term:`active space` (so a list of orbitals and electrons to make a FCI within this active space), 
    all the :term:`Internal determinants` that have been selected and that form the :term:`selected wave function` 
    can potentially be part of the :term:`generators`, by mean that the :term:`restricted H operator` 
    could be potentially applyed on all the :term:`internal determinants` to generate some other :term:`perturbers`.

    In the singles and doubles excitation on the top of a given reference wave function, the subset of :term:`generators` 
    and so the rules to recognize them, is fixed at the begining of the method. Those :term:`generators` are precisely 
    all the determinants forming the :term:`reference wave function`.


    There are the different :term:`target space` that are available :

       #) :term:`CISD`
       #) :term:`CISD+SC2`
       #) :term:`CASCI`
       #) :term:`CASCI+S`
       #) :term:`CASCI+SD`
       #) :term:`CASCI+DDCI`
       #) :term:`CASCI+DDCI+(2h-2p)PT2`
       #) :term:`CAS-CI+MRPT2`


  Target energy 
    Energy of the target space.

  H operator
    Hamiltonian operator defined in terms of creation and anihilation operators in the spin orbital space.

  Excitation restrictions
    Restriction in the :term:`H operator` that the user imposes to define the target sapce.
    For example : 

    1) If one freeze some core orbitals or delete some virtuals, it is an :term:`excitation restrictions`
    2) If one prohibits the pure inactive double excitations in a CAS+SD one get a DDCI
    3) any kind of restriction in the full application of the :term:`H operator`
 
   
  Restricted H operator
    :term:`H operator` taking into account the :term:`Excitation restrictions`


  CISD+SC2
    Method developped by JP. Malrieu that can be seen as a cheap approximation of the CCSD. 
    It makes a CISD size consistant and separable for closed shell systems. 
    It is based on a CISD calculation 
    where the diagonal part of the H matrix is dressed by the repeatable correlation energy previsously obtained.
    So it is a CISD dressed by the disconnected triples and quadruples.

  Generators
    Set of generator determinants.
    By convention a generator is written as :math:`|D_G\rangle` .
    A generator determinant is a determinant on which
    the :term:`restricted H operator` is being applied for the selection and/or the perturbation.

  Internal determinants
    Selected determinants in terms of integers keys.
    By convention an Internal determinant is written as :math:`|D_I\rangle` .
    By convention, the :term:`Generators` are at the begining of the array.

  Intern space
    Set of all the :term:`internal determinants`.
 
  Perturbers
    Determinants within the :term:`target space` but taht are not already included in the :term:`intern space`.
    They are created from the :term:`Generators` that belongs :term:`Intern space` for a given :term:`selected wave function`.
    By convention a perturber is written as :math:`|D_P\rangle`.
 
  Selectors 
    Determinants that are used to compute the perturbative properties of the :term:`perturbers`.
    By convention a selector is written as :math:`|D_S\rangle` .
    The selectors are a subset of determinant of the total wave function (that is the :term:`Internal determinants`).
    This subset contains at least the :term:`Generators` determinants.
    The perturbative properties (energy, coefficient or else) of the :term:`perturbers` are calculated on all the :term:`selectors` :math:`|D_S\rangle`

     .. math::

       c_{D_S}= \sum_{S=1,N_{\rm selectors}} \frac{c_S\langle D_S|H|D_P\rangle}{\Delta E_{P,S}} \\
       e_{D_S}=  \frac{(\sum_{S=1,N_{\rm selectors}} c_S  \langle D_S|H|D_P\rangle) ^2}{\Delta E_{P,S}}
    




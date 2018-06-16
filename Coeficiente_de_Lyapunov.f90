PROGRAM Coeficiente_de_Lyapunov

!VARIAVEIS!

	!Constantes!
	REAL(8), PARAMETER :: r = 2.5 !Taxa de crescimento!
    
	!Razoes iniciais!
	REAL(8) :: x = 0.3d0 

	!Lyapunov!
	REAL(8) :: lamb 

!VARIAVEIS DE TRABALHO!

	INTEGER :: i
	REAL(8) :: soma

!CONDICOES INICIAIS!

	soma = 0

!ITERACAO!

	DO i = 1, 100 !Geracoes!
	
		!Recalculo das razoes!
		x = r*x*(1 - x)

		!Lyapunov!
		lamb = LOG(ABS(r*(1 - 2*x)))

		!Recalculo das somas!
		soma = lamb + soma
		
	END DO

	WRITE(*,*) soma/(101)

END PROGRAM

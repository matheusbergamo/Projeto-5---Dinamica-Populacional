PROGRAM bifurcacao	

!VARIAVEIS!

	REAL(8) :: r !Taxa de crescimento!
	REAL(8) :: x !Razao populacional!

!VARIAVEIS E CONSTANTES DE TRABALHO!

	INTEGER :: i
	REAL(8), PARAMETER :: dr = 1e-3

!CONDICOES INICIAIS!
	
	r = 2.5
	x = 0.5d0

!FORMATACAO E OUTPUT!

	100 FORMAT (' ', f20.15, f20.15)
	OPEN(10, FILE = 'bifurcacao.dat')

!ITERACAO!

	DO WHILE (r .LT. 5)

		DO i = 1, 100

		x = r*x*(1 - x)

			IF (i .GT. 70) THEN

				WRITE(10,100) r, x

			END IF

		END DO

		r = r + dr

	END DO

!PLOTAGEM!

	CALL system ('gnuplot bifurcacao.gnu')
	CALL system ('display bifurcacao.png')

END PROGRAM

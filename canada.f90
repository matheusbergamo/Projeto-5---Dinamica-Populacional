PROGRAM Dinâmica_de_Lebres_e_Linces

!VARIAVEIS!

	REAL(8) :: t !Tempo!
	REAL(8) :: x !Numero de lebres!
	REAL(8) :: y !Numero de linces!

!CONSTANTES!

	REAL(8), PARAMETER :: ano = 1.0d0
	REAL(8), PARAMETER :: a = 0.481d0
	REAL(8), PARAMETER :: b = 0.025d0
	REAL(8), PARAMETER :: c = 0.927d0
	REAL(8), PARAMETER :: d = 0.028d0

!VARIAVEIS E CONSTANTES DE TRABALHO!

	INTEGER :: i
	REAL(8), PARAMETER :: dt = 1e-4*ano

!CONDICOES INICIAIS!

	!Tempo!
	t = 0

	!Numero de lebres!
	x = 30

	!Numero de linces!
	y = 4

!FORMATACAO E OUTPUT!

	100 FORMAT (' ', f10.4, f10.1, f10.1)
	OPEN(10, FILE = 'canada.dat') 

	WRITE(10,100) t + 1900, x, y

!METODO DE EULER-CROMER!

	DO i = 1, 200000

		!Recalculo do tempo!
		t = i*dt

		!Recalculo do numero lebres!
		x = x + (a*x - b*x*y)*dt
	
		!Recalculo do numero de linces!
		y = y + (-c*y + d*x*y)*dt
	
		!Output!
		WRITE(10,100) t + 1900, x, y

	END DO	

!PLOTAGEM!

	CALL system ('gnuplot canada.gnu')
	CALL system ('display canada.png')

END PROGRAM

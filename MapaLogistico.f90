PROGRAM Mapa_Logistico

!VARIAVEIS!

    REAL(8) :: x = 0.3d0 !Razao entre populacao inicial e a maxima!
	REAL(8) :: y = 0.6d0
	REAL(8) :: z = 0.9d0
	REAL(8), PARAMETER :: r = 2.5!Taxa de crescimento!

!VARIAVEIS DE TRABALHO!

    INTEGER :: i = 0

!FORMATACAO E OUTPUT!

    100 FORMAT (' ', i5, ' ', f20.15, f20.15, f20.15)
    OPEN(10, FILE = 'mapa.dat')
	WRITE(10,100) i, x, y, z

!ITERACAO!

    DO i = 1, 100 !Geracoes!

        !Recalculo do mapa!
        x = r*x*(1 - x)
		y = r*y*(1 - y)
		z = r*z*(1 - z)

        !Output!
        WRITE(10,100) i, x, y, z

    END DO

    CLOSE(10)

!PLOTAGEM!

	CALL system ('gnuplot plotmapa.gnu')
	CALL system ('display mapar25.png')

END PROGRAM

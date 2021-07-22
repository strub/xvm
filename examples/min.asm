	GSB     	__main
	STOP
__min:
	FETCH   	2       
	FETCH   	2       
	LT
	GTZ     	l000
	FETCH   	2       
	PXR
	RET
	GTO     	l001
l000:
	FETCH   	1       
	PXR
	RET
l001:
	RET
__main:
	PUSH    	3       
	PUSH    	5       
	GSB     	__min
	POP
	POP
	PRX
	PRT
	RET

	GSB     	__main
	STOP
__fact:
	PUSH    	0       
	PUSH    	1       
	WRST    	1       
	POP
l000:
	PUSH    	0       
	FETCH   	3       
	LT
	GTZ     	l001
	FETCH   	2       
	FETCH   	1       
	MUL
	WRST    	1       
	POP
	FETCH   	2       
	PUSH    	1       
	SUB
	WRST    	3       
	POP
	GTO     	l000
l001:
	FETCH   	0       
	PXR
	POP
	RET
	POP
	RET
__main:
	PUSH    	5       
	GSB     	__fact
	POP
	PRX
	PRT
	RET

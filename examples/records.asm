	GSB     	__main
	STOP
__add2:
	PUSH    	0       
	FETCH   	2       
	RHP
	PUSH    	1       
	FETCH   	3       
	RHP
	ADD
	PXR
	RET
	RET
__add3:
	PUSH    	0       
	FETCH   	2       
	RHP
	PUSH    	1       
	FETCH   	3       
	RHP
	ADD
	PUSH    	2       
	FETCH   	3       
	RHP
	ADD
	PXR
	RET
	RET
__main:
	PUSH    	0       
	PUSH    	3       
	ALLOC
	WRST    	1       
	POP
	PUSH    	10      
	PUSH    	0       
	FETCH   	2       
	WHP
	PUSH    	5       
	PUSH    	1       
	FETCH   	2       
	WHP
	PUSH    	7       
	PUSH    	2       
	FETCH   	2       
	WHP
	FETCH   	0       
	GSB     	__add2
	POP
	PRX
	PRT
	FETCH   	0       
	GSB     	__add3
	POP
	PRX
	PRT
	POP
	RET

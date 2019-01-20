{$IFNDEF UNIVDISASM_CONFIG}

{$DEFINE UNIVDISASM_CONFIG}

{$DEFINE NEED_DISPLAY} // Undef if you don't want to have instruction str.

{$DEFINE MustInline}  // Delphi will inline small functions if MustInline defined.   

{
==> NEED_VFORM <==
	If defined, UnivDisasm will distinguish between VEX instructions 
	and non VEX instructions.
	E.g:
	(If defined):     "VMOVLPS" InstID is NOT the same as "MOVLPS" InstID.
	(If not defined): "VMOVLPS" is THE SAME as "MOVLPS" InstID.
}

{ .$DEFINE NEED_VFORM } 

{$ENDIF UNIVDISASM_CONFIG}

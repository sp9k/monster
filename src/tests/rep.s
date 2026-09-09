.org $7700
.mac aslrol a
	asl
	rol a
.endmac
.rep 10,i
.endrep

.rep 10,i
	aslrol $1000+i
.endrep

.rep 1
 .db $01,$02,$03,$04,$05,$06,$07,$08,$09
.endrep

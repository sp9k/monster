;*******************************************************************************
; TABLE.ASM
; This file contains routines for reading, writing, modifying, and removing
; items to/from memory.
;*******************************************************************************

.segment "SHAREBSS"

start:      .res 3	; start address of the table data
capacity:   .res 3	; capacity of the table in bytes
iterator:   .res 3	; pointer to current location in the table
size:       .byte 0	; size of each element in the table (max $ff)
comparator: .word 0	; function to compare elements (when inserting sorted)

;*******************************************************************************
; SETUP
; Initializes table state for successive calls: the size of elements, etc.
; IN:
;   tab::el_sz: the size of each element in the table
;   tab::start: the start address of the table's data (24-bit)
.export __tab_setup
.proc __tab_setup

.endproc

;*******************************************************************************
; NEXT
; Moves to the next element in the table
.export __tab_next
.proc __tab_next
	lda iterator
	clc
	adc size
	sta iterator
	bcc :+
	inc iterator+1
	bne :+
	inc iterator+2
:	rts
.endproc

;*******************************************************************************
; COMPARE
; Compares the element at the current pointer with the given data
; IN:
;   - .XY: the address of the data to compare
; OUT:
;   - .Z: set if the given data and contents at (iterator) match (up to table
;         element size)
.export __tab_compare
.proc __tab_compare

.endproc

;*******************************************************************************
; ADD
; Adds an item to the table
; The item is placed at the next free location at the end of the table
.export __tab_add
.proc __tab_add

.endproc

;*******************************************************************************
; DEL
; Deletes an item from the table
.export __tab_del
.proc __tab_del

.endproc

;*******************************************************************************
; FIND
; Seeks the table for the given element.
; Assumes the table has been sorted first
; OUT:
;   - .XY: index of the element in the table
;   - .Z:  set if the item does not exist
.export __tab_find
.proc __tab_find
.endproc

;*******************************************************************************
; SORT
; Sorts the table using the configured comparator (tab::setup)
.export __tab_sort
.proc __tab_sort

.endproc

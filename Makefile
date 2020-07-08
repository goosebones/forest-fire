#
# Makefile for CCS Project - Forest Fire
#

#
# Location of the processign programs
#
RASM  = /home/fac/wrc/bin/rasm
RLINK = /home/fac/wrc/bin/rlink

#
# Suffixes to be used or created
#
.SUFFIXES:	.asm .obj .lst .out

#
# Transform rule: .asm into .obj
#
.asm.obj:
	$(RASM) -l $*.asm > $*.lst

#
# Transformation rule: .obj into .out
#
.obj.out:
	$(RLINK) -m -o $*.out $*.obj > $*.map

#
# Object files
#
OBJECTS = forestfire.obj
OBJECTS2 = forestfire2.obj

#
# Main target
#
forestfire.out: $(OBJECTS)
	$(RLINK) -m -o forestfire.out $(OBJECTS) > forestfire.map

forestfire2.out:	$(OBJECTS2)
	$(RLINK) -m -o forestfire2.out $(OBJECTS2) > forestfire2.map

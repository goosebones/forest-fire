#
# FILE:		$File$
# AUTHOR:	Gunther Kroth, gdk6217@rit.edu
#
# DESCRIPTION:
#	This program is an implementation of a forest fire simulation
#
# ARGUMENTS:
#	None
#
# INPUT:
#	First line - size of square grid, integer
#	Second line - number of generations to run, integer
#	Third line - wind direction, character
#	n lines remainding - string representation of a strip of forest
#
# OUTPUT:
#	Formatted string representations of generations from the forest
#
# ----------------------------------------------------------------------------

#
# CONSTANTS
#

# syscall codes
PRINT_INT = 	1
PRINT_STRING =	4
READ_INT = 	5
READ_STRING = 	8

# framesizes
FS24 = 		24
FS20 = 		20
FS16 = 		16
FS12 = 		12
FS8 =           8
FS4 = 		4

#
# DATA AREAS
#
	.data
	.align	0 		# string data doesn't have to be aligned

# _error strings
errorGridSize:
	.asciiz "ERROR: invalid grid size\n"
errorGenerations:
	.asciiz "ERROR: invalid number of generations\n"
errorWindDirection:
	.asciiz "ERROR: invalid wind direction\n"
errorGridCharacter:
	.asciiz	"ERROR: invalid character in grid\n"

# wind directions
char_N:
	.ascii	"N"
char_E:
	.ascii	"E"
char_S:
	.ascii	"S"
char_W:
	.ascii	"W"

# grid characters
char_B:
	.ascii 	"B"
char_t:
	.ascii	"t"
char_.:
	.ascii	"."
char_star:
	.ascii	"*"

# banner prints
banner1:
	.asciiz	"+-------------+\n"
banner2:
	.asciiz "| FOREST FIRE |\n"
newline:
	.asciiz "\n"

# generation prints
genHeadL:
	.asciiz	"==== #"
genHeadR:
	.asciiz " ====\n"
char_plus:
	.asciiz	"+"
char_dash:
	.asciiz	"-"
char_line:
	.asciiz	"|"

# used to pull chars from the forest array and print them using print_string
buff_char:
	.asciiz	"Q"

# junk holder used to absorb newline characters when processing input
junk:
	.asciiz "Q"

# simulation variable storage
windDirection:
	.asciiz "Q"

# used as a way to prevent out-of-bounds when checking northern neighbors
arrayUpperPad:
	.ascii 	"******************************"
# forest grid, sized to the largest possible 30x30 grid
array:
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii	"******************************"
# used as a way to prevent out-of-bounds when checking southern neighbors
arrayLowerPad:
	.ascii	"******************************"

# secondary 30x30 grid used for generating the new generations
# used because a new generation cannot be computed in place
newGen:
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.ascii 	"******************************"
	.asciiz "******************************"

# ----------------------------------------------------------------------------

#
# CODE AREAS
#
	.text		# this is program code
	.align 2	# instructions must be made on word boundires

	.globl	main	# main is a global label

#
# EXECUTION BEGINS HERE
#

#
# Name:		Main
#
# Description:	Main logic for the program.
#
#		The program reads n+3 values from standard input:
#		1) an integer representing the side length of the grid, n
#		2) an interger representing the number of generations to run
#		3) a character representing the wind direction
#		n) strings representing a stip of the forest grid
#

main:
	# stack frame
	addi	$sp, $sp, -FS12
	sw	$ra, -4+FS12($sp)
	sw	$s1, -8+FS12($sp)
	sw	$s0, -12+FS12($sp)

	# print program banner
	jal	printBanner

	# read grid size
	li	$s0, 0
	li	$v0, READ_INT
	syscall

	# bound checking for gridSize
	li	$t7, 4				# lower bound
	li	$t8, 30				# upper bound
	slt	$t9, $v0, $t7			# if (gridSize < 4)
	bne	$t9, $zero, badGridSize		# error
	slt	$t9, $t8, $v0			# if (30 < gridSize)
	bne	$t9, $zero, badGridSize		# error
	move	$s0, $v0			# s0 = gridSize

	# read number of generations
	li	$s1, 0
	li	$v0, READ_INT
	syscall

	# bound checking for genCount
	li	$t7, 0				# lower bound
	li	$t8, 20				# uper bound
	slt	$t9, $v0, $t7			# if (genCount < 0)
	bne	$t9, $zero, badGenCount		# error
	slt	$t9, $t8, $v0			# if (20 < genCount)
	bne	$t9, $zero, badGenCount		# error
	move	$s1, $v0			# s1 = genCount

	# read wind direction
	la	$a0, windDirection		# windDirect stored in reserve
	addi	$a1, $zero, 2
	li	$v0, READ_STRING
	syscall

	# get rid of newline character in the input stream
	la	$a0, junk			# junk is a reserved byte
	addi	$a1, $zero, 2
	li	$v0, READ_STRING
	syscall

	# check for valid wind direction
	lbu 	$t4, windDirection		# char at reserved location
	lbu	$t5, char_N
	beq	$t4, $t5, goodWindDirection	# check for North
	lbu	$t6, char_E
	beq	$t4, $t6, goodWindDirection	# check for East
	lbu	$t7, char_S
	beq	$t4, $t7, goodWindDirection	# check for South
	lbu	$t8, char_W
	beq	$t4, $t8, goodWindDirection	# check for West
	# _if character made it this far, then windDirection is invalid
	j	badWindDirection

badGridSize:
	li	$v0, PRINT_STRING		# print error message
	la	$a0, errorGridSize
	syscall
	j 	exit				# exit main function
badGenCount:
	li	$v0, PRINT_STRING		# print error message
	la	$a0, errorGenerations
	syscall
	j	exit				# exit main fuction
badWindDirection:
	li	$v0, PRINT_STRING		# print error message
	la	$a0, errorWindDirection
	syscall
	j 	exit				# exit main function

goodWindDirection:
	# all static inputs are good if we made it here
	# now we will get the initial grid from the user
	j	getGrid

getGrid:
	move	$a0, $s0			# pass in gridSize
	jal 	getInitialGrid

	# check for valid grid
	# getInitial grid returns 1 if valid, 0 if invalid
	# the error message is printed in the sub routine if invalid
	slt	$t9, $zero, $v0			# if (v0 <= 0)
	beq	$t9, $zero, exit		# error in grid
doSim:
	# _if we made it this far, all input is valid
	move	$a0, $s0			# pass in gridSize
	move	$a1, $s1			# pass in genCount
	jal	simulate

exit:
	# stack frame
	lw	$ra, -4+FS12($sp)
	lw	$s1, -8+FS12($sp)
	lw	$s0, -12+FS12($sp)
	addi	$sp, $sp, FS12
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		simulate
# Description:	do the forest fire simulation
# Arguments:	a0:	integer representing the grid size
#		a1:	integer representing the number of generations
# Destroys:	t9
#

simulate:
	# stack frame
	addi	$sp, $sp, -FS16
	sw	$ra, -4+FS16($sp)
	sw	$s2, -8+FS16($sp)
	sw	$s1, -12+FS16($sp)
	sw	$s0, -16+FS16($sp)

	# save local variables
	move	$s0, $a0			# s0 = gridSize
	move	$s1, $a1			# s1 = genCount
	addi	$s1, $s1, 1			# get genCount on correct index
	li	$s2, 0				# s2 = currentGen

	# print initial grid
	move	$a0, $s0			# pass in gridSize
	move	$a1, $s2			# pass in currentGen
	jal	printGrid
	addi	$s2, $s2, 1			# currentGen += 1

simulateLoop:
	slt	$t9, $s2, $s1			# if (currentGen >= genCount)
	beq	$t9, $zero, simulateDone	# all gens done

	# sim one generation
	move	$a0, $s0			# pass in gridSize
	jal	advanceGeneration		# do 1 generation

	# print out the new forest grid
	move	$a0, $s0			# pass in gridSize
	move	$a1, $s2			# pass in currentGen
	jal	printGrid

	# increment currentGen and redo
	addi	$s2, $s2, 1			# currentGen += 1
	j	simulateLoop

simulateDone:
	# stack frame
	lw	$ra, -4+FS16($sp)
	lw	$s2, -8+FS16($sp)
	lw	$s1, -12+FS16($sp)
	lw	$s0, -16+FS16($sp)
	addi	$sp, $sp, FS16
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		advanceGeneration
# Description:	advence the forest by 1 generation
# Arguments:	a0:	integer representing the grid size
# Destroys:	none
#

advanceGeneration:
	# stack frame
	addi	$sp, $sp, -FS8
	sw	$ra, -4+FS8($sp)
	sw	$s0, -8+FS8($sp)

	# store local vars
	move	$s0, $a0		# s0 = gridSize

	# do burning cells
	mul	$a0, $s0, $s0		# pass in # of forest cells
	jal	simBurningCells

	# do trees burning
	move	$a0, $s0		# pass in gridSize
	jal	simTreeBurn

	# do trees spreading
	move	$a0, $s0		# pass in gridSize
	lbu	$a1, windDirection	# pass in windDirection
	jal	simTreeSpread

	# move the new generation in
	mul	$a0, $s0, $s0		# pass in # of forest cells
	la	$a1, newGen		# a1 = copy from array
	la	$a2, array		# a2 = copy to array
	jal 	copyArray

	# stack frame
	lw	$ra, -4+FS8($sp)
	lw	$s0, -8+FS8($sp)
	addi	$sp, $sp, FS8
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		simBurningCells
# Description:	perform the generation change on the buring cells
#		burning cells from the previous gen become grass cells
#		  in the current gen
# Arguments:	a0:	integer representing number of cells in the forest
# Destroys:	t0, t1, t7, t8, t9
#

simBurningCells:
	# stack frame
	addi	$sp, $sp, -FS16
	sw	$ra, -4+FS16($sp)
	sw	$s2, -8+FS16($sp)
	sw	$s1, -12+FS16($sp)
	sw	$s0, -16+FS16($sp)

	# save local vars
	move	$s0, $a0		# s0 = byteCount
	la	$s1, array		# s1 = array
	la	$s2, newGen		# s2 = newGen
	li	$t0, 0			# t0 = offsetIndex

simBurns:
	slt	$t9, $zero, $s0		# if (byteCount <= 0)
	beq	$t9, $zero, burnsDone	# done burning

	# get cell
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex]

	# check for burning cell
	lbu	$t1, char_B
	beq	$t7, $t1, burningCell	# found a burning cell
	j 	notBurning		# no burning cell

notBurning:
	# store whatever was already there
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	sb	$t7, 0($t8)		# newGen[offsetIndex] = cell
	j 	burnRepeat

burningCell:
	# store a grass in newGen
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	lbu	$t1, char_.
	sb	$t1, 0($t8)		# newGen[offsetIndex] = grass
	j	burnRepeat

burnRepeat:
	# update indexes and repeat
	addi	$t0, $t0, 1		# offsetIndex += 1
	addi	$s0, $s0, -1		# byteCount -= 1
	j 	simBurns

burnsDone:
	# stack frame
	lw	$ra, -4+FS16($sp)
	lw	$s2, -8+FS16($sp)
	lw	$s1, -12+FS16($sp)
	lw	$s0, -16+FS16($sp)
	addi	$sp, $sp, FS16
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		simTreeBurn
# Description:	perform the generation change that makes trees burn
# 		a tree from the previous gen with any burning neighbors (nsew)
#		   will become a burning cell in the current gen
# Arguemtns:	a0:	integer representing the size of the forest grid
# Destroys:	t0, t1, t6, t7, t8, t9
#

simTreeBurn:
	# stack frame
	addi	$sp, $sp, -FS20
	sw	$ra, -4+FS20($sp)
	sw	$s7, -8+FS20($sp)
	sw	$s2, -12+FS20($sp)
	sw	$s1, -16+FS20($sp)
	sw	$s0, -20+FS20($sp)

	# save local vars
	mul	$s0, $a0, $a0		# s0 = byteCount
	la	$s1, array		# s1 = array
	la	$s2, newGen		# s2 = newGen
	move	$s7, $a0		# s7 = gridSize
	li	$t0, 0			# t0 = offsetIndex

treeBurnLoop:
	slt	$t9, $zero, $s0		# if (byteCount <= 0)
	beq	$t9, $zero, treeBurnDone# done burning trees

	# get cell
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex]

	# check for tree cell
	lbu	$t1, char_t
	beq	$t7, $t1, treeCell	# found a tree cell
	j	notTreeCell		# no tree cell

notTreeCell:
	# ignore this cell and try the next
	j	treeBurnRepeat

treeCell:
	lbu	$t1, char_B		# we are searching for B cells

treeBurnNorth:
	# check north
	li	$t6, -1
	mul	$t6, $t6, $s7		# t6 = -(gridSize)
	add	$t8, $t8, $t6		# t8 = (array + offsetIndex) - gridSize
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex - gridSize]
	beq	$t7, $t1, burnTree	# found a tree to burn

treeBurnSouth:
	# check south
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	add	$t8, $t8, $s7		# t8 = array + offsetIndex + gridSize
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex + gridSize]
	beq	$t7, $t1, burnTree	# found a tree to burn

treeBurnWest:
	# check west
	rem	$t6, $t0, $s7		# t6 = offsetIndex % gridSize
	beq	$t6, $zero, treeBurnEast# this tree is on the left boundry
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	addi	$t8, $t8, -1		# t8 = array + offsetIndex - 1
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex - 1]
	beq	$t7, $t1, burnTree	# found a tree to burn

treeBurnEast:
	# check east
	addi	$t6, $t0, 1		# t6 = offsetIndex + 1
	rem	$t6, $t6, $s7		# t6 = (offsetIndex + 1) % gridSize
	beq	$t6, $zero, keepTree	# this tree is on the right boundry
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	addi	$t8, $t8, 1		# t8 = array + offsetIndex + 1
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex + 1]
	beq	$t7, $t1, burnTree	# found a tree to burn

	# _if we made it this far, the tree does not burn
	j	keepTree

keepTree:
	# store the tree in the new generation
	lbu	$t7, char_t
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	sb	$t7, 0($t8)		# newGen[offsetIndex] = tree
	j	treeBurnRepeat

burnTree:
	# this tree burns in the new generation
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	lbu	$t1, char_B
	sb	$t1, 0($t8)		# newGen[offsetIndex] = burn
	j	treeBurnRepeat

treeBurnRepeat:
	# update indexes and repeat
	addi	$t0, $t0, 1		# offsetIndex += 1
	addi	$s0, $s0, -1		# byteCount -= 1
	j 	treeBurnLoop

treeBurnDone:
	# stack frame
	lw	$ra, -4+FS20($sp)
	lw	$s7, -8+FS20($sp)
	lw	$s2, -12+FS20($sp)
	lw	$s1, -16+FS20($sp)
	lw	$s0, -20+FS20($sp)
	addi	$sp, $sp, FS20
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		simTreeSpread
# Description:	perform the generation change that makes trees spread
#		a tree from the previous gen will spread in the direction of
#		  the wind if the neighboring cell is a grass cell
# Arguments:	a0:	integer representing the size of the forest grid
#		a1:	null terminated string that represents wind direction
# Destroys:	t0, t1, t2, t6, t7, t8, t9
#

simTreeSpread:
	# stack frame
	addi	$sp, $sp, -FS24
	sw	$ra, -4+FS24($sp)
	sw	$s7, -8+FS24($sp)
	sw	$s6, -12+FS24($sp)
	sw	$s2, -16+FS24($sp)
	sw	$s1, -20+FS24($sp)
	sw	$s0, -24+FS24($sp)

	# save local vars
	mul	$s0, $a0, $a0		# s0 = byteCount
	la	$s1, array		# s1 = array
	la	$s2, newGen		# s2 = newGen
	move	$s6, $a1		# s6 = windDirection
	move	$s7, $a0		# s7 = gridSize
	li	$t0, 0			# t0 = offsetIndex

treeSpreadLoop:
	slt	$t9, $zero, $s0		# if (byteCount <= 0)
	beq	$t9, $zero, treeSpreadDone

	# get cell
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex]

	# check for grass cell
	lbu	$t1, char_.		# looking for . cells
	beq	$t7, $t1, grassCell	# found grass cell
	j	notGrassCell		# no grass cell

notGrassCell:
	# ignore this cell and try the next
	j	treeSpreadRepeat

grassCell:
	# we have a grass cell, now we look for T cells
	lbu	$t1, char_t		# searching for trees
	lbu	$t2, char_N		# north
	beq	$s6, $t2, treeSpreadNorth
	lbu	$t2, char_S		# south
	beq	$s6, $t2, treeSpreadSouth
	lbu	$t2, char_E		# east
	beq	$s6, $t2, treeSpreadEast
	lbu	$t2, char_W		# west
	beq	$s6, $t2, treeSpreadWest

treeSpreadNorth:
	# _if wind is north, check the south neighbor
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	add	$t8, $t8, $s7		# t8 = array + offsetIndex + gridSize
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex + gridSize]
	beq	$t7, $t1, changeGrass	# found a tree to spread onto the grass
	j 	keepGrass		# no tree found to spread

treeSpreadSouth:
	# _if wind is south, check the north neighbor
	li	$t6, -1
	mul	$t6, $t6, $s7		# t6 = -(gridSize)
	add	$t8, $t8, $t6		# t8 = (array + offsetIndex) - gridSize
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex - gridSize]
	beq	$t7, $t1, changeGrass	# found a tree to spread onto the grass
	j	keepGrass		# no tree found to spread

treeSpreadEast:
	# _if wind is east, check the west neighbor
	rem	$t6, $t0, $s7		# t6 = offsetIndex % gridSize
	beq	$t6, $zero, keepGrass	# grass is on the left boundry
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	addi	$t8, $t8, -1		# t8 = array + offsetIndex - 1
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex - 1]
	beq	$t7, $t1, changeGrass	# found a tree to spread onto the grass
	j	keepGrass		# no tree found to spread

treeSpreadWest:
	# _if wind is west, check the east neighbor
	addi	$t6, $t0, 1		# t6 = offsetIndex + 1
	rem	$t6, $t6, $s7		# t6 = (offsetIndex + 1) % gridSize
	beq	$t6, $zero, keepGrass	# grass is on the right boundry
	add	$t8, $s1, $t0		# t8 = array + offsetIndex
	addi	$t8, $t8, 1		# t8 = array + offsetIndex + 1
	lbu	$t7, 0($t8)		# t7 = array[offsetIndex + 1]
	beq	$t7, $t1, changeGrass	# found a tree to spread onto the grass
	j	keepGrass		# no tree found to spread

keepGrass:
	# store a grass cell in the new generation
	lbu	$t7, char_.
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	sb	$t7, 0($t8)		# newGen[offsetIndex] = grass
	j	treeSpreadRepeat

changeGrass:
	# transform the grass cell into a new tree
	lbu	$t7, char_t
	add	$t8, $s2, $t0		# t8 = newGen + offsetIndex
	sb	$t7, 0($t8)		# newGen[offsetIndex] = tree
	j	treeSpreadRepeat


treeSpreadRepeat:
	# update indexes and repeat
	addi	$t0, $t0, 1		# offsetIndex += 1
	addi	$s0, $s0, -1		# byteCount -=1
	j	treeSpreadLoop

treeSpreadDone:
	# stack frame
	lw	$ra, -4+FS24($sp)
	lw	$s7, -8+FS24($sp)
	lw	$s6, -12+FS24($sp)
	lw	$s2, -16+FS24($sp)
	lw	$s1, -20+FS24($sp)
	lw	$s0, -24+FS24($sp)
	addi	$sp, $sp, FS24
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		copyArray
# Description:	move the contents of one array to another
# Arguments:	a0:	integer representing number of bytes to copy over
#		a1:	address of array to take bytes from
#		a2:	address of array to copy bytes to
# Destorys:	t0, t7, t8, t9
#

copyArray:
	# stack frame
	addi	$sp, $sp, -FS16
	sw	$ra, -4+FS16($sp)
	sw	$s2, -8+FS16($sp)
	sw	$s1, -12+FS16($sp)
	sw	$s0, -16+FS16($sp)

	# save local vars
	move	$s0, $a0		# s0 = byteCount
	move	$s1, $a1		# s1 = sourceArray
	move	$s2, $a2		# s2 = destArray
	li	$t0, 0			# t0 = offsetIndex

copyBytes:
	slt	$t9, $zero, $s0		# if (byteCount <= 0)
	beq	$t9, $zero, copyDone	# done copying

	# get byte
	add	$t8, $s1, $t0		# t8 = sourceArray + offsetIndex
	lbu	$t7, 0($t8)		# t7 = sourceArray[offsetIndex]

	# store byte
	add	$t8, $s2, $t0		# t8 = destArray + offsetIndex
	sb	$t7, 0($t8)		# destArray[offsetIndex] = byte

	# increment index and repeat
	addi	$t0, $t0, 1		# offsetIndex += 1
	addi	$s0, $s0, -1		# byteCount -= 1
	j 	copyBytes

copyDone:
	# stack frame
	lw	$ra, -4+FS16($sp)
	lw	$s2, -8+FS16($sp)
	lw	$s1, -12+FS16($sp)
	lw	$s0, -16+FS16($sp)
	addi	$sp, $sp, FS16
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		getInitialGrid
# Description:	get the initial forest from user input
# Arguments:	a0:	integer representing the grid size
# Returns:	v1	1	if valid grid
#			0	otherwise
# Destroys:	t0, t1, t2, t3, t9
#

getInitialGrid:
	# stack frame
	addi	$sp, $sp, -FS8
	sw	$ra, -4+FS8($sp)
	sw	$s0, -8+FS8($sp)

	# save local vars
	move	$s0, $a0		# s0 = gridSize
	li	$t0, 0			# t0 = offestIndex
	move	$t2, $s0		# t2 = stripsRemaining
	addi	$t2, $t2, -1		# get stripsRemianing on 0 indexing

readGrid:
	slt	$t9, $t2, $zero		# if (stripsRemaining < 0)
	bne	$t9, $zero, doneReading	# done reading

	# read in a grid strip of input
	la	$a0, array		# a0 = array
	add	$a0, $a0, $t0		# a0 = array + offsetIndex
	addi	$a1, $s0, 1		# a1 = gridSize + 1 (null terminator)
	li	$v0, READ_STRING
	syscall

	# get rid of newline character in input stream
	la	$a0, junk
	addi	$a1, $zero, 2
	li	$v0, READ_STRING
	syscall

	# update indexes and repeat
	add	$t0, $t0, $s0		# offestIndex += gridSize
	addi	$t2, $t2, -1		# stripsRemaining -= 1
	j readGrid

doneReading:
	# _if we made it here, the initial board has been read
	# we can now check the initial grid for valid characters

	# this gets rid of the terminator placed at the end
	lbu	$t9, char_star
	la	$t1, array
	add	$t0, $t0, $t1
	sb	$t9, 0($t0)

	# initialize variables
	li	$t0, 0			# t0 = offestIndex
	la	$t1, array		# t1 = array
checkGrid:
	add	$t9, $t0, $t1		# t9 = array + offsetIndex
	lbu	$t2, 0($t9)		# t2 = array[offestIndex] = char
	lbu	$t3, char_star		# a * indicates the end of the grid
	beq	$t2, $t3, goodGrid	# if we make it to a star, good grid
	lbu	$t3, char_B		# check for B
	beq	$t2, $t3, goodChar
	lbu	$t3, char_t		# check for t
	beq	$t2, $t3, goodChar
	lbu	$t3, char_.		# check for .
	beq	$t2, $t3, goodChar
	#_if the char made it this far, this char is not recognized
	j 	badGridChar

badGridChar:
	li	$v0, PRINT_STRING
	la	$a0, errorGridCharacter	# print error message
	syscall
	li	$v0, 0			# return 0 for bad grid
	j       getGridReturn

goodChar:
	# update index and repeat
	addi	$t0, $t0, 1		# offsetIndex += 1
	j	checkGrid		# go back and check next char

goodGrid:
	# _if we made it to this point, all of the user input is good!!!
	li	$v0, 1			# return 1 for good grid
	j       getGridReturn

getGridReturn:
	# stack frame
	lw	$ra, -4+FS8($sp)
	lw	$s0, -8+FS8($sp)
	addi	$sp, $sp, FS8
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		printBanner
# Description:	print out the program's banner
# Arguemnts:	none
# Returns:	none
# Destroys:	none
#

printBanner:
	# stack frame
	addi	$sp, $sp, -FS4
	sw	$ra, -4+FS4($sp)

	li	$v0, PRINT_STRING
	la	$a0, banner1		# top line
	syscall
	la	$a0, banner2		# forest fire text
	syscall
	la	$a0, banner1		# bottom line
	syscall
	la	$a0, newline		# blank line
	syscall

	# stack frame
	lw	$ra, -4+FS4($sp)
	addi	$sp, $sp, FS4
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name:		printGrid
# Description:	print out the forest grid
# Arguments:	a0:	integer representing grid size
#		a1:	integer representing current generation
# Returns:	none
# Destorys:	t9
#

printGrid:
	# stack frame
	addi	$sp, $sp, -FS20
	sw	$ra, -4+FS20($sp)
	sw	$s7, -8+FS20($sp)
	sw	$s6, -12+FS20($sp)
	sw	$s1, -16+FS20($sp)
	sw	$s0, -20+FS20($sp)

	# save local vars
	move	$s0, $a0		# s0 = gridSize
	move	$s1, $a1		# s1 = currentGen
	move	$s6, $a0		# s6 = gridRows
	li	$s7, 0			# s7 = offsetIndex

	# print out generation heading
	li	$v0, PRINT_STRING
	la	$a0, genHeadL		# leftside header '==== #'
	syscall
	li	$v0, PRINT_INT
	move	$a0, $s1		# currentGen 'n'
	syscall
	li	$v0, PRINT_STRING
	la	$a0, genHeadR		# rightside header ' ====\n'
	syscall

	# print top line of grid box
	move	$a0, $s0		# pass in grid width
	jal	printGridLine

printEachLine:
	# for each line of characters
	slt	$t9, $zero, $s6		# if (gridRows <= 0)
	beq	$t9, $zero, eachLineDone# done printing

	# print 1 line of characters
	move	$a0, $s0		# pass in gridSize
	move	$a1, $s7		# pass in offsetIndex
	jal	printGridChars

	# update indexes and repeat
	add	$s7, $s7, $s0		# offsetIndex += gridSize
	addi	$s6, $s6, -1		# gridRows -= 1
	j 	printEachLine

eachLineDone:
	# print bottom line of grid box
	move	$a0, $s0		# pass in grid width
	jal	printGridLine

	# print one last newline character
	li	$v0, PRINT_STRING
	la	$a0, newline		# newline after each generation
	syscall

	# stack frame
	lw	$ra, -4+FS20($sp)
	lw	$s7, -8+FS20($sp)
	lw	$s6, -12+FS20($sp)
	lw	$s1, -16+FS20($sp)
	lw	$s0, -20+FS20($sp)
	addi	$sp, $sp, FS20
	jr	$ra


# ----------------------------------------------------------------------------

#
# Name: 	printGridLine
# Description:	print out the top/bottom border line of the forest grid
# Arguments:	a0:	integer representing the grid width
# Returns:	none
# Destroys:	t0, t9
#

printGridLine:
	# stack frame
	addi	$sp, $sp, -FS4
	sw	$ra, -4+FS4($sp)

	# save local vars
	move	$t0, $a0		# t0 = gridWidth

	# print out left plus sign
	li	$v0, PRINT_STRING
	la	$a0, char_plus		# left plus sign
	syscall

printLineSegments:
	# print out 1 line segment for each gridWidth
	slt	$t9, $zero, $t0		# if (gridWidth <= 0)
	beq	$t9, $zero, linesDone	# done printing
	la	$a0, char_dash
	syscall

	# udpate index and repeat
	addi	$t0, $t0, -1		# gridWidth -= 1
	j 	printLineSegments

linesDone:
	# print out right plus sign
	la	$a0, char_plus		# right plus sign
	syscall

	# print out one last newline
	la	$a0, newline		# newline at the end
	syscall

	# stack frame
	lw	$ra, -4+FS4($sp)
	addi	$sp, $sp, FS4
	jr	$ra


# ----------------------------------------------------------------------------

# Name:		printGridChars
# Description:	print out one line of characters from the forest grid
# Arguments:	a0:	integer representing the grid width
#		a1:	integer representing the offset index of the 1st char
# Returns:	none
# Destroys:	t0, t1, t2, t3, t9
#

printGridChars:
	# stack frame
	addi	$sp, $sp, -FS4
	sw	$ra, -4+FS4($sp)

	# save local vars
	move	$t0, $a0		# t0 = gridWidth
	move	$t1, $a1		# t1 = offsetIndex

	# print out lift side grid box line
	li	$v0, PRINT_STRING
	la	$a0, char_line		# left side line
	syscall

printChars:
	# print out chars from 1 line
	slt	$t9, $zero, $t0		# if (gridWidth <= 0)
	beq	$t9, $zero, charsDone	# done printing

	# print out 1 char
	li	$v0, PRINT_STRING
	la	$t2, array		# t2 = array
	add	$t2, $t2, $t1		# t2 = array + offsetIndex
	lbu	$t3, 0($t2)		# t3 = array[offsetIndex]
	la	$a0, buff_char		# a0 = charBuffer
	sb	$t3, 0($a0)		# charBuffer = char
	syscall

	# update indexes and repeat
	addi	$t0, $t0, -1		# gridWidth -= 1
	addi	$t1, $t1, 1		# offsetIndex += 1
	j printChars

charsDone:
	# print out right side grid box line
	li	$a0, PRINT_STRING
	la	$a0, char_line		# right side line
	syscall

	# one last newline at the end of the row
	la	$a0, newline		# newline at the end
	syscall

	# stack frame
	lw	$ra, -4+FS4($sp)
	addi	$sp, $sp, FS4
	jr	$ra


# ----------------------------------------------------------------------------

#
# End of the forestfire program
#

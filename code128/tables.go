package code128

/*
 * Encoding Tables
 */

// ASCII special characters
const (
	NUL = 0x00 // '\0' Null
	SOH = 0x01 //      Start of Header
	STX = 0x02 //      Start of Text
	ETX = 0x03 //      End of Text
	EOT = 0x04 //      End of Transmission
	ENQ = 0x05 //      Enquiry
	ACK = 0x06 //      Acknowledgement
	BEL = 0x07 // '\a' Bell
	BS  = 0x08 // '\b' Backspace
	HT  = 0x09 // '\t' Horizontal Tab
	LF  = 0x0A // '\n' Line Feed
	VT  = 0x0B // '\v' Verical Tab
	FF  = 0x0C // '\f' Form Feed
	CR  = 0x0D // '\r' Carriage Return
	SO  = 0x0E //      Shift Out
	SI  = 0x0F //      Shift In
	DLE = 0x10 //      Device Idle
	DC1 = 0x11 //      Device Control 1
	DC2 = 0x12 //      Device Control 2
	DC3 = 0x13 //      Device Control 3
	DC4 = 0x14 //      Device Control 4
	NAK = 0x15 //      Negative Acknoledgement
	SYN = 0x16 //      Synchronize
	ETB = 0x17 //      End of Transmission Block
	CAN = 0x18 //      Cancel
	EM  = 0x19 //      End of Medium
	SUB = 0x1A //      Substitute
	ESC = 0x1B // '\e' Escape
	FS  = 0x1C //      Field Separator
	GS  = 0x1D //      Group Separator
	RS  = 0x1E //      Record Separator
	US  = 0x1F //      Unit Separator
	SP  = 0x20 //      Space
	DEL = 0x7F //      Delete
)

// SpecialOffset is added to the value of Code-128 special symbols, to offset
// them from the ASCII table.
const SpecialOffset = 32

// Code128 special characters
// The special characters are offset from the ASCII table by SpecialOffset.
const (
	FNC3    = 96 + SpecialOffset
	FNC2    = 97 + SpecialOffset
	SHIFT   = 98 + SpecialOffset
	CODE_C  = 99 + SpecialOffset
	CODE_B  = 100 + SpecialOffset
	FNC4_B  = 100 + SpecialOffset
	CODE_A  = 101 + SpecialOffset
	FNC4_A  = 101 + SpecialOffset
	FNC1    = 102 + SpecialOffset
	START_A = 103 + SpecialOffset
	START_B = 104 + SpecialOffset
	START_C = 105 + SpecialOffset
	STOP    = 106 + SpecialOffset
)

const REVERSE_STOP = -1

// Bitpattern of the Code-128 symbols
// A bitpattern is laid out as the array {A, B, C, M1-M6}, where A, B, C
// designate the symbol according to its corresponding character set, and M1
// through M6 are the module widths.
// The index into the array coincides with the symbol's Code-128 value
// (necessary for calculating the checksum).
// For special, non-ascii symbols, such as START, CODE, and FNC, the value is
// offset by 32 from the index, as to not overlap with ASCII.
var Bitpattern = [...][9]int{
	{' ', ' ', 0, 2, 1, 2, 2, 2, 2},
	{'!', '!', 1, 2, 2, 2, 1, 2, 2},
	{'"', '"', 2, 2, 2, 2, 2, 2, 1},
	{'#', '#', 3, 1, 2, 1, 2, 2, 3},
	{'$', '$', 4, 1, 2, 1, 3, 2, 2},
	{'%', '%', 5, 1, 3, 1, 2, 2, 2},
	{'&', '&', 6, 1, 2, 2, 2, 1, 3},
	{'\'', '\'', 7, 1, 2, 2, 3, 1, 2},
	{'(', '(', 8, 1, 3, 2, 2, 1, 2},
	{')', ')', 9, 2, 2, 1, 2, 1, 3},
	{'*', '*', 10, 2, 2, 1, 3, 1, 2},
	{'+', '+', 11, 2, 3, 1, 2, 1, 2},
	{',', ',', 12, 1, 1, 2, 2, 3, 2},
	{'-', '-', 13, 1, 2, 2, 1, 3, 2},
	{'.', '.', 14, 1, 2, 2, 2, 3, 1},
	{'/', '/', 15, 1, 1, 3, 2, 2, 2},
	{'0', '0', 16, 1, 2, 3, 1, 2, 2},
	{'1', '1', 17, 1, 2, 3, 2, 2, 1},
	{'2', '2', 18, 2, 2, 3, 2, 1, 1},
	{'3', '3', 19, 2, 2, 1, 1, 3, 2},
	{'4', '4', 20, 2, 2, 1, 2, 3, 1},
	{'5', '5', 21, 2, 1, 3, 2, 1, 2},
	{'6', '6', 22, 2, 2, 3, 1, 1, 2},
	{'7', '7', 23, 3, 1, 2, 1, 3, 1},
	{'8', '8', 24, 3, 1, 1, 2, 2, 2},
	{'9', '9', 25, 3, 2, 1, 1, 2, 2},
	{':', ':', 26, 3, 2, 1, 2, 2, 1},
	{';', ';', 27, 3, 1, 2, 2, 1, 2},
	{'<', '<', 28, 3, 2, 2, 1, 1, 2},
	{'=', '=', 29, 3, 2, 2, 2, 1, 1},
	{'>', '>', 30, 2, 1, 2, 1, 2, 3},
	{'?', '?', 31, 2, 1, 2, 3, 2, 1},
	{'@', '@', 32, 2, 3, 2, 1, 2, 1},
	{'A', 'A', 33, 1, 1, 1, 3, 2, 3},
	{'B', 'B', 34, 1, 3, 1, 1, 2, 3},
	{'C', 'C', 35, 1, 3, 1, 3, 2, 1},
	{'D', 'D', 36, 1, 1, 2, 3, 1, 3},
	{'E', 'E', 37, 1, 3, 2, 1, 1, 3},
	{'F', 'F', 38, 1, 3, 2, 3, 1, 1},
	{'G', 'G', 39, 2, 1, 1, 3, 1, 3},
	{'H', 'H', 40, 2, 3, 1, 1, 1, 3},
	{'I', 'I', 41, 2, 3, 1, 3, 1, 1},
	{'J', 'J', 42, 1, 1, 2, 1, 3, 3},
	{'K', 'K', 43, 1, 1, 2, 3, 3, 1},
	{'L', 'L', 44, 1, 3, 2, 1, 3, 1},
	{'M', 'M', 45, 1, 1, 3, 1, 2, 3},
	{'N', 'N', 46, 1, 1, 3, 3, 2, 1},
	{'O', 'O', 47, 1, 3, 3, 1, 2, 1},
	{'P', 'P', 48, 3, 1, 3, 1, 2, 1},
	{'Q', 'Q', 49, 2, 1, 1, 3, 3, 1},
	{'R', 'R', 50, 2, 3, 1, 1, 3, 1},
	{'S', 'S', 51, 2, 1, 3, 1, 1, 3},
	{'T', 'T', 52, 2, 1, 3, 3, 1, 1},
	{'U', 'U', 53, 2, 1, 3, 1, 3, 1},
	{'V', 'V', 54, 3, 1, 1, 1, 2, 3},
	{'W', 'W', 55, 3, 1, 1, 3, 2, 1},
	{'X', 'X', 56, 3, 3, 1, 1, 2, 1},
	{'Y', 'Y', 57, 3, 1, 2, 1, 1, 3},
	{'Z', 'Z', 58, 3, 1, 2, 3, 1, 1},
	{'[', '[', 59, 3, 3, 2, 1, 1, 1},
	{'\\', '\\', 60, 3, 1, 4, 1, 1, 1},
	{']', ']', 61, 2, 2, 1, 4, 1, 1},
	{'^', '^', 62, 4, 3, 1, 1, 1, 1},
	{'_', '_', 63, 1, 1, 1, 2, 2, 4},
	{NUL, '`', 64, 1, 1, 1, 4, 2, 2},
	{SOH, 'a', 65, 1, 2, 1, 1, 2, 4},
	{STX, 'b', 66, 1, 2, 1, 4, 2, 1},
	{ETX, 'c', 67, 1, 4, 1, 1, 2, 2},
	{EOT, 'd', 68, 1, 4, 1, 2, 2, 1},
	{ENQ, 'e', 69, 1, 1, 2, 2, 1, 4},
	{ACK, 'f', 70, 1, 1, 2, 4, 1, 2},
	{BEL, 'g', 71, 1, 2, 2, 1, 1, 4},
	{BS, 'h', 72, 1, 2, 2, 4, 1, 1},
	{HT, 'i', 73, 1, 4, 2, 1, 1, 2},
	{LF, 'j', 74, 1, 4, 2, 2, 1, 1},
	{VT, 'k', 75, 2, 4, 1, 2, 1, 1},
	{FF, 'l', 76, 2, 2, 1, 1, 1, 4},
	{CR, 'm', 77, 4, 1, 3, 1, 1, 1},
	{SO, 'n', 78, 2, 4, 1, 1, 1, 2},
	{SI, 'o', 79, 1, 3, 4, 1, 1, 1},
	{DLE, 'p', 80, 1, 1, 1, 2, 4, 2},
	{DC1, 'q', 81, 1, 2, 1, 1, 4, 2},
	{DC2, 'r', 82, 1, 2, 1, 2, 4, 1},
	{DC3, 's', 83, 1, 1, 4, 2, 1, 2},
	{DC4, 't', 84, 1, 2, 4, 1, 1, 2},
	{NAK, 'u', 85, 1, 2, 4, 2, 1, 1},
	{SYN, 'v', 86, 4, 1, 1, 2, 1, 2},
	{ETB, 'w', 87, 4, 2, 1, 1, 1, 2},
	{CAN, 'x', 88, 4, 2, 1, 2, 1, 1},
	{EM, 'y', 89, 2, 1, 2, 1, 4, 1},
	{SUB, 'z', 90, 2, 1, 4, 1, 2, 1},
	{ESC, '{', 91, 4, 1, 2, 1, 2, 1},
	{FS, '|', 92, 1, 1, 1, 1, 4, 3},
	{GS, '}', 93, 1, 1, 1, 3, 4, 1},
	{RS, '~', 94, 1, 3, 1, 1, 4, 1},
	{US, DEL, 95, 1, 1, 4, 1, 1, 3},
	{FNC3, FNC3, 96, 1, 1, 4, 3, 1, 1},
	{FNC2, FNC2, 97, 4, 1, 1, 1, 1, 3},
	{SHIFT, SHIFT, 98, 4, 1, 1, 3, 1, 1},
	{CODE_C, CODE_C, 99, 1, 1, 3, 1, 4, 1},
	{CODE_B, FNC4_B, CODE_B, 1, 1, 4, 1, 3, 1},
	{FNC4_A, CODE_A, CODE_A, 3, 1, 1, 1, 4, 1},
	{FNC1, FNC1, FNC1, 4, 1, 1, 1, 3, 1},
	{START_A, START_A, START_A, 2, 1, 1, 4, 1, 2},
	{START_B, START_B, START_B, 2, 1, 1, 2, 1, 4},
	{START_C, START_C, START_C, 2, 1, 1, 2, 3, 2},
}

func ModuleBits(bits [9]int) []int {
	return bits[3:9]
}

func SymbolValue(sym int, table TableIndex) int {
	switch table {
	case LookupA:
		if sym < 0x20 /* Special ASCII */ {
			return sym + 0x40
		}
		if sym > 0x7F /* Special Code-128 */ {
			return sym - SpecialOffset
		}
		return sym - 0x20 // normal ASCII
	case LookupB:
		if sym < 0x20 /* Special ASCII */ {
			panic("symbol not in table B")
		}
		if sym > 0x7F /* Special Code-128 */ {
			return sym - SpecialOffset
		}
		return sym - 0x20 // normal ASCII
	case LookupC:
		if sym > 0x7F /* Special Code-128 */ {
			return sym - SpecialOffset
		}
		return sym // literal number
	}
	panic("invalid table index")
}

type TableIndex int

const (
	LookupNone  TableIndex = -1
	LookupA     TableIndex = 0
	LookupB     TableIndex = 1
	LookupC     TableIndex = 2
	LookupShift TableIndex = 3
)

var StopPattern = [...]int{2, 3, 3, 1, 1, 1, 2}

const QuietSpace = 10

/*
 * Decoding Tables
 */

type DecodeTable [5][5][5][5][5][5]int

var DecodeTableA = DecodeTable{
	1: {
		1: {
			1: {
				1: {
					4: {
						3: FS,
					},
				},
				2: {
					2: {
						4: '_',
					},
					4: {
						2: DLE,
					},
				},
				3: {
					2: {
						3: 'A',
					},
					4: {
						1: GS,
					},
				},
				4: {
					2: {
						2: NUL,
					},
				},
			},
			2: {
				1: {
					3: {
						3: 'J',
					},
				},
				2: {
					1: {
						4: ENQ,
					},
					3: {
						2: ',',
					},
				},
				3: {
					1: {
						3: 'D',
					},
					3: {
						1: 'K',
					},
				},
				4: {
					1: {
						2: ACK,
					},
				},
			},
			3: {
				1: {
					2: {
						3: 'M',
					},
					4: {
						1: CODE_C,
					},
				},
				2: {
					2: {
						2: '/',
					},
				},
				3: {
					2: {
						1: 'N',
					},
				},
			},
			4: {
				1: {
					1: {
						3: US,
					},
					3: {
						1: CODE_B,
					},
				},
				2: {
					1: {
						2: DC3,
					},
				},
				3: {
					1: {
						1: FNC3,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						4: SOH,
					},
					4: {
						2: DC1,
					},
				},
				2: {
					2: {
						3: '#',
					},
					4: {
						1: DC2,
					},
				},
				3: {
					2: {
						2: '$',
					},
				},
				4: {
					2: {
						1: STX,
					},
				},
			},
			2: {
				1: {
					1: {
						4: BEL,
					},
					3: {
						2: '-',
					},
				},
				2: {
					1: {
						3: '&',
					},
					3: {
						1: '.',
					},
				},
				3: {
					1: {
						2: '\'',
					},
				},
				4: {
					1: {
						1: BS,
					},
				},
			},
			3: {
				1: {
					2: {
						2: '0',
					},
				},
				2: {
					2: {
						1: '1',
					},
				},
			},
			4: {
				1: {
					1: {
						2: DC4,
					},
				},
				2: {
					1: {
						1: NAK,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						3: 'B',
					},
					4: {
						1: RS,
					},
				},
				2: {
					2: {
						2: '%',
					},
				},
				3: {
					2: {
						1: 'C',
					},
				},
			},
			2: {
				1: {
					1: {
						3: 'E',
					},
					3: {
						1: 'L',
					},
				},
				2: {
					1: {
						2: '(',
					},
				},
				3: {
					1: {
						1: 'F',
					},
				},
			},
			3: {
				1: {
					2: {
						1: 'O',
					},
				},
			},
			4: {
				1: {
					1: {
						1: SI,
					},
				},
			},
		},
		4: {
			1: {
				2: {
					2: {
						1: EOT,
					},
				},
				1: {
					2: {
						2: ETX,
					},
				},
			},
			2: {
				1: {
					1: {
						2: HT,
					},
				},
				2: {
					1: {
						1: LF,
					},
				},
			},
		},
	},
	2: {
		1: {
			1: {
				1: {
					3: {
						3: REVERSE_STOP,
					},
				},
				2: {
					1: {
						4: START_B,
					},
					3: {
						2: START_C,
					},
				},
				3: {
					1: {
						3: 'G',
					},
					3: {
						1: 'Q',
					},
				},
				4: {
					1: {
						2: START_A,
					},
				},
			},
			2: {
				1: {
					2: {
						3: '>',
					},
					4: {
						1: EM,
					},
				},
				2: {
					2: {
						2: ' ',
					},
				},
				3: {
					2: {
						1: '?',
					},
				},
			},
			3: {
				1: {
					1: {
						3: 'S',
					},
					3: {
						1: 'U',
					},
				},
				2: {
					1: {
						2: '5',
					},
				},
				3: {
					1: {
						1: 'T',
					},
				},
			},
			4: {
				1: {
					2: {
						1: SUB,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						4: FF,
					},
					3: {
						2: '3',
					},
				},
				2: {
					1: {
						3: ')',
					},
					3: {
						1: '4',
					},
				},
				3: {
					1: {
						2: '*',
					},
				},
				4: {
					1: {
						1: ']',
					},
				},
			},
			2: {
				1: {
					2: {
						2: '!',
					},
				},
				2: {
					2: {
						1: '"',
					},
				},
			},
			3: {
				1: {
					1: {
						2: '6',
					},
				},
				2: {
					1: {
						1: '2',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						3: 'H',
					},
					3: {
						1: 'R',
					},
				},
				2: {
					1: {
						2: '+',
					},
				},
				3: {
					1: {
						1: 'I',
					},
				},
			},
			2: {
				1: {
					2: {
						1: '@',
					},
				},
			},
			3: {
				1: {
					1: {
						1: STOP,
					},
				},
			},
		},
		4: {
			1: {
				1: {
					1: {
						2: SO,
					},
				},
				2: {
					1: {
						1: VT,
					},
				},
			},
		},
	},
	3: {
		1: {
			1: {
				1: {
					2: {
						3: 'V',
					},
					4: {
						1: FNC4_A,
					},
				},
				2: {
					2: {
						2: '8',
					},
				},
				3: {
					2: {
						1: 'W',
					},
				},
			},
			2: {
				1: {
					1: {
						3: 'Y',
					},
					3: {
						1: '7',
					},
				},
				2: {
					1: {
						2: ';',
					},
				},
				3: {
					1: {
						1: 'Z',
					},
				},
			},
			3: {
				1: {
					2: {
						1: 'P',
					},
				},
			},
			4: {
				1: {
					1: {
						1: '\\',
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						2: '9',
					},
				},
				2: {
					2: {
						1: ':',
					},
				},
			},
			2: {
				1: {
					1: {
						2: '<',
					},
				},
				2: {
					1: {
						1: '=',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						1: 'X',
					},
				},
			},
			2: {
				1: {
					1: {
						1: '[',
					},
				},
			},
		},
	},
	4: {
		1: {
			1: {
				1: {
					1: {
						3: FNC2,
					},
					3: {
						1: FNC1,
					},
				},
				2: {
					1: {
						2: SYN,
					},
				},
				3: {
					1: {
						1: SHIFT,
					},
				},
			},
			2: {
				1: {
					2: {
						1: ESC,
					},
				},
			},
			3: {
				1: {
					1: {
						1: CR,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						2: ETB,
					},
				},
				2: {
					1: {
						1: CAN,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						1: '^',
					},
				},
			},
		},
	},
}

var DecodeTableB = DecodeTable{
	1: {
		1: {
			1: {
				1: {
					4: {
						3: '|',
					},
				},
				2: {
					2: {
						4: '_',
					},
					4: {
						2: 'p',
					},
				},
				3: {
					2: {
						3: 'A',
					},
					4: {
						1: '}',
					},
				},
				4: {
					2: {
						2: '`',
					},
				},
			},
			2: {
				1: {
					3: {
						3: 'J',
					},
				},
				2: {
					1: {
						4: 'e',
					},
					3: {
						2: ',',
					},
				},
				3: {
					1: {
						3: 'D',
					},
					3: {
						1: 'K',
					},
				},
				4: {
					1: {
						2: 'f',
					},
				},
			},
			3: {
				1: {
					2: {
						3: 'M',
					},
					4: {
						1: CODE_C,
					},
				},
				2: {
					2: {
						2: '/',
					},
				},
				3: {
					2: {
						1: 'N',
					},
				},
			},
			4: {
				1: {
					1: {
						3: DEL,
					},
					3: {
						1: FNC4_B,
					},
				},
				2: {
					1: {
						2: 's',
					},
				},
				3: {
					1: {
						1: FNC3,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						4: 'a',
					},
					4: {
						2: 'q',
					},
				},
				2: {
					2: {
						3: '#',
					},
					4: {
						1: 'r',
					},
				},
				3: {
					2: {
						2: '$',
					},
				},
				4: {
					2: {
						1: 'b',
					},
				},
			},
			2: {
				1: {
					1: {
						4: 'g',
					},
					3: {
						2: '-',
					},
				},
				2: {
					1: {
						3: '&',
					},
					3: {
						1: '.',
					},
				},
				3: {
					1: {
						2: '\'',
					},
				},
				4: {
					1: {
						1: 'h',
					},
				},
			},
			3: {
				1: {
					2: {
						2: '0',
					},
				},
				2: {
					2: {
						1: '1',
					},
				},
			},
			4: {
				1: {
					1: {
						2: 't',
					},
				},
				2: {
					1: {
						1: 'u',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						3: 'B',
					},
					4: {
						1: '~',
					},
				},
				2: {
					2: {
						2: '%',
					},
				},
				3: {
					2: {
						1: 'C',
					},
				},
			},
			2: {
				1: {
					1: {
						3: 'E',
					},
					3: {
						1: 'L',
					},
				},
				2: {
					1: {
						2: '(',
					},
				},
				3: {
					1: {
						1: 'F',
					},
				},
			},
			3: {
				1: {
					2: {
						1: 'O',
					},
				},
			},
			4: {
				1: {
					1: {
						1: 'o',
					},
				},
			},
		},
		4: {
			1: {
				1: {
					2: {
						2: 'c',
					},
				},
				2: {
					2: {
						1: 'd',
					},
				},
			},
			2: {
				1: {
					1: {
						2: 'i',
					},
				},
				2: {
					1: {
						1: 'j',
					},
				},
			},
		},
	},
	2: {
		1: {
			1: {
				1: {
					3: {
						3: REVERSE_STOP,
					},
				},
				2: {
					1: {
						4: START_B,
					},
					3: {
						2: START_C,
					},
				},
				3: {
					1: {
						3: 'G',
					},
					3: {
						1: 'Q',
					},
				},
				4: {
					1: {
						2: START_A,
					},
				},
			},
			2: {
				1: {
					2: {
						3: '>',
					},
					4: {
						1: 'y',
					},
				},
				2: {
					2: {
						2: ' ',
					},
				},
				3: {
					2: {
						1: '?',
					},
				},
			},
			3: {
				1: {
					1: {
						3: 'S',
					},
					3: {
						1: 'U',
					},
				},
				2: {
					1: {
						2: '5',
					},
				},
				3: {
					1: {
						1: 'T',
					},
				},
			},
			4: {
				1: {
					2: {
						1: 'z',
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						4: 'l',
					},
					3: {
						2: '3',
					},
				},
				2: {
					1: {
						3: ')',
					},
					3: {
						1: '4',
					},
				},
				3: {
					1: {
						2: '*',
					},
				},
				4: {
					1: {
						1: ']',
					},
				},
			},
			2: {
				1: {
					2: {
						2: '!',
					},
				},
				2: {
					2: {
						1: '"',
					},
				},
			},
			3: {
				1: {
					1: {
						2: '6',
					},
				},
				2: {
					1: {
						1: '2',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						3: 'H',
					},
					3: {
						1: 'R',
					},
				},
				2: {
					1: {
						2: '+',
					},
				},
				3: {
					1: {
						1: 'I',
					},
				},
			},
			2: {
				1: {
					2: {
						1: '@',
					},
				},
			},
			3: {
				1: {
					1: {
						1: STOP,
					},
				},
			},
		},
		4: {
			1: {
				1: {
					1: {
						2: 'n',
					},
				},
				2: {
					1: {
						1: 'k',
					},
				},
			},
		},
	},
	3: {
		1: {
			1: {
				1: {
					2: {
						3: 'V',
					},
					4: {
						1: CODE_A,
					},
				},
				2: {
					2: {
						2: '8',
					},
				},
				3: {
					2: {
						1: 'W',
					},
				},
			},
			2: {
				1: {
					1: {
						3: 'Y',
					},
					3: {
						1: '7',
					},
				},
				2: {
					1: {
						2: ';',
					},
				},
				3: {
					1: {
						1: 'Z',
					},
				},
			},
			3: {
				1: {
					2: {
						1: 'P',
					},
				},
			},
			4: {
				1: {
					1: {
						1: '\\',
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						2: '9',
					},
				},
				2: {
					2: {
						1: ':',
					},
				},
			},
			2: {
				1: {
					1: {
						2: '<',
					},
				},
				2: {
					1: {
						1: '=',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						1: 'X',
					},
				},
			},
			2: {
				1: {
					1: {
						1: '[',
					},
				},
			},
		},
	},
	4: {
		1: {
			1: {
				1: {
					1: {
						3: FNC2,
					},
					3: {
						1: FNC1,
					},
				},
				2: {
					1: {
						2: 'v',
					},
				},
				3: {
					1: {
						1: SHIFT,
					},
				},
			},
			2: {
				1: {
					2: {
						1: '{',
					},
				},
			},
			3: {
				1: {
					1: {
						1: 'm',
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						2: 'w',
					},
				},
				2: {
					1: {
						1: 'x',
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						1: '^',
					},
				},
			},
		},
	},
}

var DecodeTableC = DecodeTable{
	1: {
		1: {
			1: {
				1: {
					4: {
						3: 92,
					},
				},
				2: {
					2: {
						4: 63,
					},
					4: {
						2: 80,
					},
				},
				3: {
					2: {
						3: 33,
					},
					4: {
						1: 93,
					},
				},
				4: {
					2: {
						2: 64,
					},
				},
			},
			2: {
				1: {
					3: {
						3: 42,
					},
				},
				2: {
					1: {
						4: 69,
					},
					3: {
						2: 12,
					},
				},
				3: {
					1: {
						3: 36,
					},
					3: {
						1: 43,
					},
				},
				4: {
					1: {
						2: 70,
					},
				},
			},
			3: {
				1: {
					2: {
						3: 45,
					},
					4: {
						1: 99,
					},
				},
				2: {
					2: {
						2: 15,
					},
				},
				3: {
					2: {
						1: 46,
					},
				},
			},
			4: {
				1: {
					1: {
						3: 95,
					},
					3: {
						1: CODE_B,
					},
				},
				2: {
					1: {
						2: 83,
					},
				},
				3: {
					1: {
						1: 96,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						4: 65,
					},
					4: {
						2: 81,
					},
				},
				2: {
					2: {
						3: 3,
					},
					4: {
						1: 82,
					},
				},
				3: {
					2: {
						2: 4,
					},
				},
				4: {
					2: {
						1: 66,
					},
				},
			},
			2: {
				1: {
					1: {
						4: 71,
					},
					3: {
						2: 13,
					},
				},
				2: {
					1: {
						3: 6,
					},
					3: {
						1: 14,
					},
				},
				3: {
					1: {
						2: 7,
					},
				},
				4: {
					1: {
						1: 72,
					},
				},
			},
			3: {
				1: {
					2: {
						2: 16,
					},
				},
				2: {
					2: {
						1: 17,
					},
				},
			},
			4: {
				1: {
					1: {
						2: 84,
					},
				},
				2: {
					1: {
						1: 85,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						3: 34,
					},
					4: {
						1: 94,
					},
				},
				2: {
					2: {
						2: 5,
					},
				},
				3: {
					2: {
						1: 35,
					},
				},
			},
			2: {
				1: {
					1: {
						3: 37,
					},
					3: {
						1: 44,
					},
				},
				2: {
					1: {
						2: 8,
					},
				},
				3: {
					1: {
						1: 38,
					},
				},
			},
			3: {
				1: {
					2: {
						1: 47,
					},
				},
			},
			4: {
				1: {
					1: {
						1: 79,
					},
				},
			},
		},
		4: {
			1: {
				1: {
					2: {
						2: 67,
					},
				},
				2: {
					2: {
						1: 68,
					},
				},
			},
			2: {
				1: {
					1: {
						2: 73,
					},
				},
				2: {
					1: {
						1: 74,
					},
				},
			},
		},
	},
	2: {
		1: {
			1: {
				1: {
					3: {
						3: REVERSE_STOP,
					},
				},
				2: {
					1: {
						4: START_B,
					},
					3: {
						2: START_C,
					},
				},
				3: {
					1: {
						3: 39,
					},
					3: {
						1: 49,
					},
				},
				4: {
					1: {
						2: START_A,
					},
				},
			},
			2: {
				1: {
					2: {
						3: 30,
					},
					4: {
						1: 89,
					},
				},
				2: {
					2: {
						2: 0,
					},
				},
				3: {
					2: {
						1: 31,
					},
				},
			},
			3: {
				1: {
					1: {
						3: 51,
					},
					3: {
						1: 53,
					},
				},
				2: {
					1: {
						2: 21,
					},
				},
				3: {
					1: {
						1: 52,
					},
				},
			},
			4: {
				1: {
					2: {
						1: 90,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						4: 76,
					},
					3: {
						2: 19,
					},
				},
				2: {
					1: {
						3: 9,
					},
					3: {
						1: 20,
					},
				},
				3: {
					1: {
						2: 10,
					},
				},
				4: {
					1: {
						1: 61,
					},
				},
			},
			2: {
				1: {
					2: {
						2: 1,
					},
				},
				2: {
					2: {
						1: 2,
					},
				},
			},
			3: {
				1: {
					1: {
						2: 22,
					},
				},
				2: {
					1: {
						1: 18,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						3: 40,
					},
					3: {
						1: 50,
					},
				},
				2: {
					1: {
						2: 11,
					},
				},
				3: {
					1: {
						1: 41,
					},
				},
			},
			2: {
				1: {
					2: {
						1: 32,
					},
				},
			},
			3: {
				1: {
					1: {
						1: STOP,
					},
				},
			},
		},
		4: {
			1: {
				1: {
					1: {
						2: 78,
					},
				},
				2: {
					1: {
						1: 75,
					},
				},
			},
		},
	},
	3: {
		1: {
			1: {
				1: {
					2: {
						3: 54,
					},
					4: {
						1: CODE_A,
					},
				},
				2: {
					2: {
						2: 24,
					},
				},
				3: {
					2: {
						1: 55,
					},
				},
			},
			2: {
				1: {
					1: {
						3: 57,
					},
					3: {
						1: 23,
					},
				},
				2: {
					1: {
						2: 27,
					},
				},
				3: {
					1: {
						1: 58,
					},
				},
			},
			3: {
				1: {
					2: {
						1: 48,
					},
				},
			},
			4: {
				1: {
					1: {
						1: 60,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					2: {
						2: 25,
					},
				},
				2: {
					2: {
						1: 26,
					},
				},
			},
			2: {
				1: {
					1: {
						2: 28,
					},
				},
				2: {
					1: {
						1: 29,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					2: {
						1: 56,
					},
				},
			},
			2: {
				1: {
					1: {
						1: 59,
					},
				},
			},
		},
	},
	4: {
		1: {
			1: {
				1: {
					1: {
						3: 97,
					},
					3: {
						1: FNC1,
					},
				},
				2: {
					1: {
						2: 86,
					},
				},
				3: {
					1: {
						1: 98,
					},
				},
			},
			2: {
				1: {
					2: {
						1: 91,
					},
				},
			},
			3: {
				1: {
					1: {
						1: 77,
					},
				},
			},
		},
		2: {
			1: {
				1: {
					1: {
						2: 87,
					},
				},
				2: {
					1: {
						1: 88,
					},
				},
			},
		},
		3: {
			1: {
				1: {
					1: {
						1: 62,
					},
				},
			},
		},
	},
}

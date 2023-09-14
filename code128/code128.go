// Package code128 implements encode and decode for code128.
package code128

import (
	"errors"
	"fmt"
	"image"
	"image/color"
	"reflect"
)

type Code128 struct {
	*image.Gray16
}

func (c Code128) Scale(width, height int) (image.Image, error) {
	oldWidth := c.Bounds().Dx()
	if width < oldWidth {
		return nil, errors.New("unable to shrink image, new width too small")
	}

	scaledImage := image.NewGray16(image.Rectangle{image.Point{0, 0}, image.Point{width, height}})

	// scale width
	scale := width / oldWidth
	qz := (width % oldWidth) / 2
	for x := 0; x < qz; x++ { // extend quiet zone start
		scaledImage.SetGray16(x, 0, color.White)
	}
	for x := 0; x < oldWidth; x++ { // copy pixels, scale them
		for s := 0; s < scale; s++ {
			scaledImage.SetGray16(qz+s+x*scale, 0, c.Gray16At(x, 0))
		}
	}
	for x := 0; x < qz; x++ { // extend quiet zone end
		scaledImage.SetGray16(width-x-1, 0, color.White)
	}

	// scale height
	for y := 1; y < height; y++ {
		for x := 0; x < width; x++ {
			scaledImage.SetGray16(x, y, scaledImage.Gray16At(x, 0))
		}
	}

	return scaledImage, nil
}

type barcode struct {
	modules []int
	width int
}

func (c *barcode) add(widths ...int) {
	for _, width := range widths {
		c.modules = append(c.modules, width)
		c.width += width
	}
}

func (c *barcode) draw() Code128 {
	height := 1
	img := image.NewGray16(image.Rectangle{image.Point{0, 0}, image.Point{c.width, height}})
	xPos := 0
	for i, module := range c.modules {
		for j := 0; j < module; j++ {
			if i % 2 == 0 {
				img.SetGray16(xPos, 0, color.White)
			} else {
				img.SetGray16(xPos, 0, color.Black)
			}
			xPos++
		}
	}
	return Code128{img}
}

func Encode(text string) (Code128, error) {
	runes := []rune(text)

	var (
		table TableIndex
		cksm  *Checksum
		c128 = &barcode{}
	)

	c128.add(QuietSpace)

	table = determineTable(runes, LookupNone)
	startSym := []int{START_A, START_B, START_C}[table]
	bits := Bitpattern[startSym-SpecialOffset]
	c128.add(bits[3:9]...)
	cksm = NewChecksum(startSym - SpecialOffset)

	activeTables := [2]TableIndex{0: table}
	shift := 0
	for len(runes) > 0 {
		nextTable := determineTable(runes, activeTables[0])
		if nextTable != activeTables[shift] {
			code := []int{CODE_A, CODE_B, CODE_C, SHIFT}[nextTable]
			bits := Bitpattern[code-SpecialOffset]
			c128.add(bits[3:9]...)
			cksm.Add(code - SpecialOffset)

			activeTables[shift] = nextTable
			shift = btoi(nextTable == LookupShift)
		}

		num := int(runes[0])
		if activeTables[shift] == LookupC && isCNum(runes) {
			num = parseCNum([2]rune(runes[0:2]))
			runes = runes[2:]
		} else {
			runes = runes[1:]
		}
		bits, val := lookup(num, activeTables[shift])
		c128.add(bits[3:9]...)
		cksm.Add(val)
	}

	bits = Bitpattern[cksm.Sum()]
	c128.add(bits[3:9]...)
	c128.add(StopPattern...)
	c128.add(QuietSpace)

	return c128.draw(), nil
}

func determineTable(rs []rune, currentTable TableIndex) TableIndex {
	// ~$ man 7 ascii
	isAsciiPrintable := func(r rune) bool {
		return r >= 0x20 /* space */ && r <= 0x7F /* DEL */
	}
	isNumber := func(r rune) bool {
		return r >= 0x30 /* 0 */ && r <= 0x39 /* 9 */
	}
	isSpecial := func(r rune) bool {
		return r >= 0x00 /* NUL */ && r <= 0x1F /* US */
	}

	isA := func(rs []rune) bool {
		r := rs[0]
		if isSpecial(r) {
			return true
		}
		return r >= 0x20 /* space */ && r <= 0x5F /* _ */
	}
	isB := func(rs []rune) bool {
		return isAsciiPrintable(rs[0])
	}
	isC := func(rs []rune) bool {
		if len(rs) < 2 {
			return false
		}
		return isNumber(rs[0]) && isNumber(rs[1])
	}

	if isC(rs) {
		if currentTable == LookupC || isC(rs[2:]) {
			return LookupC
		}
	}
	if isB(rs) {
		if currentTable == LookupA {
			if !isB(rs[1:]) {
				return LookupShift
			}
		}
		return LookupB
	}
	if isA(rs) {
		if currentTable == LookupB {
			if !isA(rs[1:]) {
				return LookupShift
			}
		}
		return LookupA
	}

	panic("unreachable (hopefully)")
}

func isCNum(rs []rune) bool {
	if len(rs) < 2 {
		return false
	}
	for i := 0; i < 2; i++ {
		if rs[i] < 0x30 || rs[i] > 0x39 {
			return false
		}
	}
	return true
}

func parseCNum(rs [2]rune) int {
	d1 := rs[0]
	d2 := rs[1]
	v1 := int(d1) - 0x30
	v2 := int(d2) - 0x30
	num := v1*10+v2
	return num
}

func lookup(r int, table TableIndex) (bits []int, val int) {
	for i, bits := range Bitpattern {
		if bits[table] == r {
			return bits, i
		}
	}
	panic("unreachable (hopefully)")
}

type Checksum struct {
	Value int
	Idx   int
}

func NewChecksum(initial int) *Checksum {
	return &Checksum{Value: initial, Idx: 1}
}

func (c *Checksum) Add(val int) {
	c.Value += val * c.Idx
	c.Idx++
}

func (c *Checksum) Sum() int {
	c.Value %= 103
	return c.Value
}

func must[T any](val T, err error) T {
	if err != nil {
		panic(err)
	}
	return val
}

func must2[T1, T2 any](v1 T1, v2 T2, err error) (T1, T2) {
	if err != nil {
		panic(err)
	}
	return v1, v2
}

func btoi(b bool) int {
	if b {
		return 1
	} else {
		return 0
	}
}

// BarColorTolerance determines which colors count as a bar.
// The r, g, b color channels (multiplied by a) are summed and normalized
// between 0 and 1.
// A pixel is a bar-pixel when the resulting value is less than or equal to
// BarColorTolerance.
var BarColorTolerance = 0.7

func Decode(img image.Image) (bs []byte, err error) {
	widths, err := modules(img)
	if err != nil {
		return nil, err
	}

	_ = reverse(widths)
	qs, sta, d, c, stp, qe := segments(widths)
	_, _, _ = qs, qe, stp

	if len(d)%6 != 0 {
		return nil, errors.New("invalid data segment")
	}

	decodeTable := DecodeTableA
	charTable := CharTableA
	current, posMul := 5, 1

	defer func() {
		if r := recover(); r != nil {
			table := "?"
			if reflect.ValueOf(decodeTable).Pointer() == reflect.ValueOf(DecodeTableA).Pointer() {
				table = "A"
			}
			if reflect.ValueOf(decodeTable).Pointer() == reflect.ValueOf(DecodeTableB).Pointer() {
				table = "B"
			}
			if reflect.ValueOf(decodeTable).Pointer() == reflect.ValueOf(DecodeTableC).Pointer() {
				table = "C"
			}
			seq := ""
			for i := -5; current+i < len(d) && i <= 0; i++ {
				seq += fmt.Sprintf("%d", d[current+i])
			}
			err = fmt.Errorf("table %s: invalid sequence: %s", table, seq)
		}
	}()

	staSym := decodeTable[sta[0]][sta[1]][sta[2]][sta[3]][sta[4]][sta[5]]
	switch staSym {
	default:
		return nil, fmt.Errorf("invalid start symbol: %s -- %+v", charTable[staSym], sta)
	case SYM_START_A:
		decodeTable = DecodeTableA
		charTable = CharTableA
	case SYM_START_B:
		decodeTable = DecodeTableB
		charTable = CharTableB
	case SYM_START_C:
		decodeTable = DecodeTableC
		charTable = CharTableC
	}

	checksum := staSym

	for current < len(d) {
		sym := decodeTable[d[current-5]][d[current-4]][d[current-3]][d[current-2]][d[current-1]][d[current-0]]
		checksum += sym * posMul

		switch sym {
		default:
			bs = append(bs, []byte(charTable[sym])...)
		case SYM_CODE_A:
			decodeTable = DecodeTableA
			charTable = CharTableA
		case SYM_CODE_B:
			decodeTable = DecodeTableB
			charTable = CharTableB
		case SYM_CODE_C:
			decodeTable = DecodeTableC
			charTable = CharTableC
		case SYM_FNC3: fallthrough
		case SYM_FNC2: fallthrough
		case SYM_SHIFT_B: fallthrough
		case SYM_FNC1: fallthrough
		case SYM_START_A: fallthrough
		case SYM_START_B: fallthrough
		case SYM_START_C: fallthrough
		case SYM_STOP: fallthrough
		case SYM_REVERSE_STOP:
			return bs, fmt.Errorf("symbol %+v (%s) invalid in this position", sym, charTable[sym])
		}

		posMul++
		current += 6
	}

	checksum = checksum % 103
	cksmVal := DecodeTableA[c[0]][c[1]][c[2]][c[3]][c[4]][c[5]]
	cksmOK := cksmVal == checksum
	if !cksmOK {
		return bs, fmt.Errorf("invalid checksum: want: %d, got: %d", cksmVal, checksum)
	}

	return bs, nil
}

func modules(img image.Image) (widths []int, err error) {
	var (
		isBar             = false // bar or space; start out expecting spaces (quiet zone)
		run               = 0     // length of current bar or space
		div               = 1     // divisor to normalize module widths
		divFound          = false // has the divisor been determined yet?
		quietSpaceMissing = false // many barcodes ignore the spec and omit quiet space
	)
	for x := 0; x < img.Bounds().Dx(); x++ {
		l := 0.0
		for y := 0; y < img.Bounds().Dy(); y++ {
			c := img.At(x, y)
			r, g, b, _ := c.RGBA()
			v := float64(r+g+b) / 0x2FFFD // 0xFFFF * 3 = 0x2FFFD

			// calculate incremental average
			// v/1.1 -- add a bias towards lower values
			// @todo: use something better: https://stackoverflow.com/questions/48395434/how-to-crop-or-remove-white-background-from-an-image
			//   https://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/alg.html
			//   https://en.wikipedia.org/wiki/Edge_detection
			l = l + (v/1.1-l)/float64(y+1)
		}

		if !divFound && len(widths) == 2 {
			divFound = true

			// start symbol must start with 2-wide module
			div = widths[1] / 2

			// fixup previous runs
			widths[0] = widths[0] / div
			widths[1] = widths[1] / div
		}

		if l <= BarColorTolerance { // bar
			if isBar {
				run++
			} else { // new bar run begins; finish space run
				if run == 0 {
					// barcode didn't start with a quiet space!
					quietSpaceMissing = true
				}
				widths = append(widths, run/div)
				isBar = true
				run = 1
			}
		} else { // space
			if !isBar {
				run++
			} else { // new space run begins; finish bar run
				widths = append(widths, run/div)
				isBar = false
				run = 1
			}
		}
	}

	// don't forget to record last run!
	widths = append(widths, run/div)
	if quietSpaceMissing {
		widths = append(widths, 0)
	}
	return widths, nil
}

func reverse(widths []int) (isReversed bool) {
	startSym := widths[1:7]
	sym := DecodeTableA[startSym[0]][startSym[1]][startSym[2]][startSym[3]][startSym[4]][startSym[5]]
	if sym == SYM_REVERSE_STOP {
		isReversed = true
		for i, j := 0, len(widths)-1; i < j; i, j = i+1, j-1 {
			widths[i], widths[j] = widths[j], widths[i]
		}
	}
	return
}

func segments(widths []int) (quietStart int, startSym []int, data []int, checkSym []int, stopPat []int, quietEnd int) {
	quietStart = widths[0]
	startSym = widths[1:7]
	data = widths[7 : len(widths)-14]
	checkSym = widths[len(widths)-14 : len(widths)-8]
	stopPat = widths[len(widths)-8 : len(widths)-1]
	quietEnd = widths[len(widths)-1]
	return
}

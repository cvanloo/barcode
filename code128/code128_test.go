package code128

import (
	"image"
	_ "image/png"
	"os"
	"testing"
)

func TestDetermineIndices(t *testing.T) {
	cases := []struct {
		text     string
		expected []TableIndex
	}{
		{"Abc12", []TableIndex{LookupB, LookupB, LookupB, LookupB, LookupB}},
		{"Abc1234", []TableIndex{LookupB, LookupB, LookupB, LookupC, LookupC, LookupC, LookupC}},
		{"Hello\026", []TableIndex{LookupB, LookupB, LookupB, LookupB, LookupB, LookupA}},
		{"Hello\026o", []TableIndex{LookupB, LookupB, LookupB, LookupB, LookupB, LookupShift, LookupB}},
		{"HELLO\026", []TableIndex{LookupA, LookupA, LookupA, LookupA, LookupA, LookupA}},
		{"\026\026\026b", []TableIndex{LookupA, LookupA, LookupA, LookupB}},
		{"11223456", []TableIndex{LookupC, LookupC, LookupC, LookupC, LookupC, LookupC, LookupC, LookupC}},
		{"112234567", []TableIndex{LookupC, LookupC, LookupC, LookupC, LookupC, LookupC, LookupC, LookupC, LookupB}},
	}
	for _, c := range cases {
		text, err := NewASCII(c.text)
		if err != nil {
			t.Errorf("cannot convert %s to ASCII: %v", c.text, err)
			continue
		}
		indices := determineIndices(text)
		for i := range c.expected {
			if len(c.expected) != len(indices) {
				t.Errorf("got: %v, want: %v\n", indices, c.expected)
			}
			if c.expected[i] != indices[i] {
				t.Errorf("at index %d: got: %v, want: %v", i, indices[i], c.expected[i])
			}
		}
	}
}

func TestDecode(t *testing.T) {
	cases := []struct {
		path, expected string
	}{
		{"testfiles/test_code128-1.png", "ABCD-1234-abcd"},
		{"testfiles/test_code128-2.png", "PJJ123C"},
		{"testfiles/test_code128-3.png", "hello world"},
		{"testfiles/test_code128-4.png", "hello, world!"},
		{"testfiles/test_code128-5.png", "3456abcd"},
		{"testfiles/test_code128-6.png", "667390"},
		{"testfiles/test_code128-7.png", "biz\n"},
		{"testfiles/test_code128-8.png", "ABCDEFG"},

		// Rotated
		{"testfiles/test_code128-rotate.png", "ABCD-1234-abcd"},

		// Dirty images
		{"testfiles/ClearCutGray.png", "hello"},
		{"testfiles/ClearCutDither.png", "hello"},
		{"testfiles/ClearCutBlackAround.png", "hello"},
		{"testfiles/ClearCutWhiteAround.png", "hello"},
		{"testfiles/QuietSpaceMissing.png", "1"},

		// Data after stop
		//{"testfiles/test_code128-data-after-stop.png", "Hello, World!"}, FIXME: failing
		//{"testfiles/WhatWentWrong.png", "eaou"},

		// Test cases that failed at some point in time
		{"testfiles/WhatIsDorked.png", "439721-hello-WORLD"},
	}
	for _, c := range cases {
		f, err := os.Open(c.path)
		if err != nil {
			t.Error(err)
			continue
		}
		img, _, err := image.Decode(f)
		if err != nil {
			t.Error(err)
			continue
		}
		bs, _, err := Decode(img)
		if err != nil {
			t.Error(err)
		}
		if string(bs) != c.expected {
			t.Errorf("got: `%s', want: `%s'", string(bs), c.expected)
			continue
		}
	}
}

func TestDecodeFail(t *testing.T) {
	cases := []string{
		"testfiles/NotABarcode.png",
		"testfiles/NotABarcode2.png",
		"testfiles/WhereDidTheStartGo.png",
		"testfiles/NowThisIsGoingTooFar.png",
	}
	for _, c := range cases {
		f, err := os.Open(c)
		if err != nil {
			t.Error(err)
			continue
		}
		img, _, err := image.Decode(f)
		if err != nil {
			t.Error(err)
			continue
		}
		_, _, err = Decode(img)
		if err == nil {
			t.Error("expected an error, got nil")
			continue
		}
		t.Log(err)
	}
}

func TestEncode(t *testing.T) {
	cases := []string{
		"Hello, World!",
		"11223467", // should encode all in CODE_C
		"\026\025",
		"hello",
		"112269420", // odd number of digits, can't use CODE_C for everything
		"yoyoyoyo",
		"439721-hello-WORLD",
		"hello\026world", // should encode a SHIFT (START_B ... SHIFT(A) ...)
		"\026\025h\006",  // should encode a SHIFT (START_A ... SHIFT(B) ...)
		"\026\025H\006",  // should encode everything in CODE_A
		"eaou",
		"SYN:\026",
		"\026\026\026b",
	}

	for _, c := range cases {
		text, err := NewASCII(c)
		if err != nil {
			t.Errorf("cannot convert %s to ASCII: %v", c, err)
			continue
		}
		img, err := Encode(text)
		if err != nil {
			t.Errorf("failed to encode `%s': %v", c, err)
			continue
		}
		bs, _, err := Decode(img)
		if err != nil {
			t.Errorf("failed to decode `%s': %v", c, err)
			continue
		}
		if string(bs) != c {
			t.Errorf("got: `%s', want: `%s'", string(bs), c)
		}
	}
}

func TestEncodeScale(t *testing.T) {
	cases := []string{
		"Hello, World!",
		"11223467",
		"\026\025",
		"hello",
		"112269420",
		"yoyoyoyo",
		"439721-hello-WORLD",
		"hello\026world",
		"\026\025h\006",
		"\026\025H\006",
		"eaou",
	}

	for _, c := range cases {
		text, err := NewASCII(c)
		if err != nil {
			t.Errorf("cannot convert %s to ASCII: %v", c, err)
			continue
		}
		bc, err := Encode(text)
		if err != nil {
			t.Errorf("failed to encode `%s': %v", c, err)
			continue
		}
		img, err := bc.Scale(312, 50)
		if err != nil {
			t.Error(err)
		}
		bs, _, err := Decode(img)
		if err != nil {
			t.Errorf("failed to decode `%s': %v", c, err)
			continue
		}
		if string(bs) != c {
			t.Errorf("got: `%s', want: `%s'", string(bs), c)
		}
	}
}

func TestEncodeScaleBy(t *testing.T) {
	cases := []struct {
		input   string
		scale   float64
		mode    ScaleMode
		success bool
	}{
		{"Hello, World!", 5, ScaleWidthAndHeight, true},
		{"11223467", 2, ScaleWidthAndHeight, true},
		{"\026\025", 3, ScaleHeight, true},
		{"hello", 1.5, ScaleWidthAndHeight, true},
		{"112269420", 1.7, ScaleHeight, true},
		{"yoyoyoyo", 3.257, ScaleHeight, true},
		{"439721-hello-WORLD", 0.8, ScaleWidthAndHeight, false},
		{"hello\026world", 0.1, ScaleHeight, false},
		{"\026\025h\006", 0.215, ScaleHeight, false},
		{"\026\025H\006", 10, ScaleHeight, true},
		{"eaou", 1, ScaleWidthAndHeight, true},
		{"eaou", 1, ScaleHeight, true},
	}

	for i, c := range cases {
		t.Logf("case %d: %+v\n", i, c)
		text, err := NewASCII(c.input)
		if err != nil {
			t.Errorf("cannot convert %s to ASCII: %v", c.input, err)
			continue
		}
		bc, err := Encode(text)
		if err != nil {
			t.Errorf("failed to encode `%s': %v", text, err)
			continue
		}
		img, err := bc.ScaleBy(c.scale, c.mode)
		if err != nil {
			if c.success {
				t.Error(err)
			}
			continue
		}
		if !c.success {
			t.Errorf("should have failed, but didn't: `%s'", c.input)
		}
		bs, _, err := Decode(img)
		if err != nil {
			t.Errorf("failed to decode `%s': %v", c.input, err)
			continue
		}
		if string(bs) != c.input {
			t.Errorf("got: `%s', want: `%s'", string(bs), c.input)
		}
	}
}

func TestEncodeSyms(t *testing.T) {
	cases := []struct {
		text string
		syms []int
	}{
		{"Hello, World!", []int{START_B, 'H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', '!', 12}},
		{"11223467", []int{START_C, 11, 22, 34, 67, 47}},
		{"\026\025", []int{START_A, 026, 025, 82}},
		{"hello", []int{START_B, 'h', 'e', 'l', 'l', 'o', 37}},
		{"112269420", []int{START_C, 11, 22, 69, 42, CODE_B, '0', 133}},
		{"yoyoyoyo", []int{START_B, 'y', 'o', 'y', 'o', 'y', 'o', 'y', 'o', 50}},
		{"439721-hello-WORLD", []int{START_C, 43, 97, 21, CODE_B, '-', 'h', 'e', 'l', 'l', 'o', '-', 'W', 'O', 'R', 'L', 'D', 39}},
		{"hello\026world", []int{START_B, 'h', 'e', 'l', 'l', 'o', SHIFT, 026, 'w', 'o', 'r', 'l', 'd', 59}},
		{"\026\025h\006", []int{START_A, 026, 025, SHIFT, 'h', 06, 87}},
		{"\026\025H\006", []int{START_A, 026, 025, 'H', 06, 70}},
		{"HELLO\026", []int{START_A, 'H', 'E', 'L', 'L', 'O', 026, 72}},
		{"hello5", []int{START_B, 'h', 'e', 'l', 'l', 'o', '5', 60}},
	}

	for _, c := range cases {
		t.Logf("current entry: %s", c.text)
		text, err := NewASCII(c.text)
		if err != nil {
			t.Errorf("cannot convert %s to ASCII: %v", c.text, err)
			continue
		}
		img, err := Encode(text)
		if err != nil {
			t.Errorf("failed to encode `%s': %v", c.text, err)
			continue
		}
		bs, syms, err := Decode(img)
		if err != nil {
			t.Errorf("failed to decode `%s': %v", c.text, err)
			continue
		}
		if string(bs) != c.text {
			t.Errorf("got: `%s', want: `%s'", string(bs), c.text)
		}
		if len(c.syms) != len(syms) {
			t.Errorf("got: %v, want: %v\n", syms, c.syms)
		}
		for i := range c.syms {
			if c.syms[i] != syms[i] {
				t.Errorf("at index %d: got: %U (`%s'), want: %U (`%s')", i, rune(syms[i]), string(rune(syms[i])), rune(c.syms[i]), string(rune(c.syms[i])))
			}
		}
	}
}

func TestASCII(t *testing.T) {
	cases := []string{
		"Hello, World!",
		"11223467",
		"\026\025",
		"hello",
		"112269420",
		"yoyoyoyo",
		"439721-hello-WORLD",
		"hello\026world",
		"\026\025h\006",
		"\026\025H\006",
		"eaou",
	}
	for _, c := range cases {
		_, err := NewASCII(c)
		if err != nil {
			t.Errorf("failed to convert %s to cstring", c)
		}
	}
}

func TestASCIIFail(t *testing.T) {
	cases := []string{
		"日本語",
		"1日本語",
		"11日本語",
		"2日hell本o語",
	}
	for _, c := range cases {
		_, err := NewASCII(c)
		if err == nil {
			t.Errorf("should fail, but didn't: %s", c)
		}
	}
}

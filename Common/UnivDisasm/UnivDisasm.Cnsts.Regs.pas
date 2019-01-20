//
// *************************************************************************** //
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
// *************************************************************************** //
//
//
// *************************************************************************** //
// UnivDisasm library.
//
// This file is a part of UnivDisasm library.
//
// https://github.com/MahdiSafsafi/UnivDisasm
//
// The Original Code is UnivDisasm.Cnsts.Regs.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//
unit UnivDisasm.Cnsts.Regs;

interface

{$I Config.inc}

uses
  UnivDisasm.Cnsts;

const

  { Registers types }
  REGS_TYPE_MASK = $F000;
  REGS_GP = $01 shl $0C;
  REGS_FPU = $02 shl $0C;
  REGS_MASK = $03 shl $0C;
  REGS_MMX = $04 shl $0C;
  REGS_XMM = $05 shl $0C;
  REGS_YMM = $06 shl $0C;
  REGS_ZMM = $07 shl $0C;
  REGS_CNTRL = $08 shl $0C;
  REGS_DBG = $09 shl $0C;
  REGS_TAB = $0A shl $0C;
  REGS_BND = $0B shl $0C;
  REGS_SEG = $0C shl $0C;

  REG_NIL = $00;

  { Segment Registers }
  RID_CS = $00;
  RID_DS = $01;
  RID_SS = $02;
  RID_ES = $03;
  RID_FS = $04;
  RID_GS = $05;
  SEG_CS = REGS_SEG or RID_CS;
  SEG_DS = REGS_SEG or RID_DS;
  SEG_SS = REGS_SEG or RID_SS;
  SEG_ES = REGS_SEG or RID_ES;
  SEG_FS = REGS_SEG or RID_FS;
  SEG_GS = REGS_SEG or RID_GS;

  { 8 bit Registers }
  RID_AL = $00;
  RID_CL = $01;
  RID_DL = $02;
  RID_BL = $03;
  RID_AH = $04;
  RID_CH = $05;
  RID_DH = $06;
  RID_BH = $07;
  RID_SPL = $04;
  RID_BPL = $05;
  RID_SIL = $06;
  RID_DIL = $07;
  RID_R8B = $08;
  RID_R9B = $09;
  RID_R10B = $0A;
  RID_R11B = $0B;
  RID_R12B = $0C;
  RID_R13B = $0D;
  RID_R14B = $0E;
  RID_R15B = $0F;
  REG_AL = REGS_GP or (SIZE_BYTE shl $8) or RID_AL;
  REG_CL = REGS_GP or (SIZE_BYTE shl $8) or RID_CL;
  REG_DL = REGS_GP or (SIZE_BYTE shl $8) or RID_DL;
  REG_BL = REGS_GP or (SIZE_BYTE shl $8) or RID_BL;
  REG_AH = REGS_GP or (SIZE_BYTE shl $8) or RID_AH;
  REG_CH = REGS_GP or (SIZE_BYTE shl $8) or RID_CH;
  REG_DH = REGS_GP or (SIZE_BYTE shl $8) or RID_DH;
  REG_BH = REGS_GP or (SIZE_BYTE shl $8) or RID_BH;
  REG_SPL = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_SPL);
  REG_BPL = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_BPL);
  REG_SIL = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_SIL);
  REG_DIL = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_DIL);
  REG_R8B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R8B);
  REG_R9B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R9B);
  REG_R10B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R10B);
  REG_R11B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R11B);
  REG_R12B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R12B);
  REG_R13B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R13B);
  REG_R14B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R14B);
  REG_R15B = REGS_GP or (SIZE_BYTE shl 8) or (MagicRexMask or RID_R15B);

  { 16 bit Registers }
  RID_AX = $00;
  RID_CX = $01;
  RID_DX = $02;
  RID_BX = $03;
  RID_SP = $04;
  RID_BP = $05;
  RID_SI = $06;
  RID_DI = $07;
  RID_R8W = $08;
  RID_R9W = $09;
  RID_R10W = $0A;
  RID_R11W = $0B;
  RID_R12W = $0C;
  RID_R13W = $0D;
  RID_R14W = $0E;
  RID_R15W = $0F;
  REG_AX = REGS_GP or (SIZE_WORD shl 8) or RID_AX;
  REG_CX = REGS_GP or (SIZE_WORD shl 8) or RID_CX;
  REG_DX = REGS_GP or (SIZE_WORD shl 8) or RID_DX;
  REG_BX = REGS_GP or (SIZE_WORD shl 8) or RID_BX;
  REG_SP = REGS_GP or (SIZE_WORD shl 8) or RID_SP;
  REG_BP = REGS_GP or (SIZE_WORD shl 8) or RID_BP;
  REG_SI = REGS_GP or (SIZE_WORD shl 8) or RID_SI;
  REG_DI = REGS_GP or (SIZE_WORD shl 8) or RID_DI;
  REG_R8W = REGS_GP or (SIZE_WORD shl 8) or RID_R8W;
  REG_R9W = REGS_GP or (SIZE_WORD shl 8) or RID_R9W;
  REG_R10W = REGS_GP or (SIZE_WORD shl 8) or RID_R10W;
  REG_R11W = REGS_GP or (SIZE_WORD shl 8) or RID_R11W;
  REG_R12W = REGS_GP or (SIZE_WORD shl 8) or RID_R12W;
  REG_R13W = REGS_GP or (SIZE_WORD shl 8) or RID_R13W;
  REG_R14W = REGS_GP or (SIZE_WORD shl 8) or RID_R14W;
  REG_R15W = REGS_GP or (SIZE_WORD shl 8) or RID_R15W;

  { 32 bit Registers }
  RID_EAX = $00;
  RID_ECX = $01;
  RID_EDX = $02;
  RID_EBX = $03;
  RID_ESP = $04;
  RID_EBP = $05;
  RID_ESI = $06;
  RID_EDI = $07;
  RID_R8D = $08;
  RID_R9D = $09;
  RID_R10D = $0A;
  RID_R11D = $0B;
  RID_R12D = $0C;
  RID_R13D = $0D;
  RID_R14D = $0E;
  RID_R15D = $0F;
  REG_EAX = REGS_GP or (SIZE_DWORD shl 8) or RID_EAX;
  REG_ECX = REGS_GP or (SIZE_DWORD shl 8) or RID_ECX;
  REG_EDX = REGS_GP or (SIZE_DWORD shl 8) or RID_EDX;
  REG_EBX = REGS_GP or (SIZE_DWORD shl 8) or RID_EBX;
  REG_ESP = REGS_GP or (SIZE_DWORD shl 8) or RID_ESP;
  REG_EBP = REGS_GP or (SIZE_DWORD shl 8) or RID_EBP;
  REG_ESI = REGS_GP or (SIZE_DWORD shl 8) or RID_ESI;
  REG_EDI = REGS_GP or (SIZE_DWORD shl 8) or RID_EDI;
  REG_R8D = REGS_GP or (SIZE_DWORD shl 8) or RID_R8D;
  REG_R9D = REGS_GP or (SIZE_DWORD shl 8) or RID_R9D;
  REG_R10D = REGS_GP or (SIZE_DWORD shl 8) or RID_R10D;
  REG_R11D = REGS_GP or (SIZE_DWORD shl 8) or RID_R11D;
  REG_R12D = REGS_GP or (SIZE_DWORD shl 8) or RID_R12D;
  REG_R13D = REGS_GP or (SIZE_DWORD shl 8) or RID_R13D;
  REG_R14D = REGS_GP or (SIZE_DWORD shl 8) or RID_R14D;
  REG_R15D = REGS_GP or (SIZE_DWORD shl 8) or RID_R15D;

  { 64 bit Registers }
  RID_RAX = $00;
  RID_RCX = $01;
  RID_RDX = $02;
  RID_RBX = $03;
  RID_RSP = $04;
  RID_RBP = $05;
  RID_RSI = $06;
  RID_RDI = $07;
  RID_R8 = $08;
  RID_R9 = $09;
  RID_R10 = $0A;
  RID_R11 = $0B;
  RID_R12 = $0C;
  RID_R13 = $0D;
  RID_R14 = $0E;
  RID_R15 = $0F;
  REG_RAX = REGS_GP or (SIZE_QWORD shl 8) or RID_RAX;
  REG_RCX = REGS_GP or (SIZE_QWORD shl 8) or RID_RCX;
  REG_RDX = REGS_GP or (SIZE_QWORD shl 8) or RID_RDX;
  REG_RBX = REGS_GP or (SIZE_QWORD shl 8) or RID_RBX;
  REG_RSP = REGS_GP or (SIZE_QWORD shl 8) or RID_RSP;
  REG_RBP = REGS_GP or (SIZE_QWORD shl 8) or RID_RBP;
  REG_RSI = REGS_GP or (SIZE_QWORD shl 8) or RID_RSI;
  REG_RDI = REGS_GP or (SIZE_QWORD shl 8) or RID_RDI;
  REG_R8 = REGS_GP or (SIZE_QWORD shl 8) or RID_R8;
  REG_R9 = REGS_GP or (SIZE_QWORD shl 8) or RID_R9;
  REG_R10 = REGS_GP or (SIZE_QWORD shl 8) or RID_R10;
  REG_R11 = REGS_GP or (SIZE_QWORD shl 8) or RID_R11;
  REG_R12 = REGS_GP or (SIZE_QWORD shl 8) or RID_R12;
  REG_R13 = REGS_GP or (SIZE_QWORD shl 8) or RID_R13;
  REG_R14 = REGS_GP or (SIZE_QWORD shl 8) or RID_R14;
  REG_R15 = REGS_GP or (SIZE_QWORD shl 8) or RID_R15;

  { FPU Registers }
  RID_ST0 = $00;
  RID_ST1 = $01;
  RID_ST2 = $02;
  RID_ST3 = $03;
  RID_ST4 = $04;
  RID_ST5 = $05;
  RID_ST6 = $06;
  RID_ST7 = $07;
  REG_ST0 = REGS_FPU or RID_ST0;
  REG_ST1 = REGS_FPU or RID_ST1;
  REG_ST2 = REGS_FPU or RID_ST2;
  REG_ST3 = REGS_FPU or RID_ST3;
  REG_ST4 = REGS_FPU or RID_ST4;
  REG_ST5 = REGS_FPU or RID_ST5;
  REG_ST6 = REGS_FPU or RID_ST6;
  REG_ST7 = REGS_FPU or RID_ST7;

  { Controls Registers }
  RID_CR0 = $00;
  RID_CR1 = $01;
  RID_CR2 = $02;
  RID_CR3 = $03;
  RID_CR4 = $04;
  RID_CR5 = $05;
  RID_CR6 = $06;
  RID_CR7 = $07;
  RID_CR8 = $08;
  RID_CR9 = $09;
  RID_CR10 = $0A;
  RID_CR11 = $0B;
  RID_CR12 = $0C;
  RID_CR13 = $0D;
  RID_CR14 = $0E;
  RID_CR15 = $0F;
  REG_CR0 = REGS_CNTRL or RID_CR0;
  REG_CR1 = REGS_CNTRL or RID_CR1;
  REG_CR2 = REGS_CNTRL or RID_CR2;
  REG_CR3 = REGS_CNTRL or RID_CR3;
  REG_CR4 = REGS_CNTRL or RID_CR4;
  REG_CR5 = REGS_CNTRL or RID_CR5;
  REG_CR6 = REGS_CNTRL or RID_CR6;
  REG_CR7 = REGS_CNTRL or RID_CR7;
  REG_CR8 = REGS_CNTRL or RID_CR8;
  REG_CR9 = REGS_CNTRL or RID_CR9;
  REG_CR10 = REGS_CNTRL or RID_CR10;
  REG_CR11 = REGS_CNTRL or RID_CR11;
  REG_CR12 = REGS_CNTRL or RID_CR12;
  REG_CR13 = REGS_CNTRL or RID_CR13;
  REG_CR14 = REGS_CNTRL or RID_CR14;
  REG_CR15 = REGS_CNTRL or RID_CR15;
  REG_CR8D = REG_CR8;

  { Debug Registers }
  RID_DR0 = $00;
  RID_DR1 = $01;
  RID_DR2 = $02;
  RID_DR3 = $03;
  RID_DR4 = $04;
  RID_DR5 = $05;
  RID_DR6 = $06;
  RID_DR7 = $07;
  RID_DR8 = $08;
  RID_DR9 = $09;
  RID_DR10 = $0A;
  RID_DR11 = $0B;
  RID_DR12 = $0C;
  RID_DR13 = $0D;
  RID_DR14 = $0E;
  RID_DR15 = $0F;
  REG_DR0 = REGS_DBG or RID_DR0;
  REG_DR1 = REGS_DBG or RID_DR1;
  REG_DR2 = REGS_DBG or RID_DR2;
  REG_DR3 = REGS_DBG or RID_DR3;
  REG_DR4 = REGS_DBG or RID_DR4;
  REG_DR5 = REGS_DBG or RID_DR5;
  REG_DR6 = REGS_DBG or RID_DR6;
  REG_DR7 = REGS_DBG or RID_DR7;
  REG_DR8 = REGS_DBG or RID_DR8;
  REG_DR9 = REGS_DBG or RID_DR9;
  REG_DR10 = REGS_DBG or RID_DR10;
  REG_DR11 = REGS_DBG or RID_DR11;
  REG_DR12 = REGS_DBG or RID_DR12;
  REG_DR13 = REGS_DBG or RID_DR13;
  REG_DR14 = REGS_DBG or RID_DR14;
  REG_DR15 = REGS_DBG or RID_DR15;

  { TABLE Registers }
  RID_GDTR = $00;
  RID_IDTR = $01;
  RID_LDTR = $02;
  RID_TR = $03;
  REG_GDTR = REGS_TAB or RID_GDTR;
  REG_IDTR = REGS_TAB or RID_IDTR;
  REG_LDTR = REGS_TAB or RID_LDTR;
  REG_TR = REGS_TAB or RID_TR;

  { MMX Registers }
  RID_MM0 = $00;
  RID_MM1 = $01;
  RID_MM2 = $02;
  RID_MM3 = $03;
  RID_MM4 = $04;
  RID_MM5 = $05;
  RID_MM6 = $06;
  RID_MM7 = $07;
  RID_MM8 = $08;
  RID_MM9 = $09;
  RID_MM10 = $0A;
  RID_MM11 = $0B;
  RID_MM12 = $0C;
  RID_MM13 = $0D;
  RID_MM14 = $0E;
  RID_MM15 = $0F;
  REG_MM0 = REGS_MMX or RID_MM0;
  REG_MM1 = REGS_MMX or RID_MM1;
  REG_MM2 = REGS_MMX or RID_MM2;
  REG_MM3 = REGS_MMX or RID_MM3;
  REG_MM4 = REGS_MMX or RID_MM4;
  REG_MM5 = REGS_MMX or RID_MM5;
  REG_MM6 = REGS_MMX or RID_MM6;
  REG_MM7 = REGS_MMX or RID_MM7;
  REG_MM8 = REGS_MMX or RID_MM8;
  REG_MM9 = REGS_MMX or RID_MM9;
  REG_MM10 = REGS_MMX or RID_MM10;
  REG_MM11 = REGS_MMX or RID_MM11;
  REG_MM12 = REGS_MMX or RID_MM12;
  REG_MM13 = REGS_MMX or RID_MM13;
  REG_MM14 = REGS_MMX or RID_MM14;
  REG_MM15 = REGS_MMX or RID_MM15;

  { XMM Registers }
  RID_XMM0 = $00;
  RID_XMM1 = $01;
  RID_XMM2 = $02;
  RID_XMM3 = $03;
  RID_XMM4 = $04;
  RID_XMM5 = $05;
  RID_XMM6 = $06;
  RID_XMM7 = $07;
  RID_XMM8 = $08;
  RID_XMM9 = $09;
  RID_XMM10 = $0A;
  RID_XMM11 = $0B;
  RID_XMM12 = $0C;
  RID_XMM13 = $0D;
  RID_XMM14 = $0E;
  RID_XMM15 = $0F;
  RID_XMM16 = $10;
  RID_XMM17 = $11;
  RID_XMM18 = $12;
  RID_XMM19 = $13;
  RID_XMM20 = $14;
  RID_XMM21 = $15;
  RID_XMM22 = $16;
  RID_XMM23 = $17;
  RID_XMM24 = $18;
  RID_XMM25 = $19;
  RID_XMM26 = $1A;
  RID_XMM27 = $1B;
  RID_XMM28 = $1C;
  RID_XMM29 = $1D;
  RID_XMM30 = $1E;
  RID_XMM31 = $1F;
  REG_XMM0 = REGS_XMM or RID_XMM0;
  REG_XMM1 = REGS_XMM or RID_XMM1;
  REG_XMM2 = REGS_XMM or RID_XMM2;
  REG_XMM3 = REGS_XMM or RID_XMM3;
  REG_XMM4 = REGS_XMM or RID_XMM4;
  REG_XMM5 = REGS_XMM or RID_XMM5;
  REG_XMM6 = REGS_XMM or RID_XMM6;
  REG_XMM7 = REGS_XMM or RID_XMM7;
  REG_XMM8 = REGS_XMM or RID_XMM8;
  REG_XMM9 = REGS_XMM or RID_XMM9;
  REG_XMM10 = REGS_XMM or RID_XMM10;
  REG_XMM11 = REGS_XMM or RID_XMM11;
  REG_XMM12 = REGS_XMM or RID_XMM12;
  REG_XMM13 = REGS_XMM or RID_XMM13;
  REG_XMM14 = REGS_XMM or RID_XMM14;
  REG_XMM15 = REGS_XMM or RID_XMM15;
  REG_XMM16 = REGS_XMM or RID_XMM16;
  REG_XMM17 = REGS_XMM or RID_XMM17;
  REG_XMM18 = REGS_XMM or RID_XMM18;
  REG_XMM19 = REGS_XMM or RID_XMM19;
  REG_XMM20 = REGS_XMM or RID_XMM20;
  REG_XMM21 = REGS_XMM or RID_XMM21;
  REG_XMM22 = REGS_XMM or RID_XMM22;
  REG_XMM23 = REGS_XMM or RID_XMM23;
  REG_XMM24 = REGS_XMM or RID_XMM24;
  REG_XMM25 = REGS_XMM or RID_XMM25;
  REG_XMM26 = REGS_XMM or RID_XMM26;
  REG_XMM27 = REGS_XMM or RID_XMM27;
  REG_XMM28 = REGS_XMM or RID_XMM28;
  REG_XMM29 = REGS_XMM or RID_XMM29;
  REG_XMM30 = REGS_XMM or RID_XMM30;
  REG_XMM31 = REGS_XMM or RID_XMM31;

  { YMM Registers }
  RID_YMM0 = $00;
  RID_YMM1 = $01;
  RID_YMM2 = $02;
  RID_YMM3 = $03;
  RID_YMM4 = $04;
  RID_YMM5 = $05;
  RID_YMM6 = $06;
  RID_YMM7 = $07;
  RID_YMM8 = $08;
  RID_YMM9 = $09;
  RID_YMM10 = $0A;
  RID_YMM11 = $0B;
  RID_YMM12 = $0C;
  RID_YMM13 = $0D;
  RID_YMM14 = $0E;
  RID_YMM15 = $0F;
  RID_YMM16 = $10;
  RID_YMM17 = $11;
  RID_YMM18 = $12;
  RID_YMM19 = $13;
  RID_YMM20 = $14;
  RID_YMM21 = $15;
  RID_YMM22 = $16;
  RID_YMM23 = $17;
  RID_YMM24 = $18;
  RID_YMM25 = $19;
  RID_YMM26 = $1A;
  RID_YMM27 = $1B;
  RID_YMM28 = $1C;
  RID_YMM29 = $1D;
  RID_YMM30 = $1E;
  RID_YMM31 = $1F;
  REG_YMM0 = REGS_YMM or RID_YMM0;
  REG_YMM1 = REGS_YMM or RID_YMM1;
  REG_YMM2 = REGS_YMM or RID_YMM2;
  REG_YMM3 = REGS_YMM or RID_YMM3;
  REG_YMM4 = REGS_YMM or RID_YMM4;
  REG_YMM5 = REGS_YMM or RID_YMM5;
  REG_YMM6 = REGS_YMM or RID_YMM6;
  REG_YMM7 = REGS_YMM or RID_YMM7;
  REG_YMM8 = REGS_YMM or RID_YMM8;
  REG_YMM9 = REGS_YMM or RID_YMM9;
  REG_YMM10 = REGS_YMM or RID_YMM10;
  REG_YMM11 = REGS_YMM or RID_YMM11;
  REG_YMM12 = REGS_YMM or RID_YMM12;
  REG_YMM13 = REGS_YMM or RID_YMM13;
  REG_YMM14 = REGS_YMM or RID_YMM14;
  REG_YMM15 = REGS_YMM or RID_YMM15;
  REG_YMM16 = REGS_YMM or RID_YMM16;
  REG_YMM17 = REGS_YMM or RID_YMM17;
  REG_YMM18 = REGS_YMM or RID_YMM18;
  REG_YMM19 = REGS_YMM or RID_YMM19;
  REG_YMM20 = REGS_YMM or RID_YMM20;
  REG_YMM21 = REGS_YMM or RID_YMM21;
  REG_YMM22 = REGS_YMM or RID_YMM22;
  REG_YMM23 = REGS_YMM or RID_YMM23;
  REG_YMM24 = REGS_YMM or RID_YMM24;
  REG_YMM25 = REGS_YMM or RID_YMM25;
  REG_YMM26 = REGS_YMM or RID_YMM26;
  REG_YMM27 = REGS_YMM or RID_YMM27;
  REG_YMM28 = REGS_YMM or RID_YMM28;
  REG_YMM29 = REGS_YMM or RID_YMM29;
  REG_YMM30 = REGS_YMM or RID_YMM30;
  REG_YMM31 = REGS_YMM or RID_YMM31;

  { ZMM Registers }
  RID_ZMM0 = $00;
  RID_ZMM1 = $01;
  RID_ZMM2 = $02;
  RID_ZMM3 = $03;
  RID_ZMM4 = $04;
  RID_ZMM5 = $05;
  RID_ZMM6 = $06;
  RID_ZMM7 = $07;
  RID_ZMM8 = $08;
  RID_ZMM9 = $09;
  RID_ZMM10 = $0A;
  RID_ZMM11 = $0B;
  RID_ZMM12 = $0C;
  RID_ZMM13 = $0D;
  RID_ZMM14 = $0E;
  RID_ZMM15 = $0F;
  RID_ZMM16 = $10;
  RID_ZMM17 = $11;
  RID_ZMM18 = $12;
  RID_ZMM19 = $13;
  RID_ZMM20 = $14;
  RID_ZMM21 = $15;
  RID_ZMM22 = $16;
  RID_ZMM23 = $17;
  RID_ZMM24 = $18;
  RID_ZMM25 = $19;
  RID_ZMM26 = $1A;
  RID_ZMM27 = $1B;
  RID_ZMM28 = $1C;
  RID_ZMM29 = $1D;
  RID_ZMM30 = $1E;
  RID_ZMM31 = $1F;
  REG_ZMM0 = REGS_ZMM or RID_ZMM0;
  REG_ZMM1 = REGS_ZMM or RID_ZMM1;
  REG_ZMM2 = REGS_ZMM or RID_ZMM2;
  REG_ZMM3 = REGS_ZMM or RID_ZMM3;
  REG_ZMM4 = REGS_ZMM or RID_ZMM4;
  REG_ZMM5 = REGS_ZMM or RID_ZMM5;
  REG_ZMM6 = REGS_ZMM or RID_ZMM6;
  REG_ZMM7 = REGS_ZMM or RID_ZMM7;
  REG_ZMM8 = REGS_ZMM or RID_ZMM8;
  REG_ZMM9 = REGS_ZMM or RID_ZMM9;
  REG_ZMM10 = REGS_ZMM or RID_ZMM10;
  REG_ZMM11 = REGS_ZMM or RID_ZMM11;
  REG_ZMM12 = REGS_ZMM or RID_ZMM12;
  REG_ZMM13 = REGS_ZMM or RID_ZMM13;
  REG_ZMM14 = REGS_ZMM or RID_ZMM14;
  REG_ZMM15 = REGS_ZMM or RID_ZMM15;
  REG_ZMM16 = REGS_ZMM or RID_ZMM16;
  REG_ZMM17 = REGS_ZMM or RID_ZMM17;
  REG_ZMM18 = REGS_ZMM or RID_ZMM18;
  REG_ZMM19 = REGS_ZMM or RID_ZMM19;
  REG_ZMM20 = REGS_ZMM or RID_ZMM20;
  REG_ZMM21 = REGS_ZMM or RID_ZMM21;
  REG_ZMM22 = REGS_ZMM or RID_ZMM22;
  REG_ZMM23 = REGS_ZMM or RID_ZMM23;
  REG_ZMM24 = REGS_ZMM or RID_ZMM24;
  REG_ZMM25 = REGS_ZMM or RID_ZMM25;
  REG_ZMM26 = REGS_ZMM or RID_ZMM26;
  REG_ZMM27 = REGS_ZMM or RID_ZMM27;
  REG_ZMM28 = REGS_ZMM or RID_ZMM28;
  REG_ZMM29 = REGS_ZMM or RID_ZMM29;
  REG_ZMM30 = REGS_ZMM or RID_ZMM30;
  REG_ZMM31 = REGS_ZMM or RID_ZMM31;

  { Bound Registers }
  RID_BND0 = $00;
  RID_BND1 = $01;
  RID_BND2 = $02;
  RID_BND3 = $03;
  REG_BND0 = REGS_BND or RID_BND0;
  REG_BND1 = REGS_BND or RID_BND1;
  REG_BND2 = REGS_BND or RID_BND2;
  REG_BND3 = REGS_BND or RID_BND3;

  { Mask Registers }
  RID_K0 = $00;
  RID_K1 = $01;
  RID_K2 = $02;
  RID_K3 = $03;
  RID_K4 = $04;
  RID_K5 = $05;
  RID_K6 = $06;
  RID_K7 = $07;
  REG_K0 = REGS_MASK or RID_K0;
  REG_K1 = REGS_MASK or RID_K1;
  REG_K2 = REGS_MASK or RID_K2;
  REG_K3 = REGS_MASK or RID_K3;
  REG_K4 = REGS_MASK or RID_K4;
  REG_K5 = REGS_MASK or RID_K5;
  REG_K6 = REGS_MASK or RID_K6;
  REG_K7 = REGS_MASK or RID_K7;

  Reg8RexMask: array [0 .. 1] of array [0 .. $F] of WORD = ( //
    ( //
    REG_AL, { 00 }
    REG_CL, { 01 }
    REG_DL, { 02 }
    REG_BL, { 03 }
    REG_AH, { 04 }
    REG_CH, { 05 }
    REG_DH, { 06 }
    REG_BH, { 07 }
    0, 0, 0, 0, 0, 0, 0, 0), ( //
    REG_AL, { 00 }
    REG_CL, { 01 }
    REG_DL, { 02 }
    REG_BL, { 03 }
    REG_SPL, { 04 }
    REG_BPL, { 05 }
    REG_SIL, { 06 }
    REG_DIL, { 07 }
    REG_R8B, { 08 }
    REG_R9B, { 09 }
    REG_R10B, { 10 }
    REG_R11B, { 11 }
    REG_R12B, { 12 }
    REG_R13B, { 13 }
    REG_R14B, { 14 }
    REG_R15B { 15 }
    ));

implementation

end.

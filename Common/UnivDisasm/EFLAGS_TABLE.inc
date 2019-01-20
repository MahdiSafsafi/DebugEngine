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
// The Original Code is EFLAGS_TABLE.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

const 
EFlagsTable : array [0..$71 -1 ] of TEflags = (//
{0x00} (FOF : $00 ; FSF : $00 ; FZF : $00 ; FAF : $00 ; FPF : $00 ; FCF : $00 ; FTF : $00 ; FIF : $00 ; FDF : $00 ; FNT : $00 ; FRF : $00 ; Reserved : $00) ,{???}
{0x01} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_TM ; FPF : EF_U ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{AAA}
{0x02} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{AAD}
{0x03} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{AAM}
{0x04} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_TM ; FPF : EF_U ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{AAS}
{0x05} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ADC}
{0x06} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ADD}
{0x07} (FOF : EF_R ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_R ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{AND}
{0x08} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ARPL}
{0x09} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{BOUND}
{0x0A} (FOF : EF_U ; FSF : EF_U ; FZF : EF_M ; FAF : EF_U ; FPF : EF_U ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{BSF/BSR}
{0x0B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{BSWAP}
{0x0C} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{BT/BTS/BTR/BTC}
{0x0D} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CALL}
{0x0E} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CBW}
{0x0F} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_R ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CLC}
{0x10} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_R ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CLD}
{0x11} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_R ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CLI}
{0x12} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CLTS}
{0x13} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMC}
{0x14} (FOF : EF_T ; FSF : EF_T ; FZF : EF_T ; FAF : EF_N ; FPF : EF_T ; FCF : EF_T ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMOVcc}
{0x15} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMP}
{0x16} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMPS}
{0x17} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMPXCHG}
{0x18} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CMPXCHG8B}
{0x19} (FOF : EF_R ; FSF : EF_R ; FZF : EF_M ; FAF : EF_R ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{COMSID}
{0x1A} (FOF : EF_R ; FSF : EF_R ; FZF : EF_M ; FAF : EF_R ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{COMISS}
{0x1B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CPUID}
{0x1C} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{CWD}
{0x1D} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_TM ; FPF : EF_M ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{DAA}
{0x1E} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_TM ; FPF : EF_M ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{DAS}
{0x1F} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{DEC}
{0x20} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{DIV}
{0x21} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ENTER}
{0x22} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ESC}
{0x23} (FOF : EF_N ; FSF : EF_N ; FZF : EF_T ; FAF : EF_N ; FPF : EF_T ; FCF : EF_T ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{FCMOVcc}
{0x24} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{FCOMI/FCOMIP/FUCOMI/FUCOMIP}
{0x25} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{HLT}
{0x26} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{IDIV}
{0x27} (FOF : EF_M ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{IMUL}
{0x28} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{IN}
{0x29} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{INC}
{0x2A} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{INS}
{0x2B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_R ; FIF : EF_N ; FDF : EF_N ; FNT : EF_R ; FRF : EF_N ; Reserved : $00) ,{INT}
{0x2C} (FOF : EF_T ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_R ; FIF : EF_N ; FDF : EF_N ; FNT : EF_R ; FRF : EF_N ; Reserved : $00) ,{INTO}
{0x2D} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{INVD}
{0x2E} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{INVLPG}
{0x2F} (FOF : EF_R ; FSF : EF_R ; FZF : EF_M ; FAF : EF_R ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{UCOMSID}
{0x30} (FOF : EF_R ; FSF : EF_R ; FZF : EF_M ; FAF : EF_R ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{UCOMISS}
{0x31} (FOF : EF_P ; FSF : EF_P ; FZF : EF_P ; FAF : EF_P ; FPF : EF_P ; FCF : EF_P ; FTF : EF_P ; FIF : EF_P ; FDF : EF_P ; FNT : EF_T ; FRF : EF_N ; Reserved : $00) ,{IRET}
{0x32} (FOF : EF_T ; FSF : EF_T ; FZF : EF_T ; FAF : EF_N ; FPF : EF_T ; FCF : EF_T ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{Jcc}
{0x33} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{JCXZ}
{0x34} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{JMP}
{0x35} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LAHF}
{0x36} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LAR}
{0x37} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LDS/LES/LSS/LFS/LGS}
{0x38} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LEA}
{0x39} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LEAVE}
{0x3A} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LGDT/LIDT/LLDT/LMSW}
{0x3B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LOCK}
{0x3C} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LODS}
{0x3D} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LOOP}
{0x3E} (FOF : EF_N ; FSF : EF_N ; FZF : EF_T ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LOOPE/LOOPNE}
{0x3F} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LSL}
{0x40} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{LTR}
{0x41} (FOF : EF_U ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_U ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{MOV(control|debug|test)}
{0x42} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{MOV}
{0x43} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{MOVS}
{0x44} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{MOVSX/MOVZX}
{0x45} (FOF : EF_M ; FSF : EF_U ; FZF : EF_U ; FAF : EF_U ; FPF : EF_U ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{MUL}
{0x46} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{NEG}
{0x47} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{NOP}
{0x48} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{NOT}
{0x49} (FOF : EF_R ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_R ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{OR}
{0x4A} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{OUT}
{0x4B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{OUTS}
{0x4C} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{POP/POPA}
{0x4D} (FOF : EF_P ; FSF : EF_P ; FZF : EF_P ; FAF : EF_P ; FPF : EF_P ; FCF : EF_P ; FTF : EF_P ; FIF : EF_P ; FDF : EF_P ; FNT : EF_P ; FRF : EF_N ; Reserved : $00) ,{POPF}
{0x4E} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{PUSH/PUSHA/PUSHF}
{0x4F} (FOF : EF_M ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RCL/RCR(1)}
{0x50} (FOF : EF_U ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RCL/RCR(count)}
{0x51} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RDMSR}
{0x52} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RDPMC}
{0x53} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RDTSC}
{0x54} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{REP/REPE/REPNE}
{0x55} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{RET}
{0x56} (FOF : EF_M ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ROL/ROR(1)}
{0x57} (FOF : EF_U ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{ROL/ROR(count)}
{0x58} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_M ; FIF : EF_M ; FDF : EF_M ; FNT : EF_M ; FRF : EF_M ; Reserved : $00) ,{RSM}
{0x59} (FOF : EF_N ; FSF : EF_P ; FZF : EF_P ; FAF : EF_P ; FPF : EF_P ; FCF : EF_P ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SAHF}
{0x5A} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SAL/SAR/SHL/SHR(1)}
{0x5B} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SAL/SAR/SHL/SHR(count)}
{0x5C} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_TM ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SBB}
{0x5D} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SCAS}
{0x5E} (FOF : EF_T ; FSF : EF_T ; FZF : EF_T ; FAF : EF_N ; FPF : EF_T ; FCF : EF_T ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SETcc}
{0x5F} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SGDT/SIDT/SLDT/SMSW}
{0x60} (FOF : EF_U ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SHLD/SHRD}
{0x61} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_S ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{STC}
{0x62} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_S ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{STD}
{0x63} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_S ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{STI}
{0x64} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_T ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{STOS}
{0x65} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{STR}
{0x66} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{SUB}
{0x67} (FOF : EF_R ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_R ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{TEST}
{0x68} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{UD2}
{0x69} (FOF : EF_N ; FSF : EF_N ; FZF : EF_M ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{VERR/VERRW}
{0x6A} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{WAIT}
{0x6B} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{WBINVD}
{0x6C} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{WRMSR}
{0x6D} (FOF : EF_M ; FSF : EF_M ; FZF : EF_M ; FAF : EF_M ; FPF : EF_M ; FCF : EF_M ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{XADD}
{0x6E} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{XCHG}
{0x6F} (FOF : EF_N ; FSF : EF_N ; FZF : EF_N ; FAF : EF_N ; FPF : EF_N ; FCF : EF_N ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) ,{XLAT}
{0x70} (FOF : EF_R ; FSF : EF_M ; FZF : EF_M ; FAF : EF_U ; FPF : EF_M ; FCF : EF_R ; FTF : EF_N ; FIF : EF_N ; FDF : EF_N ; FNT : EF_N ; FRF : EF_N ; Reserved : $00) {XOR});
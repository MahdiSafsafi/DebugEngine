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
// The Original Code is ModRmFlags.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

type
  TModRmFlagsArray = array [Byte] of Byte;
  PModRmFlagsArray = ^TModRmFlagsArray;

const
  MF_USED = $01; // ModRm used.
  MF_REG = $00; // Just register without memory.
  MF_MEM = $02; // Memory addressing mode.
  MF_BASE = $04; // Register in memory.
  MF_SIB = $08; // SIB follow modrm.
  MF_SIZE_MASK = $30;
  MF_SBYTE = 1 shl 4; // 1 byte disp.
  MF_SWORD = 2 shl 4; // 2 byte disp.
  MF_SDWORD = 3 shl 4; // 4 byte disp.
  MF_EXTRA_MASK = $C0;
  { ===> MF_N1 <===
    if PM64 and adsize=64, then effective address = [RIP+sdword]
    if PM64 and adsize=32, then effective address = [EIP+sdword]
    else effective address = [sdword] }
  MF_N1 = 1 shl 6;
  MF_N2 = 2 shl 6; // if MVEX/EVEX, then sbyte is interpreted as sbyte*N.
  MF_DEF_SS = 3 shl 6; // defaults to SS segment register.

  ModRm16Flags: TModRmFlagsArray = ( //
    MF_USED or MF_MEM or MF_BASE, { 0x00 }
    MF_USED or MF_MEM or MF_BASE, { 0x01 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x02 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x03 }
    MF_USED or MF_MEM or MF_BASE, { 0x04 }
    MF_USED or MF_MEM or MF_BASE, { 0x05 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x06 }
    MF_USED or MF_MEM or MF_BASE, { 0x07 }
    MF_USED or MF_MEM or MF_BASE, { 0x08 }
    MF_USED or MF_MEM or MF_BASE, { 0x09 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x0A }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x0B }
    MF_USED or MF_MEM or MF_BASE, { 0x0C }
    MF_USED or MF_MEM or MF_BASE, { 0x0D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x0E }
    MF_USED or MF_MEM or MF_BASE, { 0x0F }
    MF_USED or MF_MEM or MF_BASE, { 0x10 }
    MF_USED or MF_MEM or MF_BASE, { 0x11 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x12 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x13 }
    MF_USED or MF_MEM or MF_BASE, { 0x14 }
    MF_USED or MF_MEM or MF_BASE, { 0x15 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x16 }
    MF_USED or MF_MEM or MF_BASE, { 0x17 }
    MF_USED or MF_MEM or MF_BASE, { 0x18 }
    MF_USED or MF_MEM or MF_BASE, { 0x19 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x1A }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x1B }
    MF_USED or MF_MEM or MF_BASE, { 0x1C }
    MF_USED or MF_MEM or MF_BASE, { 0x1D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x1E }
    MF_USED or MF_MEM or MF_BASE, { 0x1F }
    MF_USED or MF_MEM or MF_BASE, { 0x20 }
    MF_USED or MF_MEM or MF_BASE, { 0x21 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x22 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x23 }
    MF_USED or MF_MEM or MF_BASE, { 0x24 }
    MF_USED or MF_MEM or MF_BASE, { 0x25 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x26 }
    MF_USED or MF_MEM or MF_BASE, { 0x27 }
    MF_USED or MF_MEM or MF_BASE, { 0x28 }
    MF_USED or MF_MEM or MF_BASE, { 0x29 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x2A }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x2B }
    MF_USED or MF_MEM or MF_BASE, { 0x2C }
    MF_USED or MF_MEM or MF_BASE, { 0x2D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x2E }
    MF_USED or MF_MEM or MF_BASE, { 0x2F }
    MF_USED or MF_MEM or MF_BASE, { 0x30 }
    MF_USED or MF_MEM or MF_BASE, { 0x31 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x32 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x33 }
    MF_USED or MF_MEM or MF_BASE, { 0x34 }
    MF_USED or MF_MEM or MF_BASE, { 0x35 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x36 }
    MF_USED or MF_MEM or MF_BASE, { 0x37 }
    MF_USED or MF_MEM or MF_BASE, { 0x38 }
    MF_USED or MF_MEM or MF_BASE, { 0x39 }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x3A }
    MF_USED or MF_MEM or MF_BASE or MF_DEF_SS, { 0x3B }
    MF_USED or MF_MEM or MF_BASE, { 0x3C }
    MF_USED or MF_MEM or MF_BASE, { 0x3D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x3E }
    MF_USED or MF_MEM or MF_BASE, { 0x3F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x40 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x41 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x42 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x43 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x44 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x45 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x46 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x47 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x48 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x49 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x4A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x4B }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x4E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x50 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x51 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x52 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x53 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x54 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x55 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x56 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x57 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x58 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x59 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x5A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x5B }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x5E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x60 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x61 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x62 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x63 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x64 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x65 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x66 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x67 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x68 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x69 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x6A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x6B }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x6E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x70 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x71 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x72 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x73 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x74 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x75 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x76 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x77 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x78 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x79 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x7A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x7B }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x7E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7F }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x80 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x81 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x82 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x83 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x84 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x85 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x86 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x87 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x88 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x89 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x8A }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x8B }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x8C }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x8D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x8E }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x8F }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x90 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x91 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x92 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x93 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x94 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x95 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x96 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x97 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x98 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x99 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x9A }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x9B }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x9C }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x9D }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0x9E }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0x9F }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA0 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA1 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xA2 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xA3 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA4 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA5 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xA6 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA7 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA8 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xA9 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xAA }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xAB }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xAC }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xAD }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xAE }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xAF }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB0 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB1 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xB2 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xB3 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB4 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB5 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xB6 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB7 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB8 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xB9 }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xBA }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xBB }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xBC }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xBD }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD or MF_DEF_SS, { 0xBE }
    MF_USED or MF_MEM or MF_BASE or MF_SWORD, { 0xBF }
    MF_USED or MF_REG, { 0xC0 }
    MF_USED or MF_REG, { 0xC1 }
    MF_USED or MF_REG, { 0xC2 }
    MF_USED or MF_REG, { 0xC3 }
    MF_USED or MF_REG, { 0xC4 }
    MF_USED or MF_REG, { 0xC5 }
    MF_USED or MF_REG, { 0xC6 }
    MF_USED or MF_REG, { 0xC7 }
    MF_USED or MF_REG, { 0xC8 }
    MF_USED or MF_REG, { 0xC9 }
    MF_USED or MF_REG, { 0xCA }
    MF_USED or MF_REG, { 0xCB }
    MF_USED or MF_REG, { 0xCC }
    MF_USED or MF_REG, { 0xCD }
    MF_USED or MF_REG, { 0xCE }
    MF_USED or MF_REG, { 0xCF }
    MF_USED or MF_REG, { 0xD0 }
    MF_USED or MF_REG, { 0xD1 }
    MF_USED or MF_REG, { 0xD2 }
    MF_USED or MF_REG, { 0xD3 }
    MF_USED or MF_REG, { 0xD4 }
    MF_USED or MF_REG, { 0xD5 }
    MF_USED or MF_REG, { 0xD6 }
    MF_USED or MF_REG, { 0xD7 }
    MF_USED or MF_REG, { 0xD8 }
    MF_USED or MF_REG, { 0xD9 }
    MF_USED or MF_REG, { 0xDA }
    MF_USED or MF_REG, { 0xDB }
    MF_USED or MF_REG, { 0xDC }
    MF_USED or MF_REG, { 0xDD }
    MF_USED or MF_REG, { 0xDE }
    MF_USED or MF_REG, { 0xDF }
    MF_USED or MF_REG, { 0xE0 }
    MF_USED or MF_REG, { 0xE1 }
    MF_USED or MF_REG, { 0xE2 }
    MF_USED or MF_REG, { 0xE3 }
    MF_USED or MF_REG, { 0xE4 }
    MF_USED or MF_REG, { 0xE5 }
    MF_USED or MF_REG, { 0xE6 }
    MF_USED or MF_REG, { 0xE7 }
    MF_USED or MF_REG, { 0xE8 }
    MF_USED or MF_REG, { 0xE9 }
    MF_USED or MF_REG, { 0xEA }
    MF_USED or MF_REG, { 0xEB }
    MF_USED or MF_REG, { 0xEC }
    MF_USED or MF_REG, { 0xED }
    MF_USED or MF_REG, { 0xEE }
    MF_USED or MF_REG, { 0xEF }
    MF_USED or MF_REG, { 0xF0 }
    MF_USED or MF_REG, { 0xF1 }
    MF_USED or MF_REG, { 0xF2 }
    MF_USED or MF_REG, { 0xF3 }
    MF_USED or MF_REG, { 0xF4 }
    MF_USED or MF_REG, { 0xF5 }
    MF_USED or MF_REG, { 0xF6 }
    MF_USED or MF_REG, { 0xF7 }
    MF_USED or MF_REG, { 0xF8 }
    MF_USED or MF_REG, { 0xF9 }
    MF_USED or MF_REG, { 0xFA }
    MF_USED or MF_REG, { 0xFB }
    MF_USED or MF_REG, { 0xFC }
    MF_USED or MF_REG, { 0xFD }
    MF_USED or MF_REG, { 0xFE }
    MF_USED or MF_REG { 0xFF } );

  ModRm32Flags: TModRmFlagsArray = ( //
    MF_USED or MF_MEM or MF_BASE, { 0x00 }
    MF_USED or MF_MEM or MF_BASE, { 0x01 }
    MF_USED or MF_MEM or MF_BASE, { 0x02 }
    MF_USED or MF_MEM or MF_BASE, { 0x03 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x04 }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x05 }
    MF_USED or MF_MEM or MF_BASE, { 0x06 }
    MF_USED or MF_MEM or MF_BASE, { 0x07 }
    MF_USED or MF_MEM or MF_BASE, { 0x08 }
    MF_USED or MF_MEM or MF_BASE, { 0x09 }
    MF_USED or MF_MEM or MF_BASE, { 0x0A }
    MF_USED or MF_MEM or MF_BASE, { 0x0B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x0C }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x0D }
    MF_USED or MF_MEM or MF_BASE, { 0x0E }
    MF_USED or MF_MEM or MF_BASE, { 0x0F }
    MF_USED or MF_MEM or MF_BASE, { 0x10 }
    MF_USED or MF_MEM or MF_BASE, { 0x11 }
    MF_USED or MF_MEM or MF_BASE, { 0x12 }
    MF_USED or MF_MEM or MF_BASE, { 0x13 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x14 }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x15 }
    MF_USED or MF_MEM or MF_BASE, { 0x16 }
    MF_USED or MF_MEM or MF_BASE, { 0x17 }
    MF_USED or MF_MEM or MF_BASE, { 0x18 }
    MF_USED or MF_MEM or MF_BASE, { 0x19 }
    MF_USED or MF_MEM or MF_BASE, { 0x1A }
    MF_USED or MF_MEM or MF_BASE, { 0x1B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x1C }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x1D }
    MF_USED or MF_MEM or MF_BASE, { 0x1E }
    MF_USED or MF_MEM or MF_BASE, { 0x1F }
    MF_USED or MF_MEM or MF_BASE, { 0x20 }
    MF_USED or MF_MEM or MF_BASE, { 0x21 }
    MF_USED or MF_MEM or MF_BASE, { 0x22 }
    MF_USED or MF_MEM or MF_BASE, { 0x23 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x24 }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x25 }
    MF_USED or MF_MEM or MF_BASE, { 0x26 }
    MF_USED or MF_MEM or MF_BASE, { 0x27 }
    MF_USED or MF_MEM or MF_BASE, { 0x28 }
    MF_USED or MF_MEM or MF_BASE, { 0x29 }
    MF_USED or MF_MEM or MF_BASE, { 0x2A }
    MF_USED or MF_MEM or MF_BASE, { 0x2B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x2C }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x2D }
    MF_USED or MF_MEM or MF_BASE, { 0x2E }
    MF_USED or MF_MEM or MF_BASE, { 0x2F }
    MF_USED or MF_MEM or MF_BASE, { 0x30 }
    MF_USED or MF_MEM or MF_BASE, { 0x31 }
    MF_USED or MF_MEM or MF_BASE, { 0x32 }
    MF_USED or MF_MEM or MF_BASE, { 0x33 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x34 }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x35 }
    MF_USED or MF_MEM or MF_BASE, { 0x36 }
    MF_USED or MF_MEM or MF_BASE, { 0x37 }
    MF_USED or MF_MEM or MF_BASE, { 0x38 }
    MF_USED or MF_MEM or MF_BASE, { 0x39 }
    MF_USED or MF_MEM or MF_BASE, { 0x3A }
    MF_USED or MF_MEM or MF_BASE, { 0x3B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB, { 0x3C }
    MF_USED or MF_MEM or MF_SDWORD or MF_N1, { 0x3D }
    MF_USED or MF_MEM or MF_BASE, { 0x3E }
    MF_USED or MF_MEM or MF_BASE, { 0x3F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x40 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x41 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x42 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x43 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x44 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x45 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x46 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x47 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x48 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x49 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x4C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x4D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x4F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x50 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x51 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x52 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x53 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x54 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x55 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x56 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x57 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x58 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x59 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x5C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x5D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x5F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x60 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x61 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x62 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x63 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x64 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x65 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x66 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x67 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x68 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x69 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x6C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x6D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x6F }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x70 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x71 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x72 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x73 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x74 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x75 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x76 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x77 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_N2, { 0x78 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x79 }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7A }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SBYTE, { 0x7C }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE or MF_DEF_SS, { 0x7D }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7E }
    MF_USED or MF_MEM or MF_BASE or MF_SBYTE, { 0x7F }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x80 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x81 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x82 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x83 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0x84 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0x85 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x86 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x87 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x88 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x89 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x8A }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x8B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0x8C }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0x8D }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x8E }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x8F }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x90 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x91 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x92 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x93 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0x94 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0x95 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x96 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x97 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x98 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x99 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x9A }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x9B }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0x9C }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0x9D }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x9E }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0x9F }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA0 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA1 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA2 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA3 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0xA4 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0xA5 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA6 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA7 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA8 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xA9 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xAA }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xAB }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0xAC }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0xAD }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xAE }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xAF }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB0 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB1 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB2 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB3 }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0xB4 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0xB5 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB6 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB7 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB8 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xB9 }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xBA }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xBB }
    MF_USED or MF_MEM or MF_BASE or MF_SIB or MF_SDWORD, { 0xBC }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD or MF_DEF_SS, { 0xBD }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xBE }
    MF_USED or MF_MEM or MF_BASE or MF_SDWORD, { 0xBF }
    MF_USED or MF_REG, { 0xC0 }
    MF_USED or MF_REG, { 0xC1 }
    MF_USED or MF_REG, { 0xC2 }
    MF_USED or MF_REG, { 0xC3 }
    MF_USED or MF_REG, { 0xC4 }
    MF_USED or MF_REG, { 0xC5 }
    MF_USED or MF_REG, { 0xC6 }
    MF_USED or MF_REG, { 0xC7 }
    MF_USED or MF_REG, { 0xC8 }
    MF_USED or MF_REG, { 0xC9 }
    MF_USED or MF_REG, { 0xCA }
    MF_USED or MF_REG, { 0xCB }
    MF_USED or MF_REG, { 0xCC }
    MF_USED or MF_REG, { 0xCD }
    MF_USED or MF_REG, { 0xCE }
    MF_USED or MF_REG, { 0xCF }
    MF_USED or MF_REG, { 0xD0 }
    MF_USED or MF_REG, { 0xD1 }
    MF_USED or MF_REG, { 0xD2 }
    MF_USED or MF_REG, { 0xD3 }
    MF_USED or MF_REG, { 0xD4 }
    MF_USED or MF_REG, { 0xD5 }
    MF_USED or MF_REG, { 0xD6 }
    MF_USED or MF_REG, { 0xD7 }
    MF_USED or MF_REG, { 0xD8 }
    MF_USED or MF_REG, { 0xD9 }
    MF_USED or MF_REG, { 0xDA }
    MF_USED or MF_REG, { 0xDB }
    MF_USED or MF_REG, { 0xDC }
    MF_USED or MF_REG, { 0xDD }
    MF_USED or MF_REG, { 0xDE }
    MF_USED or MF_REG, { 0xDF }
    MF_USED or MF_REG, { 0xE0 }
    MF_USED or MF_REG, { 0xE1 }
    MF_USED or MF_REG, { 0xE2 }
    MF_USED or MF_REG, { 0xE3 }
    MF_USED or MF_REG, { 0xE4 }
    MF_USED or MF_REG, { 0xE5 }
    MF_USED or MF_REG, { 0xE6 }
    MF_USED or MF_REG, { 0xE7 }
    MF_USED or MF_REG, { 0xE8 }
    MF_USED or MF_REG, { 0xE9 }
    MF_USED or MF_REG, { 0xEA }
    MF_USED or MF_REG, { 0xEB }
    MF_USED or MF_REG, { 0xEC }
    MF_USED or MF_REG, { 0xED }
    MF_USED or MF_REG, { 0xEE }
    MF_USED or MF_REG, { 0xEF }
    MF_USED or MF_REG, { 0xF0 }
    MF_USED or MF_REG, { 0xF1 }
    MF_USED or MF_REG, { 0xF2 }
    MF_USED or MF_REG, { 0xF3 }
    MF_USED or MF_REG, { 0xF4 }
    MF_USED or MF_REG, { 0xF5 }
    MF_USED or MF_REG, { 0xF6 }
    MF_USED or MF_REG, { 0xF7 }
    MF_USED or MF_REG, { 0xF8 }
    MF_USED or MF_REG, { 0xF9 }
    MF_USED or MF_REG, { 0xFA }
    MF_USED or MF_REG, { 0xFB }
    MF_USED or MF_REG, { 0xFC }
    MF_USED or MF_REG, { 0xFD }
    MF_USED or MF_REG, { 0xFE }
    MF_USED or MF_REG { 0xFF } );

  AM2ModRmFlags: array [0 .. 3] of PModRmFlagsArray = ( //
    nil, { ?? }
    @ModRm16Flags, { AM 16 bit }
    @ModRm32Flags, { AM 32 bit }
    @ModRm32Flags { AM 64 bit }
    );

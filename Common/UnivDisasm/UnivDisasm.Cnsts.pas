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
// The Original Code is UnivDisasm.Cnsts.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Cnsts;

interface

{$I Config.inc}

const
  { Errors }
  ERROR_SUCCESS = 0; // No errors detected.
  ERROR_INVALID_OPERAND_SIZE = $01; // Invalid J size.
  ERROR_INVALID_EFFECTIVE_ADDRESS = $02; // Invalid memory address => May cause AV.
  ERROR_INVALID_ADDRESS_MODE = $04; // Invalid Address mode.
  ERROR_VL_EXPECTED_UPPER = $08; // Vector length size must be YWORD or ZWORD.
  ERROR_VL_EXPECTED_LOWER = $10; // Vector length size must be OWORD or YWORD.
  ERROR_INVALID_SEGMENT = $20; // Invalid segment register.
  ERROR_INVALID_VEX_ESCAPE = $40; // Invalid VEX prefix escape .
  ERROR_INVALID_EVEX_ESCAPE = $80; // Invalid EVEX prefix escape .
  ERROR_INVALID_XOP_ESCAPE = $100; // Invalid XOP prefix escape .
  ERROR_EXCEEDED_SAFE_LENGTH = $200; // Exceeded Safe length.
  ERROR_SIB_EXPECTED = $400;
  ERROR_RELATIVE_ADDRESS_PROHIBITED = $800;
  ERROR_INVALID_OPCODE = $1000;
  ERROR_INTERNAL = $80000; // Internal Error

  { Warnings }
  WARN_NIL = $00; { No warnings }
  WARN_INST_NOT_LOCKABLE = $01; // Instruction not lockable.
  WARN_XAQUIRE_INVALID = $04; // Invalid XAQUIRE prefix.
  WARN_REPNE_INVALID = $08; // Invalid REPNE prefix.
  WARN_REP_INVALID = $10; // Invalid REP prefix.
  WARN_REPE_INVALID = $20; // Invalid REPE prefix.
  WARN_XRELEASE_INVALID = $40;
  WARN_SUPERFLUOUS_PREFIX = $80; // Prefix found , but not required.
  WARN_XAQUIRE_NEED_LOCK = $100; // XAQUIRE prefix require LOCK prefix.
  WARN_XRELEASE_NEED_LOCK = $200; // XRELEASE prefix require LOCK prefix.
  WARN_SOURCE_OPERAND_NOT_MEM = $400; // Source operand must be memory.
  WARN_CS_PREFIX_IGNORED = $0800; // CS segment override prefix is ignored.
  WARN_DS_PREFIX_IGNORED = $1000; // DS segment override prefix is ignored.
  WARN_SS_PREFIX_IGNORED = $2000; // SS segment override prefix is ignored.
  WARN_ES_PREFIX_IGNORED = $4000; // ES segment override prefix is ignored.
  WARN_BND_NO_INIT = $8000; // Instruction doesn't init bnd regs .
  WARN_INDEX_REG_NOT_USED_IN_EAC = $10000; // Index register is not used in effective address calculation.

  { CPU Vendor }
  VENDOR_INTEL = 0; // Intel.
  VENDOR_AMD = 1; // AMD.
  VENDOR_CENTAUR = 2; // Centaur.

  { Architecture }
  CPUX32 = 0; // x86
  CPUX64 = 1; // x86-x64
{$IFDEF CPUX64}
  CPUX = CPUX64;
{$ELSE !CPUX64}
  CPUX = CPUX32;
{$ENDIF CPUX64}
  { Address mode }
  AM_16 = 1; // Address mode 16-bits.
  AM_32 = 2; // Address mode 32-bits.
  AM_64 = 3; // Address mode 64-bits.

  { Options }
  GO_TEST_ADDRESS = $01;

  { Instruction flags }
  GF_TABLE_MASK = $FF;
  GF_GROUP_MASK = $1F00;
  GF_TABLE_LEGACY = $00;
  GF_TABLE_FPU = $10;
  GF_TABLE_3DNOW = $20;
  GF_TABLE_SSE5 = $40;
  GF_TABLE_XOP = $80;
  GF_TABLE_1 = GF_TABLE_LEGACY or $01;
  GF_TABLE_2 = GF_TABLE_LEGACY or $02;
  GF_TABLE_38 = GF_TABLE_LEGACY or $03;
  GF_TABLE_3A = GF_TABLE_LEGACY or $04;
  { FPU TABLES }
  GF_TABLE_FPU_D8 = GF_TABLE_FPU or $01;
  GF_TABLE_FPU_D9 = GF_TABLE_FPU or $02;
  GF_TABLE_FPU_DA = GF_TABLE_FPU or $03;
  GF_TABLE_FPU_DB = GF_TABLE_FPU or $04;
  GF_TABLE_FPU_DC = GF_TABLE_FPU or $05;
  GF_TABLE_FPU_DD = GF_TABLE_FPU or $06;
  GF_TABLE_FPU_DE = GF_TABLE_FPU or $07;
  GF_TABLE_FPU_DF = GF_TABLE_FPU or $08;
  { SSE5 TABLES }
  GF_TABLE_SSE5_24 = GF_TABLE_SSE5 or $01;
  GF_TABLE_SSE5_25 = GF_TABLE_SSE5 or $02;
  GF_TABLE_SSE5_7A = GF_TABLE_SSE5 or $03;
  GF_TABLE_SSE5_7B = GF_TABLE_SSE5 or $04;
  { XOP TABLES }
  GF_TABLE_XOP_8 = GF_TABLE_XOP or $01;
  GF_TABLE_XOP_9 = GF_TABLE_XOP or $02;
  GF_TABLE_XOP_A = GF_TABLE_XOP or $03;

  { GROUPS }
  GF_GROUP_1 = $01 shl 8;
  GF_GROUP_2 = $02 shl 8;
  GF_GROUP_3 = $03 shl 8;
  GF_GROUP_4 = $04 shl 8;
  GF_GROUP_5 = $05 shl 8;
  GF_GROUP_6 = $06 shl 8;
  GF_GROUP_7 = $07 shl 8;
  GF_GROUP_8 = $08 shl 8;
  GF_GROUP_9 = $09 shl 8;
  GF_GROUP_10 = $0A shl 8;
  GF_GROUP_11 = $0B shl 8;
  GF_GROUP_12 = $0C shl 8;
  GF_GROUP_13 = $0D shl 8;
  GF_GROUP_14 = $0E shl 8;
  GF_GROUP_15 = $0F shl 8;
  GF_GROUP_16 = $10 shl 8;
  GF_GROUP_17 = $11 shl 8;
  GF_GROUP_18 = $12 shl 8;
  GF_GROUP_19 = $13 shl 8;
  GF_GROUP_20 = $14 shl 8;
  GF_GROUP_1A = $15 shl 8;
  GF_GROUP_XOP1 = $16 shl 8;
  GF_GROUP_XOP2 = $17 shl 8;
  GF_GROUP_XOP3 = $18 shl 8;
  GF_GROUP_XOP4 = $19 shl 8;
  GF_IMM_EXT = $2000; // An immediate exists and serves as an opcode extension.

  INST_GRP_AVX_MASK = $800;
  INST_GRP_AVX512_MASK = INST_GRP_AVX_MASK or $1000;
  INST_GRP_FMA_MASK = $2000;
  INST_GRP_MMX = $80;
  INST_GRP_SSE5A = $100;
  INST_GRP_VL = $200;
  INST_GRP_VSIB = $400;

  INST_GRP_3DNOW = $01;
  INST_GRP_ADX = $02;
  INST_GRP_AES = $03;
  INST_GRP_AVX = INST_GRP_AVX_MASK or $04;
  INST_GRP_AVX2 = INST_GRP_AVX_MASK or $05;
  INST_GRP_AVX512BW = INST_GRP_AVX512_MASK or $06;
  INST_GRP_AVX512CD = INST_GRP_AVX512_MASK or $07;
  INST_GRP_AVX512DQ = INST_GRP_AVX512_MASK or $08;
  INST_GRP_AVX512ER = INST_GRP_AVX512_MASK or $09;
  INST_GRP_AVX512F = INST_GRP_AVX512_MASK or $0A;
  INST_GRP_AVX512IFMA = INST_GRP_AVX512_MASK or INST_GRP_FMA_MASK or $0B;
  INST_GRP_AVX512PF = INST_GRP_AVX512_MASK or $0C;
  INST_GRP_AVX512VBMI = INST_GRP_AVX512_MASK or $0D;
  INST_GRP_BMI = $0E;
  INST_GRP_BMI2 = $0F;
  INST_GRP_F16C = $10;
  INST_GRP_FMA = INST_GRP_FMA_MASK or $11;
  INST_GRP_FMA4 = INST_GRP_FMA_MASK or $12;
  INST_GRP_FPU = $13;
  INST_GRP_GP = $14;
  INST_GRP_ICEBP = $15;
  INST_GRP_INVPCID = $16;
  INST_GRP_LWP = $17;
  INST_GRP_MPX = $1A;
  INST_GRP_PCLMUL = $1B;
  INST_GRP_PREFIXES = $1C;
  INST_GRP_SHA = $1D;
  INST_GRP_SMM = $1E;
  INST_GRP_SSE = $1F;
  INST_GRP_SSE2 = $20;
  INST_GRP_SSE3 = $21;
  INST_GRP_SSE4A = $22;
  INST_GRP_SSE4V1 = $23;
  INST_GRP_SSE4V2 = $24;
  INST_GRP_SSSE3 = $26;
  INST_GRP_SYSTEM = $27;
  INST_GRP_TBM = $28;
  INST_GRP_TSX = $29;
  INST_GRP_VME = $2B;
  INST_GRP_VMX = $2C;
  INST_GRP_XOP = $2E;
  INST_GRP_INSX = $00;
  INST_GRP_MEM = $00;

  INST_CATEGORY_ARITHMETIC = $80000000;
  INST_CATEGORY_FLOATING_POINT = $40000000;

  INST_CATEGORY_BIT_BYTE = $1000000;
  INST_CATEGORY_INTEGER = $2000000;
  INST_CATEGORY_BINARY = $3000000;
  INST_CATEGORY_128_BIT = $4000000;
  INST_CATEGORY_DECIMAL = $5000000;
  INST_CATEGORY_16_BIT = $6000000;
  INST_CATEGORY_64_BIT = $7000000;

  INST_CATEGORY_SIGNED = $40000;
  INST_CATEGORY_UNSIGNED = $20000;

  INST_CATEGORY_SINGLE_PRECISION = $10000;
  INST_CATEGORY_DOUBLE_PRECISION = $8000;

  INST_CATEGORY_DATA_TRANSFER = $80000;
  INST_CATEGORY_CONVERT = $100000;
  INST_CATEGORY_LOAD = $200000;
  INST_CATEGORY_COMPARE = $300000;
  INST_CATEGORY_STORE = $400000;
  INST_CATEGORY_CONTROL = $500000;
  INST_CATEGORY_MISCELLANEOUS = $600000;
  INST_CATEGORY_STRING = $700000;
  INST_CATEGORY_LOGICAL = $800000;
  INST_CATEGORY_STATE_MANAGEMENT = $900000;
  INST_CATEGORY_CONTROL_TRANSFER = $A00000;
  INST_CATEGORY_SEGMENT_REGISTER = $B00000;
  INST_CATEGORY_IO = $C00000;
  INST_CATEGORY_SHIFT = $D00000;
  INST_CATEGORY_ROTATE = $E00000;
  INST_CATEGORY_CONSTANTS = $F00000;
  INST_CATEGORY_FLAG_CONTROL = $180000;
  INST_CATEGORY_ENTER_LEAVE = $280000;

  INST_CATEGORY_ROUND = $4000;
  INST_CATEGORY_SCALE = $2000;
  INST_CATEGORY_SCALAR = $1000;

  INST_CATEGORY_PACK = $800;
  INST_CATEGORY_PACKED = $400;
  INST_CATEGORY_UNALIGNED = $200;
  INST_CATEGORY_UNPACK = $100;
  INST_CATEGORY_ALIGNED = $80;

  INST_CATEGORY_SIMD = $20;
  INST_CATEGORY_BMI1 = $40;
  INST_CATEGORY_X87_FPU = $60;

  INST_CATEGORY_CONDITIONAL = $1;
  INST_CATEGORY_HINT = $2;
  INST_CATEGORY_MIN = $3;
  INST_CATEGORY_MAX = $4;
  INST_CATEGORY_SHUFFLE = $5;
  INST_CATEGORY_HORIZONTAL = $6;
  INST_CATEGORY_BLENDING = $7;
  INST_CATEGORY_ALIGN_RIGHT = $8;
  INST_CATEGORY_EXTRACTION = $9;
  INST_CATEGORY_INSERTION = $A;
  INST_CATEGORY_DOT_PRODUCT = $B;
  INST_CATEGORY_SUMS_ABSOLUTE_DIFFERENCES = $C;
  INST_CATEGORY_TRANSCENDENTAL = $D;
  INST_CATEGORY_CACHEABILITY_CONTROL = $E;
  INST_CATEGORY_AGENT_SYNCHRONIZATION = $F;
  INST_CATEGORY_RANDOM_NUMBER_GENERATOR = $10;
  INST_CATEGORY__ = 0;
  INST_CATEGORY_NIL = 0;

  { Encoding }
  ENC_V = 4; // VEX || EVEX.
  ENC_EVEX = ENC_V or 1;
  ENC_VEX = ENC_V or 2;
  ENC_REX = $07;
  ENC_DREX = $08;
  ENC_XOP = $09;

  { Mandatory Prefixes }
  MND_PRF_66 = 1;
  MND_PRF_F0 = 2;
  MND_PRF_F2 = 4;
  MND_PRF_F3 = 8;
  MND_PRF_NA = $10;

  { Prefix flags }
  PF_USED = $01; // Prefix is used.
  PF_VALID = $02; // Prefix make sense.
  PF_SUPERFLUOUS = $04; // Prefix not required.
  PF_MANDATORY = $08; // Prefix serves as a mandatory to select instruction.
  PF_TYPE_MASK = $E0;
  { May used only with F2 prefix ! }
  PF_REPNE = $01 shl $05;
  PF_BND = $02 shl $05;
  PF_XAQUIRE = $03 shl $05;
  { May used only with F3 prefix ! }
  PF_REP = $04 shl $05;
  PF_REPE = $05 shl $05;
  PF_XRELEASE = $06 shl $05;
  { May used only when VEX ! }
  PF_VEX2 = $100;
  PF_VEX3 = $200;

  { Args flags }
  AF_USED = $01; // Argument is used.
  AF_TYPE_MASK = $0E;
  AF_REG = $01 shl 1; // Argument is a register.
  AF_SEG = AF_REG;
  AF_MEM = $02 shl 1; // Argument is a memory.
  AF_IMM = $03 shl 1; // Argument is an immediate.
  AF_RELATIVE_OFFSET = $04 shl 1; // Argument is a relative offset <J>.
  AF_ADDRESS = $05 shl 1;
  AF_OFFSET = $06 shl 1;
  AF_MASK = $10; // {K}
  AF_ZEROING = $20; // {z}
  AF_EMB = $40; // Embeded.
  AF_ROUNDING = $80;
  AF_SAE = $100;
  AF_BROADCAST32 = $200;
  AF_BROADCAST64 = $400;

  { Displacement flags }
  DF_USED = $01; // Exists Displacement in memory.
  DF_REL = $02; // Relative displacement.
  DF_DISP8N = $04; // EVEX disp 8 * N.

  { Sib flags }
  SF_USED = $1; // Sib exists.
  SF_VSIB = $2; // VSIB form.
  SF_MIB = $4; // MIB form where index not used in effective address calculation.

  { Branch && relative offset flags }
  JF_USED = $01;
  JF_BRANCH = $02; // distinguish between instruction that uses J offset and real branch.
  JF_REL = $04; // Relative.
  JF_REG = $10; // JMP|CALL REG.
  JF_ABSOLUTE = $20;
  JF_INDIRECT = $40;
  JF_SHORT = $80; // Short branch.
  JF_NEAR = $100; // Near branch.
  JF_FAR = $200; // Far branch.
  JF_JCC = $400; // Conditional jump.

  { Rounding }
  RN_SAE = 1;
  RD_SAE = 2;
  RU_SAE = 3;
  RZ_SAE = 4;

  { SIZE }
  SIZE_1_BYTE = 1;
  SIZE_2_BYTE = 2;
  SIZE_4_BYTE = 4;
  SIZE_6_BYTE = 6;
  SIZE_8_BYTE = 8;
  SIZE_10_BYTE = 10;
  SIZE_14_BYTE = 14;
  SIZE_16_BYTE = 16;
  SIZE_32_BYTE = 32;
  SIZE_64_BYTE = 64;
  SIZE_28_BYTE = 28;
  SIZE_98_BYTE = 98;
  SIZE_108_BYTE = 108;
  SIZE_BYTE = SIZE_1_BYTE;
  SIZE_WORD = SIZE_2_BYTE;
  SIZE_DWORD = SIZE_4_BYTE;
  SIZE_FWORD = SIZE_6_BYTE;
  SIZE_QWORD = SIZE_8_BYTE;
  SIZE_TBYTE = SIZE_10_BYTE;
  SIZE_OWORD = SIZE_16_BYTE;
  SIZE_YWORD = SIZE_32_BYTE;
  SIZE_ZWORD = SIZE_64_BYTE;

  { Vector length }
  VL128 = SIZE_OWORD;
  VL256 = SIZE_YWORD;
  VL512 = SIZE_ZWORD;

  { Tuples Type to decode disp8 * N }
  TT_FV = $01;
  TT_HV = $02;
  TT_FVM = $03;
  TT_HVM = $04;
  TT_QVM = $05;
  TT_OVM = $06;
  TT_T2 = $07;
  TT_T4 = $08;
  TT_T8 = $09;
  TT_T1S = $0A;
  TT_T1F = $0B;
  TT_M128 = $0C;
  TT_DUP = $0D;

  { EFlags }
  EF_N = $01; // Instruction does not affect flag.
  EF_U = $02; // Instruction's effect on flag is undefined.
  EF_T = $04; // Instruction tests flag.
  EF_M = $08; // Instruction modifies flag (either sets or resets depending on operands).
  EF_R = $10; // Instruction resets flag.
  EF_S = $20; // Instruction sets flag.
  EF_P = $40; // Instruction restores prior value of flag.
  EF_TM = EF_T or EF_M;

  { InstStr Length }
  MAX_MNEM_LENGTH = 32;
  MAX_INST_STR_LENGTH = 128;

  { Internal constants used by UnivDisasm }
  SP_DISP8_VE = $01;
  SP_DISP8_VE_32 = SP_DISP8_VE or $02;
  SP_DISP8_VE_64 = SP_DISP8_VE or $04;
  MagicRexMask = $80;
  PM64 = CPUX64; // Only for x64.
  I64 = CPUX32; // Invalid when PM64.

implementation

end.

// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.AsmRegUtils
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.AsmRegUtils.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.AsmRegUtils;

interface

{$I DebugEngine.inc}

const
{$IFDEF CPUX86}
  NumberOfLegacyRegisters = 8;
  NumberOfVectorRegisters = 8;
{$ELSE !CPUX86}
  NumberOfLegacyRegisters = 16;
  NumberOfVectorRegisters = 16;
{$ENDIF CPUX86}
  NumberOfFPURegisters = 8;
  NumberOfMMXRegisters = 8;

  { Size in bytes. }
{$IFDEF CPUX86}
  SizeOfLegacyRegister = 04;
{$ELSE !CPUX86}
  SizeOfLegacyRegister = 08;
{$ENDIF CPUX86}
  SizeOfMMXRegister = 08;
  SizeOfFPURegister = 10;
  SizeOfXMMRegister = 16;
  SizeOfYMMRegister = 32;
  SizeOfZMMRegister = 64;
  SizeOfFPUEnvironnement = 108;
  SizeOfFPUEnvironnementX = 512;

type
  TData8 = ShortInt; // Register (8 Byte) data (AL).
  TData16 = SmallInt; // Register (16 Byte) data (AX).
  TData32 = Integer; // Register (32 Byte) data (EAX).
  TData64 = Int64; // Register (64 Byte) data (RAX).
  TMMXData = TData64; // MMX register.
  TStData = Extended80; // FPU register.

  TRflags = type NativeUInt; // Rflags register.
  TMXCSR = type Cardinal; // MXCSR register.

{$IFDEF CPUX86}

  TEAXRegister = record
    case Integer of
      0: (AsEAX: TData32);
      1: (AsAX: TData16);
      2: (AsAL, AsAH: TData8);
  end;

  PEAXRegister = ^TEAXRegister;

  TECXRegister = record
    case Integer of
      0: (AsECX: TData32);
      1: (AsCX: TData16);
      2: (AsCL, AsCH: TData8);
  end;

  PECXRegister = ^TECXRegister;

  TEDXRegister = record
    case Integer of
      0: (AsEDX: TData32);
      1: (AsDX: TData16);
      2: (AsDL, AsDH: TData8);
  end;

  PEDXRegister = ^TEDXRegister;

  TEBXRegister = record
    case Integer of
      0: (AsEBX: TData32);
      1: (AsBX: TData16);
      2: (AsBL, AsBH: TData8);
  end;

  PEBXRegister = ^TEBXRegister;

  TESPRegister = record
    case Integer of
      0: (AsESP: TData32);
      1: (AsSP: TData16);
  end;

  PESPRegister = ^TESPRegister;

  TEBPRegister = record
    case Integer of
      0: (AsEBP: TData32);
      1: (AsBP: TData16);
  end;

  PEBPRegister = ^TEBPRegister;

  TESIRegister = record
    case Integer of
      0: (AsESI: TData32);
      1: (AsSI: TData16);
  end;

  PESIRegister = ^TESIRegister;

  TEDIRegister = record
    case Integer of
      0: (AsEDI: TData32);
      1: (AsDI: TData16);
  end;

  PEDIRegister = ^TEDIRegister;

  TLegacyRegisters32 = record
    EAX: TEAXRegister;
    ECX: TECXRegister;
    EDX: TEDXRegister;
    EBX: TEBXRegister;
    ESP: TESPRegister;
    EBP: TEBPRegister;
    ESI: TESIRegister;
    EDI: TEDIRegister;
  end;

  PLegacyRegisters32 = ^TLegacyRegisters32;
{$ELSE !CPUX86}

  TRAXRegister = record
    case Integer of
      0: (AsRAX: TData64);
      1: (AsR0: TData64); // Same as RAX.
      2: (AsEAX: TData32);
      3: (AsR0D: TData32); // Same as EAX.
      4: (AsAX: TData16);
      5: (AsR0W: TData16); // Same as AX.
      6: (AsAL, AsAH: TData8);
      7: (AsR0B: TData8); // Same as AL.
  end;

  PRAXRegister = ^TRAXRegister;

  TRCXRegister = record
    case Integer of
      0: (AsRCX: TData64);
      1: (AsR1: TData64);
      2: (AsECX: TData32);
      3: (AsR1D: TData32);
      4: (AsCX: TData16);
      5: (AsR1W: TData16);
      6: (AsCL, AsCH: TData8);
      7: (AsR1B: TData8);
  end;

  PRCXRegister = ^TRCXRegister;

  TRDXRegister = record
    case Integer of
      0: (AsRDX: TData64);
      1: (AsR2: TData64);
      2: (AsEDX: TData32);
      3: (AsR2D: TData32);
      4: (AsDX: TData16);
      5: (AsR2W: TData16);
      6: (AsDL, AsDH: TData8);
      7: (AsR2B: TData8);
  end;

  PRDXRegister = ^TRDXRegister;

  TRBXRegister = record
    case Integer of
      0: (AsRBX: TData64);
      1: (AsR3: TData64);
      2: (AsEBX: TData32);
      3: (AsR3D: TData32);
      4: (AsBX: TData16);
      5: (AsR3W: TData16);
      6: (AsBL, AsBH: TData8);
      7: (AsR3B: TData8);
  end;

  PRBXRegister = ^TRBXRegister;

  TRSPRegister = record
    case Integer of
      0: (AsRSP: TData64);
      1: (AsR4: TData64);
      2: (AsESP: TData32);
      3: (AsR4D: TData32);
      4: (AsSP: TData16);
      5: (AsR4W: TData16);
      6: (AsSPL: TData8);
      7: (AsR4B: TData8);
  end;

  PRSPRegister = ^TRSPRegister;

  TRBPRegister = record
    case Integer of
      0: (AsRBP: TData64);
      1: (AsR5: TData64);
      2: (AsEBP: TData32);
      3: (AsR5D: TData32);
      4: (AsBP: TData16);
      5: (AsR5W: TData16);
      6: (AsBPL: TData8);
      7: (AsR5B: TData8);
  end;

  PRBPRegister = ^TRBPRegister;

  TRSIRegister = record
    case Integer of
      0: (AsRSI: TData64);
      1: (AsR6: TData64);
      2: (AsESI: TData32);
      3: (AsR6D: TData32);
      4: (AsSI: TData16);
      5: (AsR6W: TData16);
      6: (AsSIL: TData8);
      7: (AsR6B: TData8);
  end;

  PRSIRegister = ^TRSIRegister;

  TRDIRegister = record
    case Integer of
      0: (AsRDI: TData64);
      1: (AsR7: TData64);
      2: (AsEDI: TData32);
      3: (AsR7D: TData32);
      4: (AsDI: TData16);
      5: (AsR7W: TData16);
      6: (AsDIL: TData8);
      7: (AsR7B: TData8);
  end;

  PRDIRegister = ^TRDIRegister;

  TR8Register = record
    case Integer of
      0: (AsR8: TData64);
      1: (AsR8D: TData32);
      2: (AsR8W: TData16);
      3: (AsR8B: TData8);
  end;

  PR8Register = ^TR8Register;

  TR9Register = record
    case Integer of
      0: (AsR9: TData64);
      1: (AsR9D: TData32);
      2: (AsR9W: TData16);
      3: (AsR9B: TData8);
  end;

  PR9Register = ^TR9Register;

  TR10Register = record
    case Integer of
      0: (AsR10: TData64);
      1: (AsR10D: TData32);
      2: (AsR10W: TData16);
      3: (AsR10B: TData8);
  end;

  PR10Register = ^TR10Register;

  TR11Register = record
    case Integer of
      0: (AsR11: TData64);
      1: (AsR11D: TData32);
      2: (AsR11W: TData16);
      3: (AsR11B: TData8);
  end;

  PR11Register = ^TR11Register;

  TR12Register = record
    case Integer of
      0: (AsR12: TData64);
      1: (AsR12D: TData32);
      2: (AsR12W: TData16);
      3: (AsR12B: TData8);
  end;

  PR12Register = ^TR12Register;

  TR13Register = record
    case Integer of
      0: (AsR13: TData64);
      1: (AsR13D: TData32);
      2: (AsR13W: TData16);
      3: (AsR13B: TData8);
  end;

  PR13Register = ^TR13Register;

  TR14Register = record
    case Integer of
      0: (AsR14: TData64);
      1: (AsR14D: TData32);
      2: (AsR14W: TData16);
      3: (AsR14B: TData8);
  end;

  PR14Register = ^TR14Register;

  TR15Register = record
    case Integer of
      0: (AsR15: TData64);
      1: (AsR15D: TData32);
      2: (AsR15W: TData16);
      3: (AsR15B: TData8);
  end;

  PR15Register = ^TR15Register;

  TLegacyRegisters64 = record
    RAX: TRAXRegister;
    RCX: TRCXRegister;
    RDX: TRDXRegister;
    RBX: TRBXRegister;
    RSP: TRSPRegister;
    RBP: TRBPRegister;
    RSI: TRSIRegister;
    RDI: TRDIRegister;
    R8: TR8Register;
    R9: TR9Register;
    R10: TR10Register;
    R11: TR11Register;
    R12: TR12Register;
    R13: TR13Register;
    R14: TR14Register;
    R15: TR15Register;
  end;

  PLegacyRegisters64 = ^TLegacyRegisters64;
{$ENDIF CPUX86}

  { MMX registers }
  TMMXRegisters = packed record
    case Boolean of
      True: (MM0: TMMXData);
      False: (Reserved0: array [0 .. 10 - 1] of Byte;
          case Boolean of
            True: (MM1: TMMXData);
            False: (Reserved1: array [0 .. 10 - 1] of Byte;
              case Boolean of
                True: (MM2: TMMXData);
                False: (Reserved2: array [0 .. 10 - 1] of Byte;
                  case Boolean of
                    True: (MM3: TMMXData);
                    False: (Reserved3: array [0 .. 10 - 1] of Byte;
                      case Boolean of
                        True: (MM4: TMMXData);
                        False: (Reserved4: array [0 .. 10 - 1] of Byte;
                          case Boolean of
                            True: (MM5: TMMXData);
                            False: (Reserved5: array [0 .. 10 - 1] of Byte;
                              case Boolean of
                                True: (MM6: TMMXData);
                                False: (Reserved6: array [0 .. 10 - 1] of Byte;
                                  case Boolean of
                                    True: (MM7: TMMXData);
                                    False: (Reserved7: array [0 .. 10 - 1] of Byte;

                                      );););););););
          );
  end;

  PMMXRegisters = ^TMMXRegisters;

  { FPU <St(x)> registers }
  TFPURegisters = packed record
    St0: TStData;
    St1: TStData;
    St2: TStData;
    St3: TStData;
    St4: TStData;
    St5: TStData;
    St6: TStData;
    St7: TStData;
  end;

  PFPURegisters = ^TFPURegisters;

  { Dump memory for FRSTOR/FSAVE/FNSAVE instructions. }
  TFPUEnvironnement = packed record
    ControlWord: Cardinal; // Only low word is used.
    StatusWord: Cardinal; // Only low word is used.
    TagWord: Cardinal; // Only low word is used.
    FIP: Cardinal; // Full dword.
    FIPSelector: Word;
    FPUOpCode: Word; // Only 0-10 bits are used.
    FDP: Cardinal; // Full dword.
    FDPSelector: Cardinal; // Only low word is used.
    case Integer of
      0: (FPUStack: array [0 .. SizeOf(TFPURegisters) - 1] of Byte);
      1: (FPURegisters: TFPURegisters);
      2: (MMXRegisters: TMMXRegisters);
  end;

  PFPUEnvironnement = ^TFPUEnvironnement;

  { Vector XMM register. }
  TXMMRegister = record
    case Integer of
      0: (AsPackedInteger: array [0 .. 4 - 1] of Integer);
      1: (AsPackedSingle: array [0 .. 4 - 1] of Single);
      2: (AsPackedDouble: array [0 .. 2 - 1] of Double);
      3: (AsScalarDouble: Double);
      4: (AsScalarSingle: Single);
  end;

  PXMMRegister = ^TXMMRegister;

  { Vector YMM register. }
  TYMMRegister = record
    case Integer of
      0: (AsPackedInteger: array [0 .. 8 - 1] of Integer);
      1: (AsPackedSingle: array [0 .. 8 - 1] of Single);
      2: (AsPackedDouble: array [0 .. 4 - 1] of Double);
  end;

  PYMMRegister = ^TYMMRegister;

  { Vector ZMM register. }
  TZMMRegister = record
    case Integer of
      0: (AsPackedInteger: array [0 .. 16 - 1] of Integer);
      1: (AsPackedSingle: array [0 .. 16 - 1] of Single);
      2: (AsPackedDouble: array [0 .. 8 - 1] of Double);
  end;

  PZMMRegister = ^TZMMRegister;

  TFPUMMXRegistersSaveAreaX = packed record
    case Integer of
      0: (MM0: TMMXData);
      1: (St0: TStData);
      2: (Reserved0: array [0 .. 16 - 1] of Byte;
          case Integer of
            0: (MM1: TMMXData);
            1: (St1: TStData);
            2: (Reserved1: array [0 .. 16 - 1] of Byte;
              case Integer of
                0: (MM2: TMMXData);
                1: (St2: TStData);
                2: (Reserved2: array [0 .. 16 - 1] of Byte;
                  case Integer of
                    0: (MM3: TMMXData);
                    1: (St3: TStData);
                    2: (Reserved3: array [0 .. 16 - 1] of Byte;
                      case Integer of
                        0: (MM4: TMMXData);
                        1: (St4: TStData);
                        2: (Reserved4: array [0 .. 16 - 1] of Byte;
                          case Integer of
                            0: (MM5: TMMXData);
                            1: (St5: TStData);
                            2: (Reserved5: array [0 .. 16 - 1] of Byte;
                              case Integer of
                                0: (MM6: TMMXData);
                                1: (St6: TStData);
                                2: (Reserved6: array [0 .. 16 - 1] of Byte;
                                  case Integer of
                                    0: (MM7: TMMXData);
                                    1: (St7: TStData);
                                    2: (Reserved7: array [0 .. 16 - 1] of Byte;

                                      );

                                    );

                                );

                            );

                        );

                    );

                );

          );
  end;

  PFPUMMXRegistersSaveAreaX = ^TFPUMMXRegistersSaveAreaX;

  { TXMMRegisters }
  TXMMRegisters = record
    XMM0: TXMMRegister;
    XMM1: TXMMRegister;
    XMM2: TXMMRegister;
    XMM3: TXMMRegister;
    XMM4: TXMMRegister;
    XMM5: TXMMRegister;
    XMM6: TXMMRegister;
    XMM7: TXMMRegister;
{$IFDEF CPUX64}
    XMM8: TXMMRegister;
    XMM9: TXMMRegister;
    XMM10: TXMMRegister;
    XMM11: TXMMRegister;
    XMM12: TXMMRegister;
    XMM13: TXMMRegister;
    XMM14: TXMMRegister;
    XMM15: TXMMRegister;
{$ENDIF CPUX64}
  end;

  PXMMRegisters = ^TXMMRegisters;

  { Dump memory for FXRSTOR/FXSAVE instructions
    ==> Memory must be aligned on a 16-byte boundary ! }
  TFPUEnvironnementX = packed record
    ControlWord: Word;
    StatusWord: Word;
    TagWord: Byte;
    Reserved: Byte;
    FPUOpCode: Word;
    case Boolean of
      False: (FIP: Cardinal; FIPSelector: Word);
      True: (FIP64: UInt64;
          case Boolean of
            False: (FDP: Cardinal; FDPSelector: Word);
            True: (FDP64: UInt64; //
              MXCSR: Cardinal; //
              MXCSR_MASK: Cardinal; //
              FPUMMXRegisters: TFPUMMXRegistersSaveAreaX; //
              XMMRegisters: TXMMRegisters;
{$IFDEF CPUX86}
              __Reserved: array [0 .. $E0 - 1] of Byte;
{$ELSE !CPUX86}
              __Reserved: array [0 .. $60 - 1] of Byte;
{$ENDIF CPUX86}
              );
          );

  end;

  PFPUEnvironnementX = ^TFPUEnvironnementX;

  { TVectorRegisters }
  TVectorRegisters = record
    case Integer of
      0: (XMM0: TXMMRegister);
      1: (YMM0: TYMMRegister);
      2: (ZMM0: TZMMRegister;
          case Integer of
            0: (XMM1: TXMMRegister);
            1: (YMM1: TYMMRegister);
            2: (ZMM1: TZMMRegister;
              case Integer of
                0: (XMM2: TXMMRegister);
                1: (YMM2: TYMMRegister);
                2: (ZMM2: TZMMRegister;
                  case Integer of
                    0: (XMM3: TXMMRegister);
                    1: (YMM3: TYMMRegister);
                    2: (ZMM3: TZMMRegister;
                      case Integer of
                        0: (XMM4: TXMMRegister);
                        1: (YMM4: TYMMRegister);
                        2: (ZMM4: TZMMRegister;
                          case Integer of
                            0: (XMM5: TXMMRegister);
                            1: (YMM5: TYMMRegister);
                            2: (ZMM5: TZMMRegister;
                              case Integer of
                                0: (XMM6: TXMMRegister);
                                1: (YMM6: TYMMRegister);
                                2: (ZMM6: TZMMRegister;
                                  case Integer of
                                    0: (XMM7: TXMMRegister);
                                    1: (YMM7: TYMMRegister);
                                    2: (ZMM7: TZMMRegister;
{$IFDEF CPUX64}
                                      case Integer of
                                        0: (XMM8: TXMMRegister);
                                        1: (YMM8: TYMMRegister);
                                        2: (ZMM8: TZMMRegister;
                                        case Integer of
                                        0: (XMM9: TXMMRegister);
                                        1: (YMM9: TYMMRegister);
                                        2: (ZMM9: TZMMRegister;
                                        case Integer of
                                        0: (XMM10: TXMMRegister);
                                        1: (YMM10: TYMMRegister);
                                        2: (ZMM10: TZMMRegister;
                                        case Integer of
                                        0: (XMM11: TXMMRegister);
                                        1: (YMM11: TYMMRegister);
                                        2: (ZMM11: TZMMRegister;
                                        case Integer of
                                        0: (XMM12: TXMMRegister);
                                        1: (YMM12: TYMMRegister);
                                        2: (ZMM12: TZMMRegister;
                                        case Integer of
                                        0: (XMM13: TXMMRegister);
                                        1: (YMM13: TYMMRegister);
                                        2: (ZMM13: TZMMRegister;
                                        case Integer of
                                        0: (XMM14: TXMMRegister);
                                        1: (YMM14: TYMMRegister);
                                        2: (ZMM14: TZMMRegister;
                                        case Integer of
                                        0: (XMM15: TXMMRegister);
                                        1: (YMM15: TYMMRegister);
                                        2: (ZMM15: TZMMRegister;

                                        );););););

                                        );););
{$ENDIF CPUX64}
                                        );

                                    );

                                );

                            );

                        );

                    );

                );

          );

  end;

  PVectorRegisters = ^TVectorRegisters;

{$IFDEF CPUX86}
  TLegacyRegisters = TLegacyRegisters32;
{$ELSE !CPUX86}
  TLegacyRegisters = TLegacyRegisters64;
{$ENDIF CPUX86}

  { -----> Helper <----- }
  { TRFlagsHlp }
  { Do not edit ! => TRFlagsHlp was auto generated by RFlagsGen.pl }
  TRFlagsHlp = record helper for TRflags
  private
    function GetCF: Boolean;
    procedure SetCF(Value: Boolean);
    function GetPF: Boolean;
    procedure SetPF(Value: Boolean);
    function GetAF: Boolean;
    procedure SetAF(Value: Boolean);
    function GetZF: Boolean;
    procedure SetZF(Value: Boolean);
    function GetSF: Boolean;
    procedure SetSF(Value: Boolean);
    function GetTF: Boolean;
    procedure SetTF(Value: Boolean);
    function GetIF: Boolean;
    procedure SetIF(Value: Boolean);
    function GetDF: Boolean;
    procedure SetDF(Value: Boolean);
    function GetOF: Boolean;
    procedure SetOF(Value: Boolean);
    function GetNT: Boolean;
    procedure SetNT(Value: Boolean);
    function GetRF: Boolean;
    procedure SetRF(Value: Boolean);
    function GetVM: Boolean;
    procedure SetVM(Value: Boolean);
    function GetAC: Boolean;
    procedure SetAC(Value: Boolean);
    function GetVIF: Boolean;
    procedure SetVIF(Value: Boolean);
    function GetVIP: Boolean;
    procedure SetVIP(Value: Boolean);
    function GetID: Boolean;
    procedure SetID(Value: Boolean);
    function GetIOPL: ShortInt;
    procedure SetIOPL(Value: ShortInt);
  public
    property CF: Boolean read GetCF write SetCF;
    property PF: Boolean read GetPF write SetPF;
    property AF: Boolean read GetAF write SetAF;
    property ZF: Boolean read GetZF write SetZF;
    property SF: Boolean read GetSF write SetSF;
    property TF: Boolean read GetTF write SetTF;
    property &IF: Boolean read GetIF write SetIF;
    property DF: Boolean read GetDF write SetDF;
    property &OF: Boolean read GetOF write SetOF;
    property NT: Boolean read GetNT write SetNT;
    property RF: Boolean read GetRF write SetRF;
    property VM: Boolean read GetVM write SetVM;
    property AC: Boolean read GetAC write SetAC;
    property VIF: Boolean read GetVIF write SetVIF;
    property VIP: Boolean read GetVIP write SetVIP;
    property ID: Boolean read GetID write SetID;
    property IOPL: ShortInt read GetIOPL write SetIOPL;
  end;

  { TMXCSRHlp }
  { Do not edit ! => TMXCSRHlp was auto generated by MXCSRGen.pl }
  TMXCSRHlp = record helper for TMXCSR
  private
    function GetIE: Boolean;
    procedure SetIE(Value: Boolean);
    function GetDE: Boolean;
    procedure SetDE(Value: Boolean);
    function GetZE: Boolean;
    procedure SetZE(Value: Boolean);
    function GetOE: Boolean;
    procedure SetOE(Value: Boolean);
    function GetUE: Boolean;
    procedure SetUE(Value: Boolean);
    function GetPE: Boolean;
    procedure SetPE(Value: Boolean);
    function GetDAZ: Boolean;
    procedure SetDAZ(Value: Boolean);
    function GetIM: Boolean;
    procedure SetIM(Value: Boolean);
    function GetDM: Boolean;
    procedure SetDM(Value: Boolean);
    function GetZM: Boolean;
    procedure SetZM(Value: Boolean);
    function GetOM: Boolean;
    procedure SetOM(Value: Boolean);
    function GetUM: Boolean;
    procedure SetUM(Value: Boolean);
    function GetPM: Boolean;
    procedure SetPM(Value: Boolean);
    function GetFZ: Boolean;
    procedure SetFZ(Value: Boolean);
    function GetRC: ShortInt;
    procedure SetRC(Value: ShortInt);
  public
    property IE: Boolean read GetIE write SetIE;
    property DE: Boolean read GetDE write SetDE;
    property ZE: Boolean read GetZE write SetZE;
    property OE: Boolean read GetOE write SetOE;
    property UE: Boolean read GetUE write SetUE;
    property PE: Boolean read GetPE write SetPE;
    property DAZ: Boolean read GetDAZ write SetDAZ;
    property IM: Boolean read GetIM write SetIM;
    property DM: Boolean read GetDM write SetDM;
    property ZM: Boolean read GetZM write SetZM;
    property OM: Boolean read GetOM write SetOM;
    property UM: Boolean read GetUM write SetUM;
    property PM: Boolean read GetPM write SetPM;
    property FZ: Boolean read GetFZ write SetFZ;
    property RC: ShortInt read GetRC write SetRC;
  end;

  /// <summary> Clone all legacy registers.
  /// </summary>
function SnapshotOfLegacyRegisters(var Registers: TLegacyRegisters): Boolean;

/// <summary> Clone all FPU registers.
/// </summary>
function SnapshotOfFPURegisters(var Registers: TFPURegisters): Boolean;

/// <summary> Clone all MMX registers.
/// </summary>
function SnapshotOfMMXRegisters(var Registers: TMMXRegisters): Boolean;

/// <summary> Clone all vector registers.
/// </summary>
/// <remarks>
/// Because vector registers are alias to others registers,
/// the function will try first to clone registers that have biggest size.
/// Thus, try to copy ZMM registers first. if fails, it passes to YMM registers,
/// if fails, it passes finally to try copy XMM registers.
/// </remarks>
function SnapshotOfVectorRegisters(var Registers: TVectorRegisters): Boolean;

/// <summary> Copy current FPU environnement.
/// </summary>
function SnapshotOfFPUEnvironnement(var Environnement: TFPUEnvironnement): Boolean;

/// <summary> Copy current FPU environnement.
/// </summary>
/// <remarks>
/// 1- This function does the same job as <see cref="SnapshotOfFPUEnvironnement" />
/// However, it supports XMM registers and executes faster.
/// <para></para>
/// 2- It's better to provide an aligned Environnement variable to 16-byte boundary.
/// If the Environnement variable is not aligned, the function will use a local aligned
/// variable and than it will copy this local variable to the Environnement variable.
/// </remarks>
function SnapshotOfFPUEnvironnementX(var Environnement: TFPUEnvironnementX): Boolean;

/// <summary> Return current Rflags register value.
/// </summary>
function SnapshotOfRFlagsRegister: TRflags;

/// <summary> Return current MXCSR register value.
/// </summary>
function SnapshotOfMXCSRRegister: TMXCSR;

implementation

{$REGION 'AsmCode'}
{ Disable stack frames for all asm code. }
{$STACKFRAMES OFF}

function SnapshotOfRFlagsRegister: TRflags;
asm
  {$IFDEF CPUX86}
  pushfd
  pop     eax
  {$ELSE !CPUX86}
  pushfq
  pop     rax
  {$ENDIF CPUX86}
end;

procedure GetMXCSR(var MXCSR: TMXCSR);
asm
  { Save MXCSR register to MXCSR variable. }
  {$IFDEF CPUX86}
  stmxcsr   dword [eax]
  {$ELSE !CPUX86}
  stmxcsr   dword [rcx]
  {$ENDIF CPUX86}
end;

procedure CopyLegacyRegisters(var Registers: TLegacyRegisters);
asm
  {$IFDEF CPUX86}
  mov dword [eax].TEAXRegister.AsEAX.TLegacyRegisters.&EAX, eax
  mov dword [eax].TECXRegister.AsECX.TLegacyRegisters.&ECX, ecx
  mov dword [eax].TEDXRegister.AsEDX.TLegacyRegisters.&EDX, edx
  mov dword [eax].TEBXRegister.AsEBX.TLegacyRegisters.&EBX, ebx
  mov dword [eax].TESPRegister.AsESP.TLegacyRegisters.&ESP, esp
  mov dword [eax].TEBPRegister.AsEBP.TLegacyRegisters.&EBP, ebp
  mov dword [eax].TESIRegister.AsESI.TLegacyRegisters.&ESI, esi
  mov dword [eax].TEDIRegister.AsEDI.TLegacyRegisters.&EDI, edi
  {$ELSE !CPUX86}
  // Auto generated by internal script.
  mov qword [rcx].TRAXRegister.AsRAX.TLegacyRegisters.&RAX,  rax
  mov qword [rcx].TRCXRegister.AsRCX.TLegacyRegisters.&RCX,  rcx
  mov qword [rcx].TRDXRegister.AsRDX.TLegacyRegisters.&RDX,  rdx
  mov qword [rcx].TRBXRegister.AsRBX.TLegacyRegisters.&RBX,  rbx
  mov qword [rcx].TRSPRegister.AsRSP.TLegacyRegisters.&RSP,  rsp
  mov qword [rcx].TRBPRegister.AsRBP.TLegacyRegisters.&RBP,  rbp
  mov qword [rcx].TRSIRegister.AsRSI.TLegacyRegisters.&RSI,  rsi
  mov qword [rcx].TRDIRegister.AsRDI.TLegacyRegisters.&RDI,  rdi
  mov qword [rcx].TR8Register.AsR8.TLegacyRegisters.&R8,     r8
  mov qword [rcx].TR9Register.AsR9.TLegacyRegisters.&R9,     r9
  mov qword [rcx].TR10Register.AsR10.TLegacyRegisters.&R10,  r10
  mov qword [rcx].TR11Register.AsR11.TLegacyRegisters.&R11,  r11
  mov qword [rcx].TR12Register.AsR12.TLegacyRegisters.&R12,  r12
  mov qword [rcx].TR13Register.AsR13.TLegacyRegisters.&R13,  r13
  mov qword [rcx].TR14Register.AsR14.TLegacyRegisters.&R14,  r14
  mov qword [rcx].TR15Register.AsR15.TLegacyRegisters.&R15,  r15
  {$ENDIF CPUX86}
end;

procedure CopyFPUEnvironnement(var Environnement: TFPUEnvironnement);
asm
  {$IFDEF CPUX86}
  fsave   [eax]
  frstor  [eax]// fsave will initialize fpu !
  {$ELSE !CPUX86}
  fsave   [rcx]
  frstor  [rcx]// fsave will initialize fpu !
  {$ENDIF CPUX86}
end;

procedure CopyFPUEnvironnementX(var Environnement: TFPUEnvironnementX);
asm
  { FXSAVE does not initialize fpu ! }

  { Intel says that fxsave runs faster than the old fsave instruction
  However, it requires memory to be aligned to 16 byte boundary ! }

  {$IFDEF CPUX86}
  fxsave  [eax]
  {$ELSE !CPUX86}
  fxsave  [rcx]
  {$ENDIF CPUX86}
end;

procedure CopyZMMRegisters(var Registers: TVectorRegisters);
asm
  { For the moment there is no cpu commercialised that supports EVEX encoding. }
  nop
end;

procedure CopyXMMRegistersU(var Registers: TVectorRegisters);
asm
  { Copy unaligned memory }
  {$IFDEF CPUX86}

  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM0,xmm0
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM1,xmm1
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM2,xmm2
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM3,xmm3
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM4,xmm4
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM5,xmm5
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM6,xmm6
  movdqu [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM7,xmm7

  {$ELSE !CPUX86}

  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM0,xmm0
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM1,xmm1
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM2,xmm2
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM3,xmm3
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM4,xmm4
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM5,xmm5
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM6,xmm6
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM7,xmm7
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM8,xmm8
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM9,xmm9
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM10,xmm10
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM11,xmm11
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM12,xmm12
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM13,xmm13
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM14,xmm14
  movdqu [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM15,xmm15

  {$ENDIF CPUX86}
end;

procedure CopyXMMRegistersA(var Registers: TVectorRegisters);
asm
  { Copy aligned memory }
  {$IFDEF CPUX86}

  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM0,xmm0
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM1,xmm1
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM2,xmm2
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM3,xmm3
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM4,xmm4
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM5,xmm5
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM6,xmm6
  movdqa [eax].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM7,xmm7

  {$ELSE !CPUX86}

  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM0,xmm0
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM1,xmm1
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM2,xmm2
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM3,xmm3
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM4,xmm4
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM5,xmm5
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM6,xmm6
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM7,xmm7
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM8,xmm8
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM9,xmm9
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM10,xmm10
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM11,xmm11
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM12,xmm12
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM13,xmm13
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM14,xmm14
  movdqa [rcx].TXMMRegister.AsPackedInteger.TVectorRegisters.&XMM15,xmm15

  {$ENDIF CPUX86}
end;

procedure CopyYMMRegistersU(var Registers: TVectorRegisters);
asm
  { Copy unaligned memory }

  { Unfortunately Delphi does not support vex instructions
  => So we need to use raw opcodes. }

  { Do not copy directly ! => Always access memory through ecx/rax first ! }

  { ==> Opcodes extracted from nasm compiler. <== }
  {$IFDEF CPUX86}

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM0
  db $c5 db $fe db $7f db $01	// vmovdqu YMMWORD PTR [ecx],ymm0

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM1
  db $c5 db $fe db $7f db $09	// vmovdqu YMMWORD PTR [ecx],ymm1

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM2
  db $c5 db $fe db $7f db $11	// vmovdqu YMMWORD PTR [ecx],ymm2

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM3
  db $c5 db $fe db $7f db $19	// vmovdqu YMMWORD PTR [ecx],ymm3

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM4
  db $c5 db $fe db $7f db $21	// vmovdqu YMMWORD PTR [ecx],ymm4

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM5
  db $c5 db $fe db $7f db $29	// vmovdqu YMMWORD PTR [ecx],ymm5

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM6
  db $c5 db $fe db $7f db $31	// vmovdqu YMMWORD PTR [ecx],ymm6

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM7
  db $c5 db $fe db $7f db $39	// vmovdqu YMMWORD PTR [ecx],ymm7

  {$ELSE !CPUX86}

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM0
  db $c5 db $fe db $7f db $00 // vmovdqu YMMWORD PTR [rax],ymm0

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM1
  db $c5 db $fe db $7f db $08 // vmovdqu YMMWORD PTR [rax],ymm1

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM2
  db $c5 db $fe db $7f db $10 // vmovdqu YMMWORD PTR [rax],ymm2

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM3
  db $c5 db $fe db $7f db $18 // vmovdqu YMMWORD PTR [rax],ymm3

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM4
  db $c5 db $fe db $7f db $20 // vmovdqu YMMWORD PTR [rax],ymm4

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM5
  db $c5 db $fe db $7f db $28 // vmovdqu YMMWORD PTR [rax],ymm5

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM6
  db $c5 db $fe db $7f db $30 // vmovdqu YMMWORD PTR [rax],ymm6

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM7
  db $c5 db $fe db $7f db $38 // vmovdqu YMMWORD PTR [rax],ymm7

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM8
  db $c5 db $7e db $7f db $00 // vmovdqu YMMWORD PTR [rax],ymm8

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM9
  db $c5 db $7e db $7f db $08 // vmovdqu YMMWORD PTR [rax],ymm9

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM10
  db $c5 db $7e db $7f db $10 // vmovdqu YMMWORD PTR [rax],ymm10

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM11
  db $c5 db $7e db $7f db $18 // vmovdqu YMMWORD PTR [rax],ymm11

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM12
  db $c5 db $7e db $7f db $20 // vmovdqu YMMWORD PTR [rax],ymm12

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM13
  db $c5 db $7e db $7f db $28 // vmovdqu YMMWORD PTR [rax],ymm13

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM14
  db $c5 db $7e db $7f db $30 // vmovdqu YMMWORD PTR [rax],ymm14

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM15
  db $c5 db $7e db $7f db $38 // vmovdqu YMMWORD PTR [rax],ymm15

  {$ENDIF CPUX86}
end;

procedure CopyYMMRegistersA(var Registers: TVectorRegisters);
asm
  { Copy aligned memory }

  { Unfortunately Delphi does not support vex instructions
  => So we need to use raw opcodes. }

  { Do not copy directly ! => Always access memory through ecx/rax first ! }

  { ==> Opcodes extracted from nasm compiler. <== }
  {$IFDEF CPUX86}

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM0
  db $c5 db $fd db $7f db $01	// vmovdqa YMMWORD PTR [ecx],ymm0

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM1
  db $c5 db $fd db $7f db $09	// vmovdqa YMMWORD PTR [ecx],ymm1

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM2
  db $c5 db $fd db $7f db $11	// vmovdqa YMMWORD PTR [ecx],ymm2

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM3
  db $c5 db $fd db $7f db $19	// vmovdqa YMMWORD PTR [ecx],ymm3

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM4
  db $c5 db $fd db $7f db $21	// vmovdqa YMMWORD PTR [ecx],ymm4

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM5
  db $c5 db $fd db $7f db $29	// vmovdqa YMMWORD PTR [ecx],ymm5

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM6
  db $c5 db $fd db $7f db $31	// vmovdqa YMMWORD PTR [ecx],ymm6

  lea ecx,[eax].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM7
  db $c5 db $fd db $7f db $39	// vmovdqa YMMWORD PTR [ecx],ymm7
  {$ELSE !CPUX86}

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM0
  db $c5 db $fd db $7f db $00 // vmovdqa YMMWORD PTR [rax],ymm0

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM1
  db $c5 db $fd db $7f db $08 // vmovdqa YMMWORD PTR [rax],ymm1

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM2
  db $c5 db $fd db $7f db $10 // vmovdqa YMMWORD PTR [rax],ymm2

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM3
  db $c5 db $fd db $7f db $18 // vmovdqa YMMWORD PTR [rax],ymm3

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM4
  db $c5 db $fd db $7f db $20 // vmovdqa YMMWORD PTR [rax],ymm4

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM5
  db $c5 db $fd db $7f db $28 // vmovdqa YMMWORD PTR [rax],ymm5

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM6
  db $c5 db $fd db $7f db $30 // vmovdqa YMMWORD PTR [rax],ymm6

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM7
  db $c5 db $fd db $7f db $38 // vmovdqa YMMWORD PTR [rax],ymm7

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM8
  db $c5 db $7d db $7f db $00 // vmovdqa YMMWORD PTR [rax],ymm8

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM9
  db $c5 db $7d db $7f db $08 // vmovdqa YMMWORD PTR [rax],ymm9

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM10
  db $c5 db $7d db $7f db $10 // vmovdqa YMMWORD PTR [rax],ymm10

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM11
  db $c5 db $7d db $7f db $18 // vmovdqa YMMWORD PTR [rax],ymm11

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM12
  db $c5 db $7d db $7f db $20 // vmovdqa YMMWORD PTR [rax],ymm12

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM13
  db $c5 db $7d db $7f db $28 // vmovdqa YMMWORD PTR [rax],ymm13

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM14
  db $c5 db $7d db $7f db $30 // vmovdqa YMMWORD PTR [rax],ymm14

  lea rax,[rcx].TYMMRegister.AsPackedInteger.TVectorRegisters.&YMM15
  db $c5 db $7d db $7f db $38 // vmovdqa YMMWORD PTR [rax],ymm15
  {$ENDIF CPUX86}
end;

{ Enable stack frames for all pascal code. }
{$STACKFRAMES ON}
{$ENDREGION 'AsmCode'}
{$REGION 'RegistersSnapshot'}

function SnapshotOfMXCSRRegister: TMXCSR;
begin
  GetMXCSR(Result);
end;

function SnapshotOfVectorRegistersZ(var Registers: TVectorRegisters): Boolean;
begin
  // See CopyZMMRegisters.
  Result := False;
end;

function SnapshotOfVectorRegistersY(var Registers: TVectorRegisters): Boolean;
begin
  try
    if (NativeUInt(@Registers) mod SizeOfYMMRegister = 0) then
      CopyYMMRegistersA(Registers) // Memory is aligned.
    else
      CopyYMMRegistersU(Registers); // Memory not aligned.
  except
    Exit(False);
  end;
  Result := True;
end;

function SnapshotOfVectorRegistersX(var Registers: TVectorRegisters): Boolean;
begin
  try
    if (NativeUInt(@Registers) mod SizeOfXMMRegister = 0) then
      CopyXMMRegistersA(Registers) // Memory is aligned.
    else
      CopyXMMRegistersU(Registers); // Memory not aligned.
  except
    Exit(False);
  end;
  Result := True;
end;

function SnapshotOfLegacyRegisters(var Registers: TLegacyRegisters): Boolean;
begin
  try
    CopyLegacyRegisters(Registers);
  except
    Exit(False);
  end;
  Result := True;
end;

function SnapshotOfFPURegisters(var Registers: TFPURegisters): Boolean;
var
  Environnement: TFPUEnvironnement;
begin
  Result := SnapshotOfFPUEnvironnement(Environnement);
  if Result then
    Move(Pointer(@Environnement.FPUStack[0])^, Registers, SizeOf(Registers));
end;

function SnapshotOfMMXRegisters(var Registers: TMMXRegisters): Boolean;
var
  Environnement: TFPUEnvironnement;
begin
  Result := SnapshotOfFPUEnvironnement(Environnement);
  if Result then
    Move(Pointer(@Environnement.FPUStack[0])^, Registers, SizeOf(Registers));
end;

function SnapshotOfFPUEnvironnement(var Environnement: TFPUEnvironnement): Boolean;
begin
  try
    CopyFPUEnvironnement(Environnement);
  except
    Exit(False);
  end;
  Result := True;
end;

function SnapshotOfFPUEnvironnementX(var Environnement: TFPUEnvironnementX): Boolean;
var
  P: PByte;
  Q: PByte;
begin
  Result := True;
  P := nil;
  Q := nil;
  { Memory must be aligned. If not => #GP exception ! }

  if NativeUInt(@Environnement) mod 16 <> 00 then
  begin
    { If we are here that means memory isn't aligned correctly,
      So we use a custom aligned memory ... after getting fpu environnement
      we copy this custom memory to the Environnement variable and finally
      we free it. }
    GetMem(P, SizeOf(Environnement) + 16);
    Q := P;
    P := Pointer(((NativeUInt(P) + 15) div 16) * 16); // Align memory to 16 byte boundary.
  end;
  try
    try
      if Assigned(P) then
      begin
        CopyFPUEnvironnementX(PFPUEnvironnementX(P)^);
        Environnement := PFPUEnvironnementX(P)^;
      end
      else
        // User provided aligned environnement.
        CopyFPUEnvironnementX(Environnement);
    except
      Result := False;
    end;
  finally
    if Assigned(Q) then
      FreeMem(Q);
  end;
end;

function SnapshotOfVectorRegisters(var Registers: TVectorRegisters): Boolean;
begin
  FillChar(Registers, SizeOf(TVectorRegisters), #00);
  if SnapshotOfVectorRegistersZ(Registers) then
    Exit(True);
  if SnapshotOfVectorRegistersY(Registers) then
    Exit(True);
  Result := SnapshotOfVectorRegistersX(Registers);
end;

{$ENDREGION 'RegistersSnapshot'}
{$REGION 'RegistersHlp'}

{ TRFlagsHlp }
function TRFlagsHlp.GetCF: Boolean;
begin
  Result := (Self and $000001 <> $00);
end;

procedure TRFlagsHlp.SetCF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFFFE)) or (NativeUInt(Value) shl $00);
end;

function TRFlagsHlp.GetPF: Boolean;
begin
  Result := (Self and $000004 <> $00);
end;

procedure TRFlagsHlp.SetPF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFFFB)) or (NativeUInt(Value) shl $02);
end;

function TRFlagsHlp.GetAF: Boolean;
begin
  Result := (Self and $000010 <> $00);
end;

procedure TRFlagsHlp.SetAF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFFEF)) or (NativeUInt(Value) shl $04);
end;

function TRFlagsHlp.GetZF: Boolean;
begin
  Result := (Self and $000040 <> $00);
end;

procedure TRFlagsHlp.SetZF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFFBF)) or (NativeUInt(Value) shl $06);
end;

function TRFlagsHlp.GetSF: Boolean;
begin
  Result := (Self and $000080 <> $00);
end;

procedure TRFlagsHlp.SetSF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFF7F)) or (NativeUInt(Value) shl $07);
end;

function TRFlagsHlp.GetTF: Boolean;
begin
  Result := (Self and $000100 <> $00);
end;

procedure TRFlagsHlp.SetTF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFEFF)) or (NativeUInt(Value) shl $08);
end;

function TRFlagsHlp.GetIF: Boolean;
begin
  Result := (Self and $000200 <> $00);
end;

procedure TRFlagsHlp.SetIF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFDFF)) or (NativeUInt(Value) shl $09);
end;

function TRFlagsHlp.GetDF: Boolean;
begin
  Result := (Self and $000400 <> $00);
end;

procedure TRFlagsHlp.SetDF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFFBFF)) or (NativeUInt(Value) shl $0A);
end;

function TRFlagsHlp.GetOF: Boolean;
begin
  Result := (Self and $000800 <> $00);
end;

procedure TRFlagsHlp.SetOF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFF7FF)) or (NativeUInt(Value) shl $0B);
end;

function TRFlagsHlp.GetNT: Boolean;
begin
  Result := (Self and $004000 <> $00);
end;

procedure TRFlagsHlp.SetNT(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFBFFF)) or (NativeUInt(Value) shl $0E);
end;

function TRFlagsHlp.GetRF: Boolean;
begin
  Result := (Self and $010000 <> $00);
end;

procedure TRFlagsHlp.SetRF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFEFFFF)) or (NativeUInt(Value) shl $10);
end;

function TRFlagsHlp.GetVM: Boolean;
begin
  Result := (Self and $020000 <> $00);
end;

procedure TRFlagsHlp.SetVM(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFDFFFF)) or (NativeUInt(Value) shl $11);
end;

function TRFlagsHlp.GetAC: Boolean;
begin
  Result := (Self and $040000 <> $00);
end;

procedure TRFlagsHlp.SetAC(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFBFFFF)) or (NativeUInt(Value) shl $12);
end;

function TRFlagsHlp.GetVIF: Boolean;
begin
  Result := (Self and $080000 <> $00);
end;

procedure TRFlagsHlp.SetVIF(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFF7FFFF)) or (NativeUInt(Value) shl $13);
end;

function TRFlagsHlp.GetVIP: Boolean;
begin
  Result := (Self and $100000 <> $00);
end;

procedure TRFlagsHlp.SetVIP(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFEFFFFF)) or (NativeUInt(Value) shl $14);
end;

function TRFlagsHlp.GetID: Boolean;
begin
  Result := (Self and $200000 <> $00);
end;

procedure TRFlagsHlp.SetID(Value: Boolean);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFDFFFFF)) or (NativeUInt(Value) shl $15);
end;

function TRFlagsHlp.GetIOPL: ShortInt;
begin
  Result := (Self and $003000) shr $0C;
end;

procedure TRFlagsHlp.SetIOPL(Value: ShortInt);
begin
  Self := (Self and NativeUInt($FFFFFFFFFFFFCFFF)) or (NativeUInt(Value and $03) shl $0C);
end;

{ TMXCSRHlp }

function TMXCSRHlp.GetIE: Boolean;
begin
  Result := (Self and $000001 <> $00);
end;

procedure TMXCSRHlp.SetIE(Value: Boolean);
begin
  Self := (Self and $FFFFFFFE) or (Cardinal(Value) shl $00);
end;

function TMXCSRHlp.GetDE: Boolean;
begin
  Result := (Self and $000002 <> $00);
end;

procedure TMXCSRHlp.SetDE(Value: Boolean);
begin
  Self := (Self and $FFFFFFFD) or (Cardinal(Value) shl $01);
end;

function TMXCSRHlp.GetZE: Boolean;
begin
  Result := (Self and $000004 <> $00);
end;

procedure TMXCSRHlp.SetZE(Value: Boolean);
begin
  Self := (Self and $FFFFFFFB) or (Cardinal(Value) shl $02);
end;

function TMXCSRHlp.GetOE: Boolean;
begin
  Result := (Self and $000008 <> $00);
end;

procedure TMXCSRHlp.SetOE(Value: Boolean);
begin
  Self := (Self and $FFFFFFF7) or (Cardinal(Value) shl $03);
end;

function TMXCSRHlp.GetUE: Boolean;
begin
  Result := (Self and $000010 <> $00);
end;

procedure TMXCSRHlp.SetUE(Value: Boolean);
begin
  Self := (Self and $FFFFFFEF) or (Cardinal(Value) shl $04);
end;

function TMXCSRHlp.GetPE: Boolean;
begin
  Result := (Self and $000020 <> $00);
end;

procedure TMXCSRHlp.SetPE(Value: Boolean);
begin
  Self := (Self and $FFFFFFDF) or (Cardinal(Value) shl $05);
end;

function TMXCSRHlp.GetDAZ: Boolean;
begin
  Result := (Self and $000040 <> $00);
end;

procedure TMXCSRHlp.SetDAZ(Value: Boolean);
begin
  Self := (Self and $FFFFFFBF) or (Cardinal(Value) shl $06);
end;

function TMXCSRHlp.GetIM: Boolean;
begin
  Result := (Self and $000080 <> $00);
end;

procedure TMXCSRHlp.SetIM(Value: Boolean);
begin
  Self := (Self and $FFFFFF7F) or (Cardinal(Value) shl $07);
end;

function TMXCSRHlp.GetDM: Boolean;
begin
  Result := (Self and $000100 <> $00);
end;

procedure TMXCSRHlp.SetDM(Value: Boolean);
begin
  Self := (Self and $FFFFFEFF) or (Cardinal(Value) shl $08);
end;

function TMXCSRHlp.GetZM: Boolean;
begin
  Result := (Self and $000200 <> $00);
end;

procedure TMXCSRHlp.SetZM(Value: Boolean);
begin
  Self := (Self and $FFFFFDFF) or (Cardinal(Value) shl $09);
end;

function TMXCSRHlp.GetOM: Boolean;
begin
  Result := (Self and $000400 <> $00);
end;

procedure TMXCSRHlp.SetOM(Value: Boolean);
begin
  Self := (Self and $FFFFFBFF) or (Cardinal(Value) shl $0A);
end;

function TMXCSRHlp.GetUM: Boolean;
begin
  Result := (Self and $000800 <> $00);
end;

procedure TMXCSRHlp.SetUM(Value: Boolean);
begin
  Self := (Self and $FFFFF7FF) or (Cardinal(Value) shl $0B);
end;

function TMXCSRHlp.GetPM: Boolean;
begin
  Result := (Self and $001000 <> $00);
end;

procedure TMXCSRHlp.SetPM(Value: Boolean);
begin
  Self := (Self and $FFFFEFFF) or (Cardinal(Value) shl $0C);
end;

function TMXCSRHlp.GetFZ: Boolean;
begin
  Result := (Self and $008000 <> $00);
end;

procedure TMXCSRHlp.SetFZ(Value: Boolean);
begin
  Self := (Self and $FFFF7FFF) or (Cardinal(Value) shl $0F);
end;

function TMXCSRHlp.GetRC: ShortInt;
begin
  Result := (Self and $006000) shr $0D;
end;

procedure TMXCSRHlp.SetRC(Value: ShortInt);
begin
  Self := (Self and $FFFF9FFF) or (Cardinal(Value and $03) shl $0D);
end;

{$ENDREGION 'RegistersHlp'}

initialization

{ If one of this test fails ... we will have a serious problem ! }
Assert(SizeOf(TLegacyRegisters) = NumberOfLegacyRegisters * SizeOfLegacyRegister);
Assert(SizeOf(TXMMRegister) = SizeOfXMMRegister);
Assert(SizeOf(TYMMRegister) = SizeOfYMMRegister);
Assert(SizeOf(TZMMRegister) = SizeOfZMMRegister);
Assert(SizeOf(TFPUEnvironnement) = SizeOfFPUEnvironnement);
Assert(SizeOf(TFPUEnvironnementX) = SizeOfFPUEnvironnementX);
Assert(SizeOf(TVectorRegisters) = NumberOfVectorRegisters * SizeOfZMMRegister);

end.

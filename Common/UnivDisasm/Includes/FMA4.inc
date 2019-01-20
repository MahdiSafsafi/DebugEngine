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
// The Original Code is FMA4.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_FMA4_VPERMILzz2PS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VPERMILzz2PS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vx_Hx_Lx_Wx(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPERMILzz2PS_Array[PInst^.OpImmExt and $03];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPERMILzz2PS_Array[PInst^.OpImmExt and $03]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VPERMILzz2PS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VPERMILzz2PS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vx_Hx_Wx_Lx(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPERMILzz2PS_Array[PInst^.OpImmExt and $03];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPERMILzz2PS_Array[PInst^.OpImmExt and $03]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VPERMILzz2PD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VPERMILzz2PD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vx_Hx_Lx_Wx(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPERMILzz2PD_Array[PInst^.OpImmExt and $03];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPERMILzz2PD_Array[PInst^.OpImmExt and $03]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VPERMILzz2PD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VPERMILzz2PD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vx_Hx_Wx_Lx(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPERMILzz2PD_Array[PInst^.OpImmExt and $03];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPERMILzz2PD_Array[PInst^.OpImmExt and $03]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSUBPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMADDSUBPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUBPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSUBPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMADDSUBPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUBPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSUBPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMADDSUBPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUBPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSUBPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMADDSUBPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUBPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBADDPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMSUBADDPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADDPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBADDPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMSUBADDPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADDPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBADDPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMSUBADDPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADDPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBADDPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMSUBADDPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADDPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMADDPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMADDPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMADDPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMADDPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSS_Vo_Ho_Lo_Wd(PInst: PInstruction);
begin
  // VFMADDSS  Vo,Ho,Lo,Wd
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSS;
  Decode_Vo_Ho_Lo_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSS_Vo_Ho_Wd_Lo(PInst: PInstruction);
begin
  // VFMADDSS  Vo,Ho,Wd,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSS;
  Decode_Vo_Ho_Wd_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSD_Vo_Ho_Lo_Wq(PInst: PInstruction);
begin
  // VFMADDSD  Vo,Ho,Lo,Wq
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSD;
  Decode_Vo_Ho_Lo_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMADDSD_Vo_Ho_Wq_Lo(PInst: PInstruction);
begin
  // VFMADDSD  Vo,Ho,Wq,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSD;
  Decode_Vo_Ho_Wq_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMSUBPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMSUBPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFMSUBPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFMSUBPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBSS_Vo_Ho_Lo_Wd(PInst: PInstruction);
begin
  // VFMSUBSS  Vo,Ho,Lo,Wd
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBSS;
  Decode_Vo_Ho_Lo_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBSS_Vo_Ho_Wd_Lo(PInst: PInstruction);
begin
  // VFMSUBSS  Vo,Ho,Wd,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBSS;
  Decode_Vo_Ho_Wd_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBSD_Vo_Ho_Lo_Wq(PInst: PInstruction);
begin
  // VFMSUBSD  Vo,Ho,Lo,Wq
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBSD;
  Decode_Vo_Ho_Lo_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFMSUBSD_Vo_Ho_Wq_Lo(PInst: PInstruction);
begin
  // VFMSUBSD  Vo,Ho,Wq,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBSD;
  Decode_Vo_Ho_Wq_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFNMADDPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFNMADDPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFNMADDPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFNMADDPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDSS_Vo_Ho_Lo_Wd(PInst: PInstruction);
begin
  // VFNMADDSS  Vo,Ho,Lo,Wd
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDSS;
  Decode_Vo_Ho_Lo_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDSS_Vo_Ho_Wd_Lo(PInst: PInstruction);
begin
  // VFNMADDSS  Vo,Ho,Wd,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDSS;
  Decode_Vo_Ho_Wd_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDSD_Vo_Ho_Lo_Wq(PInst: PInstruction);
begin
  // VFNMADDSD  Vo,Ho,Lo,Wq
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDSD;
  Decode_Vo_Ho_Lo_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMADDSD_Vo_Ho_Wq_Lo(PInst: PInstruction);
begin
  // VFNMADDSD  Vo,Ho,Wq,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADDSD;
  Decode_Vo_Ho_Wq_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBPS_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFNMSUBPS  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBPS;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFNMSUBPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBPD_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  // VFNMSUBPD  Vx,Hx,Lx,Wx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBPD;
  Decode_Vx_Hx_Lx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VFNMSUBPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBSS_Vo_Ho_Lo_Wd(PInst: PInstruction);
begin
  // VFNMSUBSS  Vo,Ho,Lo,Wd
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBSS;
  Decode_Vo_Ho_Lo_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBSS_Vo_Ho_Wd_Lo(PInst: PInstruction);
begin
  // VFNMSUBSS  Vo,Ho,Wd,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBSS;
  Decode_Vo_Ho_Wd_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBSD_Vo_Ho_Lo_Wq(PInst: PInstruction);
begin
  // VFNMSUBSD  Vo,Ho,Lo,Wq
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBSD;
  Decode_Vo_Ho_Lo_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA4_VFNMSUBSD_Vo_Ho_Wq_Lo(PInst: PInstruction);
begin
  // VFNMSUBSD  Vo,Ho,Wq,Lo
  PInst^.InstGroups := INST_GRP_FMA4;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUBSD;
  Decode_Vo_Ho_Wq_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

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
// The Original Code is AVX.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX_VPSHUFB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSHUFB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSHUFB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHADDW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHADDW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHADDD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHADDD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHADDSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHADDSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMADDUBSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMADDUBSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMADDUBSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHSUBW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHSUBW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHSUBD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHSUBD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHSUBSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPHSUBSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSIGNB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSIGNB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGNB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSIGNW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSIGNW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGNW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGNW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSIGND_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSIGND  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGND;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULHRSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULHRSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHRSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHRSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPERMILPS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPERMILPS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMILPS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMILPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPERMILPD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPERMILPD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMILPD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMILPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VTESTPS_Vx_Wx(PInst: PInstruction);
begin
  // VTESTPS  Vx,Wx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VTESTPS;
  Decode_Vx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VTESTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VTESTPD_Vx_Wx(PInst: PInstruction);
begin
  // VTESTPD  Vx,Wx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VTESTPD;
  Decode_Vx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VTESTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPTEST_Vo_Wo(PInst: PInstruction);
begin
  // VPTEST  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPTEST;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPTEST_Vy_Wy(PInst: PInstruction);
begin
  // VPTEST  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPTEST;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBROADCASTSS_Vx_Mo_d(PInst: PInstruction);
begin
  // VBROADCASTSS  Vx,Mo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTSS;
  Decode_Vx_Mo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBROADCASTSD_Vy_Mo_q(PInst: PInstruction);
begin
  // VBROADCASTSD  Vy,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTSD;
  Decode_Vy_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBROADCASTF128_Vy_Mo(PInst: PInstruction);
begin
  // VBROADCASTF128  Vy,Mo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTF128;
  Decode_Vy_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTF128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPABSB_Vo_Wo(PInst: PInstruction);
begin
  // VPABSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPABSW_Vo_Wo(PInst: PInstruction);
begin
  // VPABSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPABSD_Vo_Wo(PInst: PInstruction);
begin
  // VPABSD  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXBW_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVSXBW  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBW;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXBD_Vo_Wo_d(PInst: PInstruction);
begin
  // VPMOVSXBD  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBD;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXBQ_Vo_Wo_w(PInst: PInstruction);
begin
  // VPMOVSXBQ  Vo,Wo.w
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBQ;
  Decode_Vo_Wo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXWD_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVSXWD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXWD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXWQ_Vo_Wo_d(PInst: PInstruction);
begin
  // VPMOVSXWQ  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXWQ;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVSXDQ_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVSXDQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXDQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPEQQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPEQQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTDQA_Vo_Mo(PInst: PInstruction);
begin
  // VMOVNTDQA  Vo,Mo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTDQA;
  Decode_Vo_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPACKUSDW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPACKUSDW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKUSDW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMASKMOVPS_Vx_Hx_Mx(PInst: PInstruction);
begin
  // VMASKMOVPS  Vx,Hx,Mx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMASKMOVPS;
  Decode_Vx_Hx_Mx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMASKMOVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMASKMOVPD_Vx_Hx_Mx(PInst: PInstruction);
begin
  // VMASKMOVPD  Vx,Hx,Mx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMASKMOVPD;
  Decode_Vx_Hx_Mx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMASKMOVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMASKMOVPS_Mx_Hx_Vx(PInst: PInstruction);
begin
  // VMASKMOVPS  Mx,Hx,Vx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMASKMOVPS;
  Decode_Mx_Hx_Vx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMASKMOVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMASKMOVPD_Mx_Hx_Vx(PInst: PInstruction);
begin
  // VMASKMOVPD  Mx,Hx,Vx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMASKMOVPD;
  Decode_Mx_Hx_Vx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMASKMOVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXBW_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVZXBW  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBW;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXBD_Vo_Wo_d(PInst: PInstruction);
begin
  // VPMOVZXBD  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBD;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXBQ_Vo_Wo_w(PInst: PInstruction);
begin
  // VPMOVZXBQ  Vo,Wo.w
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBQ;
  Decode_Vo_Wo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXWD_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVZXWD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXWD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXWQ_Vo_Wo_d(PInst: PInstruction);
begin
  // VPMOVZXWQ  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXWQ;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVZXDQ_Vo_Wo_q(PInst: PInstruction);
begin
  // VPMOVZXDQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXDQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPGTQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPGTQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPGTQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPGTQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINSD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINSD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINUW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINUW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINUD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINUD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXSD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXSD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXUW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXUW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXUD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXUD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULLD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULLD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULLD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPHMINPOSUW_Vo_Wo(PInst: PInstruction);
begin
  // VPHMINPOSUW  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHMINPOSUW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHMINPOSUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESIMC_Vo_Wo(PInst: PInstruction);
begin
  // VAESIMC  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESIMC;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESIMC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESENC_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VAESENC  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESENC;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESENC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESENCLAST_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VAESENCLAST  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESENCLAST;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESENCLAST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESDEC_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VAESDEC  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESDEC;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESDEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESDECLAST_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VAESDECLAST  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESDECLAST;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESDECLAST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPERMILPS_Vx_Wx_Ib(PInst: PInstruction);
begin
  // VPERMILPS  Vx,Wx,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMILPS;
  Decode_Vx_Wx_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMILPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPERMILPD_Vx_Wx_Ib(PInst: PInstruction);
begin
  // VPERMILPD  Vx,Wx,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMILPD;
  Decode_Vx_Wx_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMILPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPERM2F128_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VPERM2F128  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERM2F128;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERM2F128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDPS_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VROUNDPS  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDPS;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDPS_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VROUNDPS  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDPS;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDPD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VROUNDPD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDPD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDPD_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VROUNDPD  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDPD;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDSS_Vo_Ho_Wo_d_Ib(PInst: PInstruction);
begin
  // VROUNDSS  Vo,Ho,Wo.d,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDSS;
  Decode_Vo_Ho_Wo_d_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VROUNDSD_Vo_Ho_Wo_q_Ib(PInst: PInstruction);
begin
  // VROUNDSD  Vo,Ho,Wo.q,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VROUNDSD;
  Decode_Vo_Ho_Wo_q_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VROUNDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDPS_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VBLENDPS  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDPS;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDPS_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VBLENDPS  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDPS;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDPD_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VBLENDPD  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDPD;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDPD_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VBLENDPD  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDPD;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPBLENDW_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VPBLENDW  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBLENDW;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPALIGNR_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VPALIGNR  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPALIGNR;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPALIGNR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRB_Mb_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRB  Mb,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRB;
  Decode_Mb_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRB_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRB  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRB;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRW_Mw_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Mw,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Mw_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRW_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRQ_Eq_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRQ  Eq,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRQ;
  Decode_Eq_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRD_Ed_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRD  Ed,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRD;
  Decode_Ed_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VEXTRACTPS_Md_Vo_Ib(PInst: PInstruction);
begin
  // VEXTRACTPS  Md,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VEXTRACTPS;
  Decode_Md_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VEXTRACTPS_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VEXTRACTPS  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VEXTRACTPS;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VINSERTF128_Vy_Hy_Wo_Ib(PInst: PInstruction);
begin
  // VINSERTF128  Vy,Hy,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VINSERTF128;
  Decode_Vy_Hy_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTF128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VEXTRACTF128_Wo_Vy_Ib(PInst: PInstruction);
begin
  // VEXTRACTF128  Wo,Vy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VEXTRACTF128;
  Decode_Wo_Vy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTF128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRB_Vo_Ho_Mb_Ib(PInst: PInstruction);
begin
  // VPINSRB  Vo,Ho,Mb,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRB;
  Decode_Vo_Ho_Mb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRB_Vo_Ho_Rd_Ib(PInst: PInstruction);
begin
  // VPINSRB  Vo,Ho,Rd,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRB;
  Decode_Vo_Ho_Rd_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VINSERTPS_Vo_Ho_Md_Ib(PInst: PInstruction);
begin
  // VINSERTPS  Vo,Ho,Md,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VINSERTPS;
  Decode_Vo_Ho_Md_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VINSERTPS_Vo_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VINSERTPS  Vo,Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VINSERTPS;
  Decode_Vo_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRQ_Vo_Ho_Eq_Ib(PInst: PInstruction);
begin
  // VPINSRQ  Vo,Ho,Eq,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRQ;
  Decode_Vo_Ho_Eq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRD_Vo_Ho_Ed_Ib(PInst: PInstruction);
begin
  // VPINSRD  Vo,Ho,Ed,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRD;
  Decode_Vo_Ho_Ed_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDPPS_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VDPPS  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VDPPS;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDPPS_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VDPPS  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VDPPS;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDPPD_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VDPPD  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VDPPD;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDPPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMPSADBW_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VMPSADBW  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMPSADBW;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCLMULQDQ_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VPCLMULQDQ  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCLMULQDQ;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCLMULQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDVPS_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VBLENDVPS  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDVPS;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VBLENDVPD_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  // VBLENDVPD  Vx,Hx,Wx,Lx
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBLENDVPD;
  Decode_Vx_Hx_Wx_Lx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBLENDVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPBLENDVB_Vo_Ho_Wo_Lo(PInst: PInstruction);
begin
  // VPBLENDVB  Vo,Ho,Wo,Lo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBLENDVB;
  Decode_Vo_Ho_Wo_Lo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPESTRM_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPCMPESTRM  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPESTRM;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPESTRM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPESTRI_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPCMPESTRI  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPESTRI;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPESTRI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPISTRM_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPCMPISTRM  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPISTRM;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPISTRM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPISTRI_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPCMPISTRI  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPISTRI;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPISTRI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VAESKEYGENASSIST_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VAESKEYGENASSIST  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VAESKEYGENASSIST;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VAESKEYGENASSIST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPD_Vo_Wo(PInst: PInstruction);
begin
  // VMOVUPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVUPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPS_Vo_Wo(PInst: PInstruction);
begin
  // VMOVUPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVUPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPD_Vy_Wy(PInst: PInstruction);
begin
  // VMOVUPD  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVUPD;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPS_Vy_Wy(PInst: PInstruction);
begin
  // VMOVUPS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVUPS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSD_Vo_Mo_q(PInst: PInstruction);
begin
  // VMOVSD  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSD_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVSD  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSS_Vo_Mo_d(PInst: PInstruction);
begin
  // VMOVSS  Vo,Mo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Vo_Mo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSS_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVSS  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPD_Wo_Vo(PInst: PInstruction);
begin
  // VMOVUPD  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVUPD;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPS_Wo_Vo(PInst: PInstruction);
begin
  // VMOVUPS  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVUPS;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPD_Wy_Vy(PInst: PInstruction);
begin
  // VMOVUPD  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVUPD;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVUPS_Wy_Vy(PInst: PInstruction);
begin
  // VMOVUPS  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVUPS;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSD_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVSD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSD_Uo_Ho_Vo(PInst: PInstruction);
begin
  // VMOVSD  Uo,Ho,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Uo_Ho_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSS_Mo_d_Vo(PInst: PInstruction);
begin
  // VMOVSS  Mo.d,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Mo_d_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSS_Uo_Ho_Vo(PInst: PInstruction);
begin
  // VMOVSS  Uo,Ho,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Uo_Ho_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDDUP_Vo_Wo_q(PInst: PInstruction);
begin
  // VMOVDDUP  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVDDUP;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSLDUP_Vo_Wo(PInst: PInstruction);
begin
  // VMOVSLDUP  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSLDUP;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSLDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDDUP_Vy_Wy(PInst: PInstruction);
begin
  // VMOVDDUP  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMOVDDUP;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSLDUP_Vy_Wy(PInst: PInstruction);
begin
  // VMOVSLDUP  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSLDUP;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSLDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVLPS_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVLPS  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVLPS;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVHLPS_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVHLPS  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVHLPS;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVLPD_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVLPD  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVLPD;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVLPD_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVLPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVLPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVLPS_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVLPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVLPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKLPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VUNPCKLPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VUNPCKLPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKLPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VUNPCKLPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VUNPCKLPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKLPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VUNPCKLPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VUNPCKLPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKLPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VUNPCKLPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VUNPCKLPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKHPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VUNPCKHPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VUNPCKHPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKHPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VUNPCKHPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VUNPCKHPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKHPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VUNPCKHPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VUNPCKHPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUNPCKHPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VUNPCKHPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VUNPCKHPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUNPCKHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSHDUP_Vo_Wo(PInst: PInstruction);
begin
  // VMOVSHDUP  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSHDUP;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSHDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVSHDUP_Vy_Wy(PInst: PInstruction);
begin
  // VMOVSHDUP  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVSHDUP;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSHDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVHPS_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVHPS  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVHPS;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVLHPS_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVLHPS  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVLHPS;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVHPD_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVHPD  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVHPD;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVHPD_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVHPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVHPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVHPS_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVHPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVHPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPD_Vo_Wo(PInst: PInstruction);
begin
  // VMOVAPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVAPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPS_Vo_Wo(PInst: PInstruction);
begin
  // VMOVAPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVAPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPD_Vy_Wy(PInst: PInstruction);
begin
  // VMOVAPD  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVAPD;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPS_Vy_Wy(PInst: PInstruction);
begin
  // VMOVAPS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVAPS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPD_Wo_Vo(PInst: PInstruction);
begin
  // VMOVAPD  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVAPD;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPS_Wo_Vo(PInst: PInstruction);
begin
  // VMOVAPS  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVAPS;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPD_Wy_Vy(PInst: PInstruction);
begin
  // VMOVAPD  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVAPD;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVAPS_Wy_Vy(PInst: PInstruction);
begin
  // VMOVAPS  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVAPS;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSI2SD_Vo_Ho_Ey(PInst: PInstruction);
begin
  // VCVTSI2SD  Vo,Ho,Ey
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTSI2SD;
  Decode_Vo_Ho_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSI2SS_Vo_Ho_Ey(PInst: PInstruction);
begin
  // VCVTSI2SS  Vo,Ho,Ey
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTSI2SS;
  Decode_Vo_Ho_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTPD_Mo_Vo(PInst: PInstruction);
begin
  // VMOVNTPD  Mo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTPD;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTPS_Mo_Vo(PInst: PInstruction);
begin
  // VMOVNTPS  Mo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVNTPS;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTPD_My_Vy(PInst: PInstruction);
begin
  // VMOVNTPD  My,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTPD;
  Decode_My_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTPS_My_Vy(PInst: PInstruction);
begin
  // VMOVNTPS  My,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVNTPS;
  Decode_My_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTSD2SI_Gy_Wo_q(PInst: PInstruction);
begin
  // VCVTTSD2SI  Gy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTTSD2SI;
  Decode_Gy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTSS2SI_Gy_Wo_d(PInst: PInstruction);
begin
  // VCVTTSS2SI  Gy,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTTSS2SI;
  Decode_Gy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSD2SI_Gy_Wo_q(PInst: PInstruction);
begin
  // VCVTSD2SI  Gy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTSD2SI;
  Decode_Gy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSS2SI_Gy_Wo_d(PInst: PInstruction);
begin
  // VCVTSS2SI  Gy,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTSS2SI;
  Decode_Gy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUCOMISD_Vo_Wo_q(PInst: PInstruction);
begin
  // VUCOMISD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VUCOMISD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUCOMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VUCOMISS_Vo_Wo_d(PInst: PInstruction);
begin
  // VUCOMISS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VUCOMISS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUCOMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCOMISD_Vo_Wo_q(PInst: PInstruction);
begin
  // VCOMISD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCOMISD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCOMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCOMISS_Vo_Wo_d(PInst: PInstruction);
begin
  // VCOMISS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VCOMISS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCOMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVMSKPD_Gy_Uo(PInst: PInstruction);
begin
  // VMOVMSKPD  Gy,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVMSKPD;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVMSKPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVMSKPS_Gy_Uo(PInst: PInstruction);
begin
  // VMOVMSKPS  Gy,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVMSKPS;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVMSKPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVMSKPD_Gy_Uy(PInst: PInstruction);
begin
  // VMOVMSKPD  Gy,Uy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVMSKPD;
  Decode_Gy_Uy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVMSKPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVMSKPS_Gy_Uy(PInst: PInstruction);
begin
  // VMOVMSKPS  Gy,Uy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMOVMSKPS;
  Decode_Gy_Uy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVMSKPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTPD_Vo_Wo(PInst: PInstruction);
begin
  // VSQRTPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSQRTPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTPS_Vo_Wo(PInst: PInstruction);
begin
  // VSQRTPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSQRTPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTPD_Vy_Wy(PInst: PInstruction);
begin
  // VSQRTPD  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSQRTPD;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTPS_Vy_Wy(PInst: PInstruction);
begin
  // VSQRTPS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSQRTPS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VSQRTSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VSQRTSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSQRTSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VSQRTSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VSQRTSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRSQRTPS_Vo_Wo(PInst: PInstruction);
begin
  // VRSQRTPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VRSQRTPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRSQRTPS_Vy_Wy(PInst: PInstruction);
begin
  // VRSQRTPS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VRSQRTPS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRSQRTSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VRSQRTSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VRSQRTSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRCPPS_Vo_Wo(PInst: PInstruction);
begin
  // VRCPPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VRCPPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRCPPS_Vy_Wy(PInst: PInstruction);
begin
  // VRCPPS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VRCPPS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VRCPSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VRCPSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VRCPSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCPSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VANDPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VANDPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VANDPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VANDPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VANDPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VANDPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VANDPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VANDPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDNPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VANDNPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VANDNPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDNPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VANDNPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VANDNPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDNPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VANDNPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VANDNPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VANDNPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VANDNPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VANDNPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VORPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VORPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VORPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VORPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VORPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VORPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VORPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VORPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VORPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VORPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VORPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VORPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VXORPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VXORPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VXORPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VXORPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VXORPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VXORPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VXORPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VXORPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VXORPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VXORPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VXORPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VXORPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VADDPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VADDPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VADDPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VADDPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VADDPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VADDPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VADDPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VADDPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VADDSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VADDSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VADDSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VADDSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMULPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMULPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMULPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMULPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMULPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMULPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMULPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMULPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VMULSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMULSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMULSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VMULSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMULSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPD2PS_Vo_Wo(PInst: PInstruction);
begin
  // VCVTPD2PS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPD2PS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPS2PD_Vo_Wo_q(PInst: PInstruction);
begin
  // VCVTPS2PD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VCVTPS2PD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPD2PS_Vo_Wy(PInst: PInstruction);
begin
  // VCVTPD2PS  Vo,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPD2PS;
  Decode_Vo_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPS2PD_Vy_Wo(PInst: PInstruction);
begin
  // VCVTPS2PD  Vy,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VCVTPS2PD;
  Decode_Vy_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSD2SS_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VCVTSD2SS  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTSD2SS;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTSS2SD_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VCVTSS2SD  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTSS2SD;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPS2DQ_Vo_Wo(PInst: PInstruction);
begin
  // VCVTPS2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPS2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTPS2DQ_Vo_Wo(PInst: PInstruction);
begin
  // VCVTTPS2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTTPS2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTDQ2PS_Vo_Wo(PInst: PInstruction);
begin
  // VCVTDQ2PS  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VCVTDQ2PS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTDQ2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPS2DQ_Vy_Wy(PInst: PInstruction);
begin
  // VCVTPS2DQ  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPS2DQ;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTPS2DQ_Vy_Wy(PInst: PInstruction);
begin
  // VCVTTPS2DQ  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTTPS2DQ;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTDQ2PS_Vy_Wy(PInst: PInstruction);
begin
  // VCVTDQ2PS  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VCVTDQ2PS;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTDQ2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VSUBPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSUBPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VSUBPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSUBPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VSUBPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSUBPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VSUBPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSUBPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VSUBSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VSUBSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSUBSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VSUBSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VSUBSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMINPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMINPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMINPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMINPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMINPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMINPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMINPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMINPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VMINSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMINSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMINSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VMINSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMINSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VDIVPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VDIVPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VDIVPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VDIVPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VDIVPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VDIVPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VDIVPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VDIVPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VDIVSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VDIVSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VDIVSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VDIVSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VDIVSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMAXPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMAXPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VMAXPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMAXPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMAXPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMAXPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VMAXPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMAXPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VMAXSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VMAXSD;
  Decode_Vo_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMAXSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VMAXSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMAXSS;
  Decode_Vo_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKLBW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKLBW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLBW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKLWD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKLWD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLWD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKLDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKLDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPACKSSWB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPACKSSWB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKSSWB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPGTB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPGTB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPGTW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPGTW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPGTD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPGTD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPACKUSWB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPACKUSWB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKUSWB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKHBW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKHBW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHBW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKHWD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKHWD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHWD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKHDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKHDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPACKSSDW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPACKSSDW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKSSDW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKLQDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKLQDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLQDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPUNPCKHQDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPUNPCKHQDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHQDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVQ_Vo_Eq(PInst: PInstruction);
begin
  // VMOVQ  Vo,Eq
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Vo_Eq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVD_Vo_Ed(PInst: PInstruction);
begin
  // VMOVD  Vo,Ed
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVD;
  Decode_Vo_Ed(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQA_Vo_Wo(PInst: PInstruction);
begin
  // VMOVDQA  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVDQA;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQU_Vo_Wo(PInst: PInstruction);
begin
  // VMOVDQU  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVDQU;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQA_Vy_Wy(PInst: PInstruction);
begin
  // VMOVDQA  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVDQA;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQU_Vy_Wy(PInst: PInstruction);
begin
  // VMOVDQU  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVDQU;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSHUFD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPSHUFD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSHUFD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSHUFLW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPSHUFLW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VPSHUFLW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSHUFHW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // VPSHUFHW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPSHUFHW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPEQB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPEQB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPEQW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPEQW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPCMPEQD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPCMPEQD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VZEROUPPER_void(PInst: PInstruction);
begin
  // VZEROUPPER  void
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VZEROUPPER;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VZEROUPPER);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VZEROALL_void(PInst: PInstruction);
begin
  // VZEROALL  void
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VZEROALL;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VZEROALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHADDPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VHADDPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VHADDPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHADDPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VHADDPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VHADDPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHADDPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VHADDPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VHADDPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHADDPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VHADDPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VHADDPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHSUBPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VHSUBPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VHSUBPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHSUBPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VHSUBPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VHSUBPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHSUBPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VHSUBPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VHSUBPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VHSUBPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VHSUBPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VHSUBPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VHSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVQ_Eq_Vo(PInst: PInstruction);
begin
  // VMOVQ  Eq,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Eq_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVD_Ed_Vo(PInst: PInstruction);
begin
  // VMOVD  Ed,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVD;
  Decode_Ed_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVQ_Vo_Wo_q(PInst: PInstruction);
begin
  // VMOVQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQA_Wo_Vo(PInst: PInstruction);
begin
  // VMOVDQA  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVDQA;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQU_Wo_Vo(PInst: PInstruction);
begin
  // VMOVDQU  Wo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVDQU;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQA_Wy_Vy(PInst: PInstruction);
begin
  // VMOVDQA  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVDQA;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVDQU_Wy_Vy(PInst: PInstruction);
begin
  // VMOVDQU  Wy,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMOVDQU;
  Decode_Wy_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VCMPccPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vo_Ho_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccPD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccPD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VCMPccPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  Decode_Vo_Ho_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccPS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccPS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VCMPccPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vy_Hy_Wy(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccPD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccPD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VCMPccPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  Decode_Vy_Hy_Wy(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccPS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccPS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccSD_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  // VCMPccSD  Vo,Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  Decode_Vo_Ho_Wo_q(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccSD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccSD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCMPccSS_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  // VCMPccSS  Vo,Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  Decode_Vo_Ho_Wo_d(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccSS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccSS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRW_Vo_Ho_Mw_Ib(PInst: PInstruction);
begin
  // VPINSRW  Vo,Ho,Mw,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRW;
  Decode_Vo_Ho_Mw_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPINSRW_Vo_Ho_Rv_Ib(PInst: PInstruction);
begin
  // VPINSRW  Vo,Ho,Rv,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPINSRW;
  Decode_Vo_Ho_Rv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPEXTRW_Gy_Uo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Gy,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Gy_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSHUFPD_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VSHUFPD  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSHUFPD;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSHUFPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSHUFPS_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  // VSHUFPS  Vo,Ho,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSHUFPS;
  Decode_Vo_Ho_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSHUFPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSHUFPD_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VSHUFPD  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VSHUFPD;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSHUFPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VSHUFPS_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VSHUFPS  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSHUFPS;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSHUFPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSUBPD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VADDSUBPD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VADDSUBPD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSUBPS_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VADDSUBPS  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VADDSUBPS;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSUBPD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VADDSUBPD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VADDSUBPD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VADDSUBPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VADDSUBPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VADDSUBPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSRLW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSRLD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSRLQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULLW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULLW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULLW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVQ_Wo_q_Vo(PInst: PInstruction);
begin
  // VMOVQ  Wo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Wo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMOVMSKB_Gy_Uo(PInst: PInstruction);
begin
  // VPMOVMSKB  Gy,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVMSKB;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVMSKB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBUSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBUSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBUSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBUSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBUSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBUSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINUB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINUB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPAND_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPAND  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAND;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDUSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDUSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDUSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDUSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDUSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDUSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXUB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXUB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPANDN_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPANDN  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPANDN;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPANDN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPAVGB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPAVGB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAVGB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRAW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSRAW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRAD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSRAD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPAVGW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPAVGW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAVGW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULHUW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULHUW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHUW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULHW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULHW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTPD2DQ_Vo_Wo(PInst: PInstruction);
begin
  // VCVTTPD2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTTPD2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPD2DQ_Vo_Wo(PInst: PInstruction);
begin
  // VCVTPD2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTPD2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTDQ2PD_Vo_Wo_q(PInst: PInstruction);
begin
  // VCVTDQ2PD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTDQ2PD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTDQ2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTTPD2DQ_Vo_Wy(PInst: PInstruction);
begin
  // VCVTTPD2DQ  Vo,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTTPD2DQ;
  Decode_Vo_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTPD2DQ_Vo_Wy(PInst: PInstruction);
begin
  // VCVTPD2DQ  Vo,Wy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VCVTPD2DQ;
  Decode_Vo_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VCVTDQ2PD_Vy_Wo(PInst: PInstruction);
begin
  // VCVTDQ2PD  Vy,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VCVTDQ2PD;
  Decode_Vy_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTDQ2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTDQ_Mo_Vo(PInst: PInstruction);
begin
  // VMOVNTDQ  Mo,Vo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTDQ;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMOVNTDQ_My_Vy(PInst: PInstruction);
begin
  // VMOVNTDQ  My,Vy
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTDQ;
  Decode_My_Vy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMINSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMINSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPOR_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPOR  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPOR;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDSB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDSB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDSB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMAXSW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMAXSW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPXOR_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPXOR  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPXOR;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPXOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VLDDQU_Vo_Mo(PInst: PInstruction);
begin
  // VLDDQU  Vo,Mo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VLDDQU;
  Decode_Vo_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VLDDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VLDDQU_Vy_My(PInst: PInstruction);
begin
  // VLDDQU  Vy,My
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VLDDQU;
  Decode_Vy_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VLDDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSLLW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSLLD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSLLQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMULUDQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMULUDQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULUDQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULUDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPMADDWD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPMADDWD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMADDWD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSADBW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSADBW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSADBW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VMASKMOVDQU_Vo_Uo(PInst: PInstruction);
begin
  // VMASKMOVDQU  Vo,Uo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMASKMOVDQU;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMASKMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSUBQ_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPSUBQ  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBQ;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDB_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDB  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDB;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDW_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDW  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDW;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPADDD_Vo_Ho_Wo(PInst: PInstruction);
begin
  // VPADDD  Vo,Ho,Wo
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDD;
  Decode_Vo_Ho_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLW_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRLW  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRAW_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRAW  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLW_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSLLW  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLD_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRLD  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLD;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRAD_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRAD  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAD;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLD_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSLLD  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLD;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLQ_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRLQ  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLQ;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSRLDQ_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSRLDQ  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLDQ;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLQ_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSLLQ  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLQ;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX_VPSLLDQ_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VPSLLDQ  Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLDQ;
  Decode_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

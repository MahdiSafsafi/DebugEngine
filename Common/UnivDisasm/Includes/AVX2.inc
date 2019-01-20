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
// The Original Code is AVX2.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX2_VPSHUFB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSHUFB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSHUFB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHADDW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHADDW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHADDD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHADDD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHADDSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHADDSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHADDSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMADDUBSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMADDUBSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMADDUBSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHSUBW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHSUBW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHSUBD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHSUBD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPHSUBSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPHSUBSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPHSUBSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPHSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSIGNB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSIGNB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGNB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSIGNW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSIGNW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGNW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGNW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSIGND_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSIGND  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSIGND;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSIGND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULHRSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULHRSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHRSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHRSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPERMPS_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPERMPS  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMPS;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VBROADCASTSS_Vx_Uo(PInst: PInstruction);
begin
  // VBROADCASTSS  Vx,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTSS;
  Decode_Vx_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VBROADCASTSD_Vy_Uo(PInst: PInstruction);
begin
  // VBROADCASTSD  Vy,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTSD;
  Decode_Vy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPABSB_Vy_Wy(PInst: PInstruction);
begin
  // VPABSB  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSB;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPABSW_Vy_Wy(PInst: PInstruction);
begin
  // VPABSW  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSW;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPABSD_Vy_Wy(PInst: PInstruction);
begin
  // VPABSD  Vy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPABSD;
  Decode_Vy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXBW_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVSXBW  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBW;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXBD_Vy_Wo_q(PInst: PInstruction);
begin
  // VPMOVSXBD  Vy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBD;
  Decode_Vy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXBQ_Vy_Wo_d(PInst: PInstruction);
begin
  // VPMOVSXBQ  Vy,Wo.d
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXBQ;
  Decode_Vy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXWD_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVSXWD  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXWD;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXWQ_Vy_Wo_q(PInst: PInstruction);
begin
  // VPMOVSXWQ  Vy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXWQ;
  Decode_Vy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVSXDQ_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVSXDQ  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVSXDQ;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPEQQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPEQQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VMOVNTDQA_Vy_My(PInst: PInstruction);
begin
  // VMOVNTDQA  Vy,My
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMOVNTDQA;
  Decode_Vy_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVNTDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPACKUSDW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPACKUSDW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKUSDW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXBW_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVZXBW  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBW;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXBD_Vy_Wo_q(PInst: PInstruction);
begin
  // VPMOVZXBD  Vy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBD;
  Decode_Vy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXBQ_Vy_Wo_d(PInst: PInstruction);
begin
  // VPMOVZXBQ  Vy,Wo.d
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXBQ;
  Decode_Vy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXWD_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVZXWD  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXWD;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXWQ_Vy_Wo_q(PInst: PInstruction);
begin
  // VPMOVZXWQ  Vy,Wo.q
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXWQ;
  Decode_Vy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVZXDQ_Vy_Wo_o(PInst: PInstruction);
begin
  // VPMOVZXDQ  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVZXDQ;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPERMD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPERMD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINSD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINSD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINUW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINUW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINUD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINUD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXSD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXSD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXUW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXUW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXUD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXUD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULLD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULLD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULLD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLVQ_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPSRLVQ  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLVQ;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLVD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPSRLVD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLVD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRAVD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPSRAVD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAVD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLVQ_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPSLLVQ  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLVQ;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLVD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VPSLLVD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLVD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTD_Vx_Mo_d(PInst: PInstruction);
begin
  // VPBROADCASTD  Vx,Mo.d
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTD;
  Decode_Vx_Mo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTD_Vx_Uo(PInst: PInstruction);
begin
  // VPBROADCASTD  Vx,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTD;
  Decode_Vx_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTQ_Vx_Mo_q(PInst: PInstruction);
begin
  // VPBROADCASTQ  Vx,Mo.q
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTQ;
  Decode_Vx_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTQ_Vx_Uo(PInst: PInstruction);
begin
  // VPBROADCASTQ  Vx,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTQ;
  Decode_Vx_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VBROADCASTI128_Vy_Mo(PInst: PInstruction);
begin
  // VBROADCASTI128  Vy,Mo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VBROADCASTI128;
  Decode_Vy_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTI128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTB_Vx_Mo_b(PInst: PInstruction);
begin
  // VPBROADCASTB  Vx,Mo.b
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTB;
  Decode_Vx_Mo_b(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTB_Vx_Uo(PInst: PInstruction);
begin
  // VPBROADCASTB  Vx,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTB;
  Decode_Vx_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTW_Vx_Mo_w(PInst: PInstruction);
begin
  // VPBROADCASTW  Vx,Mo.w
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTW;
  Decode_Vx_Mo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBROADCASTW_Vx_Uo(PInst: PInstruction);
begin
  // VPBROADCASTW  Vx,Uo
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTW;
  Decode_Vx_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMASKMOVQ_Vx_Hx_Mx(PInst: PInstruction);
begin
  // VPMASKMOVQ  Vx,Hx,Mx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMASKMOVQ;
  Decode_Vx_Hx_Mx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMASKMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMASKMOVD_Vx_Hx_Mx(PInst: PInstruction);
begin
  // VPMASKMOVD  Vx,Hx,Mx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMASKMOVD;
  Decode_Vx_Hx_Mx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMASKMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMASKMOVQ_Mx_Hx_Vx(PInst: PInstruction);
begin
  // VPMASKMOVQ  Mx,Hx,Vx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMASKMOVQ;
  Decode_Mx_Hx_Vx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMASKMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMASKMOVD_Mx_Hx_Vx(PInst: PInstruction);
begin
  // VPMASKMOVD  Mx,Hx,Vx
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMASKMOVD;
  Decode_Mx_Hx_Vx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMASKMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPERMQ_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VPERMQ  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMQ;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPERMPD_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VPERMPD  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERMPD;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBLENDD_Vx_Hx_Wx_Ib(PInst: PInstruction);
begin
  // VPBLENDD  Vx,Hx,Wx,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBLENDD;
  Decode_Vx_Hx_Wx_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBLENDW_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VPBLENDW  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBLENDW;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPALIGNR_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VPALIGNR  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPALIGNR;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPALIGNR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VINSERTI128_Vy_Hy_Wo_Ib(PInst: PInstruction);
begin
  // VINSERTI128  Vy,Hy,Wo,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VINSERTI128;
  Decode_Vy_Hy_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTI128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VEXTRACTI128_Wo_Vy_Ib(PInst: PInstruction);
begin
  // VEXTRACTI128  Wo,Vy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VEXTRACTI128;
  Decode_Wo_Vy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTI128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VMPSADBW_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VMPSADBW  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMPSADBW;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPERM2I128_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  // VPERM2I128  Vy,Hy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPERM2I128;
  Decode_Vy_Hy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERM2I128);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPBLENDVB_Vy_Hy_Wy_Ly(PInst: PInstruction);
begin
  // VPBLENDVB  Vy,Hy,Wy,Ly
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBLENDVB;
  Decode_Vy_Hy_Wy_Ly(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKLBW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKLBW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLBW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKLWD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKLWD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLWD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKLDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKLDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPACKSSWB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPACKSSWB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKSSWB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPGTB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPGTB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPGTW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPGTW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPGTD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPGTD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPGTD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPACKUSWB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPACKUSWB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKUSWB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKHBW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKHBW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHBW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKHWD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKHWD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHWD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKHDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKHDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPACKSSDW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPACKSSDW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPACKSSDW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKLQDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKLQDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKLQDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPUNPCKHQDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPUNPCKHQDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPUNPCKHQDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSHUFD_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VPSHUFD  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSHUFD;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSHUFLW_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VPSHUFLW  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_VPSHUFLW;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSHUFHW_Vy_Wy_Ib(PInst: PInstruction);
begin
  // VPSHUFHW  Vy,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPSHUFHW;
  Decode_Vy_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPEQB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPEQB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPEQW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPEQW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPCMPEQD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPCMPEQD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPCMPEQD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSRLW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSRLD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSRLQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULLW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULLW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULLW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMOVMSKB_Gy_Uy(PInst: PInstruction);
begin
  // VPMOVMSKB  Gy,Uy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMOVMSKB;
  Decode_Gy_Uy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVMSKB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBUSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBUSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBUSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBUSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBUSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBUSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINUB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINUB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINUB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPAND_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPAND  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAND;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDUSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDUSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDUSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDUSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDUSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDUSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXUB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXUB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXUB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPANDN_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPANDN  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPANDN;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPANDN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPAVGB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPAVGB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAVGB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRAW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSRAW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRAD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSRAD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPAVGW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPAVGW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPAVGW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULHUW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULHUW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHUW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULHW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULHW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULHW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMINSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMINSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMINSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPOR_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPOR  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPOR;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDSB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDSB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDSB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMAXSW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMAXSW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMAXSW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPXOR_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPXOR  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPXOR;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPXOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSLLW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSLLD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSLLQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMULUDQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMULUDQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMULUDQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULUDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPMADDWD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPMADDWD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPMADDWD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSADBW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSADBW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSADBW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSUBQ_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPSUBQ  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSUBQ;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDB_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDB  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDB;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDW_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDW  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDW;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPADDD_Vy_Hy_Wy(PInst: PInstruction);
begin
  // VPADDD  Vy,Hy,Wy
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPADDD;
  Decode_Vy_Hy_Wy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLW_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRLW  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRAW_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRAW  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLW_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSLLW  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLD_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRLD  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLD;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRAD_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRAD  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRAD;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLD_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSLLD  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLD;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLQ_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRLQ  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLQ;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSRLDQ_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSRLDQ  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSRLDQ;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLQ_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSLLQ  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLQ;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VPSLLDQ_Hy_Uy_Ib(PInst: PInstruction);
begin
  // VPSLLDQ  Hy,Uy,Ib
  PInst^.InstGroups := INST_GRP_AVX2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPSLLDQ;
  Decode_Hy_Uy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

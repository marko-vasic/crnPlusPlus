(* ::Package:: *)

Get[NotebookDirectory[]<>"init.m"];

(* This is the direction of the ray from camera that corresponds to pixel *)
rayMarchNormedDir[pixelX_, pixelY_] :=
  Module[{dir = mat . {1, pixelX, pixelY}}, dir / Total[dir]];

rayMarchRsys::usage="CRN++ implementation of Ray Marching algorithm.
Arguments:
- pixelX: pixel x coordinate (0..1)
- pixelY: pixel y coordinate (0..1)
- fbRate: convergance speed (0..1)
Return value: pixel brightness (0 - black, 1 - white) at provided coordinates.";

rayMarchRsys[pixelX_, pixelY_, fbRate_] := {
  (* we omit initial transformations of input coordinates *)
  conc[dirX, rayMarchNormedDir[pixelX, pixelY][[1]]],
  conc[dirY, rayMarchNormedDir[pixelX, pixelY][[2]]],
  conc[dirZ, rayMarchNormedDir[pixelX, pixelY][[3]]],
  
  (* negative parts of x, y, z coordinates like in dual-rail. They are constants. *)
  conc[xNegative, cam[[1]]],
  conc[yNegative, cam[[2]]],
  conc[zNegative, cam[[3]]],

  conc[cubeSize, 0.3], (* cube size, const *)
  conc[feedbackRate, fbRate], (* feedback rate, const *)

  conc[v, 0], (* accumulator, same v as in ray marching *)

  (* calculate positive parts of x, y, z coordinates *)
  mul[v, dirX, xPositive],
  mul[v, dirY, yPositive],
  mul[v, dirZ, zPositive],

  (* we use identity: Abs[x - y] = add[sub[x, y], sub[y, x]] *)
  (* xAbsolute = xa1 + xa2 = Abs[xPositive - xNegative] *)
  sub[xNegative, xPositive, xa1],
  sub[xPositive, xNegative, xa2],
  add[xa1, xa2, xAbsolute],
  (* yAbsolute = ya1 + ya2 = Abs[yPositive - yNegative] *)
  sub[yNegative, yPositive, ya1],
  sub[yPositive, yNegative, ya2],
  add[ya1, ya2, yAbsolute],
  (* zAbsolute = za1 + za2 = Abs[zPositive - zNegative] *)
  sub[zNegative, zPositive, za1],
  sub[zPositive, zNegative, za2],
  add[za1, za2, zAbsolute],

  (* we use identity: Max[a, b] = add[sub[a, b], b] *)
  (* maxXY = Max[xAbsolute, yAbsolute] *)
  sub[xAbsolute, yAbsolute, maxXYtemp],
  add[maxXYtemp, yAbsolute, maxXY],

  (* maxXYZ = Max[maxXY, zAbsolute] *)
  sub[zAbsolute, maxXY, maxXYZtemp],
  add[maxXYZtemp, maxXY, maxXYZ],

  (* feedbackRaw = maxXYZ - cubeSize *)
  sub[maxXYZ, cubeSize, feedbackRaw],

  (* feedback = feedbackRaw * feedbackRate *)
  mul[feedbackRaw, feedbackRate, feedback],

  (* nextV = v + feedback; v = nextV *)
  step[{
    add[v, feedback, nextV]
  }],
  step[{
    ld[nextV, v]
  }]
};

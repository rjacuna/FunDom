MAXGENUS := 0;

/////////////////////////////////////////////////////////////////
// precompute list od sl2 and gl2 for later use
/////////////////////////////////////////////////////////////////

sl2_generators := function(p,N)
// in permutation representation
// print "sl2_generators",p,N;
//////////////
if p eq 2 then
//////////////
  n := p^(2*N);
  m := p^N;

  T:=[1..n-1];
  for i:=1 to n-1 do
    a:=i mod m;
    b:=i div m;
    T[i]:=a+(a+b mod m)*m mod n;
  end for;

  S:=[1..n-1];
  for i:=1 to n-1 do
    a:=i mod m;
    b:=i div m;
    S[i]:=((-b mod m)+a*m) mod n;
  end for;

  return S,T;

//////////////
else // p ne 2
//////////////
  n := p^(N+1);
  m := p^N;

  T:=[1..n-1];
  for i:=1 to n-1 do
    a := i mod p;
    b := i div p;
    if a eq 0 and (b mod p) eq 0 then
      T[i] := i;
    elif a ne 0 then
      T[i] :=a+(a+b)*p mod n;
    else
      a0 := b mod p;
      b0 := b - a0;
      b1 := b0*InverseMod(a0+b0,m)*( (a0+b0) mod p) mod m;
      T[i] :=p*(a0+b1);
    end if;
  end for;

  S:=[1..n-1];
  for i:=1 to n-1 do
    a:= i mod p;
    b:= i div p;
    if a eq 0 and (b mod p) eq 0 then
      S[i] := i;
    elif a ne 0 and (b mod p) eq 0 then
      S[i] := p*(a+(-b mod m));
    elif a ne 0 and (b mod p) ne 0 then
      S[i] := (-b mod p) + ((a*(-b mod p)*InverseMod(-b,m)) mod m)*p;
    elif a eq 0 and (b mod p) ne 0 then
      a0 := b mod p;
      b0 := b - a0;
      S[i] :=(-a0 mod p) + p*(b0*(-a0 mod p)*InverseMod(-a0,m) mod m);
    end if;
  end for;

  return S,T;
/////////////
end if;
/////////////
end function;


sl2_pre := function(m)

  print "computing SL( 2,",m,")";

  factL := Factorization(m);

  M := 0;
  S := [ ];
  T := [ ];
  for fact in factL do
    s,t := sl2_generators(fact[1],fact[2]);
    ss := [ u+M : u in s ];
    tt := [ u+M : u in t ];
    S cat:= ss;
    T cat:= tt;
    M := M + #s;
  end for;
  G := Sym(M);
  gens:=[G|S,T];
  H:=sub<G | gens>;
  H:=DegreeReduction(H);

  return H;
end function;
/////////////////////////////////////////////////////////////////

gl2_prim := function(p,N,prim)

n:=p^(2*N);
m1:=p^N;

L:=[1..n-1];
for i:=1 to n-1 do
 a:=i mod m1;
 b:=i div m1;
 L[i]:=a+(prim*b mod m1)*m1  mod n;
end for;

return L;
end function;


gl2_extra_generators := function(m)

  U,phi:=UnitGroup(quo<Integers()|m>);
  if m ne 2 then
   ugens:=[Integers()!phi(i) : i in Generators(U)]; 
  else
   ugens:=[1];
  end if;
  gens:=[];

  factL := Factorization(m);

  for u in ugens do
   M := 0;
   D := [ ];
   for fact in factL do
     d := gl2_prim(fact[1],fact[2],u); 
     dd := [ u+M : u in d ];
     D cat:= dd;
     M := M + #d;
   end for;
   Append(~gens,D);
  end for;

  return(gens);  
end function;

///////////////////////////////////////////////////////////////


nsl2_generators := function(p,N)

  n:=p^(2*N);
  m1:=p^N;

  L:=[1..n-1];
  for i:=1 to n-1 do
   a:=i mod m1;
   b:=i div m1;
   L[i]:=a+(a+b mod m1)*m1  mod n;
  end for;

  Y:=L;


 for i:=1 to n-1 do
   a:=i mod m1;
   b:=i div m1;
   L[i]:=((-b mod m1)+a*m1) mod n;
  end for;

  X:=L;

  return X,Y;
end function;



nsl2 := function(m)

  factL := Factorization(m);

  M := 0;
  S := [ ];
  T := [ ];
  for fact in factL do
//    print M;
    s,t := nsl2_generators(fact[1],fact[2]); 
    ss := [ u+M : u in s ];
    tt := [ u+M : u in t ];
    S cat:= ss;
    T cat:= tt;
    M := M + #s;
  end for;

  G := Sym(M);
  gens:=[G|S,T];
  H:=sub<G | gens>;
  return(H);  

end function;

////////////////////////////////////////////////////////////////

gl2_pre:=function(m);

  g1:=nsl2(m);
  gens:=gl2_extra_generators(m);
  M:=Degree(g1);
  H:=sub<Sym(M)|g1,gens>;
  D:=sub<Sym(M)|gens>;
  return H,D;
end function;

///////////////////////////////////////////////////////////////////

significant_levels := function(N,prime_bound)

  list := [1..N];
  for i := 2 to N do
    factL := Factorization(i);
    if factL[#factL][1] gt prime_bound then
      Undefine(~list,i);
    end if;
  end for;
  res1 := [ ];
  for i := 2 to N do
    if IsDefined(list,i) then
      Append(~res1,i);
    end if;
  end for;

  return res1;

end function;

level_bound := function(genus)

  if genus eq 0 then
    return 168;
  else
    return  Floor(12*genus+(1/2)*(13*Sqrt(48*genus+121)+145));
  end if;
end function;

////////////////////////////////////////////////////////////////////


FillLists := procedure(genus, ~sl2_list, ~gl2_list)
//////////////////////////////////////
// fill the list ~sl2_list with all principal congruence subgroups
// (in permutation representation as returned by sl2_pre, which is
//  the same as sl2 without the table lookup), which are needed to
// compute all congruence groups of genus up to genus
//////////////////////////////////////
  sl2_list[1] := sub<Sym(1) | [1],[1]>;
  prime_bound := 12*genus+13;
  levels := significant_levels(level_bound(genus),prime_bound);
  for level in levels do
    sl2_list[level] := sl2_pre(level);
    g1,g2 := gl2_pre(level);
    gl2_list[level] := [g1,g2];
  end for;
end procedure;

////////////////////////////////////////////////////////////////////////
// we precompute a list containing permutation representations of
// all principal congruence subgroups needed to compute all congruence
// subgroups up to MAXGENUS
////////////////////////////////////////////////////////////////////////

SL2 := [ ];
GL2 := [ ];
FillLists(MAXGENUS,~SL2, ~GL2);



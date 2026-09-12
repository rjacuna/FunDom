
/////////////////////////////////////////
// the generators represented as matrices
//////////////////////////////////////////////////////////////////
// comparison functions
//////////////////////////////////////////////////////////////////


_fl:=function(x,y);

  if #x ne #y then return #x-#y;end if;
  for i:=1 to Minimum(#x,#y) do
    if x[i] ne y[i] then return x[i]-y[i];end if;
  end for;
//  if #x ne #y then return #x-#y;end if;
  return 0;
end function;


_fl2:=function(x,y);

//  if #x ne #y then return #x-#y;end if;
  for i:=1 to Minimum(#x,#y) do
    if x[i] ne y[i] then return x[i]-y[i];end if;
  end for;
  if #x ne #y then return #x-#y;end if;
  return 0;
end function;


_fp:=function(x,y);

  if #x ne #y then return #x-#y;end if;
  for i:=1 to #x do
    a:=StringToCode(x[i][2]);
    b:=StringToCode(y[i][2]);
    if x[i][1] ne y[i][1] then return x[i][1]-y[i][1];end if;
    if x[i][3] ne y[i][3] then return x[i][3]-y[i][3];end if;
    if a ne b then return a-b;end if;
  end for;
  return 0;
end function;


csg_compare := function(x,y)

 if assigned(x`genus) and assigned(y`genus) then
  if x`genus ne y`genus then return x`genus-y`genus; end if;
 end if;
 if assigned(x`level) and assigned(y`level) then
  if x`level ne y`level then return x`level-y`level; end if;
 end if;
 if assigned(x`index) and assigned(y`index) then
  if x`index ne y`index then return x`index-y`index; end if;
 end if;
 if assigned(x`contains_minus_one) and assigned(y`contains_minus_one) then
  if x`contains_minus_one ne y`contains_minus_one then 
   if x`contains_minus_one then return 1; else return -1;end if;
  end if;
 end if;
 if assigned(x`GLZConj) and assigned(y`GLZConj) then
  if x`GLZConj ne y`GLZConj then return x`GLZConj-y`GLZConj; end if;
 end if;
 if assigned(x`length) and assigned(y`length) then
  if x`length ne y`length then return x`length-y`length; end if;
 end if;
 if assigned(x`c2) and assigned(y`c2) then
  if _fl(x`c2,y`c2) ne 0 then return _fl(x`c2,y`c2);end if;
 end if;
 if assigned(x`c3) and assigned(y`c3) then
  if _fl(x`c3,y`c3) ne 0 then return _fl(x`c3,y`c3);end if;
 end if;
 if assigned(x`cusps) and assigned(y`cusps) then
  if _fl(x`cusps,y`cusps) ne 0 then return _fl(x`cusps,y`cusps);end if;
 end if;
 if assigned(x`gc) and assigned(y`gc) then
  if _fl(x`gc,y`gc) ne 0 then return _fl(x`gc,y`gc);end if;
 end if;
 if assigned(x`direct_supergroups) and assigned(y`direct_supergroups) then
  if _fl(x`direct_supergroups,y`direct_supergroups) ne 0 then 
   return _fl(x`direct_supergroups,y`direct_supergroups);
  end if;
 end if;
 if assigned(x`direct_subgroups) and assigned(y`direct_subgroups) then
  if _fl2(x`direct_subgroups,y`direct_subgroups) ne 0 then 
   return _fl2(x`direct_subgroups,y`direct_subgroups);
  end if;
 end if;
return 0;
end function;
/////////////////////////////////////////



csg_compare2 := function(x,y)

 if assigned(x`genus) and assigned(y`genus) then
  if x`genus ne y`genus then return x`genus-y`genus; end if;
 end if;
 if assigned(x`level) and assigned(y`level) then
  if x`level ne y`level then return x`level-y`level; end if;
 end if;
 if assigned(x`index) and assigned(y`index) then
  if x`index ne y`index then return x`index-y`index; end if;
 end if;
 if assigned(x`contains_minus_one) and assigned(y`contains_minus_one) then
  if x`contains_minus_one ne y`contains_minus_one then 
   if x`contains_minus_one then return 1; else return -1;end if;
  end if;
 end if;
 if assigned(x`GLZConj) and assigned(y`GLZConj) then
  if x`GLZConj ne y`GLZConj then return x`GLZConj-y`GLZConj; end if;
 end if;
 if assigned(x`length) and assigned(y`length) then
  if x`length ne y`length then return x`length-y`length; end if;
 end if;
 if assigned(x`c2) and assigned(y`c2) then
  if _fl(x`c2,y`c2) ne 0 then return _fl(x`c2,y`c2);end if;
 end if;
 if assigned(x`c3) and assigned(y`c3) then
  if _fl(x`c3,y`c3) ne 0 then return _fl(x`c3,y`c3);end if;
 end if;
 if assigned(x`cusps) and assigned(y`cusps) then
  if _fl(x`cusps,y`cusps) ne 0 then return _fl(x`cusps,y`cusps);end if;
 end if;
 if assigned(x`gc) and assigned(y`gc) then
  if _fl(x`gc,y`gc) ne 0 then return _fl(x`gc,y`gc);end if;
 end if;
 if assigned(x`direct_supergroups) and assigned(y`direct_supergroups) then
  if _fl(x`direct_supergroups,y`direct_supergroups) ne 0 then 
   return _fl(x`direct_supergroups,y`direct_supergroups);
  end if;
 end if;
return 0;
end function;
/////////////////////////////////////////


csg_is_subgroup := function(grp1, grp2)
///////////////////////////////////////
// returns true if grp2 is a subgroup of grp1
///////////////////////////////////////

  if grp1`index eq 1 then return true; end if;
  if grp2`index eq 1 and grp1`index ne 1 then return false; end if;

  if grp1`genus gt grp2`genus then return false; end if;
  if grp1`level gt grp2`level then return false; end if;
  if grp1`index gt grp2`index then return false; end if;
  if grp2`level mod grp1`level ne 0 then return false; end if;
//print grp1;
//print grp2;

// print "subgroup ?";

  G2 := csg_perm_group(grp2);
  gam := sl2(grp2`level);
  G1 := sub<gam |  fgamma(gam,sl2(grp1`level)),
                   flisttogroup(gam, grp1`generators)>;

  R := RightTransversal(gam, G1);
  for r in R do
    if G2 subset G1^(r^-1) or G2 subset G1^(r) then
      return true;
    end if;
  end for;

  return false;

end function;


/////////////////////////////////////////

_csg_G:=function(n)
return GeneralLinearGroup(2,quo<Integers()|n>);
end function;

_csg_M:=function(n)
return sub<_csg_G(n) | [0,1,-1,0], [1,1,0,1]>;
end function;

_matgens:=function(level,glist)
 return ElementToSequence(flisttoelt(_csg_M(level),glist));
end function;

csg_matrix_generators:=function(grp);

  mgens:=[];
  gens:=grp`generators;
  for i:=1 to #gens do
    Append(~mgens,_matgens(grp`level,gens[i]));
  end for;
  return mgens;
end function;

/////////////////////////////////////////////

inj:=function(m);
  return hom<sl2(m) -> gl2(m) | gl2(m).1,gl2(m).2>;
end function;

/////////////////////////////////////////////

_csg_c3_sub := function(G,S)
   if (G.1*G.2)^3 eq G.0 then 
    T := sub<G | G.1^3*G.2>;
   else
    T := sub<G | G.1*G.2>;
  end if;
  c := CosetTable(G,S);
  p := CosetTableToRepresentation(G,c);
  lev :=  [#l : l in Orbits(T@p)];
  Sort(~lev);
  return lev;
end function;

csg_c3:= function(grp);
  if grp`index eq 1 then
    return [1];
  else  
    c:=_csg_c3_sub(sl2(grp`level),flisttogroup(sl2(grp`level),grp`generators));
    return c;
  end if;
end function;

///////////////////////////////////////////////

_csg_c2_sub := function(G,S)
  T := sub<G | G.1>;
  c := CosetTable(G,S);
//print "coset table",c;
  p := CosetTableToRepresentation(G,c);
//print "representation",p;
//print "T@p", T@p;
//print "gen", Generators(T@p);
//print "orbits",Orbits(T@p);
  lev :=  [#l : l in Orbits(T@p)];
  Sort(~lev);
  return lev;
end function;

csg_c2:= function(grp);
  if grp`index eq 1 then
    return [1];
  else
    c:=_csg_c2_sub(sl2(grp`level), csg_perm_group(grp));
    return c;
  end if;
end function;

/////////////////////////////////////////////////

number_of_fixed_points:= function(c);
  return #[i : i in c | i eq 1];
end function;


////////////////////////////////////////////////

_csg_cusp_widths_sub := function(G,S)

    T := sub<G | G.2>;
    c := CosetTable(G,S);
    p := CosetTableToRepresentation(G,c);
    lev :=  [#l : l in Orbits(T@p)];
    Sort(~lev);
    return lev;
end function;

csg_cusp_widths := function(grp);
   
  if grp`index eq 1 then
    return [1];
  else          
    cusps:=_csg_cusp_widths_sub
           (sl2(grp`level), csg_perm_group(grp));
    return cusps;
  end if;
end function;

////////////////////////////////////////////////

csg_length:=function(grp);
  s:=sl2(grp`level);
  g:=flisttogroup(s,grp`generators);
  len:=#s/#Normaliser(s,g);
  return len;
end function;

/////////////////////////////////////////////////

csg_galois_conjugates:=function(g)


  if g`index eq 1 then
    return [1];
  end if;
  
  s:=sl2(g`level);
  gm:=flisttogroup(s,g`generators);
  gl,dd:=gl2(g`level);
  phi:=inj(g`level);
  pg:=phi(gm);
//  ps:=phi(s);
  n:=Normaliser(gl,pg);
  c := CosetTable(gl,n);
  p := CosetTableToRepresentation(gl,c);
  lc:= [#l : l in Orbits(dd@p)];
  Sort(~lc);
  
  return lc;
end function;


/*
  if g`index eq 1 then
    return [1];
  else
    lc:=[];
    s:=sl2(g`level);
    gm:=flisttogroup(s,g`generators);
    gl,dd := gl2(g`level);
    phi:=hom<sl2(g`level) -> gl | gl.1,gl.2>;
    pg:=phi(gm);
    ps:=phi(s);
    n:=Normaliser(ps,pg);
    c := Transversal(ps,n);
    for j in c do
      ng:=pg^j;
      Append(~lc,#{ng^k:k in dd});
    end for;
    Sort(~lc);
    return lc;
  end if;
end function;
*/














